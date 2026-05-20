const PIECE_CODES = {
  P: "wP", N: "wN", B: "wB", R: "wR", Q: "wQ", K: "wK",
  p: "bP", n: "bN", b: "bB", r: "bR", q: "bQ", k: "bK",
};

let current = null;
let selected = null;
let flipped = false;
let lastMove = null;
const history = [];

const boardEl = document.getElementById("board");
const movesListEl = document.getElementById("movesList");
const openingListEl = document.getElementById("openingList");
const topGamesListEl = document.getElementById("topGamesList");
const topGamesMessageEl = document.getElementById("topGamesMessage");
const emptyMessageEl = document.getElementById("emptyMessage");
const statusTextEl = document.getElementById("statusText");
const fenInputEl = document.getElementById("fenInput");

function squareName(file, rank) {
  return "abcdefgh"[file] + String(rank + 1);
}

function parseFenBoard(fen) {
  const placement = fen.split(" ")[0];
  const out = {};
  let rank = 7;
  let file = 0;
  for (const ch of placement) {
    if (ch === "/") {
      rank -= 1;
      file = 0;
    } else if (/\d/.test(ch)) {
      file += Number(ch);
    } else {
      out[squareName(file, rank)] = ch;
      file += 1;
    }
  }
  return out;
}

function renderBoard() {
  const pieces = parseFenBoard(current.fen);
  const legalTargets = selected
    ? new Set(current.legal_moves.filter(m => m.from === selected).map(m => m.to))
    : new Set();
  boardEl.innerHTML = "";
  for (let row = 0; row < 8; row += 1) {
    for (let col = 0; col < 8; col += 1) {
      const file = flipped ? 7 - col : col;
      const rank = flipped ? row : 7 - row;
      const sq = squareName(file, rank);
      const square = document.createElement("div");
      square.className = `square ${(file + rank) % 2 ? "light" : "dark"}`;
      if (lastMove && (sq === lastMove.from || sq === lastMove.to)) square.classList.add("last-move");
      if (sq === selected) square.classList.add("selected");
      if (legalTargets.has(sq)) square.classList.add("legal");
      if (legalTargets.has(sq) && pieces[sq]) square.classList.add("capture");
      square.dataset.square = sq;
      square.addEventListener("click", onSquareClick);
      square.addEventListener("dragover", ev => ev.preventDefault());
      square.addEventListener("drop", onDrop);

      const piece = pieces[sq];
      if (piece) {
        const pieceEl = document.createElement("img");
        pieceEl.className = "piece";
        pieceEl.src = `/assets/pieces/cburnett/${PIECE_CODES[piece]}.svg`;
        pieceEl.alt = PIECE_CODES[piece];
        pieceEl.decoding = "async";
        pieceEl.draggable = true;
        pieceEl.addEventListener("dragstart", ev => {
          selected = sq;
          ev.dataTransfer.setData("text/plain", sq);
          renderBoard();
        });
        square.appendChild(pieceEl);
      }
      if (row === 7) {
        const coord = document.createElement("span");
        coord.className = "coord file";
        coord.textContent = "abcdefgh"[file];
        square.appendChild(coord);
      }
      if (col === 0) {
        const coord = document.createElement("span");
        coord.className = "coord rank";
        coord.textContent = String(rank + 1);
        square.appendChild(coord);
      }
      boardEl.appendChild(square);
    }
  }
}

function moveSquaresFromUci(uci) {
  return {
    from: uci.slice(0, 2),
    to: uci.slice(2, 4),
  };
}

function legalMovesFrom(square) {
  return current.legal_moves.filter(move => move.from === square);
}

function chooseMove(from, to) {
  const candidates = current.legal_moves.filter(move => move.from === from && move.to === to);
  if (!candidates.length) return null;
  if (candidates.length === 1) return candidates[0];
  const promotion = prompt("Promote to q, r, b, or n", "q") || "q";
  return candidates.find(move => move.promotion === promotion.toLowerCase()) || candidates[0];
}

function onSquareClick(event) {
  const sq = event.currentTarget.dataset.square;
  if (selected) {
    const move = chooseMove(selected, sq);
    if (move) {
      playMove(move.uci);
      return;
    }
  }
  selected = legalMovesFrom(sq).length ? sq : null;
  renderBoard();
}

function onDrop(event) {
  event.preventDefault();
  const from = event.dataTransfer.getData("text/plain") || selected;
  const to = event.currentTarget.dataset.square;
  const move = chooseMove(from, to);
  if (move) {
    playMove(move.uci);
  }
}

async function loadPosition(fen = "startpos", options = {}) {
  const {
    pushHistory = false,
    restoredLastMove = null,
    clearHistory = false,
  } = options;
  const response = await fetch(`/api/position?fen=${encodeURIComponent(fen)}`);
  const payload = await response.json();
  if (!response.ok || payload.error) {
    alert(payload.error || "Could not load position.");
    return;
  }
  if (clearHistory) history.length = 0;
  if (pushHistory && current) history.push({fen: current.fen, lastMove});
  current = payload;
  lastMove = restoredLastMove;
  selected = null;
  renderAll();
}

async function playMove(uci) {
  const response = await fetch("/api/move", {
    method: "POST",
    headers: {"Content-Type": "application/json"},
    body: JSON.stringify({fen: current.fen, move: uci}),
  });
  const payload = await response.json();
  if (!response.ok || payload.error) {
    alert(payload.error || "Illegal move.");
    selected = null;
    renderBoard();
    return;
  }
  history.push({fen: current.fen, lastMove});
  current = payload;
  lastMove = moveSquaresFromUci(uci);
  selected = null;
  renderAll();
}

function undoMove() {
  const previous = history.pop();
  if (previous) loadPosition(previous.fen, {restoredLastMove: previous.lastMove});
}

function renderAll() {
  fenInputEl.value = current.fen;
  statusTextEl.textContent = `${current.turn} to move · ${current.total_games.toLocaleString()} Titled Tuesday position occurrences in the first 10 moves`;
  renderBoard();
  renderOpenings();
  renderTopGames();
  renderMoves();
}

function renderOpenings() {
  openingListEl.innerHTML = "";
  if (!current.opening_names.length || current.ply === 0) {
    return;
  }
  const item = current.opening_names[0];
  const row = document.createElement("div");
  row.className = "opening-pill";
  const label = item.opening_name || item.eco || "Unknown opening";
  row.innerHTML = `<strong>${escapeHtml(label)}</strong><span>${item.count.toLocaleString()}</span>`;
  openingListEl.appendChild(row);
}

function pct(value) {
  return `${Math.round(value * 100)}%`;
}

function elo(value) {
  return value == null ? "?" : Math.round(value).toLocaleString();
}

function resultClass(result) {
  if (result === "1-0") return "white-win";
  if (result === "0-1") return "black-win";
  if (result === "1/2-1/2") return "draw";
  return "";
}

function continuationText(game) {
  return game.continuation_san || "No continuation recorded";
}

function renderTopGames() {
  topGamesListEl.innerHTML = "";
  const topGames = current.top_games || [];
  topGamesMessageEl.textContent = current.top_games_message || "";
  topGamesMessageEl.hidden = Boolean(topGames.length);
  if (!topGames.length) return;

  for (const game of topGames) {
    const row = document.createElement(game.link ? "a" : "div");
    row.className = "game-row";
    if (game.link) {
      row.href = game.link;
      row.target = "_blank";
      row.rel = "noreferrer";
    }
    row.innerHTML = `
      <div class="game-rank">${game.rank}</div>
      <div class="game-main">
        <div class="game-players">
          <span>${escapeHtml(game.white)}</span>
          <span class="elo">${elo(game.white_elo)}</span>
          <span class="versus">vs</span>
          <span>${escapeHtml(game.black)}</span>
          <span class="elo">${elo(game.black_elo)}</span>
        </div>
        <div class="game-subline">
          <span>${escapeHtml(game.date || "")}</span>
          <span class="continuation">${escapeHtml(continuationText(game))}</span>
        </div>
      </div>
      <div class="game-result ${resultClass(game.result)}">${escapeHtml(game.result || "*")}</div>
    `;
    topGamesListEl.appendChild(row);
  }
}

function renderMoves() {
  movesListEl.innerHTML = "";
  emptyMessageEl.hidden = current.in_database;
  emptyMessageEl.textContent = current.message || "";
  if (!current.in_database) return;
  for (const move of current.top_moves) {
    const row = document.createElement("div");
    row.className = "move-row";
    row.addEventListener("click", () => playMove(move.uci));
    const avgRating = move.avg_rating == null ? "n/a" : Math.round(move.avg_rating).toLocaleString();
    row.innerHTML = `
      <div class="san">${escapeHtml(move.san)}</div>
      <div class="metric">${move.count.toLocaleString()} plays</div>
      <div class="metric">avg ${avgRating}</div>
      <div class="winbar" style="--w:${Math.max(move.white_pct * 100, 3)}fr;--d:${Math.max(move.draw_pct * 100, 3)}fr;--b:${Math.max(move.black_pct * 100, 3)}fr">
        <div class="bar-white">${pct(move.white_pct)}</div>
        <div class="bar-draw">${pct(move.draw_pct)}</div>
        <div class="bar-black">${pct(move.black_pct)}</div>
      </div>
    `;
    movesListEl.appendChild(row);
  }
}

function escapeHtml(value) {
  return String(value)
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;");
}

document.getElementById("resetBtn").addEventListener("click", () => loadPosition("startpos", {clearHistory: true}));
document.getElementById("undoBtn").addEventListener("click", undoMove);
document.getElementById("flipBtn").addEventListener("click", () => {
  flipped = !flipped;
  renderBoard();
});
document.getElementById("loadFenBtn").addEventListener("click", () => loadPosition(fenInputEl.value, {clearHistory: true}));
fenInputEl.addEventListener("keydown", ev => {
  if (ev.key === "Enter") loadPosition(fenInputEl.value, {clearHistory: true});
});
document.addEventListener("keydown", ev => {
  if (ev.key === "ArrowLeft" && !["INPUT", "TEXTAREA"].includes(document.activeElement.tagName)) {
    ev.preventDefault();
    undoMove();
  }
});

loadPosition("startpos");
