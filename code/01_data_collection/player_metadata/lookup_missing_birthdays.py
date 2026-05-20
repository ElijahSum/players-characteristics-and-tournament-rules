#!/usr/bin/env python3
"""Match missing chess-player birth years against FIDE's public player list.

Input rows are those with a non-empty real_name and an empty birthday field.
The script writes source-traced candidate rows plus a one-row-per-target summary.
"""

from __future__ import annotations

import argparse
import csv
import re
import unicodedata
import xml.etree.ElementTree as ET
import zipfile
from collections import defaultdict
from pathlib import Path


FIDE_SOURCE = (
    "FIDE players_list_xml_foa.xml downloaded 2026-05-12 from "
    "https://ratings.fide.com/download/players_list_xml.zip"
)

MANUAL_FIDE_IDS = {
    # User/web search by handle found AnonymousSS as FIDE ID 5012899; the dataset real_name is inconsistent here.
    "259": ("5012899", "https://ratings.fide.com/profile/5012899"),
    # Chess.com identifies EDaMatta as Eduardo DaMatta; FIDE stores the player as Da Matta, Eduardo Cesar.
    "998": ("2193108", "https://www.chess.com/member/edamatta"),
    # Wikidata links Chess.com member Herman-NY to Matthew Herman and FIDE ID 2028964.
    "1472": ("2028964", "https://www.wikidata.org/wiki/Q27525772"),
    # Chess.com/WorldChess identify Jospem as GM Jose Eduardo Martinez Alcantara.
    "1725": ("3805662", "https://worldchess.com/news/jospem-fide-world-cup-jose-martinez"),
    # Chess.com identifies KorolDimitriy75 as Ukrainian NM Димитрий Король; FIDE stores Korol, Dimitry.
    "1908": ("14108003", "https://www.chess.com/nl/members/titled-players/national-masters"),
    # Chess.com identifies SmarterChess as NM Matt Jensen; FIDE stores Jensen, Matthew R.
    "3111": ("2018110", "https://www.chess.com/blog/SmarterChess/future-champions-square-off"),
    # Chess.com identifies maxz1996 as Max Zinski; FIDE stores Zinski, Maximilian.
    "4959": ("2070774", "https://www.chess.com/is/members/titled-players/national-masters?page=33"),
    # ChessArena links Roberto Carlos MIRAMONTES to FIDE ID 3510409; Chess.com handle is momia77.
    "7308": ("3510409", "https://chessarena.com/profile/220624"),
    # Lichess identifies josmito as GM Jose Manuel Lopez Martinez; FIDE stores Josep Manuel.
    "4656": ("2212943", "https://lichess.org/coach/josmito"),
    # FIDE search and Chess.com identify m-garcett as Matheus Garcett Souza dos Santos.
    "4892": ("2144492", "https://ratings.fide.com/profile/2144492"),
    # ChessPrime links semchik90/Dmitry Semenenko to FIDE ID 14118645.
    "5404": ("14118645", "https://chessprime.com/players/player/803294/"),
    # Chess.com and FIDE search identify shrek4life02 as FM Borna Derakhshani.
    "5430": ("12518450", "https://ratings.fide.com/profile/12518450"),
    # Chess.com identifies Chenitha201583 as CM Chenitha Karunasena; FIDE stores the full initials.
    "6223": ("29982596", "https://www.chess.com/members/titled-players/candidate-masters?page=14"),
    # Chess.com identifies chessmotives as CM Lakshmi Narayan MV.
    "6264": ("5040647", "https://www.chess.com/member/chessmotives"),
    # Chess.com and FIDE search identify xiaoxuan2012 as FM Zhang, Haoxuan(ZJ).
    "8207": ("8643172", "https://ratings.fide.com/profile/8643172"),
    # Chess.com identifies Yyaroslavchess2010 as Ukrainian NM Yaroslav Shevchenko.
    "8248": ("34144684", "https://www.chess.com/member/yyaroslavchess2010"),
    # Chess.com titled list identifies Academia3 as Argentine NM Jorge Leal.
    "5868": ("102326", "https://www.chess.com/members/titled-players?page=111"),
    # Chess.com identifies aguss_ok5 as NM Leandro Agustin Hurtado.
    "5894": ("194689", "https://www.chess.com/member/aguss_ok5"),
    # Chess.com identifies Berlin_wa11 as CM Robert Mcligeyo; FIDE stores Oluka Robert Mcligeyo.
    "6092": ("10814647", "https://www.chess.com/cs/members/titled-players?page=171"),
    # Chess.com identifies chess13524678 as NM Andrew Wu; the rated USA FIDE row has the matching name.
    "6226": ("30985749", "https://www.chess.com/member/chess13524678"),
    # Chess.com identifies ChessEvolve1 as Leonardo Correa Braga Camara de Almeida Neves.
    "6246": ("44786328", "https://www.chess.com/members/titled-players/national-masters?page=30"),
    # Chess.com identifies ChosenCheese as NM Alexander Heimann from the USA.
    "6290": ("2006073", "https://www.chess.com/members/titled-players/national-masters?page=33"),
    # Chess.com identifies ChrisSuleman as NM Felix Guo; the rated USA FIDE row has the matching name.
    "6291": ("30997550", "https://www.chess.com/member/chrissuleman"),
    # Chess.com identifies columbiachessclub as NM Daniel Johnston from the USA.
    "6319": ("2048078", "https://www.chess.com/he/members/titled-players?page=186"),
    # Chess.com identifies DAESDO as Spanish FM Daniel Dominguez; initials match Daniel Escobar Dominguez.
    "6341": ("2211351", "https://www.chess.com/members/titled-players/fide-masters?page=52"),
    # Chess.com identifies TERMINATOR_PC as IM Panagiotis Hristodoulou; FIDE transliterates Christodoulou.
    "3225": ("4214340", "https://www.chess.com/member/terminator_pc"),
    # Chess.com titled list directly links Tobias_Koelle to FIDE ID 16207378.
    "3355": ("16207378", "https://www.chess.com/et/members/titled-players/international-masters?page=16"),
    # Bundesliga records identify David Hoeffer/Höffer as FIDE ID 24606766.
    "3423": ("24606766", "https://schachbundesliga.de/spieler/1035967"),
    # Wikidata links Chess.com member jasonbolivar to Jeison Bolivar Losada, FIDE ID 4404793.
    "4606": ("4404793", "https://www.wikidata.org/wiki/Q71313828"),
    # Wikidata links josmito80 to Josep Manuel Lopez Martinez; duplicate of josmito.
    "4658": ("2212943", "https://www.wikidata.org/wiki/Q3824526"),
    # Chess.com/Lichess identify misha_melnichuk as Mikhail Melnichuk.
    "5013": ("14117533", "https://lichess.org/streamer/Misha_Melnichuk"),
    # Chess.com identifies monikarozman as WFM Monika Rozman; FIDE now stores Spalir, Monika.
    "5033": ("14609118", "https://www.chess.com/players/monika-rozman"),
    # Chess.com identifies mrntzchess as IM Tobias Kuegel.
    "5050": ("4636295", "https://www.chess.com/member/mrntzchess"),
    # Wikidata links theblitzchamp to FM Yi Ren Daniel Chan, FIDE ID 5801583.
    "5563": ("5801583", "https://www.wikidata.org/wiki/Q27524121"),
    # Chess.com titled list identifies santig8 as FM Santiago Guevara from El Salvador.
    "5372": ("6708870", "https://www.chess.com/is/members/titled-players?page=108"),
    # Chess.com profile identifies AngelEscarenoR as Mexican NM Angel Escareno Rojas.
    "5974": ("5107733", "https://www.chess.com/clubs/about/instituto-ajedrecistico-de-cd-juarez"),
    # Chess.com identifies boby_chess_2010 as Peruvian CM Adrian Abad.
    "6132": ("3843459", "https://www.chess.com/members/titled-players?page=155"),
    # Chess.com identifies camilochess as CM Andres Camilo Rodriguez Lopez.
    "6188": ("4410343", "https://www.chess.com/sk/members/titled-players"),
    # Chess.com identifies Dani097 as Ghanaian CM Daniel Frempong-Smart.
    "6346": ("12601659", "https://www.chess.com/member/dani097"),
    # Web search verified that this decorative real_name row is WFM K M Dahamdi Sanudula.
    "819": ("9932470", "https://ratings.fide.com/profile/9932470"),
    # Chess.com and biographical sources identify ElDivis as IM David Martinez Martin.
    "1031": ("2209748", "https://en.wikipedia.org/wiki/David_Mart%C3%ADnez_%28chess_player%29"),
    # Web search found the exact FIDE profile for Wellington Sampaio de Albuquerque Junior.
    "1196": ("2151480", "https://ratings.fide.com/profile/2151480"),
    # Lichess/Chess.com identify HackingKid98 as Senthil Maran K.
    "1431": ("25035681", "https://ratings.fide.com/profile/25035681"),
    # Chess.com identifies JSMastropiero as Daniel Lopez Gonzalez; ChessPrime links FIDE ID 2231948.
    "1634": ("2231948", "https://chessprime.com/players/player/510974/"),
    # Chess.com identifies MeekinsKris as NM Kris Meekins; FIDE stores Kristopher C Meekins.
    "2255": ("2005964", "https://www.chess.com/member/meekinskris"),
    # WorldChess search links Komal Kishore Pothuri to FIDE ID 5016703; FIDE stores the name as "P. Komal Kishore".
    "1893": ("5016703", "https://worldchess.com/profile/196443"),
    # Chess.com identifies NaciSAN as Cumali Unver; ChessPrime links Cumali Unver to FIDE ID 6304940.
    "2418": ("6304940", "https://chessprime.com/players/player/914443/"),
    # ChessArena/WorldChess identify Rathnakaran Kantholi as FIDE ID 5005507; FIDE stores the name as Ratnakaran, K.
    "2741": ("5005507", "https://chessarena.com/profile/228215"),
    # Chess.com titled list identifies Raskolnikov6 as FM Ivan Ramirez Martin from Mexico; FIDE stores Ramirez Marin, Ivan.
    "2786": ("5117399", "https://www.chess.com/members/titled-players?page=115"),
    # Chess.com identifies Rakhimzhan_Alen as Alen Rakhimzhan; FIDE now stores the player as Suleyev, Alen.
    "2767": ("13707213", "https://www.chess.com/member/rakhimzhan_alen"),
    # Chess.com identifies Ramla2020 as IM Mykhailo Shinkarev.
    "2773": ("4145348", "https://www.chess.com/member/ramla2020"),
    # Lichess/Chess.com identify Roenko_Artur as Artur Roenko/Roienko.
    "2860": ("14136732", "https://lichess.org/coach/Roenko_Artur"),
    # ChessPrime links Oleksandr Kuzhilniy to FIDE ID 34110500; FIDE stores Kuzhylnyi.
    "2969": ("34110500", "https://chessprime.com/players/player/478209/"),
    # Chess.com identifies Sergisanch as Spanish FM Sergio Sanchez; FIDE stores Sanchez Gonzalez, Sergio 1992.
    "3022": ("2273020", "https://www.chess.com/members/titled-players?page=16"),
    # Chess.com identifies SharkAnto55 as IM Antonio Sebastian Almiron Villalba.
    "3045": ("3701166", "https://www.chess.com/member/sharkanto55"),
    # FIDE search for Mohamed-almado returns Mohamed Yousuf Al-Ali.
    "2337": ("9325930", "https://ratings.fide.com/profile/9325930"),
    # ChessPrime links Liduino Furtado De VJunior to FIDE ID 2109913.
    "2016": ("2109913", "https://chessprime.com/players/player/210409/"),
    # Nemsko is Nemo Zhou; FIDE stores the player as Qiyu Zhou.
    "2454": ("505161", "https://en.wikipedia.org/wiki/Nemo_Zhou"),
    # Oglob71 plus the Cyrillic real name identify IM Nikolay Ogloblin.
    "2537": ("4139658", "https://ratings.fide.com/profile/4139658"),
    # Chess.com identifies Omk124 as FM Om Kadam; FIDE stores the player as Kadam Om Manish.
    "2554": ("45076294", "https://www.chess.com/member/omk124"),
    # Chess.com identifies Siniauski as FM Артём Синявский; FIDE stores the player as Siniauski, Artsiom.
    "3084": ("13512269", "https://www.chess.com/ru/member/siniauski"),
    # Chess.com identifies SuperGPatzer as Finnish NM Sauli Titta; FIDE stores Tiitta, Sauli.
    "3196": ("502251", "https://www.chess.com/zh-TW/members/titled-players?page=184"),
    # Chess.com identifies TheChessChannel as NM Mike Ellenbogen; FIDE stores Michael Ellenbogen.
    "3279": ("2027143", "https://www.chess.com/member/thechesschannel"),
    # Chess.com identifies Kyiv-1 as NM Ivan Martynenko from the USA.
    "7061": ("2053179", "https://www.chess.com/member/kyiv-1"),
    # Chess.com titled list links MaurinM to FIDE ID 16265696.
    "7229": ("16265696", "https://www.chess.com/ka/members/titled-players/fide-masters"),
    # Chess.com titled list identifies randomkiblitzer as NM Cameron Goh from Singapore.
    "7606": ("5804540", "https://www.chess.com/members/titled-players/national-masters?page=37"),
    # Chess.com identifies Sameer555 as Indian IM Sameer Kath; FIDE stores Kathmale, Sameer.
    "7716": ("5024498", "https://www.chess.com/member/sameer555"),
    # Chess.com titled list identifies SparkyChessTiger as NM Jayden Wu from the USA.
    "7837": ("30972558", "https://www.chess.com/members/titled-players/national-masters?page=26"),
    # Chess.com titled list identifies TalyaKid07 as Turkish FM Can Yurtseven.
    "7913": ("6300081", "https://www.chess.com/members/titled-players?page=4"),
    # Chess.com titled list identifies taylotrista as Canadian NM Tristan Taylor.
    "7923": ("2635054", "https://www.chess.com/members/titled-players/national-masters"),
    # Chess.com/Wikidata identify Semetey as IM Semetey Tologontegin; FIDE stores Tologon tegin, Semetei.
    "3007": ("13800574", "https://www.chess.com/member/semetey"),
    # Chess.com identifies Varvara_Poliakova as WFM Варвара Полякова; FIDE stores Poliakova, Varvara.
    "3470": ("13518313", "https://www.chess.com/ru/member/varvara_poliakova"),
    # The Chess.com handle ashwanitiwari and FM title identify Ashwani Tiwari despite the display name Punjabi Sphinx.
    "3877": ("5006961", "https://www.chess.com/member/ashwanitiwari"),
    # Chess.com identifies almatyonelove as IM Evgeniy Pak; FIDE stores Pak, Yevgeniy.
    "3786": ("14201712", "https://www.chess.com/member/almatyonelove"),
    # Chess.com identifies beatnoguera as Beatriz Noguera; FIDE stores the full name Noguera Da Almeida, Beatriz Elvira.
    "3925": ("3901734", "https://www.chess.com/member/beatnoguera"),
    # Chess.com titled list identifies bobo_panda as NM Siddharth Singh from the USA.
    "3961": ("39907023", "https://www.chess.com/members/titled-players/national-masters?page=16"),
    # Chess.com identifies honestgames as Egor Lashkin; FIDE stores the transliteration as Jegor Lashkin.
    "4526": ("13907808", "https://www.chess.com/member/honestgames"),
    # FIDE search identifies Arash Akbarinia as Sayed Arash Akbarinia.
    "5960": ("12501468", "https://ratings.fide.com/profile/12501468"),
    # Biographical sources identify Asylbek Abdyjapar as FIDE ID 13800337; FIDE stores Abdyzhapar.
    "6024": ("13800337", "https://en.wikipedia.org/wiki/Asyl_Abdyjapar"),
    # Chess.com identifies DrWonderKid as CM Baris Cinar Sahbudak.
    "6441": ("26351633", "https://www.chess.com/member/drwonderkid"),
    # Chess.com identifies Gabriel-Ledezma as FM Gabriel Ledezma; Chess.com bio gives full name Gabriel Enrique Ledezma Villamizar.
    "6617": ("3922049", "https://www.chess.com/players/gabriel-enrique-ledezma-villamizar"),
    # Chess.com identifies GambitByLiana as WCM Liana Pieter Rodriguez from Curacao; FIDE stores Pieter, Liana.
    "6630": ("7502192", "https://www.chess.com/members/titled-players/woman-candidate-masters?page=3"),
    # Chess.com titled list identifies Glezcanow as Gustavo Walter Oscar Lezcano from Buenos Aires.
    "6673": ("103306", "https://www.chess.com/ka/members/titled-players/national-masters?page=22"),
    # Chess.com identifies gmalsayed as Qatari GM Mohammed Alsayed; FIDE stores Al-Sayed, Mohammed.
    "6681": ("12100196", "https://www.chess.com/member/gmalsayed"),
    # Chess.com identifies granguerrerov as CM Juan Andres Guerrero from Santiago; FIDE stores Guerrero Cataldo, Juan Andres.
    "6715": ("3421260", "https://www.chess.com/es/member/granguerrerov"),
    # Chess.com identifies HiTechPanama as Panamanian CM Carlos Neira.
    "6786": ("6600948", "https://www.chess.com/member/hitechpanama"),
    # Chess.com identifies HLuis7 as Cuban NM Hector Fuentes de feria; FIDE stores Hector Luis.
    "6788": ("3530108", "https://www.chess.com/members/titled-players/national-masters?page=10"),
    # Chess.com titled list identifies kaan_akbas67 as Turkish NM Kaan Akbas.
    "6947": ("34550321", "https://www.chess.com/members/titled-players?name=&page=182&sortby=alphabetical"),
    # Chess.com identifies FearlessFighter007 as IM Vadym Petrovskyi; FIDE stores Petrovskiy.
    "6558": ("14165210", "https://www.chess.com/member/fearlessfighter007"),
    # Lichess links elchechereche/Jose Fernando Mata Gonzalez to FIDE ID 5107423.
    "6476": ("5107423", "https://lichess.org/@/elchechereche"),
    # Lichess identifies fidezivkovic as FM Ivan Zivkovic from Serbia.
    "6567": ("935972", "https://lichess.org/coach/fidezivkovic"),
    # Chess.com identifies GM_Petrovich as GM Petr Kiryakov; FIDE stores Kiriakov.
    "6679": ("4119231", "https://www.chess.com/member/gm_petrovich"),
    # Chess.com identifies Gen-Gutman as GM Gennadii Gutman.
    "6648": ("14102277", "https://www.chess.com/member/gen-gutman"),
    # Chess.com identifies GMJankovic as GM Alojzije Jankovic.
    "6688": ("14505959", "https://www.chess.com/players/alojzije-jankovic"),
    # Chess.com/Wikidata identify Maksat-94 as GM Maksat Atabayew/Atabayev.
    "7169": ("14001101", "https://www.chess.com/member/maksat-94"),
    # Chess.com/ChessArena identify Tricky_voldemort as CM Yasser Hadj Khoulti.
    "8031": ("9003258", "https://chessarena.com/profile/269793"),
    # Chess.com titled list identifies SadigMammadov_003 as IM Sadiq/Sadig Mammadov.
    "7704": ("13408712", "https://www.chess.com/member/sadigmammadov_003"),
    # Chess.com identifies yawnpawn as CM Emil Jr Schnabel; FIDE stores Johann Emil Schnabel.
    "8225": ("14311674", "https://www.chess.com/member/yawnpawn"),
    # Chess.com identifies Beljavski3 as FM Dragan Stojanovic with FIDE 2070; this matches FIDE ID 916587.
    "6082": ("916587", "https://www.chess.com/members/titled-players/fide-masters"),
    # Chess.com identifies APOLLONZEROTV as FM Zaur Gasanov; FIDE stores Hasanov, Zaur.
    "5996": ("34125970", "https://www.chess.com/member/apollonzerotv"),
    # Chess.com identifies danir29 as GM Dani Raznikov; FIDE stores Danny Raznikov.
    "6356": ("2811154", "https://www.chess.com/member/danir29"),
    # Chess.com/Wikipedia identify vladimirssvesnikovs as IM Vladimirs Svesnikovs; FIDE stores Vladimir Sveshnikov.
    "5703": ("11601884", "https://www.chess.com/lt/members/titled-players/international-masters"),
    # Chess.com titled list identifies PattyCastillo as WFM Patricia Castillo from the Dominican Republic.
    "2613": ("6412106", "https://www.chess.com/member/pattycastillo"),
    # Chess.com identifies Piotrek1979 as Piotrek Mickiewicz; FIDE stores the player as Piotr Mickiewicz.
    "2673": ("1114891", "https://www.chess.com/pl/member/piotrek1979"),
    # Chess.com identifies Etud-otradnoe as the Etude chess school; RCF lists its head as IM Alexander S. Zakharov, FIDE ID 4120132.
    "1084": ("4120132", "https://ratings.ruchess.ru/people/1493"),
    # Chess.com identifies FGHSMN as GM Bharath Subramaniyam.H Harishankkar; FIDE stores Bharath Subramaniyam H.
    "1108": ("46634827", "https://www.chess.com/member/fghsmn"),
    # Duplicate FGHSMN row.
    "1109": ("46634827", "https://www.chess.com/member/fghsmn"),
    # Lichess/Chess.com identify Golden3agle as Roberto Calheiros; FIDE stores Miranda Jr, Roberto Calheiros de.
    "1363": ("2109417", "https://lichess.org/coach/Calh3iros"),
    # Chess.com identifies IrI_Cheetah as FM mohamadmiran khademi; FIDE stores Khademi, Mohammad Miran.
    "1585": ("12503550", "https://www.chess.com/member/iri_cheetah"),
    # Chess.com identifies Irina_Mikhaylova as WGM Irina Mikhaylova; FIDE stores Mikhailova, Irina.
    "1592": ("4109040", "https://www.chess.com/member/irina_mikhaylova"),
    # Duplicate Irina_Mikhaylova row.
    "1593": ("4109040", "https://www.chess.com/member/irina_mikhaylova"),
    # Chess.com identifies Sasha_Solovev_777 as CM Соловьев Александр from Bratsk; the listed FIDE 2237 matches FIDE ID 4155807.
    "2970": ("4155807", "https://www.chess.com/member/sasha_solovev_777"),
    # Chess.com titled list identifies Kate1606 as WFM Kate/Ekaterina Nikanova; ChessPrime links her to FIDE ID 13506374.
    "1811": ("13506374", "https://chessprime.com/players/player/627569/"),
    # Chess.com and Chess-Results identify LUISMIGUELOV as CM Luis Miguel Floresvillar Gonzalez, FIDE ID 5100542.
    "1960": ("5100542", "https://www.chess.com/member/luismiguelov"),
    # FIDE profile identifies Ringouz's real name as Regis Gabetta de Souza.
    "2838": ("22756930", "https://ratings.fide.com/profile/22756930"),
    # Chess.com bio identifies SamCopeland as NM Sam Copeland; FIDE stores Samuel Copeland.
    "2941": ("2007347", "https://www.chess.com/players/sam-copeland"),
    # Chess.com identifies Swiss_Fighter as IM Gabriel Gähwiler; FIDE stores Gaehwiler, Gabriel.
    "3212": ("1314530", "https://www.chess.com/member/swiss_fighter"),
    # Chess.com identifies safikiet as NM Alexander Fikiet; 365Chess links Alex Fikiet to FIDE ID 2052717.
    "5358": ("2052717", "https://www.chess.com/member/safikiet"),
    # Chess.com identifies sanbruh as CM Александр Брюхович from Brovary; FIDE stores Bruhovich, Olexandr.
    "5370": ("14119919", "https://www.chess.com/member/sanbruh"),
    # Chess.com identifies zubridis as GM Зубарев Александр from Ukraine; the listed FIDE 2494 matches GM Alexander Zubarev, FIDE ID 14104385.
    "5831": ("14104385", "https://www.chess.com/member/zubridis"),
    # Chess.com identifies TioCridineno as NM/MN Vinicius Saito.
    "3348": ("2113384", "https://www.chess.com/member/tiocridineno"),
    # Chess.com identifies Undisputed92 as GM Shyaamnikhil P; FIDE stores Shyaam Nikhil P.
    "3426": ("5024218", "https://www.chess.com/member/undisputed92"),
    # Chess.com identifies ValiantCeleb as IM Caleb Levi; FIDE stores Levitan, Caleb Levi.
    "3456": ("14332620", "https://www.chess.com/member/valiantceleb"),
    # Chess.com/Wikidata identify annazero as Anna Afonasieva; FIDE now stores Golubova, Anna.
    "3833": ("24173606", "https://www.wikidata.org/wiki/Q28479808"),
    # Chess.com identifies Bravecapy as FM David Martinez Lopez; ChessPrime links FIDE ID 22235779.
    "6150": ("22235779", "https://www.chess.com/es/member/bravecapy"),
    # Chess.com titled lists identify CanidateMaster as NM Quang Huy Bui from Vietnam; FIDE profile 12431265 matches the rating band and name.
    "6191": ("12431265", "https://ratings.fide.com/profile/12431265"),
    # Chess.com titled list identifies Cavalogrande as NM Jose Antonio Madalena De Sousa Jr; FIDE event pages link ID 2119218.
    "6201": ("2119218", "https://www.chess.com/member/cavalogrande"),
    # Chess.com identifies ChampArden as CM Alexander/Aleksandr Logachiov; Chess.com player bio gives born 1965, matching FIDE ID 24123706.
    "6214": ("24123706", "https://www.chess.com/member/champarden"),
    # Chess.com identifies ChessMasterJBlack as National Master James Black Jr.; biographical sources link him to FIDE ID 2048280.
    "6260": ("2048280", "https://www.chess.com/member/chessmasterjblack"),
    # Chess.com identifies JaackCamel as CM Aleksandr Ivanov from the United States; the listed FIDE rating matches FIDE ID 2092980.
    "1638": ("2092980", "https://www.chess.com/member/jaackcamel"),
    # Chess.com identifies Dr_MorbiusBat as CM Adriano Levano from Lima; FIDE stores Levano Loayza, Adriano Jaffet.
    "6428": ("3865584", "https://www.chess.com/member/dr_morbiusbat"),
    # Chess.com identifies Guzikov_Daniil as CM Даниил Гузиков from Tomsk; FIDE stores Guzikov, Danil.
    "6740": ("24161004", "https://www.chess.com/member/guzikov_daniil"),
    # Chess.com titled list identifies protectiago as NM Tiago Batista Oliveira from Brazil; ChessPrime links FIDE ID 2108291.
    "5248": ("2108291", "https://www.chess.com/member/protectiago"),
    # Chess.com identifies FMLarry as NM Larry Yang from Ontario, Canada; the titled list rating matches FIDE ID 2637618.
    "6589": ("2637618", "https://www.chess.com/member/fmlarry"),
    # Chess.com identifies Grem_Lin as NM Андрій Сергієнко; FIDE stores Andrii Sergiienko, ID 14194376.
    "6723": ("14194376", "https://www.chess.com/member/grem_lin"),
    # Chess.com titled list identifies Mestre_Soares as CM Roberto Suardi Jr.; FIDE stores Suardi, Roberto Jr.
    "7254": ("2111845", "https://www.chess.com/member/mestre_soares"),
    # Chess.com identifies MgaPR as NM Gabriel Tortola F. Vieira; FIDE stores Vieira, Gabriel Tortola Flores.
    "7259": ("2119595", "https://www.chess.com/member/mgapr"),
    # Chess.com profile for Nikolaj_Katishonok links directly to FIDE ID 11600225.
    "7406": ("11600225", "https://www.chess.com/member/nikolaj_katishonok"),
    # Chess.com identifies RafaelVaganian as GM Rafael Vahanyan; FIDE stores the historical transliteration Vaganian, Rafael A.
    "7594": ("13300016", "https://www.chess.com/member/rafaelvaganian"),
    # Chess.com identifies sindarovjr as CM Ислом Синдаров from Uzbekistan; FIDE stores Sindarov, Islombek.
    "7798": ("14205475", "https://www.chess.com/member/sindarovjr"),
    # Chess.com titled list identifies SupermouseBR as NM Alberto Mousse from Brazil; FIDE stores Sebastiao Alberto Mousse Neto.
    "7883": ("2137640", "https://www.chess.com/member/supermousebr"),
    # Chess.com identifies Ykow2 as IM Agustin Droin; FIDE stores Augustin Droin.
    "8233": ("45105987", "https://www.chess.com/member/ykow2"),
    # Chess.com identifies Wilsy19 as WFM Wilsaida Diaz; FIDE stores Diaz Cesar, Wilsaida Pieranlly.
    "8153": ("6403557", "https://www.chess.com/member/wilsy19"),
    # Marshall Chess Club entry links GMJustin to IM Justin Sarkar; FIDE stores Sarkar, Justin.
    "1258": ("2010011", "https://www.marshallchessclub.org/tournaments/entry_list/5338"),
    # Chess.com identifies Kassimov_B as IM Касимов Бауыржан; FIDE stores Kasimov, Baurzhan.
    "1809": ("13704800", "https://www.chess.com/member/kassimov_b"),
    # Chess.com identifies MikeWizard as NM Mike Hehir; FIDE stores Michael Hehir.
    "2300": ("2047411", "https://www.chess.com/member/mikewizard"),
    # Chess.com titled list identifies Mikhal867 as Ukrainian NM Володимир Михальський.
    "2307": ("14144352", "https://www.chess.com/members/titled-players/national-masters?page=25"),
    # Chess.com identifies Nick12772 as NM Nicholas Figorito; FIDE stores Nick Figorito.
    "2466": ("30923689", "https://www.chess.com/member/nick12772"),
    # Chess.com identifies leykunmesfin as Ethiopian FM Leykunmesfin Sisay; FIDE stores Mesfin, Leykun.
    "4838": ("15700097", "https://www.chess.com/members/titled-players/fide-masters?page=4"),
    # Chess.com titled list identifies lukazz029 as Lucas Aparecido Oliveira Dos Santos.
    "4885": ("2138409", "https://www.chess.com/members/titled-players?page=33"),
    # Chess.com identifies masteryoungK as FM Kevin Meneses from Tenerife; FIDE stores Kevin Moises Meneses Gonzalez.
    "4947": ("2284502", "https://www.chess.com/member/masteryoungk"),
    # Chess.com identifies muncettin as Turkish NM Munci Inonu; FIDE stores Muhsin Munci Inonu.
    "5055": ("6306446", "https://www.chess.com/member/muncettin"),
    # Chess.com identifies nikitadovbnya2008 as CM Никита Довбня; FIDE stores Dovbnia, Nikita.
    "5097": ("54104599", "https://www.chess.com/member/nikitadovbnya2008"),
    # Chess.com identifies purpurice as NM Eric Zhang; the rated USA FIDE row matches the Chess.com FIDE rating.
    "5254": ("30907667", "https://www.chess.com/member/purpurice"),
    # Chess.com/Wikipedia link reevecanada to IM Aaron Reeve Mendes, FIDE ID 25954938.
    "5284": ("25954938", "https://www.chess.com/member/reevecanada"),
    # Chess.com identifies rtdrtd67 as NM Ethan Gu; the older rated USA FIDE row is the compatible match.
    "5338": ("30909236", "https://www.chess.com/member/rtdrtd67"),
    # Chess.com identifies sari07 as FM Yasin Sari from Turkey.
    "5377": ("6320902", "https://www.chess.com/member/sari07"),
    # Chess.com identifies skoularikii as Giannis Kalogeris; FIDE stores the Greek given name as Ioannis.
    "5448": ("4239563", "https://www.chess.com/member/skoularikii"),
    # Lichess and Chess.com identify stolencandy13 as Canadian NM Zane/Zehn Nasir.
    "5498": ("2613239", "https://lichess.org/coach/stolencandy13"),
    # Chess.com identifies the-do0on as Saudi CM مساعد المطيري; FIDE stores Musaad Almutairi.
    "5556": ("21530327", "https://www.chess.com/member/the-do0on"),
    # Chess.com identifies AlexTargarian as GM Vitaliy Kiselev with FIDE 2525; FIDE stores Vitalii Kiselev.
    "5938": ("4194128", "https://www.chess.com/members/titled-players/grandmasters?page=48"),
    # Chess.com identifies chessvedant09 as NM Vedant Maheshwari from San Diego.
    "6272": ("30988616", "https://www.chess.com/member/chessvedant09"),
    # Chess.com titled list identifies dfedunov as NM Daniil Fedunov; FIDE stores Danil Fedunov in the USA row.
    "6390": ("2006413", "https://www.chess.com/members/titled-players/national-masters?page=35"),
    # Chess.com titled list identifies dmschess2024 as WCM Mariia Dolinskaya from Russia.
    "6413": ("54140064", "https://www.chess.com/members/titled-players/woman-candidate-masters"),
    # Chess.com titled list identifies GarnikN as Armenian CM Garnik Nersesyan; FIDE transliterates Nersisyan.
    "6638": ("13300741", "https://www.chess.com/members/titled-players?page=94"),
    # Chess.com and Lichess identify Gianmarco_es as IM Gianmarco Leiva Rodriguez.
    "6664": ("3804399", "https://lichess.org/streamer/pasino"),
    # Chess.com identifies GORA2012 as Colombian CM Gabriel Rueda from Barranquilla.
    "6708": ("4496329", "https://www.chess.com/member/gora2012"),
    # Chess.com identifies laiditmang05_ducminh as Vietnamese NM Duc Minh Lai; 2005 birth year matches the handle.
    "7068": ("12420387", "https://www.chess.com/member/laiditmang05_ducminh"),
    # Chess.com/Lichess identify Matheusmdr2005 as FM Matheus Mendes Domingues Ribeiro.
    "7216": ("324226923", "https://lichess.org/fide/324226923/Ribeiro_Matheus_Mendes_Domingues"),
    # Chess.com identifies Priyansh_2011 as NM Priyansh Garg; the USA FIDE row matches the handle year and current rating.
    "7555": ("30984696", "https://www.chess.com/member/priyansh_2011"),
    # Chess.com identifies RafaJumps as CM Rafael Alcides from the UK; FIDE stores Rafael Alcides Perez Coyula.
    "7595": ("3509931", "https://www.chess.com/members/titled-players?page=161"),
    # Chess.com identifies SharankovG as CM Grigoriy Sharankov; FIDE stores Grigory Sharankov.
    "7774": ("4168356", "https://www.chess.com/member/sharankovg"),
    # Chess.com identifies sicalyra as WIM Sila Caglar.
    "7788": ("6364063", "https://www.chess.com/member/sicalyra"),
    # Lichess/Chess.com identify Respectful_Dave as FM David Maycock; FIDE stores David Henry Maycock Bates.
    "7632": ("5142547", "https://lichess.org/streamer/respectful_dave"),
    # Chess.com identifies Sandkorn108 as CM Cornelius Gähler; FIDE stores Gaehler.
    "7720": ("4658582", "https://www.chess.com/members/titled-players"),
    # Chess.com identifies teju0607 as WIM Tejaswini Ganesan from Chennai.
    "7927": ("25749978", "https://www.chess.com/members/titled-players?name=&page=51&sortby=alphabetical"),
    # Chess.com identifies TzoumpasAnastasis as FM Anastasios Tzoumpas; FIDE stores Tzoumbas.
    "8050": ("4200519", "https://www.chess.com/member/tzoumpasanastasis"),
    # Chess.com identifies VKMATTA as FM Matta Vinaykumar; FIDE stores Matta, Vinay Kumar.
    "8104": ("5016649", "https://www.chess.com/member/vkmatta"),
    # Chess.com identifies vraghav2010 as FM Raghav Vijay from Chennai; FIDE stores Raghav V.
    "8117": ("25922475", "https://www.chess.com/members/titled-players/fide-masters?page=34"),
    # Chess.com identifies Flaccidmoves as NM Dan Herman from Colorado Springs; tournament records/FIDE ratings identify Daniel Herman.
    "1176": ("30935008", "https://www.chess.com/member/flaccidmoves"),
    # Duplicate Flaccidmoves row.
    "1177": ("30935008", "https://www.chess.com/member/flaccidmoves"),
    # Chess.com identifies JoshWeinstein as Josh Weinstein; the USA FIDE rapid rating row stores Joshua Ross Weinstein.
    "1721": ("30927714", "https://www.chess.com/member/joshweinstein"),
    # Chess.com/CBX and Chess-Results identify romulocardoso as Romulo Cardoso S. Rodrigues Mota.
    "5329": ("2187442", "https://www.chess.com/member/romulocardoso"),
    # Chess.com identifies alqudaimi as IM Basheer Alqudaimi; FIDE stores Al Qudaimi, Basheer.
    "5955": ("9400346", "https://www.chess.com/member/alqudaimi"),
    # Chess.com identifies catkeson as NM Chris Atkeson; FIDE stores Christopher Timothy Atkeson.
    "6199": ("39956547", "https://www.chess.com/member/catkeson"),
    # Chess.com identifies chesszeror as Rene Federico Lacayo Cortez; FIDE stores Lacayo, Rene.
    "6280": ("6100031", "https://www.chess.com/member/chesszeror"),
    # Chess.com identifies gaidym72 as FM Mikhail Gaydym; FIDE stores Michail Gaydym.
    "6625": ("24180211", "https://www.chess.com/member/gaidym72"),
    # Chess.com identifies GiulioBorgo as IM Giulio Borgo; dataset display has a typo in the first name.
    "6672": ("800180", "https://www.chess.com/member/giulioborgo"),
    # Chess.com identifies heytbeteacher as FM Baver Yilmaz.
    "6779": ("26363542", "https://www.chess.com/member/heytbeteacher"),
    # Chess.com identifies historicodeatleta as Bergson Fernandes; FIDE stores Fernandes Filho, Bergson Fragoso.
    "6784": ("2128128", "https://www.chess.com/member/historicodeatleta"),
    # Chess.com identifies iwant2lose as Benjamin Al-shami from Cleveland; FIDE stores Al-Shami, Benjamin.
    "6864": ("2018454", "https://www.chess.com/member/iwant2lose"),
    # Chess.com identifies J-Isaac as CM Jonathan Montevilla; FIDE stores Montevilla Cahuasa, Jonathan I.
    "6866": ("3322220", "https://www.chess.com/member/j-isaac"),
    # Chess.com identifies Janukshan as CM Janukshan Brahalathanan; FIDE stores Janukshan, B.
    "6876": ("29945399", "https://www.chess.com/member/janukshan"),
    # Ruchess/FIDE sources identify Юлия Карпова as WIM Yulia Karpova.
    "6938": ("4119878", "https://ratings.ruchess.ru/people/36429"),
    # Chess.com identifies Kimmich_Mindset as FM Lukas Stöttner; FIDE stores Stoettner, Lukas.
    "6997": ("16289951", "https://www.chess.com/member/kimmich_mindset"),
    # Chess.com identifies lavogadinni as Daniel Pereira Lavogade; FIDE stores the surname as Lavogadae.
    "7081": ("22799362", "https://www.chess.com/member/lavogadinni"),
    # Chess.com identifies lehjoz as CM Jozsef Lehocz Jr.
    "7085": ("799270", "https://www.chess.com/member/lehjoz"),
    # Chess.com identifies malexC05 as FM Manuel Melendez.
    "7172": ("6706800", "https://www.chess.com/member/malexc05"),
    # Chess.com identifies misitu as Dr. Christian Schubert; FIDE has a title-bearing Dr. Christian Schubert row.
    "7291": ("4604229", "https://www.chess.com/member/misitu"),
    # Chess.com identifies NickKR as CM Nick Kowalski from Jerez; FIDE stores Nicholas Kowalski Rubiales.
    "7395": ("54592763", "https://www.chess.com/member/nickkr"),
    # Chess.com identifies Nimmy-A-George as WIM Nimmy A. George; FIDE stores Nimmy, A.G.
    "7409": ("5015308", "https://www.chess.com/member/nimmy-a-george"),
    # Chess.com identifies pamapseba as C Sebastián Oppezzo; FIDE stores Oppezzo, Sebastian.
    "7478": ("112321", "https://www.chess.com/member/pamapseba"),
    # Chess.com identifies pawndissection as FM Ali Layth Ahmed Ahmed; FIDE stores Alothman, Ali Layth Ahmed.
    "7498": ("4801920", "https://www.chess.com/member/pawndissection"),
    # Chess.com identifies Proktofantasmist as CM Artyom Pechnikov; FIDE stores Artiom Pechnikov.
    "7559": ("24104124", "https://www.chess.com/member/proktofantasmist"),
    # Chess.com/Chess-Results identify rey_jacinto as Ronald Fernando Avellaneda Arevalo.
    "7634": ("4411536", "https://www.chess.com/member/rey_jacinto"),
    # Chess.com identifies SasviduWeera as CM Malisha Sasvidu Weerathunga; Chess-Results gives FIDE ID 9970410.
    "7731": ("9970410", "https://www.chess.com/member/sasviduweera"),
    # Chess.com/Lichess identify sawkitoo as CM Carlos Abiad; FIDE profile gives Carlos Jose Abiad Parra.
    "7736": ("3910202", "https://lichess.org/coach/sawkito"),
    # Chess.com titled list identifies sdv_013 as FM Дмитрий Солонков; FIDE stores Dmitrij Solonkov.
    "7747": ("4150139", "https://www.chess.com/uz/members/titled-players?page=5"),
    # Chess.com/Lichess identify skzt as GM Sergiy Kryvosheya; FIDE/Wikidata store Sergei Krivoshey.
    "7808": ("14102226", "https://lichess.org/coach/skzt"),
    # Chess.com/Wikipedia identify spectralsoul as IM Esteban Valderrama Quiceno.
    "7839": ("4442024", "https://www.chess.com/players/esteban-alb-valderrama-quiceno"),
    # Chess.com/Wikipedia identify stanyga as GM Stany G.A.
    "7852": ("5029104", "https://en.wikipedia.org/wiki/Stany_G.A."),
    # Chess.com titled list identifies tonilator as FM Helmut Hürter; FIDE stores Huerter.
    "8013": ("4675711", "https://www.chess.com/members/titled-players/fide-masters?page=54"),
    # Chess.com identifies Tu_Pikachu_Favorito as WIM Heidy García; FIDE stores Heidy Nicole Garcia Andrada.
    "8037": ("3831779", "https://www.chess.com/member/tu_pikachu_favorito"),
    # Chess.com identifies guillejr as CM Gui Ribero; FIDE stores Guillermo Andres Ribero.
    "6734": ("4423208", "https://www.chess.com/member/guillejr"),
    # Chess.com titled list identifies R12thehills as NM Thiago Alves da Silva with FIDE 1959.
    "7587": ("2137585", "https://www.chess.com/members/titled-players/national-masters?page=13"),
    # Chess.com titled list identifies Vodkajunior4 as CM Samuel Garcia Garcia; FIDE stores Samuel Garcia Garcia 2002 I.
    "8110": ("24576158", "https://www.chess.com/es/members/titled-players/candidate-masters?page=9"),
    # Chess.com identifies will_graham as FM Ali Syed Ahmad; 365Chess/ChessBase link the exact FIDE record.
    "8150": ("7801076", "https://www.365chess.com/players/Ali_Syed_Ahmad"),
    # Turkmen chess reports identify ahmet_RD_1337 as Ahmet Gubatayew/Gubatayev.
    "5897": ("14004526", "https://turkmenportal.com/tm/blog/78930/turkmen-chess-players-started-with-victories-at-the-junior-world-championship-in-india"),
    # Ruchess identifies Yuriy Ayrapetyan as GM Ajrapetjan, Yuriy with FIDE ID 14109069.
    "5811": ("14109069", "https://ratings.ruchess.ru/people/65246"),
    # Chess.com titled list identifies sebastianmarinp as FM Sebastian Marin from Medellin; FIDE stores Marin Posada.
    "5398": ("4402782", "https://www.chess.com/zh/members/titled-players?page=123"),
    # Chess.com titled list identifies Witold_lechowski as FM Witek Lechowski; FIDE stores Witold Lechowski.
    "8159": ("21040338", "https://www.chess.com/members/titled-players/fide-masters?page=49"),
    # Chess.com titled list identifies NTBT_DHS as Vietnamese IM Sơn Đặng; FIDE stores Dang Hoang Son.
    "2411": ("12402435", "https://www.chess.com/members/titled-players/international-masters?page=16"),
    # The dataset real name identifies Brazilian Francisco De Assis Medeiros Jr.; FIDE stores the same player with one-s spelling.
    "3224": ("2114267", "https://ratings.fide.com/profile/2114267"),
    # Chess.com/Wikipedia identify YoreDea's Hebrew display name as GM Alexander/Aaron Bagrationi.
    "3630": ("14104954", "https://en.wikipedia.org/wiki/Alexander_Bagrationi_(chess_player)"),
    # Chess.com identifies WanderingPuppet as NM Matthew O'Brien; the USA FIDE row has the matching name and rating band.
    "3545": ("2059150", "https://www.chess.com/member/wanderingpuppet"),
    # Chess.com identifies el_luiso96 as Cuban IM Luis Hernandez; FIDE stores Luis Daniel Rodriguez Hernandez.
    "4282": ("3506770", "https://www.chess.com/member/el_luiso96"),
    # Chess.com identifies juli2512 as WFM Yulya Pishchal; FIDE stores Pischal, Yulia.
    "4671": ("14102390", "https://www.chess.com/member/juli2512"),
    # Chess.com identifies manmanlai1 as IM Martin Brüdigam; FIDE stores Bruedigam, Martin.
    "4916": ("24608033", "https://www.chess.com/coaches?near_me=1&page=12&searched=1&titled_only=1"),
    # Chess.com profile for maurovargas01 gives full name Mauricio Vargas Benavides and FIDE ID 4404572.
    "4956": ("4404572", "https://www.chess.com/members/titled-players?page=146"),
    # Chess.com identifies maxip32 as Argentine IM Maxi Perez; FIDE stores Perez, Maximiliano.
    "4958": ("117234", "https://www.chess.com/member/maxip32"),
    # Chess.com identifies nelsi as NM Nelson M. Lopez II; FIDE stores Lopez II, Nelson M.
    "5078": ("2027895", "https://www.chess.com/member/nelsi"),
    # Lichess/Chess.com identify pifion as Colombian IM Sebastian Sanchez Lizcano, born 1989; FIDE stores Sanchez, Sebastian Felipe.
    "5210": ("4403304", "https://lichess.org/coach/pifion"),
    # Chess.com identifies poluprovodnik01 as WFM Алёна/Aliona Garmash.
    "5226": ("34127620", "https://www.chess.com/member/poluprovodnik01"),
    # Chess.com identifies rdarruda as FM Ricardo D Darruda; FIDE stores D'Arruda, Ricardo D.
    "5279": ("100692", "https://www.chess.com/members/titled-players/fide-masters?page=43"),
    # Chess.com titled list identifies wang5ter as NM Henry Wang from the United States; this matches the rated USA FIDE row.
    "5729": ("30927080", "https://www.chess.com/member/wang5ter"),
    # Chess.com identifies 24pablo209 as Adriano Gaspar de Lima Salguero from Argentina; FIDE stores De Lima, Adriano Gaspar.
    "5842": ("190861", "https://www.chess.com/member/24pablo209"),
    # User provided the exact FIDE profile for Дмитрий Залесский; FIDE stores Zalesskiy, Dmitriy.
    "645": ("14175800", "https://ratings.fide.com/profile/14175800"),
    # User provided the exact FIDE profile for Илья Дерябин; FIDE stores Derjabin, Ilja.
    "4196": ("14104768", "https://ratings.fide.com/profile/14104768"),
    # User provided the exact FIDE profile for Kiril/Kyrylo Nesterenko.
    "7015": ("14176335", "https://ratings.fide.com/profile/14176335"),
    # Chess.com identifies Akylbek2021 as FM A Daurimbetov; FIDE stores Daurimbetov, Azamat.
    "126": ("14201950", "https://www.chess.com/players/a-daurimbetov"),
    # Chess.com identifies Alexm42100 as USA NM Alex Moore; this matches the USA FIDE row.
    "179": ("30910692", "https://www.chess.com/members/titled-players"),
    # Wikidata/Chess.com identify Kargan90 as GM Karthikeyan Pandian; FIDE stores Karthikeyan, P.
    "1798": ("5018226", "https://www.wikidata.org/wiki/Q27526418"),
    # Chess.com identifies BocharovD as GM Dmitriy Bocharov.
    "504": ("4138716", "https://www.chess.com/member/bocharovd"),
    # FIDE exact profile for French IM Guillaume Vallin.
    "1478": ("614823", "https://ratings.fide.com/profile/614823"),
    # ChessPrime/FIDE identify Ilya/Iljya Grigorjev as FIDE ID 24159565.
    "2913": ("24159565", "https://chessprime.com/players/player/329309/"),
    # FIDE exact profile for IM Licael Roderick Ticona Rocabado.
    "3174": ("3319113", "https://ratings.fide.com/profile/3319113"),
    # Chess.com/Lichess identify Victor_0liveira as Brazilian CM Victor Oliveira.
    "3495": ("2137798", "https://www.chess.com/member/victor_0liveira"),
    # FIDE exact profile for GM Salvador Gabriel Del Rio De Angelis.
    "5311": ("2203138", "https://ratings.fide.com/profile/2203138"),
    # Chess.com identifies ticokiller as Costa Rican IM Francisco Hernandez.
    "5579": ("6500226", "https://www.chess.com/members/titled-players/international-masters?page=38"),
    # Lichess identifies vovasev as FM Vladimir Sevostianov, FIDE ID 4144163.
    "5715": ("4144163", "https://lichess.org/coach/vovasev"),
    # Chess.com identifies whatupy0dog as USA NM Josh Harrison; this matches the Joshua Harrison FIDE row.
    "5737": ("30926530", "https://www.chess.com/member/whatupy0dog"),
    # Web search found the exact FIDE profile for FM Pranav K P.
    "7600": ("25685805", "https://ratings.fide.com/profile/25685805"),
    # KCF University Cup links tutifrutty to WFM Parahitha Legowo; FIDE stores Legowo Parahita Millyena.
    "8048": ("7105851", "https://ratings.fide.com/profile/7105851"),
    # Chess.com identifies Vagoff as CM Jose Chaluja; FIDE stores Chaluja Garcia Prieto, Jose Antonio.
    "8073": ("6600565", "https://ratings.fide.com/profile/6600565"),
    # Web search found the exact FIDE profile for FM Yanki Taspinar.
    "8221": ("34561188", "https://ratings.fide.com/profile/34561188"),
}

WEB_BIRTH_YEAR_OVERRIDES = {
    "910": {
        "found_birth_year": "1984",
        "status": "found_web_source_chessbase",
        "source": "https://players.chessbase.com/en/player/ballecer_dino/16044",
        "fide_id": "5203597",
        "fide_name": "Ballecer, Dino",
        "fide_country": "PHI",
        "fide_title": "FM",
        "fide_max_rating": "2372",
        "match_kind": "web_profile_fide_id",
    },
    "4952": {
        "found_birth_year": "1984",
        "status": "found_web_source_chessbase",
        "source": "https://players.chessbase.com/en/player/Khodjimatov_Sherzod/721871",
        "fide_id": "2618060",
        "fide_name": "Khodjimatov, Sherzod",
        "fide_country": "CAN",
        "fide_title": "",
        "fide_max_rating": "2165",
        "match_kind": "web_profile_fide_id",
    },
    "3153": {
        "found_birth_year": "1985",
        "status": "found_web_source_wikidata",
        "source": "https://www.wikidata.org/wiki/Q55865276",
        "fide_id": "5204100",
        "fide_name": "Severino, Sander",
        "fide_country": "PHI",
        "fide_title": "FM",
        "fide_max_rating": "2340",
        "match_kind": "web_profile_fide_id",
    },
    "6798": {
        "found_birth_year": "1986",
        "status": "found_web_source_chessbase",
        "source": "https://players.chessbase.com/en/player/Pereyra_Horacio/369837",
        "fide_id": "113700",
        "fide_name": "Pereyra, Horacio",
        "fide_country": "ARG",
        "fide_title": "",
        "fide_max_rating": "2149",
        "match_kind": "web_profile_fide_id",
    },
    "7685": {
        "found_birth_year": "1990",
        "status": "found_web_source_chesscom",
        "source": "https://www.chess.com/players/ross-lam",
        "fide_id": "3221890",
        "fide_name": "Lam, Ross",
        "fide_country": "AUS",
        "fide_title": "CM",
        "fide_max_rating": "2059",
        "match_kind": "web_profile_fide_id",
    },
    "2109": {
        "found_birth_year": "2006",
        "status": "found_web_source_fide_profile",
        "source": "https://ratings.fide.com/profile/1651374",
        "fide_id": "1651374",
        "fide_name": "Miazhynski, Michael",
        "fide_country": "AUT",
        "fide_title": "CM",
        "fide_max_rating": "2258",
        "match_kind": "web_profile_fide_id",
    },
}

TITLE_STRENGTH = {"GM": 8, "IM": 7, "WGM": 6, "FM": 5, "WIM": 4, "CM": 3, "WFM": 2, "WCM": 1}

CYRILLIC = {
    "а": "a",
    "б": "b",
    "в": "v",
    "г": "g",
    "д": "d",
    "е": "e",
    "ё": "e",
    "ж": "zh",
    "з": "z",
    "и": "i",
    "й": "i",
    "к": "k",
    "л": "l",
    "м": "m",
    "н": "n",
    "о": "o",
    "п": "p",
    "р": "r",
    "с": "s",
    "т": "t",
    "у": "u",
    "ф": "f",
    "х": "kh",
    "ц": "ts",
    "ч": "ch",
    "ш": "sh",
    "щ": "shch",
    "ы": "y",
    "э": "e",
    "ю": "yu",
    "я": "ya",
    "ь": "",
    "ъ": "",
    "і": "i",
    "ї": "yi",
    "є": "e",
    "ґ": "g",
}


def missing_birthday(value: str | None) -> bool:
    text = (value or "").strip()
    return text == "" or text.lower() == "nan"


def strip_accents(value: str) -> str:
    value = unicodedata.normalize("NFKD", value)
    return "".join(ch for ch in value if not unicodedata.combining(ch))


def transliterate_cyrillic(value: str) -> str:
    out = []
    for ch in value.lower():
        out.append(CYRILLIC.get(ch, ch))
    return "".join(out)


def normalized(value: str) -> str:
    value = strip_accents(value or "").lower()
    value = value.replace("ø", "o").replace("ł", "l").replace("đ", "d").replace("þ", "th")
    value = re.sub(r"[^a-z0-9 -]+", " ", value)
    value = value.replace("-", " ")
    return re.sub(r"\s+", " ", value).strip()


def expand_latin_variants(value: str) -> set[str]:
    variants = {value}
    pending = [value]
    processed = set()
    while pending:
        item = pending.pop()
        if item in processed:
            continue
        processed.add(item)
        before = len(variants)
        tokens = item.split()
        if tokens and tokens[0] == "alex":
            variants.add(" ".join(["alexander", *tokens[1:]]))
        if tokens and tokens[0] in {"aleksandr", "alexandr"}:
            variants.add(" ".join(["alexander", *tokens[1:]]))
        if tokens and tokens[0] == "zach":
            variants.add(" ".join(["zachary", *tokens[1:]]))
        if tokens and tokens[0] == "max":
            variants.add(" ".join(["maxim", *tokens[1:]]))
        if tokens and tokens[0] == "leo":
            variants.add(" ".join(["leonardo", *tokens[1:]]))
        if tokens and tokens[0] == "artemiki":
            variants.add(" ".join(["artem", *tokens[1:]]))
        if tokens and tokens[0] == "eldiar":
            variants.add(" ".join(["eldiyar", *tokens[1:]]))
        if tokens and tokens[0] == "christian":
            variants.add(" ".join(["cristian", *tokens[1:]]))
        if tokens and tokens[0] in {"dmitrii", "dmitri"}:
            variants.add(" ".join(["dmitry", *tokens[1:]]))
        if tokens and tokens[0] in {"evgenii", "evgeny"}:
            variants.add(" ".join(["evgeniy", *tokens[1:]]))
            variants.add(" ".join(["evgenij", *tokens[1:]]))
        if tokens and tokens[0] in {"yurii", "yuri"}:
            variants.add(" ".join(["jouri", *tokens[1:]]))
        if tokens and tokens[0] == "olexandr":
            variants.add(" ".join(["oleksandr", *tokens[1:]]))
        if tokens and tokens[0] == "shakhzod":
            variants.add(" ".join(["shahzod", *tokens[1:]]))
        if tokens and tokens[0] == "koryun":
            variants.add(" ".join(["korun", *tokens[1:]]))
        if tokens and tokens[0] in {"vasilii", "vasiliy", "vasily"}:
            variants.add(" ".join(["vasilij", *tokens[1:]]))
        if tokens and tokens[0] == "darya":
            variants.add(" ".join(["daria", *tokens[1:]]))
        if tokens and tokens[0] == "iryna":
            variants.add(" ".join(["irina", *tokens[1:]]))
        if tokens and tokens[0] in {"vitalii", "vitali"}:
            variants.add(" ".join(["vitaly", *tokens[1:]]))
        if tokens and tokens[0] == "valery":
            variants.add(" ".join(["valerij", *tokens[1:]]))
        if tokens and tokens[0] == "srivastan":
            variants.add(" ".join(["srivatsan", *tokens[1:]]))
        if tokens and tokens[0] == "mariya":
            variants.add(" ".join(["maria", *tokens[1:]]))
        if tokens and tokens[0] in {"aleksei", "alexei"}:
            variants.add(" ".join(["alexey", *tokens[1:]]))
        if tokens and tokens[0] == "jeff":
            variants.add(" ".join(["jeffrey", *tokens[1:]]))
        word_replacements = {
            "vitalii": "vitaly",
            "vitali": "vitaly",
            "valerii": "valery",
            "natalya": "nataliya",
            "klyashtornyi": "klyashtorny",
            "dmitro": "dmytro",
            "yurii": "yury",
            "yuri": "yury",
            "volodimir": "volodymyr",
            "radibratovi": "radibratovic",
            "biletskii": "biletskyy",
            "biletsky": "biletskyy",
        }
        for src, dst in word_replacements.items():
            if src in tokens:
                variants.add(" ".join(dst if token == src else token for token in tokens))
        if tokens and tokens[-1] == "ash":
            variants.add(" ".join([*tokens[:-1], "arsh"]))
        if " ash " in f" {item} ":
            variants.add(item.replace(" ash ", " arsh "))
        if tokens and tokens[0] in {"mikola", "mykola"}:
            variants.add(" ".join(["nikolay", *tokens[1:]]))
            variants.add(" ".join(["mykola", *tokens[1:]]))
        if tokens and tokens[-1] == "gedgaf":
            variants.add(" ".join([*tokens[:-1], "gedgafov"]))
        if len(tokens) >= 2:
            variants.add(" ".join([tokens[0] + tokens[1], *tokens[2:]]))
        if len(tokens) == 4:
            variants.add(" ".join([*tokens[1:], tokens[0]]))
        if "weichao" in item:
            variants.add(item.replace("weichao", "wei chao"))
        if "naidin" in item:
            variants.add(item.replace("naidin", "najdin"))
        if "ghaemmaghami" in item:
            variants.add(item.replace("ghaemmaghami", "ghaem maghami"))
        if "sengdorzh" in item:
            variants.add(item.replace("sengdorzh", "sengdorji"))
        if "goryachkin" in item:
            variants.add(item.replace("goryachkin", "goriatchkin"))
        if "lindholdt" in item:
            variants.add(item.replace("lindholdt", "lindholt"))
        if "prohorov" in item:
            variants.add(item.replace("prohorov", "prokhorov"))
        if "vokhidov" in item:
            variants.add(item.replace("vokhidov", "vakhidov"))
        if "gyulamiryan" in item:
            variants.add(item.replace("gyulamiryan", "gulamirian"))
        if "tulchinskii" in item:
            variants.add(item.replace("tulchinskii", "tulchynskyi"))
        if "tulchinsky" in item:
            variants.add(item.replace("tulchinsky", "tulchynskyi"))
        if "glidzhian" in item:
            variants.add(item.replace("glidzhian", "glidzhain"))
        if "halynyazow" in item:
            variants.add(item.replace("halynyazow", "halynyazov"))
        if "kazantsev" in item:
            variants.add(item.replace("kazantsev", "kazancev"))
        if "kopenkin" in item:
            variants.add(item.replace("kopenkin", "kopjonkin"))
        if "kliashtornyi" in item:
            variants.add(item.replace("kliashtornyi", "klyashtorny"))
        if "kliashtorny" in item:
            variants.add(item.replace("kliashtorny", "klyashtorny"))
        if "kovalev" in item:
            variants.add(item.replace("kovalev", "kovalyov"))
        if "legenya" in item:
            variants.add(item.replace("legenya", "legenia"))
        if "maevskiy" in item:
            variants.add(item.replace("maevskiy", "maevsky"))
        if "furtado" in item and "junior" not in item:
            variants.add(f"{item} de vjunior")
        if "manukyan" in item:
            variants.add(item.replace("manukyan", "manukian"))
        if "nechitailo" in item:
            variants.add(item.replace("nechitailo", "nechitaylo"))
        if "glidzhyan" in item:
            variants.add(item.replace("glidzhyan", "glidzhain"))
        if "shchekach" in item:
            variants.add(item.replace("shchekach", "schekach"))
        if "weishautel" in item:
            variants.add(item.replace("weishautel", "weishaeutel"))
        if "lobler" in item:
            variants.add(item.replace("lobler", "loebler"))
        if "annaberdiev" in item:
            variants.add(item.replace("annaberdiev", "annaberdiyev"))
        if "knyazev" in item:
            variants.add(item.replace("knyazev", "kniazev"))
        if "kh" in item:
            variants.add(item.replace("kh", "h"))
        if "ks" in item:
            variants.add(item.replace("ks", "x"))
        if "th" in item:
            variants.add(item.replace("th", "t"))
        if "ei" in item:
            variants.add(item.replace("ei", "ey"))
        if "ey" in item:
            variants.add(item.replace("ey", "ei"))
        if "sarkisyan" in item:
            variants.add(item.replace("sarkisyan", "sargsyan"))
        if "sarkissian" in item:
            variants.add(item.replace("sarkissian", "sargsyan"))
        if "skii" in item:
            variants.add(item.replace("skii", "sky"))
        if len(variants) > before:
            pending.extend(sorted(variants - processed - set(pending)))
    return variants


def clean_real_name(value: str) -> str:
    value = re.sub(r"\[[^\]]+\]", " ", value or "")
    value = re.sub(r"\([A-Z]{2,3}\)", " ", value)
    value = re.sub(r"\s+", " ", value).strip()
    return value


def target_forms(real_name: str) -> dict[str, set[str]]:
    names = {clean_real_name(real_name)}
    translit = transliterate_cyrillic(real_name)
    if translit != real_name.lower():
        names.add(translit)

    exact = set()
    first_last = set()
    swapped = set()
    for name in names:
        normalized_names = expand_latin_variants(normalized(name))
        for n in normalized_names:
            if not n:
                continue
            exact.add(n)
            toks = n.split()
            if len(toks) >= 2:
                first_last.add(f"{toks[0]} {toks[-1]}")
                swapped.add(" ".join([toks[-1], *toks[:-1]]))
                swapped.add(f"{toks[-1]} {toks[0]}")
    return {"exact": exact, "first_last": first_last, "swapped": swapped}


def fide_forms(name: str) -> dict[str, set[str]]:
    names = {name}
    if "," in name:
        last, rest = name.split(",", 1)
        names.add(f"{rest.strip()} {last.strip()}".strip())

    exact = set()
    first_last = set()
    swapped = set()
    for item in names:
        n = normalized(item)
        if not n:
            continue
        exact.add(n)
        toks = n.split()
        if len(toks) >= 2:
            first_last.add(f"{toks[0]} {toks[-1]}")
            swapped.add(" ".join([toks[-1], *toks[:-1]]))
            swapped.add(f"{toks[-1]} {toks[0]}")
    return {"exact": exact, "first_last": first_last, "swapped": swapped}


def match_strength(target: dict[str, set[str]], fide: dict[str, set[str]]) -> tuple[int, str, str]:
    exact = (target["exact"] | target["swapped"]) & fide["exact"]
    if exact:
        return 3, "exact_full_name", ";".join(sorted(exact))
    embedded = set()
    for target_name in target["exact"] | target["swapped"]:
        for fide_name in fide["exact"]:
            if fide_name.startswith(f"{target_name} "):
                embedded.add(target_name)
    if embedded:
        return 2, "embedded_name", ";".join(sorted(embedded))
    partial = target["first_last"] & (fide["first_last"] | fide["exact"])
    if partial:
        return 1, "first_last_partial", ";".join(sorted(partial))
    return 0, "", ""


def max_rating(row: dict[str, str]) -> int:
    values = []
    for key in ("fide_standard_rating", "fide_rapid_rating", "fide_blitz_rating"):
        try:
            values.append(int(row.get(key) or 0))
        except ValueError:
            values.append(0)
    return max(values or [0])


def title_score(title: str) -> int:
    return TITLE_STRENGTH.get((title or "").strip().upper(), 0)


def candidate_score(row: dict[str, str]) -> tuple[int, int, int, int]:
    country = int((row["dataset_federation"] or "").upper() == (row["fide_country"] or "").upper())
    return (
        int(row["match_strength"]),
        country,
        title_score(row.get("fide_title", "")),
        max_rating(row),
    )


def has_full_target_name_overlap(row: dict[str, str]) -> bool:
    raw_tokens = [
        token
        for token in normalized(clean_real_name(row.get("real_name", ""))).split()
        if len(token) > 1
    ]
    if len(raw_tokens) <= 2:
        required = len(raw_tokens)
    else:
        required = len(raw_tokens) - 1
    for value in (row.get("match_values") or "").split(";"):
        if len(value.split()) >= required:
            return True
    return False


def select_candidate(candidates: list[dict[str, str]]) -> tuple[dict[str, str] | None, str]:
    if not candidates:
        return None, "not_found"

    manual = [c for c in candidates if c.get("match_kind") == "manual_fide_id_web_verified"]
    if manual:
        return sorted(manual, key=candidate_score, reverse=True)[0], "found_high_confidence_web_verified_fide_id"

    exact_country = [
        c
        for c in candidates
        if int(c["match_strength"]) >= 2
        and (c["dataset_federation"] or "").upper() == (c["fide_country"] or "").upper()
    ]
    if len(exact_country) == 1:
        return exact_country[0], "found_high_confidence_fide_exact_country"

    if exact_country and len({c["found_birth_year"] for c in exact_country}) == 1:
        return sorted(exact_country, key=candidate_score, reverse=True)[0], "found_high_confidence_fide_same_year_country"

    if exact_country:
        ranked_country = sorted(exact_country, key=lambda row: max_rating(row), reverse=True)
        best_country = ranked_country[0]
        second_country = ranked_country[1] if len(ranked_country) > 1 else None
        if max_rating(best_country) >= 1800 and (
            second_country is None or max_rating(best_country) - max_rating(second_country) >= 300
        ):
            return best_country, "found_medium_confidence_best_rated_exact_country"

    exact_full_name = [
        c
        for c in candidates
        if c["match_kind"] == "exact_full_name" and has_full_target_name_overlap(c)
    ]
    if len(exact_full_name) == 1 and (
        title_score(exact_full_name[0]["fide_title"]) or max_rating(exact_full_name[0]) >= 1400
    ):
        return exact_full_name[0], "found_high_confidence_unique_fide_exact_name"

    exact = [c for c in candidates if int(c["match_strength"]) >= 2]
    if len(exact) == 1 and (title_score(exact[0]["fide_title"]) or max_rating(exact[0]) >= 2200):
        return exact[0], "found_medium_confidence_unique_strong_exact"
    if len(exact) == 1 and max_rating(exact[0]) >= 1800:
        return exact[0], "found_medium_confidence_unique_rated_exact"

    if exact:
        ranked = sorted(exact, key=candidate_score, reverse=True)
        best = ranked[0]
        second = ranked[1] if len(ranked) > 1 else None
        if (
            (title_score(best["fide_title"]) or max_rating(best) >= 2200)
            and (second is None or candidate_score(best)[:3] > candidate_score(second)[:3])
        ):
            return best, "found_medium_confidence_best_titled_exact"
        if title_score(best["fide_title"]) >= TITLE_STRENGTH["FM"] and (
            second is None or max_rating(best) - max_rating(second) >= 500
        ):
            return best, "found_medium_confidence_best_strong_exact"
        strong_ranked = sorted(
            exact,
            key=lambda row: (title_score(row["fide_title"]), max_rating(row)),
            reverse=True,
        )
        best_strong = strong_ranked[0]
        next_strong = strong_ranked[1] if len(strong_ranked) > 1 else None
        if title_score(best_strong["fide_title"]) >= TITLE_STRENGTH["FM"] and max_rating(best_strong) >= 2200:
            if next_strong is None or (
                title_score(best_strong["fide_title"]) > title_score(next_strong["fide_title"])
                and max_rating(best_strong) - max_rating(next_strong) >= 500
            ):
                return best_strong, "found_medium_confidence_best_strong_exact"

    partial_country = [
        c
        for c in candidates
        if int(c["match_strength"]) == 1
        and (c["dataset_federation"] or "").upper() == (c["fide_country"] or "").upper()
    ]
    if len(partial_country) == 1 and (title_score(partial_country[0]["fide_title"]) or max_rating(partial_country[0]) >= 1400):
        return partial_country[0], "found_medium_confidence_partial_country"

    partial = [c for c in candidates if int(c["match_strength"]) == 1]
    if len(partial) == 1 and (title_score(partial[0]["fide_title"]) or max_rating(partial[0]) >= 2100):
        return partial[0], "found_medium_confidence_unique_strong_partial"
    if partial:
        ranked = sorted(partial, key=candidate_score, reverse=True)
        best = ranked[0]
        second = ranked[1] if len(ranked) > 1 else None
        if (
            (title_score(best["fide_title"]) >= TITLE_STRENGTH["FM"] or max_rating(best) >= 2300)
            and (second is None or max_rating(best) - max_rating(second) >= 500)
        ):
            return best, "found_medium_confidence_best_strong_partial"
        if max_rating(best) >= 1400 and (second is None or max_rating(best) - max_rating(second) >= 500):
            return best, "found_medium_confidence_best_rated_partial"

    return None, "needs_review_multiple_candidates"


def input_delimiter(input_csv: Path) -> str:
    with input_csv.open(newline="", encoding="utf-8") as fh:
        header = fh.readline()
    return ";" if header.count(";") > header.count(",") else ","


def load_targets(input_csv: Path) -> tuple[list[dict[str, str]], list[str], str]:
    delimiter = input_delimiter(input_csv)
    with input_csv.open(newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh, delimiter=delimiter)
        rows = list(reader)
        fields = reader.fieldnames or []

    targets = []
    for line, row in enumerate(rows, start=2):
        if (row.get("real_name") or "").strip() and missing_birthday(row.get("birthday")):
            target = dict(row)
            target["_csv_line"] = str(line)
            target["_forms"] = target_forms(row["real_name"])
            targets.append(target)
    return targets, fields, delimiter


def build_variant_index(targets: list[dict[str, str]]) -> dict[str, list[dict[str, str]]]:
    index: dict[str, list[dict[str, str]]] = defaultdict(list)
    for target in targets:
        for group in target["_forms"].values():
            for value in group:
                index[value].append(target)
    return index


def scan_fide(fide_zip: Path, targets: list[dict[str, str]]) -> list[dict[str, str]]:
    variant_index = build_variant_index(targets)
    manual_targets_by_id: dict[str, list[tuple[dict[str, str], str]]] = defaultdict(list)
    for target in targets:
        if target["_csv_line"] in MANUAL_FIDE_IDS:
            fide_id, source_url = MANUAL_FIDE_IDS[target["_csv_line"]]
            manual_targets_by_id[fide_id].append((target, source_url))
    matches = []
    seen = set()
    with zipfile.ZipFile(fide_zip) as zf:
        xml_name = zf.namelist()[0]
        with zf.open(xml_name) as fh:
            for event, elem in ET.iterparse(fh, events=("end",)):
                if elem.tag != "player":
                    continue
                player = {child.tag: (child.text or "").strip() for child in elem}
                birthday = player.get("birthday", "")
                fide_name = player.get("name", "")
                if birthday and fide_name:
                    manual_targets = manual_targets_by_id.get(player.get("fideid", ""))
                    if manual_targets:
                        for target, source_url in manual_targets:
                            key = (target["_csv_line"], player.get("fideid", ""))
                            if key not in seen:
                                seen.add(key)
                                matches.append(
                                    {
                                        "csv_line": target["_csv_line"],
                                        "player_name": target.get("player_name", ""),
                                        "real_name": target.get("real_name", ""),
                                        "dataset_federation": target.get("federation", ""),
                                        "dataset_country_name": target.get("country_name", ""),
                                        "found_birth_year": birthday,
                                        "source": f"{FIDE_SOURCE}; FIDE ID verified at {source_url}",
                                        "source_type": "FIDE player list with web-verified FIDE ID",
                                        "fide_id": player.get("fideid", ""),
                                        "fide_name": fide_name,
                                        "fide_country": player.get("country", ""),
                                        "fide_title": player.get("title", ""),
                                        "fide_standard_rating": player.get("rating", ""),
                                        "fide_rapid_rating": player.get("rapid_rating", ""),
                                        "fide_blitz_rating": player.get("blitz_rating", ""),
                                        "match_strength": "4",
                                        "match_kind": "manual_fide_id_web_verified",
                                        "match_values": player.get("fideid", ""),
                                    }
                                )
                    forms = fide_forms(fide_name)
                    possible_targets = []
                    for group in forms.values():
                        for value in group:
                            possible_targets.extend(variant_index.get(value, []))
                    for target in possible_targets:
                        strength, kind, overlap = match_strength(target["_forms"], forms)
                        if not strength:
                            continue
                        key = (target["_csv_line"], player.get("fideid", ""))
                        if key in seen:
                            continue
                        seen.add(key)
                        matches.append(
                            {
                                "csv_line": target["_csv_line"],
                                "player_name": target.get("player_name", ""),
                                "real_name": target.get("real_name", ""),
                                "dataset_federation": target.get("federation", ""),
                                "dataset_country_name": target.get("country_name", ""),
                                "found_birth_year": birthday,
                                "source": FIDE_SOURCE,
                                "source_type": "FIDE player list",
                                "fide_id": player.get("fideid", ""),
                                "fide_name": fide_name,
                                "fide_country": player.get("country", ""),
                                "fide_title": player.get("title", ""),
                                "fide_standard_rating": player.get("rating", ""),
                                "fide_rapid_rating": player.get("rapid_rating", ""),
                                "fide_blitz_rating": player.get("blitz_rating", ""),
                                "match_strength": str(strength),
                                "match_kind": kind,
                                "match_values": overlap,
                            }
                        )
                elem.clear()
    return matches


def write_outputs(
    input_csv: Path,
    fieldnames: list[str],
    input_csv_delimiter: str,
    targets: list[dict[str, str]],
    matches: list[dict[str, str]],
    candidates_csv: Path,
    summary_csv: Path,
    filled_csv: Path,
    found_csv: Path,
    unresolved_csv: Path,
) -> None:
    by_line: dict[str, list[dict[str, str]]] = defaultdict(list)
    for row in matches:
        by_line[row["csv_line"]].append(row)

    selected = {}
    summary_rows = []
    for target in targets:
        line = target["_csv_line"]
        choice, status = select_candidate(by_line.get(line, []))
        if choice:
            selected[line] = (choice, status)
            summary_rows.append(
                {
                    "csv_line": line,
                    "player_name": target.get("player_name", ""),
                    "real_name": target.get("real_name", ""),
                    "dataset_federation": target.get("federation", ""),
                    "dataset_country_name": target.get("country_name", ""),
                    "found_birth_year": choice["found_birth_year"],
                    "status": status,
                    "source": choice["source"],
                    "fide_id": choice["fide_id"],
                    "fide_name": choice["fide_name"],
                    "fide_country": choice["fide_country"],
                    "fide_title": choice["fide_title"],
                    "fide_max_rating": str(max_rating(choice)),
                    "match_kind": choice["match_kind"],
                    "candidate_count": str(len(by_line.get(line, []))),
                }
            )
        elif line in WEB_BIRTH_YEAR_OVERRIDES:
            override = WEB_BIRTH_YEAR_OVERRIDES[line]
            selected[line] = (override, override["status"])
            summary_rows.append(
                {
                    "csv_line": line,
                    "player_name": target.get("player_name", ""),
                    "real_name": target.get("real_name", ""),
                    "dataset_federation": target.get("federation", ""),
                    "dataset_country_name": target.get("country_name", ""),
                    "found_birth_year": override["found_birth_year"],
                    "status": override["status"],
                    "source": override["source"],
                    "fide_id": override["fide_id"],
                    "fide_name": override["fide_name"],
                    "fide_country": override["fide_country"],
                    "fide_title": override["fide_title"],
                    "fide_max_rating": override["fide_max_rating"],
                    "match_kind": override["match_kind"],
                    "candidate_count": str(len(by_line.get(line, []))),
                }
            )
        else:
            summary_rows.append(
                {
                    "csv_line": line,
                    "player_name": target.get("player_name", ""),
                    "real_name": target.get("real_name", ""),
                    "dataset_federation": target.get("federation", ""),
                    "dataset_country_name": target.get("country_name", ""),
                    "found_birth_year": "",
                    "status": status,
                    "source": "",
                    "fide_id": "",
                    "fide_name": "",
                    "fide_country": "",
                    "fide_title": "",
                    "fide_max_rating": "",
                    "match_kind": "",
                    "candidate_count": str(len(by_line.get(line, []))),
                }
            )

    candidate_fields = [
        "csv_line",
        "player_name",
        "real_name",
        "dataset_federation",
        "dataset_country_name",
        "found_birth_year",
        "source",
        "source_type",
        "fide_id",
        "fide_name",
        "fide_country",
        "fide_title",
        "fide_standard_rating",
        "fide_rapid_rating",
        "fide_blitz_rating",
        "match_strength",
        "match_kind",
        "match_values",
    ]
    with candidates_csv.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=candidate_fields)
        writer.writeheader()
        writer.writerows(sorted(matches, key=lambda r: (int(r["csv_line"]), -int(r["match_strength"]), r["fide_id"])))

    summary_fields = [
        "csv_line",
        "player_name",
        "real_name",
        "dataset_federation",
        "dataset_country_name",
        "found_birth_year",
        "status",
        "source",
        "fide_id",
        "fide_name",
        "fide_country",
        "fide_title",
        "fide_max_rating",
        "match_kind",
        "candidate_count",
    ]
    with summary_csv.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=summary_fields)
        writer.writeheader()
        writer.writerows(summary_rows)

    for path, rows in (
        (found_csv, [row for row in summary_rows if row["status"].startswith("found_")]),
        (unresolved_csv, [row for row in summary_rows if not row["status"].startswith("found_")]),
    ):
        with path.open("w", newline="", encoding="utf-8") as fh:
            writer = csv.DictWriter(fh, fieldnames=summary_fields)
            writer.writeheader()
            writer.writerows(rows)

    with input_csv.open(newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh, delimiter=input_csv_delimiter)
        original_rows = list(reader)

    filled_fields = list(fieldnames)
    for extra in ("birthday_lookup_status", "birthday_source", "birthday_fide_id", "birthday_fide_name"):
        if extra not in filled_fields:
            filled_fields.append(extra)

    for line, row in enumerate(original_rows, start=2):
        key = str(line)
        if key in selected and missing_birthday(row.get("birthday")):
            choice, status = selected[key]
            row["birthday"] = choice["found_birth_year"]
            row["birthday_lookup_status"] = status
            row["birthday_source"] = choice["source"]
            row["birthday_fide_id"] = choice["fide_id"]
            row["birthday_fide_name"] = choice["fide_name"]
        else:
            row.setdefault("birthday_lookup_status", "")
            row.setdefault("birthday_source", "")
            row.setdefault("birthday_fide_id", "")
            row.setdefault("birthday_fide_name", "")

    with filled_csv.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(
            fh,
            fieldnames=filled_fields,
            delimiter=input_csv_delimiter,
            quotechar='"',
            quoting=csv.QUOTE_MINIMAL,
        )
        writer.writeheader()
        writer.writerows(original_rows)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", type=Path, default=Path("players_final_data_merged.csv"))
    parser.add_argument("--fide-zip", type=Path, default=Path("/private/tmp/players_list_xml.zip"))
    parser.add_argument("--candidates", type=Path, default=Path("missing_birthdays_fide_candidates.csv"))
    parser.add_argument("--summary", type=Path, default=Path("missing_birthdays_lookup_summary.csv"))
    parser.add_argument("--filled", type=Path, default=Path("players_final_data_merged_birthdays_filled.csv"))
    parser.add_argument("--found", type=Path, default=Path("missing_birthdays_found.csv"))
    parser.add_argument("--unresolved", type=Path, default=Path("missing_birthdays_unresolved.csv"))
    args = parser.parse_args()

    targets, fields, input_csv_delimiter = load_targets(args.input)
    matches = scan_fide(args.fide_zip, targets)
    write_outputs(
        args.input,
        fields,
        input_csv_delimiter,
        targets,
        matches,
        args.candidates,
        args.summary,
        args.filled,
        args.found,
        args.unresolved,
    )
    found = 0
    review = 0
    not_found = 0
    with args.summary.open(newline="", encoding="utf-8") as fh:
        for row in csv.DictReader(fh):
            if row["status"].startswith("found_"):
                found += 1
            elif row["status"] == "not_found":
                not_found += 1
            else:
                review += 1
    print(f"target rows: {len(targets)}")
    print(f"candidate matches: {len(matches)}")
    print(f"selected found: {found}")
    print(f"needs review: {review}")
    print(f"not found: {not_found}")
    print(f"wrote {args.candidates}")
    print(f"wrote {args.summary}")
    print(f"wrote {args.filled}")
    print(f"wrote {args.found}")
    print(f"wrote {args.unresolved}")


if __name__ == "__main__":
    main()
