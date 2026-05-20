library(chromote)
library(dplyr)
library(tictoc)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

da = read.csv("new_missed_links_2026.csv")

b = ChromoteSession$new()
b$view()


# JavaScript to extract information based on the CSS selector
script <- "
(function() {
  var elements = document.querySelectorAll('.game-overview-row+ .game-overview-row .review-rating-component span');
  var results = [];
  elements.forEach(function(element) {
    results.push(element.innerText);
  });
  return results;
})();
"

da$accuracy1 = NA
da$accuracy2 = NA

# iterate over links to get accuracy

for (i in 1:NROW(da)){
  
  print(i)
  
  b$Page$navigate(da$links[i])
  b$Page$loadEventFired(wait_ = TRUE)  # Wait until the page is fully loaded
  
  Sys.sleep(7)
  
  accuracies <- b$Runtime$evaluate(
    expression = script,
    returnByValue = TRUE
  )$result$value
  
  if (length(accuracies) == 0){
    print("error")
    next
  }
  
  da$accuracy1[i] = as.numeric(accuracies[[1]])
  da$accuracy2[i] = as.numeric(accuracies[[2]])
  
  print(paste("Accuracies:", accuracies[[1]], "|", accuracies[[2]]))
  
  
  if (i %% 500 == 0) {
    filename = paste0("all_", i, ".rds")
    saveRDS(da, filename)
  }
  
  
}

