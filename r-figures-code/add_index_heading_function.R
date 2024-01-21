
# Create IDs in # Headings ------------------------------------------------
add_ids_in_headings <- function(qmdname){
  # Read the headings
  #qmdname <- "ch4n.qmd"
  qmdlines <- readLines(qmdname)
  headings <- grep("^##+", qmdlines, value = TRUE)
  #headings
  
  # Create IDs
  indexes <- gsub("^#+", "", headings) %>% # remove #s
    gsub("[^a-zA-Z ]", "", .) %>% # retain only letters
    trimws() %>% # remove backspace in front
    gsub(" ", "-", .) %>% # connect with '-'
    paste0("{#sec-", ., "}") # make ID string
  
  # Create new Headings with IDs
  headings_w_indexes <- paste(headings, indexes)
  
  # Replace old with new
  qmdlinesNEW <- qmdlines
  for (i in seq_along(headings)) {
    qmdlinesNEW <- gsub(headings[i], headings_w_indexes[i], qmdlinesNEW, fixed = TRUE)
  }
  # Check
  if(length(qmdlinesNEW) == length(qmdlines)){
    write(qmdlinesNEW, qmdname)
  }
}

qmdnames <- list.files(pattern = "^ch.*\\.qmd$")
sapply(qmdnames, add_ids_in_headings)
  
