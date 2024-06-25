
# Extract image names -----------------------------------------------------

extract_image_names <- function(file_path) {
  # Read the contents of the file
  content <- readLines(file_path, warn = FALSE)
  
  # Concatenate all lines into a single string
  content <- paste(content, collapse = "\n")
  
  # Regular expression to match markdown image syntax ![alt text](image.jpg)
  image_pattern <- "!\\[.*?\\]\\((.*?)\\)"
  
  # Find all matches
  matches <- regmatches(content, gregexpr(image_pattern, content, perl = TRUE))[[1]]
  
  # Extract image file names
  image_names <- gsub("!\\[.*?\\]\\((.*?)\\)", "\\1", matches)
  
  # Remove paths, keeping only the file names
  image_names <- basename(image_names)
  
  return(image_names)
}

QMDS <- list.files(pattern = "ch[a-zA-Z0-9]{0,2}\\.qmd$") #ch** EXCLUDIND ch5_pdf
names <- list()
for(i in 1:length(QMDS)) {
  if(length(QMDS) != 8) break 
  chapter <- paste0('ch',i)
  img_names <- extract_image_names(QMDS[i])
  numbering <- paste(chapter, 1:length(img_names), sep='_')
  names[[i]] <- cbind(numbering, img_names)
}
write.csv(do.call(rbind, names), file = paste0('figure_names-', format(Sys.Date(), "%d-%b-%Y"),'.csv'), row.names = TRUE)
