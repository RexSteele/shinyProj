#Mild modification on function from https://shiny.rstudio.com/articles/persistent-data-storage.html#dropbox

library(rdrop2)
token <- readRDS("droptoken.rds")

saveData <- function(bioFrame) {

  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(bioFrame))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(bioFrame, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(dtoken = token, filePath)
}
