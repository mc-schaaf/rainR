# reads in the files supplied and merges the data into one data frame via appending
# Hence, files must have the same data format (number and identity of columns)
# Returns the merged dataset, with the additional information of the files Name and -Path

mergeFiles <- function(files) {
  for (file in files){
    if (exists("dataset")){
      temp_dataset <-  data.table::fread(file)
      temp_dataset$fileName <- paste(file, sep = "")
      temp_dataset$path <- paste(getwd(), sep = "")
      dataset<-rbind(dataset, temp_dataset, use.names = F)
      rm(temp_dataset)
    }
    if (!exists("dataset")){
      dataset<- data.table::fread(file)
      dataset$fileName <- paste(file, sep = "")
      dataset$path <- paste(getwd(), sep = "")
    }
  }
  return(dataset)
}
