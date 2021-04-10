setwd("/Users/pmc959/Desktop/00_Projects/00_SIMP/Phageome/recovery/")
files <- list.files()

## Import all datasets
datalist <- lapply(files, function(x)read.table(x, header=F, row.names=1))  ## import all datasets as a list of dataframes 
samples <- strsplit(files, '\\.') ## pre-process the names of the dataframes
names(datalist) <- unlist(lapply(samples, function(x)(x[c(T, F)]))) ## rename list elements with sample names

## list all bacterial elements
all.phages <- unique(unlist(lapply(datalist, row.names))) 

## generate list of data.frames listing all the bacteria and the correspoding GRiD value per each sample. If there's no value, store a NA
output.final <- lapply(datalist, function(x){
  output <- vector()
  for (i in all.phages){
    if (length(grep(i, row.names(x)))!=0) {
      output[i] <- x[grep(i, row.names(x)), 1] 
    } 
    else {
      output[i] <- NA}
  }
  return(output)
})

## process and convert final list into a single dataframe, with bacteria in rows and samples in columns
output.final.df <- do.call(rbind.data.frame, output.final) ## convert list of DFs to a single DF
names(output.final.df) <- names(output.final[[1]]) ## name bacteria present
output.final.df <- as.data.frame(t(output.final.df)) ## transpose DF so that bacteria are in rows and samples in columns
names(output.final.df) <- names(datalist) ## store samples names in DF
write.table(output.final.df, 'phagesrecovery.txt', sep = '\t')
