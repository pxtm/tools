## Metadata extraction by patient ID ##
library(xlsx)
setwd('G:/Rubén')
ids <- read.csv2('ids.csv')
metadata <- read.csv2('metadata.csv', header=T)

metadata_new <- metadata[metadata$Código.salida%in%ids$IDS, ]
write.xlsx(metadata_new, 'metadata_selected.xlsx', sheetName = 'Sheet1')


corr.data <- read.xlsx2('simca_data.xlsx', sheetIndex = 1, header=T)
corr.data[, 3:10] <- sapply(corr.data[, 3:10], function(a)as.numeric(as.character(a)))
## boxplots
library(ggplot2); library(ggbeeswarm); library(ggpubr)
pdf('boxplots_clinics.pdf')
for (i in seq_along(corr.data[,4:10])){
  print(ggplot(corr.data, aes_string(x='Class', y=names(corr.data[3+i])))+ geom_boxplot()+ geom_beeswarm()+
        labs(title=names(corr.data[3+i])) + stat_compare_means(method='t.test'))  
}
dev.off()

## correlations
pdf('corrs_clinics_allsamples.pdf')
for (i in seq_along(corr.data[,4:10])){
  print(ggplot(corr.data, aes_string(x='MCH2418', y=names(corr.data[3+i])))+ geom_point()+ geom_smooth()+
          labs(title=names(corr.data[3+i])) + stat_cor(method='pearson')) 
}
dev.off()

corr.data.positive <- subset(corr.data, corr.data$Class=='Positivo')
pdf('corrs_clinics_onlymch.pdf')
for (i in seq_along(corr.data.positive[,4:10])){
  print(ggplot(corr.data.positive, aes_string(x='MCH2418', y=names(corr.data.positive[3+i])))+ geom_point()+ geom_smooth()+
          labs(title=names(corr.data.positive[3+i])) + stat_cor(method='pearson')) 
}
dev.off()
