library(xlsx)
setwd('C:/Users/mclos/Desktop')
tosimca <- read.xlsx('simca_allclusters_3g.xlsx', header = T, sheetIndex = 2, row.names=1)


classes <- tosimca[,1:2]
tosimca <- tosimca[,-c(1,2)]

tosimca <- as.data.frame(t(tosimca), stringsAsFactors = F)
tosimca[] <- sapply(tosimca, function(x)x/sum(x))
tosimca <- as.data.frame(t(tosimca), stringsAsFactors = F)
tosimca <- data.frame(classes, tosimca)
write.xlsx(tosimca, 'tosimca3G_normalized.xlsx', sheetName ='Sheet 1')


## model ####
library(glmulti)
library(leaps)
library(MASS)

#remove unusure data
unsure <- subset(tosimca, tosimca$Status=='Unsure')
data <- subset(tosimca, tosimca$Status!='Unsure')
data$X.ClassID <- NULL

result1 <- glmulti(Status~., data=data, level=1, crit=aicc, method='l', fitfunction = lm) 

