install.packages('scholar')
library(scholar)

ID <- 'cWXp5J4AAAAJ'
# Citation history
cite.by.year<-get_citation_history(ID)

# How many papers have I published?
lol<-get_num_articles(ID)

# Number of unique journals
not.bad<-get_num_distinct_journals(ID)

# Yeah no
top.journals<-get_num_top_journals(ID,)

# But that's only according to this paper so who cares
# DE Acuna, S Allesina, KP Kording (2012) Future impact: Predicting scientific success. Nature, 489, 201-202. http://dx.doi.org/10.1038/489201a.

# When did I become a scientist?
get_oldest_article(ID)

# Get profile information
profile<-get_profile(ID)

# Get publications and write to a csv file
pubs<-get_publications(ID)
write.csv(pubs, file = "citations.csv")

# Predict my h-index
h.index<-predict_h_index(ID)