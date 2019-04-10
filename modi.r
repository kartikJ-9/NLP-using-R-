#Loading the libraries for NLP
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

#readLines lets us interactively choose the word document. User Friendly --> CHECKED :D
#For establishing a hypothesis it is important to convert the text to Corpus. 
modi = readLines(file.choose())
modiC = Corpus(VectorSource(modi))
inspect(modiC)

#Text Cleaning
sp = content_transformer(function(x, pattern) gsub(pattern, " ",x ))
modiC = tm_map(modiC, sp, "/")
modiC = tm_map(modiC, sp, "@")
modiC = tm_map(modiC, sp, "\\|")
modiC = tm_map(modiC, sp, "#")
modiC = tm_map(modiC, sp, "!")
modiC = tm_map(modiC, sp, "^")

#For stemming, we imported SnowballC. 
#tm_map is for processing text so that it is suitable for use.
modiC = tm_map(modiC,content_transformer(tolower))
modiC = tm_map(modiC,removeWords,stopwords("english"))
modiC = tm_map(modiC,removeWords,c("#DELIMITER#","#delimiter#"))
modiC = tm_map(modiC,removePunctuation)
modiC = tm_map(modiC,stripWhitespace)

#Generate a term document matrix that will be input to our word cloud
modi_tdm = TermDocumentMatrix(modiC)
m = as.matrix(modi_tdm)
v = sort(rowSums(m),decreasing = TRUE)
d = data.frame(word = names(v),freq = v)
head(d,10)

#Lets now create a word cloud...
set.seed(1234)
wordcloud(words = d$word,freq = d$freq,min.freq = 1,max.words = 200,random.order = FALSE,rot.per = 0.35,colors = brewer.pal(8,"Dark2"))
findFreqTerms(modi_tdm,lowfreq = 4)

findAssocs(modi_tdm,terms = "scam",corlimit = 0.3)
f = d[1:10,]$freq

barplot(f,las = 2,names.arg = d[1:10,]$word,col = "orange",main = "Most Frequent words",ylab = "Word frequencies")





