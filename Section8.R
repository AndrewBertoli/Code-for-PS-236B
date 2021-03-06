# Text Analysis Code-Make sure you run this in R Studio

# Start by installing these packages

install.packages("tm") 
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("lda")
install.packages("austin", repos="http://r-forge.r-project.org", type="source")
install.packages("ReadMe", repos = "http://r.iq.harvard.edu", type="source")
install.packages("quadprog")
install.packages("igraph")

data = readLines("http://gutenberg.org/cache/epub/100/pg100.txt")

head(data)

# First, let's get rid of all the blank spaces in this data, as well as the copyright information.

data=data[-which(data=="")]
data=data[-grep("Copyright",data,ignore.case=TRUE)]
data=data[-grep("Electronic",data,ignore.case=TRUE)]
data=data[-grep("Shakespeare",data,ignore.case=TRUE)]

# Now we have to extract the 36 plays. They all start with four digit years. They are proceeded by the Sonets, with start 
# with a four digit year, and are followed by "A LOVER'S COMPLAINT", which also starts with a 4 digit year.

years.index <- grep("^[[:digit:]]{1,4}$", data, perl = TRUE)
length(years.index)

# So lets get rid of the sonnets and "A LOVER'S COMPLAINT"

years.index=years.index[-c(1,38)]
years=data[years.index]

# Now let's get all the play names. The names of the plays always come right after the year.

titles=data[1+years.index]

titles

# Now let's find the ends to all the plays. Fortunately, each play ends with the words "THE END" (as does the sonnets and 
# "A LOVER'S COMPLAINT")

end.index=grep("[.]?THE END", data, perl = TRUE)
length(end.index)
end.index=end.index[-c(1,38)]

# extract the plays
plays = rep(0,36)
for(i in 1:36){
    plays[i]=paste(data[years.index[i]:end.index[i]], collapse=" ")}

# Let's recover the indexes for years and ends

years.index <- grep("^[[:digit:]]{1,4}$", data, perl = TRUE)
end.index=grep("[.]?THE END", data, perl = TRUE)

# We will have to go a little out of our way here to get rid of the names in the plays. We must start by putting the 
# plays into a list. 

plays=list()
for(i in 1:36){
    plays[[i]]=data[years.index[i]:end.index[i]]	
}

# Now let's break our plays down from lines to words

for(i in 1:36){
    plays[[i]]=	unlist(strsplit(plays[[i]],split=" "))}


# Now let's get rid of the characters names, which are all in uppercase.

for(i in 1:36){
    plays[[i]]=	plays[[i]][-which(plays[[i]] ==toupper(plays[[i]]))]}

# Now we will proceed as we normally would with text analysis. Let's first put the data into a Corpus

plays.vector=rep(0,36)
for(i in 1:36){
    plays.vector[i]=paste(plays[[i]]	,collapse=" ")
}

# We will start by using package "tm", which is one of the most popular text analysis packages.

require(tm)


plays=Corpus(VectorSource(plays.vector))

meta(plays)=titles

# Now let's do some data cleaning

plays=tm_map(plays,stripWhitespace) # Get rid of large white spaces

plays=tm_map(plays, removePunctuation) # Remove all punctuation

plays=tm_map(plays, removeNumbers) # Remove numbers

plays=tm_map(plays, removeWords, c(stopwords("english"),"thee","thy","thou","yet","upon","tis","sir","now","like","make",
"enter","can","will","shall","say","no","one","come","let")) # Stopwords has about three-hundred commonly used words in English. I added some other terms that it doesn't have after that.

# Now let's stem the words. This will require the package SnowBall

require(SnowballC)
plays=tm_map(plays, stemDocument)

# Now let's make a document-term matrix

TDM=TermDocumentMatrix(plays)

inspect(TDM[1:20,1:20])

# Let's look at frequent terms

findFreqTerms(TDM, 1000)

# Let's make a quick graph of the frequency of the word "War" over time

war.count=as.numeric(inspect(TDM["war",])[1:36])

plot(years,war.count, main="Usage of War in Shakespeare over Time",ylab="year")

abline(lm(war.count~as.numeric(years)),col="green")

# Now let's make a Word Cloud

require(wordcloud)
wordcloud(plays,max.words=50,scale=c(3,0),colors=c(rep("cornflowerblue",15),rep("blue",20),rep("darkblue",15)))

# Now let's set things up to use Wordfish

require("austin")

TDM.Common=removeSparseTerms(TDM,0.3) # Get rid of words that do not appear in 30% of the documents or more

TDM.Matrix=as.matrix(TDM.Common)
colnames(TDM.Matrix)=titles

# So we will use Julius Caesar (16) as the classic example of a tragedy and Merchant of Venice (21) as the classic 
# example of a comedy

wfresults=wordfish(wfm(TDM.Matrix),dir=c(16,21), control=list(tol=1e-4,sigma=3,startparams=NULL)) 

# To get the plays ordered most serious to least serious (by these estimates)

titles[order(wfresults$theta)]

# The vector denoting which plays are comedies is

comedy=c(1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,1,1,0,0,0,1,1,1)

# To combine the two

cbind(titles[order(wfresults$theta)],comedy[order(wfresults$theta)])

# To get the 10 words most associated with tragedies 

wfresults$words[order(wfresults$beta)[1:10]]

# To get the 10 words most associated with comedies

wfresults$words[order(wfresults$beta,decreasing=TRUE)[1:10]]




# Now we can try lda for scaling

require(lda)

# Now let's make the corpus for lda

plays.lda=lexicalize(plays.vector)

out=lda.collapsed.gibbs.sampler(plays.lda$documents,K=2,plays.lda$vocab,num.iterations=100,alpha=0,eta=1)

top.topic.documents(out$document_sums,num.documents=10)

# To get the names

cbind(titles[top.topic.documents(out$document_sums,num.documents=10)[,1]],comedy[top.topic.documents(out$document_sums,
num.documents=10)[,1]])

cbind(titles[top.topic.documents(out$document_sums,num.documents=10)[,2]],comedy[top.topic.documents(out$document_sums,
num.documents=10)[,2]])




# Now let's try structural topic model package

require(stm)

plays.stm=readCorpus(t(TDM),type="dtm")

prepped.plays=prepDocuments(plays.stm$documents,plays.stm$vocab)

output=stm(prepped.plays$documents, vocab=prepped.plays$vocab, K=5)

labelTopics(output,n=10)

plot.STM(output,type="summary")

plot(output,type="labels")

plot(output,type="perspectives",topics=c(1,2))

topic.corrs=topicCorr(output)

plot(topic.corrs)

# Now we can try lda for classification

require(lda)

plays.lda=lexicalize(plays.vector)

out=lda.collapsed.gibbs.sampler(plays.lda$documents,K=3,plays.lda$vocab,num.iterations=100,alpha=0,eta=1)

# To get the names of the top 10 plays in the three categories

cbind(titles[top.topic.documents(out$document_sums,num.documents=10)[,1]],comedy[top.topic.documents(out$document_sums,
num.documents=10)[,1]])

cbind(titles[top.topic.documents(out$document_sums,num.documents=10)[,2]],comedy[top.topic.documents(out$document_sums,
num.documents=10)[,2]])

cbind(titles[top.topic.documents(out$document_sums,num.documents=10)[,3]],comedy[top.topic.documents(out$document_sums,
num.documents=10)[,3]])
