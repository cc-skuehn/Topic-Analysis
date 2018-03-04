# Topic Analysis - Latent Dirichlet Allocation
# Blog Articles from blog.codecentric.de

#################
### Preprocessing
#################

setwd("~/Documents/Programming_R/TopicAnalysis/Code")

### Read sql dump from mysql

library(RMySQL)
library(DBI)

#con <- dbConnect(RMySQL::MySQL(), host="localhost",dbname="ccblog",user='sk',password = "test")
con <- dbConnect(RMySQL::MySQL(), host="localhost",dbname="ccblog", group = "my-db")

# Print information
dbListTables(con)
dbListFields(con, "wp_users")
dbListFields(con, "wp_2_posts")
# Set encoding
rs <- dbGetQuery(con, 'set character set utf8')
# Read data from MySQL DB (only users and posts)
blog_users <- dbReadTable(con, "wp_users") 
blog_posts <- dbReadTable(con, "wp_2_posts")
# Summary
str(blog_posts)
str(blog_users)

# Sanity check(s) 
# Incomplete cases (No missing values at all)
length(blog_posts[,1]) - sum(complete.cases(blog_posts))
# Max length of fields
nctr <- sapply(blog_posts,nchar)
max_nctr <- nctr[1,] 
for (i in 1:length(max_nctr)) max_nctr[i] = max(nctr[,i])
names(max_nctr) <- names(blog_posts)
max_nctr
# View some tables
table(blog_posts$post_type)
table(blog_posts$post_author)
table(blog_posts$post_status)

############################################################
### Filter post_type == post und post_status == publish
### ---
### Identify Languag of Post via matches with stopdword list
### ---
### Stemming is optional
### ---
### Topic Model packages: LDA, topicmodels, stm 
############################################################

# Filter posts according to type "post" and status "publish"
published_posts <- blog_posts[blog_posts$post_type=="post" & blog_posts$post_status=="publish",]
nof_pp <- length(published_posts[,1])

# Remove blog artifacts (linebreaks, whitespaces) \r,\n,<*>
published_content <- published_posts$post_content
for (i in 1:nof_pp) {
  published_content[i] <- gsub(x=published_content[i],pattern="\\s+",replacement=" ")
  published_content[i] <- gsub(x=published_content[i],pattern="<[^>]*>",replacement=" ")
  published_content[i] <- gsub(x=published_content[i],pattern="\\W",replacement=" ")
  published_content[i] <- gsub(x=published_content[i],pattern="\\s+",replacement=" ")
  published_content[i] <- trimws(x=published_content[i],which = "both")
}

# Read list of english stopwords
stopwords_eng <- read.table("../Data/stop-words.txt",as.is = TRUE)
nof_stopwords_eng <- length(stopwords_eng[,1])
# Read list of german stopwords
stopwords_ger <- read.table("../Data/stopwords_german.txt",comment.char ="|",as.is = TRUE)
nof_stopwords_ger <- length(stopwords_ger[,1])

count_words_and_stopwords <- data.frame(cbind(rep(0,nof_pp),rep(0,nof_pp),rep(0,nof_pp)))
for (i in 1:nof_pp) {
  words <- unlist(strsplit(tolower(published_content[i])," "))
  count_words_and_stopwords[i,1] <- length(words)
  for (j in 1:nof_stopwords_eng) count_words_and_stopwords[i,2] <- count_words_and_stopwords[i,2] + sum(stopwords_eng[j,1]==words)
  for (j in 1:nof_stopwords_ger) count_words_and_stopwords[i,3] <- count_words_and_stopwords[i,3] + sum(stopwords_ger[j,1]==words)
}
colnames(count_words_and_stopwords) = c("Words","EnglishStopwords","GermanStopwords")
count_words_and_stopwords$Ratio_Eng <- count_words_and_stopwords[,2]/count_words_and_stopwords[,1]
count_words_and_stopwords$Ratio_Ger <- count_words_and_stopwords[,3]/count_words_and_stopwords[,1]
head(count_words_and_stopwords)
# Plot resuls of language detection
library(ggplot2)
raw_plot <- ggplot(data=count_words_and_stopwords,aes(x=Ratio_Eng,y=Ratio_Ger))+geom_point()
raw_plot
cluster_detection <- kmeans(count_words_and_stopwords[,4:5],centers = 100)
language_plot <- ggplot(data=count_words_and_stopwords,aes(x=Ratio_Eng,y=Ratio_Ger)) + geom_point(color=(cluster_detection$cluster+1))#
language_plot <- language_plot + ggplot2::annotate(geom="text",x=0.5,y=0.5,label="Colors by kmeans clustering") + ggtitle("Language Detection")
language_plot
cluster_detection$centers
cluster_plot <- ggplot(data=data.frame(cluster_detection$centers),aes(x=Ratio_Eng,y=Ratio_Ger))+geom_point(size=(2*cluster_detection$size/median(cluster_detection$size)));
cluster_plot
# Outlier identification
library(Rlof)
library(DMwR)
outlier_score_01 <- lof(data = count_words_and_stopwords[,4:5],k=50) # lofactor was exactly equal, seems to be the same implementation
scaled_count_words <- count_words_and_stopwords
scaled_count_words$Words <- scaled_count_words$Words/max(scaled_count_words$Words)
scaled_count_words$EnglishStopwords <- scaled_count_words$EnglishStopwords/max(scaled_count_words$EnglishStopwords)
scaled_count_words$GermanStopwords <- scaled_count_words$GermanStopwords/max(scaled_count_words$GermanStopwords)
outlier_score_02 <- lof(data = scaled_count_words,k=50) 
qplot(outlier_score_01,outlier_score_02)
#outlier_plot <- ggplot(data=count_words_and_stopwords,aes(x=Ratio_Eng,y=Ratio_Ger))+geom_point(color=(outlier_score_01>3)+1,size=(outlier_score_01>3)+1)
outlier_plot <- ggplot(data=count_words_and_stopwords,aes(x=Ratio_Eng,y=Ratio_Ger))+geom_point(color=(outlier_score_01>3)+1,size=(1/(count_words_and_stopwords$Words/200)))
outlier_plot
outlier_content <- published_content[outlier_score>2]
# Extract IDs and Language Info
id_language_list <- data.frame(cbind(published_posts$ID,cluster_detection$cluster,"Language"),stringsAsFactors = FALSE)
names(id_language_list)=c("Post_ID","Cluster","Language")
id_language_list[id_language_list[,2]==2,3] <- "German" 
id_language_list[id_language_list[,2]==1,3] <- "English"
id_language_list$Ratio_Eng <- unlist(count_words_and_stopwords$Ratio_Eng)
id_language_list$Ratio_Ger <- unlist(count_words_and_stopwords$Ratio_Ger)
write.csv(x=id_language_list,file="ID_list.csv")

published_content_english = published_content[id_language_list$Language=="English"]
nof_english_posts = length(published_content_english)
published_content_german = published_content[id_language_list$Language=="German"]

###############
### LDA und CTM
###############

library(lda)
library(topicmodels)
library(NLP)
library(tm)
# Create corpus with package tm
german_corpus <- VCorpus(VectorSource(published_content_german),readerControl=list(language="de"))
english_corpus <- VCorpus(VectorSource(published_content_english))
# Check Language Info
for (i in 1:length(english_corpus)) if((english_corpus[[i]]$meta)$language!="en") print(i)
for (i in 1:length(german_corpus)) if((german_corpus[[i]]$meta)$language=="en") print(i) 
# Strip whitespaces and convert to lower case
german_corpus <- tm_map(german_corpus, stripWhitespace)
german_corpus <- tm_map(german_corpus, content_transformer(tolower))
german_corpus <- tm_map(german_corpus, removeWords, stopwords("german"))
german_corpus <- tm_map(german_corpus, removeWords, stopwords("english"))
#german_corpus <- tm_map(german_corpus, stemDocument) # not working for unknown reasons -> TODO
english_corpus <- tm_map(english_corpus, stripWhitespace)
english_corpus <- tm_map(english_corpus, content_transformer(tolower))
english_corpus <- tm_map(english_corpus, removeWords, stopwords("english"))
english_corpus <- tm_map(english_corpus, removeWords, stopwords("german"))
#english_corpus <- tm_map(english_corpus, stemDocument) # not working for unknown reasons -> TODO

# Prepare DocumentTermMatrix
DTM_german <- DocumentTermMatrix(german_corpus)
DTM_english <- DocumentTermMatrix(english_corpus)
inspect(DTM_german)
inspect(DTM_english)
# Apply LDA to corpusses
lda_model_german_10topics <- LDA(DTM_german,k=10) 
terms(lda_model_german_10topics,10)
lda_model_english_10topics <- LDA(DTM_english,k=10) 
terms(lda_model_english_10topics,10)

str(cora.documents)
str(cora.vocab)
# Apply LDA - example
require("ggplot2")
require("reshape2")
data(cora.documents)
data(cora.vocab)
theme_set(theme_bw())
set.seed(8675309)
K <- 10 ## Num clusters
result <- lda.collapsed.gibbs.sampler(cora.documents,
                                      K,  ## Num clusters
                                      cora.vocab,
                                      25,  ## Num iterations
                                      0.1,
                                      0.1,
                                      compute.log.likelihood=TRUE) 
## Get the top words in the cluster
top.words <- top.topic.words(result$topics, 5, by.score=TRUE)
## Number of documents to display
N <- 10
topic.proportions <- t(result$document_sums) / colSums(result$document_sums)
topic.proportions <- topic.proportions[sample(1:dim(topic.proportions)[1], N),]
topic.proportions[is.na(topic.proportions)] <-  1 / K
colnames(topic.proportions) <- apply(top.words, 2, paste, collapse=" ")
topic.proportions.df <- melt(cbind(data.frame(topic.proportions),
                                   document=factor(1:N)),
                             variable.name="topic",
                             id.vars = "document")  
qplot(topic, value, fill=document, ylab="proportion", data=topic.proportions.df)+ theme(axis.text.x = element_text(angle=90, hjust=1))+coord_flip() +facet_wrap(~ document, ncol=5)
# Apply LDA - example
require("ggplot2")
require("reshape2")
data(cora.documents)
data(cora.vocab)
theme_set(theme_bw())
set.seed(8675309)
K <- 10 ## Num clusters
result <- lda.collapsed.gibbs.sampler(cora.documents,
                                      K,  ## Num clusters
                                      cora.vocab,
                                      25,  ## Num iterations
                                      0.1,
                                      0.1,
                                      compute.log.likelihood=TRUE) 
## Get the top words in the cluster
top.words <- top.topic.words(result$topics, 5, by.score=TRUE)
## Number of documents to display
N <- 10
topic.proportions <- t(result$document_sums) / colSums(result$document_sums)
topic.proportions <- topic.proportions[sample(1:dim(topic.proportions)[1], N),]
topic.proportions[is.na(topic.proportions)] <-  1 / K
colnames(topic.proportions) <- apply(top.words, 2, paste, collapse=" ")
topic.proportions.df <- melt(cbind(data.frame(topic.proportions),
                                   document=factor(1:N)),
                             variable.name="topic",
                             id.vars = "document")  
qplot(topic, value, fill=document, ylab="proportion", data=topic.proportions.df)+ theme(axis.text.x = element_text(angle=90, hjust=1))+coord_flip() +facet_wrap(~ document, ncol=5)
# , geom="bar", stat="identity") 
#######################
### Close DB connection
#######################

# Disconnect from the database
dbDisconnect(con)

###########
### End ###
###########

###############
### Exploratory
###############
blog_links <- dbReadTable(con, "wp_2_links")
dbListFields(con, "wp_2_postmeta")
blog_meta <- dbReadTable(con, "wp_2_postmeta")

#####################
### As starting point
#####################

# ##################
# ### mtcars example
# ##################
# library(DBI)
# # Connect to my-db as defined in ~/.my.cnf (one has to create the cnf-file manually)
# con <- dbConnect(RMySQL::MySQL(), group = "my-db")
# 
# dbListTables(con)
# dbWriteTable(con, "mtcars", mtcars)
# dbListTables(con)
# 
# dbListFields(con, "mtcars")
# dbReadTable(con, "mtcars")
# 
# # You can fetch all results:
# res <- dbSendQuery(con, "SELECT * FROM mtcars WHERE cyl = 4")
# dbFetch(res)
# dbClearResult(res)
# 
# # Or a chunk at a time
# res <- dbSendQuery(con, "SELECT * FROM mtcars WHERE cyl = 4")
# while(!dbHasCompleted(res)){
#   chunk <- dbFetch(res, n = 5)
#   print(nrow(chunk))
# }
# # Clear the result
# dbClearResult(res)
# 
# # Disconnect from the database
# dbDisconnect(con)