plot.word_count=function(author){
  str <- str_c(data$sentence_str[data$author==author])
  str.all <- Corpus(VectorSource(str))
  str.all<-tm_map(str.all, stripWhitespace)
  str.all<-tm_map(str.all, content_transformer(tolower))
  str.all<-tm_map(str.all, removeWords, stopwords("english"))
  str.all<-tm_map(str.all, removeWords, character(0))
  str.all<-tm_map(str.all, removePunctuation)
  
  tdm.all<-TermDocumentMatrix(str.all)
  tdm.tidy=tidy(tdm.all)
  kable(tdm.tidy[1:10,])
  # Get the Overall Counts over the Whole Corpus
  tdm.overall=summarise(group_by(tdm.tidy, term), sum(count))
  
  names(tdm.overall) <- c('word', 'count')
  plt <- ggplot(subset(tdm.overall, count>1000)) + geom_bar(aes(x=word, y=count), stat="identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + xlab("Word") + ylab("Count") + labs(title = author)

  print(plt)
}

plot.word_cloud=function(author){
  str <- str_c(data$sentence_str[data$author==author])
  str.all <- Corpus(VectorSource(str))
  str.all<-tm_map(str.all, stripWhitespace)
  str.all<-tm_map(str.all, content_transformer(tolower))
  str.all<-tm_map(str.all, removeWords, stopwords("english"))
  str.all<-tm_map(str.all, removeWords, character(0))
  str.all<-tm_map(str.all, removePunctuation)
  
  tdm.all<-TermDocumentMatrix(str.all)
  tdm.tidy=tidy(tdm.all)
  kable(tdm.tidy[1:10,])
  # Get the Overall Counts over the Whole Corpus
  tdm.overall=summarise(group_by(tdm.tidy, term), sum(count))
  
  wordcloud(tdm.overall$term, tdm.overall$`sum(count)`,
            scale=c(5,0.5),
            max.words=100,
            min.freq=1,
            random.order=FALSE,
            rot.per=0.3,
            random.color=FALSE,
            colors=brewer.pal(9,"Dark2"))
}