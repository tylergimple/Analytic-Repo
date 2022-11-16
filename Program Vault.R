#Hello Welcome to Havas' Program Vault.
#Feel free to use any of the templates here in your projects.
#If you would like to add a template or program of your own for another Havas friend to use please contact tyler.gimple@havasmedia.com and I will add it to the vault :)


#Make sure these packages are installed and loaded.
library(ggplot2)
library(dplyr)
library(plotly)
library(corrplot)
library(RColorBrewer)
library(gapminder)

#Read In Data. Be sure the data is clean, in .csv format, and in the working directory.
Data<-as.data.frame(read.csv("Data.csv"))

#Remove any row or column if desired, e.g. removing outliers. This removes the 1st and 2nd row.
Data<-as.data.frame(Data[-c(1,2),])

#Simple Bubble chart of 4 dimensional data. Update x, y, size, color, label to desired variables
ggplot(Data, aes(x='xaxis', y='yaxis', size='sizemetric', color='colormetric')) + geom_point(alpha=0.5) + 
  geom_text(label='label', nudge_x = 0.25, nudge_y = 0.25, check_overlap = T)+theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

#Plotly Bubble Chart (more interactive and sightly). Update x, y, size, color to desired variables. Be sure to keep the ~ to preserve plotly's syntax.
bubbleplot2 <- plot_ly(Data, x = ~xaxis, y = ~yaxis,
                       text = ~text, size = ~sizemetric,
                       sizes = c(10, 50),
                       marker =
                         list(opacity = 0.7,
                              sizemode = "diameter"), color = ~colormetric, colors = 'Spectral')%>%
  layout(title = 'Title', plot_bgcolor = "#e5ecf6")
bubbleplot2



#Simple Bar Chart. Update x, y to desired variables.
ggplot(data=DemoData, aes(x=Name, y=Income)) +
  geom_bar(stat="identity", width=0.6,fill="purple")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Correlegram, Remember to remove non-numeric columns. This is useful to understand the level of correlation between two or more variables.
CorData<-as.data.frame(subset(Data, select = -c(non-numeric1,non-numeric2)))
corrplot(Data, is.corr = FALSE, method = "circle")
M<-cor(CorData)
corrplot(M, method="number")
pairs(~Metric1 + Metric2 + Metric3 + Metric4 + Metric5, data = Data)

#Graphs with Animation. Useful to graph the movement of data over time. E.G. how has a group of influencers changed over time in terms of followers and engagement.
Data <- Data #Load your data here
fig <- Data %>%
  plot_ly(
    x = ~xaxis, 
    y = ~yaxis, 
    size = ~sizemtric, 
    color = ~colormetric, 
    frame = ~animation, 
    text = ~textmetric, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  )
fig <- fig %>% layout(
  xaxis = list(
    type = "log"
  )
)
fig

#Text Analytics. This is useful if you have a large amount of text and want to see what the most common words being used are. E.G. you want to see the most common
#words from every Glassdoor post. Make sure these packages are loaded.
library(wordcloud)
library(wordcloud2)
library(tm)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(plotly)
library(SnowballC)

#Read in your data
Text1<-as.data.frame(read.csv("File1.csv"))
Text2<-as.data.frame(read.csv("File2.csv"))
Text3<-as.data.frame(read.csv("File3.csv"))
Text4<-as.data.frame(read.csv("File4.csv"))


words<-as.character(File$Column) #Specifiy the File and Column that you want to analyze
word.corpus<-Corpus(VectorSource(words))
word.corpus<-word.corpus%>%
  tm_map(removePunctuation)%>% ##eliminate punctuation
  tm_map(removeNumbers)%>% #no numbers
  tm_map(stripWhitespace)#white spaces
word.corpus<-word.corpus%>%
  tm_map(tolower)%>% ##make all words lowercase
  tm_map(removeWords, stopwords("english"))
word.corpus <- tm_map(word.corpus, removeWords, c("the", "and","for","this","that","with","will","also","i'm")) 
word.corpus<-tm_map(word.corpus, stemDocument)
word.counts<-as.matrix(TermDocumentMatrix(word.corpus))
word.freq<-sort(rowSums(word.counts), decreasing=TRUE)
head(word.freq)##what are the top words?
word.freq
plot<-as.table(word.freq)
df<-as.data.frame(plot)
#Remove Irrelevant Words
df2<-df[df$Var1 != "and" & df$Var1 != "the" & df$Var1 != "for" & df$Var1 != "you" & df$Var1 != "are" & df$Var1 != "that" & df$Var1 != "if" & df$Var1 != "your" & df$Var1 != "get" & df$Var1 != "with" & df$Var1 != "they" & df$Var1 != "good" & df$Var1 != "great" & df$Var1 != "job" & df$Var1 != "have" & df$Var1 != "not" & df$Var1 != "but" & df$Var1 != "year" & df$Var1 != "this" & df$Var1 != "lot" & df$Var1 != "will" & df$Var1 != "have" & df$Var1 != "some" & df$Var1 != "from" & df$Var1 != "all" & df$Var1 != "too" & df$Var1 != "was" & df$Var1 != "can" & df$Var1 != "their" & df$Var1 != "their" & df$Var1 != "work" & df$Var1 != "big" & df$Var1 != "there" & df$Var1 != "much" & df$Var1 != "need" & df$Var1 != "compani" & df$Var1 != "veri" & df$Var1 != "has" & df$Var1 != "out" & df$Var1 != "dont" & df$Var1 != "like" & df$Var1 != "make" & df$Var1 != "who" & df$Var1 != "mani" & df$Var1 != "said" & df$Var1 != "were", ]
df3<-head(df2,25)

#Graph your data
ggplot(df3,aes(Var1, Freq))+geom_bar(stat='identity',fill="blue") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + 
  geom_text(
    aes(label = Freq),
    colour = "Black", size = 2,
    vjust = -0.5, position = position_dodge(.9)
  )
