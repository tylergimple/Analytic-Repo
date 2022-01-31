library(ggplot2)
library(dplyr)
library(plotly)
library(corrplot)
library(RColorBrewer)
library(gapminder)

#Read In Data. Be sure the data is clean, in .csv format, and in the working directory.
DemoData<-as.data.frame(read.csv("RPracticeData.csv"))

#Remove any row or column if desired, e.g. removing outliers. -c(x,x) Removes rows x and x
DemoData2<-as.data.frame(DemoData[-c(1,2),])

#Simple Bubble chart of 4 dimensional data. Update x, y, size, color, label to desired values.
ggplot(DemoData, aes(x=Impressions, y=Reach, size=Engagement, color=Followers)) + geom_point(alpha=0.5) + 
  geom_text(label=DemoData$Name, nudge_x = 0.5, nudge_y = 0.5, check_overlap = T)+ theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

#Plotly Bubble Chart (more interactive and sightly)
bubbleplot2 <- plot_ly(DemoData, x = ~Impressions, y = ~Reach,
                       text = ~Name, size = ~Engagement,
                       sizes = c(10, 50),
                       marker =
                         list(opacity = 0.7,
                              sizemode = "diameter"), color = ~Followers, colors = 'Spectral')
bubbleplot2


#Animation Examples
df <- gapminder 
fig <- df %>%
  plot_ly(
    x = ~gdpPercap, 
    y = ~lifeExp, 
    size = ~pop, 
    color = ~continent, 
    frame = ~year, 
    text = ~country, 
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

COR = cor.test(DemoData$Height,DemoData$Weight)[c("estimate","p.value")]
COR_text = paste(c("R=","p="),signif(as.numeric(COR,3),3),collapse=" ")


#Simple Bar Chart
ggplot(data=Data, aes(x=xaxis, y=yaxis)) +
  geom_bar(stat="identity", width=0.6,fill="purple")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




#Correlegram

#First remove non-numeric columns
CorData<-as.data.frame(subset(DemoData, select = -c(Name,Gender)))

corrplot(DemoData, is.corr = FALSE, method = "circle")
M<-cor(CorData)
corrplot(M, method="number")
pairs(~Height + Weight + Income + Age, data = CorData)


#Text Analytics
library(wordcloud)
library(wordcloud2)
library(tm)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(plotly)
library(SnowballC)
Boeing_Glassdoor<-as.data.frame(read.csv("Boeing_Glassdoor.csv"))
Airbus_Glassdoor<-as.data.frame(read.csv("Airbus_Glassdoor.csv"))
Boeing_Google<-as.data.frame(read.csv("Boeing_Google.csv"))
Airbus_Google<-as.data.frame(read.csv("Airbus_Google.csv"))

words<-as.character(Airbus_Glassdoor$Cons)
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
df2<-df[df$Var1 != "and" & df$Var1 != "the" & df$Var1 != "for" & df$Var1 != "you" & df$Var1 != "are" & df$Var1 != "that" & df$Var1 != "if" & df$Var1 != "your" & df$Var1 != "get" & df$Var1 != "with" & df$Var1 != "they" & df$Var1 != "good" & df$Var1 != "great" & df$Var1 != "job" & df$Var1 != "have" & df$Var1 != "not" & df$Var1 != "but" & df$Var1 != "year" & df$Var1 != "this" & df$Var1 != "lot" & df$Var1 != "will" & df$Var1 != "have" & df$Var1 != "some" & df$Var1 != "from" & df$Var1 != "all" & df$Var1 != "too" & df$Var1 != "was" & df$Var1 != "can" & df$Var1 != "their" & df$Var1 != "their" & df$Var1 != "work" & df$Var1 != "big" & df$Var1 != "there" & df$Var1 != "much" & df$Var1 != "need" & df$Var1 != "compani" & df$Var1 != "veri" & df$Var1 != "has" & df$Var1 != "out" & df$Var1 != "dont" & df$Var1 != "like" & df$Var1 != "make" & df$Var1 != "who" & df$Var1 != "mani" & df$Var1 != "said" & df$Var1 != "were", ]
df3<-head(df2,25)

ggplot(df3,aes(Var1, Freq))+geom_bar(stat='identity',fill="blue") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + 
  geom_text(
    aes(label = Freq),
    colour = "Black", size = 2,
    vjust = -0.5, position = position_dodge(.9)
  )
