#This is WS Program Vault examples. It runs all programs in the program vault with hypothetical data. Hopefully this will help you run and/or figure out any syntax from 
#all of the programs in the program vault. If you added a generic program to the vault, please also prepare an example here with hypothetical data.
#Be sure to install all packages and run and load all packages.
library(ggplot2)
library(dplyr)
library(plotly)
library(corrplot)
library(RColorBrewer)
library(gapminder)
library(shiny)
library(shinydashboard)
library(DT)
library(rsconnect)

#Read In Data. Be sure the data is clean, in .csv format, and in the working directory. This demonstration data looks at 75 hypothetical influencers and contains their
#names, gender, impressions, reach, engagement, and followers.
DemoData<-as.data.frame(read.csv("RPracticeData.csv"))

#Remove any row or column if desired, e.g. removing outliers. -c(x,x) Removes rows x and x. This is useful if one or more influencers is clearly an outlier.
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



#Simple Bar Chart
ggplot(data=DemoData, aes(x = reorder(Name, -Reach), y=Reach)) +
  geom_bar(stat="identity", width=0.6,fill="red")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+xlab("Name")


#Correlegram
COR = cor.test(DemoData$Reach,DemoData$Impressions)[c("estimate","p.value")]
COR_text = paste(c("R=","p="),signif(as.numeric(COR,3),3),collapse=" ")
#First remove non-numeric columns
CorData<-as.data.frame(subset(DemoData, select = -c(Name,Gender)))

corrplot(DemoData, is.corr = FALSE, method = "circle")
M<-cor(CorData)
corrplot(M, method="number")
pairs(~Impressions + Reach + Engagement + Followers, data = CorData)



#Animation Examples. This uses a different data set, but could be applied to viewing influencers over time, for example.
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

#Text Analytics. This example looks at words from Company 1 and Company 2 glassdoor and google news pages. It then allows you to see the most commonly used words on
#those pages. Be sure to have packages below installed and to run lines 80-106.
library(wordcloud)
library(wordcloud2)
library(tm)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(plotly)
library(SnowballC)
Company1_Glassdoor<-as.data.frame(read.csv("Company2_Glassdoor.csv"))
Company2_Glassdoor<-as.data.frame(read.csv("Company1_Glassdoor.csv"))
Company1_Google<-as.data.frame(read.csv("Company2_Google.csv"))
Company2_Google<-as.data.frame(read.csv("Company1_Google.csv"))

words<-as.character(Company2_Glassdoor$Cons)
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

#graph the data
ggplot(df3,aes(Var1, Freq))+geom_bar(stat='identity',fill="blue") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + 
  geom_text(
    aes(label = Freq),
    colour = "Black", size = 2,
    vjust = -0.5, position = position_dodge(.9)
  )

#Shiny Dashboards
ui<-dashboardPage(skin="blue",
                  dashboardHeader(title = "Tyler's Dashboard"),
                  dashboardSidebar(
                    sidebarMenu(
                      menuItem("Influencers", tabName = "Influencers", icon = icon("money")),
                      menuItem("Cars", tabName = "Cars", icon = icon("car")),
                      menuItem("Iris", tabName = "Iris", icon = icon("tree"))
                    )
                  ),
                  dashboardBody(
                    tabItems(
                      tabItem("Influencers",
                              box(plotlyOutput("correlation_plot"), width=10),
                              box(
                                selectInput("x_axis", "X_Axis:", 
                                            c("Impressions", "Reach","Engagement","Followers")),
                                selectInput("y_axis", "Y_Axis", 
                                            c("Impressions", "Reach","Engagement","Followers")),
                                selectInput("color", "Color", 
                                            c("Impressions", "Reach","Engagement","Followers")),
                                selectInput("size", "Size", 
                                            c("Impressions", "Reach","Engagement","Followers")),
                                width = 2
                              )
                      ),
                      tabItem("Cars",
                              fluidPage(
                                h1("Cars"),
                                dataTableOutput("carstable")
                              )),
                      tabItem("Iris",
                              fluidPage(
                                h1("Iris"))
                      )
                    )
                  )
)
server<- function(input, output){
  output$correlation_plot <- renderPlotly({
    plot_ly(Influencers, x = ~Influencers[[input$x_axis]], y = ~Influencers[[input$y_axis]],
            text = ~Name, size = ~Influencers[[input$size]],
            sizes = c(10, 50),
            marker =
              list(opacity = 0.7,
                   sizemode = "diameter"), color = ~Influencers[[input$color]], colors = 'Spectral') %>%
      layout(title = 'Influencers', xaxis = list(title = 'X-Axis '), font=t, plot_bgcolor = "#e5ecf6",
             yaxis = list(title = 'Y-Axis'), legend = list(title=list(text='Legend')))
  })
  output$carstable<-renderDataTable(mtcars)
}

shinyApp(ui, server)


