library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(rsconnect)

DemoData<-as.data.frame(read.csv("RPracticeData.csv"))

#Shiny Dashboards
ui<-dashboardPage(skin="blue",
                  dashboardHeader(title = "WS Dashboard"),
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
