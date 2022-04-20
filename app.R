# Load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
# Loading data ----
gegar <- read_excel(file.choose())
summary(gegar)
gegar <- mutate(gegar, Profit = Sales - Cost)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# app.R #
library(shiny)
library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)
server <- function(input, output) { }
shinyApp(ui, server)

# Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "GegarGegar Music Berhad")  
#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Visit-us", icon = icon("send",lib='glyphicon'), 
             href = "https://www.gegargegarmusic.com")
  )
)

frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)
frow2 <- fluidRow( 
  box(
    title = "Profit per Product"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("profitbyPrd", height = "300px")
  )
  ,box(
    title = "Profit by Region"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("profitbyRegion", height = "300px")
  )
)
frow3 <- fluidRow(
  class="vertical-align",
  column(12, p(strong("Hello"), ", this application allows you to analyze the profit made by GegarGegar Music Berhad", 
           "based on the product and the region. The box on the left is profit made by product and region and the box", 
           "on the right is the profit made by region."), 
 )
)
# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2, frow3)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'GegarGegar Music Berhad', header, sidebar, body, skin='red')

# create the server functions for the dashboard  
server <- function(input, output) { 
  #some data manipulation to derive the values of KPI boxes
  total.profit <- sum(gegar$Profit)
  profit.region <- gegar %>% group_by(Region) %>% summarise(value = sum(Profit)) %>% filter(value==max(value))
  prof.prod <- gegar %>% group_by(Product) %>% summarise(value = sum(Profit)) %>% filter(value==max(value))
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(profit.region$value, format="d", big.mark=',')
      ,paste('Top Region:',profit.region$Region)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC(total.profit, format="d", big.mark=',')
      ,'Total Profit'
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "green")  
  })
  output$value3 <- renderValueBox({
    valueBox(
      formatC(prof.prod$value, format="d", big.mark=',')
      ,paste('Top Product:',prof.prod$Product)
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")   
  })
  #creating the plotOutput content
  output$profitbyPrd <- renderPlot({
    ggplot(data = gegar, 
           aes(x=Product, y=Profit, fill=factor(Region))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Profit") + 
      xlab("Product") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Profit by Product") + labs(fill = "Region")
  })
  output$profitbyRegion <- renderPlot({
    ggplot(data = gegar, 
           aes(x=Region, y=Profit, fill=factor(Region))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Profit") + 
      xlab("Region") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Profit by Region") + labs(fill = "Region")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
