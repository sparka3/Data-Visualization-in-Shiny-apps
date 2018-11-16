install.packages("shiny")
install.packages("shinydashboard")
install.packages("plotly")
library(shiny)
library(shinydashboard)
library(plotly)

header <- dashboardHeader(title = "Uber Vs LYft")

sidebar <- dashboardSidebar(sidebarMenu(
  selectInput(
    inputId = "month",
    label = "Select Month",
    choices = c(
      "June",
      "July",
      "August",
      "September",
      "October",
      "November",
      "December"
    )
  ),
  menuItem (
    "Dashboard",
    tabName = "dashboard",
    icon = icon("dashboard")
  ),
  menuItem (
    "Statistics",
    tabName = "statistics",
    icon = icon("stats", lib = 'glyphicon')
  )
  
))

frow1 <- fluidRow(valueBoxOutput("UberBox"),
                  valueBoxOutput("LyftBox"))

frow2 <- fluidRow(
  box(
    title = "Distribution",
    status = "primary",
    solidHeader =  TRUE,
    collapsible = TRUE,
    plotlyOutput("pieChart")
  ),
  box(
    title = "Frequency",
    status = "primary",
    solidHeader =  TRUE,
    collapsible = TRUE,
    plotlyOutput("barChart")
  )
)
frow3 <- fluidPage(
  box(
    title = "Rides Data",
    status = "primary",
    solidHeader =  TRUE,
    collapsible = TRUE,
    dataTableOutput("data_table"),
    width = 500
  )
)
frow4 <- fluidRow(
  box(
    title = "Revenue Data",
    status = "primary",
    solidHeader =  TRUE,
    collapsible = TRUE,
    plotlyOutput("revenueByMonth"),
    width = 500
  )
)

body <- dashboardBody(tabItems(
  tabItem(tabName = "dashboard",
          frow1,
          frow2,
          frow3),
  tabItem(tabName = "statistics",
          frow4)
  
))


ui <- dashboardPage(header, sidebar, body)
server <- function(input, output) {
  selectmon <- reactive ({
    switch(
      input$month,
      "June" = "Jun" ,
      "July" = "Jul",
      "August" = "Aug" ,
      "September" = "Sept",
      "October" = "Oct",
      "November" = "Nov",
      "December" = "Dec"
    )
  })
  readdf <- reactive({
    read.csv(paste("www/data/AVN TNC PickUp", selectmon(), "2016.csv"))
  })
  
  output$data_table <- renderDataTable(readdf())
  uberCount <-
    reactive({
      nrow(readdf()[which(readdf()$Service_Provider == "Uber"), ])
    })
  lyftCount <-
    reactive({
      nrow(readdf()[which(readdf()$Service_Provider == "Lyft"), ])
    })
  
  
  output$UberBox <- renderValueBox ({
    valueBox(uberCount(), " Uber Count",
             color = "purple")
  })
  
  output$LyftBox <- renderValueBox ({
    valueBox(lyftCount(), "Lyft Count",
             color = "purple")
  })
  
  
  
  
  output$pieChart <- renderPlotly({
    pie_data <- data.frame(
      Categorie = c("Uber" , "Lyft"),
      Count = c(uberCount(), lyftCount())
    )
    
    plot_ly(
      pie_data,
      labels =  ~ Categorie,
      values =  ~ Count,
      type = 'pie'
    )
    
  })
  
  output$barChart <- renderPlotly({
    bar_data <- data.frame(
      Categorie = c("Uber" , "Lyft"),
      Count = c(uberCount(), lyftCount())
    )
    
    plot_ly(bar_data,
            x =  ~ Categorie,
            y =  ~ Count,
            type = 'bar')
  })
  
  output$revenueByMonth <- renderPlotly({
    uberRevenue = c()
    LyftRevenue = c()
    month <- c("Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec")
    number = 1
    
    for (i in month) {
      df <- read.csv(paste("www/data/AVN TNC PickUp", i, "2016.csv"))
      
      ubercoun <- 25 * nrow(df[which(df$Service_Provider == "Uber"), ])
      lyfcoun <- 25 * nrow(df[which(df$Service_Provider == "Lyft"), ])
      
      uberRevenue[number] = ubercoun * 25
      LyftRevenue[number] = lyfcoun * 25
      
      number = number + 1
    }
    data_revenue <-
      data.frame(month, Uber = uberRevenue, Lyft = LyftRevenue)
    
    data_revenue$month <- factor(data_revenue$month, levels = data_revenue[['month']])
    
    plot_ly(data_revenue, x = ~ month) %>%
      add_trace(
        y = ~ Uber,
        type = 'scatter',
        mode = 'lines',
        line = list(color = 'rgba(67,67,67,1)', width = 2)
      )  %>%
      add_trace(
        y = ~ Lyft,
        type = 'scatter',
        mode = 'lines',
        line = list(color = 'rgba(49,130,189, 1)', width = 4)
      ) %>%
      layout(title = "Uber VS Lyft Revenue")
  })
}

shinyApp(ui, server)
