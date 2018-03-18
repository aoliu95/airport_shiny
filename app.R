library(shinydashboard)
library(lubridate)
library(dplyr)
library(tidyverse)
library(maps)
library(geosphere)
library(DT)
library(plotly)

airline <- read.csv(url("https://github.com/aoliu95/airport_shiny/raw/master/airport_loc.csv"))%>%select(locationID,Longitude,Latitude)
rownames(airline) <- airline$locationID
mysample<-read.csv(url("https://github.com/aoliu95/airport_shiny/raw/master/flight.csv"))



ui <- dashboardPage(
  dashboardHeader(title = "Flight Delay Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Table Data", icon = icon("th"), tabName = "tables")),
    # Select variable for Destination
    selectInput(inputId = "Origin", 
                  label = "Select Origin Airport:",
                  choices = c('ATL','LAX','ORD','DFW','JFK','DEN','SFO'), 
                  selected = "LAX"),
      
      # Select variable for Destination
      selectInput(inputId = "Destination", 
                  label = "Select Destination Airport:",
                  choices = c('ATL','LAX','ORD','DFW','JFK','DEN','SFO','LAS','CLT',
                              'SEA','PHX','MIA','MCO','IAH','EWR','MSP','BOS','DTW','PHL','LGA'), 
                  selected = "JFK",multiple = TRUE),
      br(),
      # Select variable for Airline
      selectInput(inputId = "Airline", 
                  label = "Select Airline:",
                  choices = c('Delta'='DL','United'='UA','Southwest'='WN',
                              'American'='AA'),
                  selected = "UA",multiple = FALSE),
      br(),
      # Select which types of movies to plot
      selectInput(inputId = "date",
                label = "Select report date:",
                choices = c('Jan'=1,'Feb'=2,'Mar'=3,
                            'Apr'=4,'May'=5,
                            'Jun'=6,'Jul'=7,
                            'Aug'=8,'Sep'=9,'Oct'=10,'Nov'=11),
                selected = "DL",multiple = FALSE),
    br(),
    h5("  Made with ❤️ from",
       img(src = "https://github.com/aoliu95/airport_shiny/raw/master/black.png", height = "50px"),
       br(),
       "  Powered By",
       img(src = "https://github.com/aoliu95/airport_shiny/raw/master/shinyLogo.png", height = "30px"))
    
    # ,checkboxInput(inputId = "show_data",
    #               label = "Show detailed data",
    #               value = TRUE)
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",fluidRow(
      # Dynamic infoBoxes
      valueBoxOutput("select_airline"),
      valueBoxOutput("flight_num"),
      valueBoxOutput("delayed_num"),
      valueBoxOutput("cancel_num"),
      valueBoxOutput("avg_delay"),
      valueBoxOutput("ind_average")
    ),
    fluidRow(
      box(title = "Flight Route", width = 12, solidHeader = TRUE, 
        status = "primary",plotOutput(outputId = "flight_route"))),
    fluidRow(box(title = "Flight Route", width = 6, solidHeader = TRUE, 
                 status = "primary",plotlyOutput("bar_chart")),
             box(title = "Flight Route", width = 6, solidHeader = TRUE, 
                 status = "primary",plotlyOutput("pie_chart")
  ))),
  tabItem(tabName = "tables",
          DT::dataTableOutput(outputId = "flighttable")
  )
)))

server <- function(input, output) {

  flight <- reactive({
    req(input$Destination) # ensure availablity of value before proceeding
    mysample%>%subset((Origin%in%input$Origin)&(UniqueCarrier%in%input$Airline)&(Dest%in%input$Destination)&(Month==input$date))
  })
  industry <- reactive({
    req(input$Destination) # ensure availablity of value before proceeding
    mysample%>%subset((Origin%in%input$Origin)&(Dest%in%input$Destination)&(Month==input$date))
  })
  table_dis <- reactive({
    req(input$Destination) # ensure availablity of value before proceeding
    flight()%>%select(Origin,Dest,Month,DayofMonth,CRSDepTime,CRSArrTime,UniqueCarrier,FlightNum,AirTime,ArrDelay,Distance,
                      CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay)
  })
  dealy_weekday <- reactive({
    req(input$Destination) # ensure availablity of value before proceeding
    flight()%>%select(Origin,Dest,Month,DayOfWeek,ArrDelay)%>%
      group_by(DayOfWeek)%>%summarise(ArrDelay=mean(ArrDelay,na.rm = TRUE))
  })
  dealy_channel <- reactive({
    req(input$Destination) # ensure availablity of value before proceeding
    flight()%>%select(CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay)%>%
      summarise(CarrierDelay=round(mean(CarrierDelay,na.rm = TRUE),2),
                WeatherDelay=round(mean(WeatherDelay,na.rm = TRUE),2),
                NASDelay=round(mean(NASDelay,na.rm = TRUE),2),
                SecurityDelay=round(mean(SecurityDelay,na.rm = TRUE),2),
                LateAircraftDelay=round(mean(LateAircraftDelay,na.rm = TRUE),2))%>%t()
  })
  output$Origin_text<-renderText({input$Origin})
  output$Destination_text<-renderText({input$Destination})
  output$Airline_text<-renderText({input$Airline})
  output$date_text<-renderText({input$date})
  output$flight_route<-renderPlot({
    req(input$Destination)
    par(mar=c(0,0,0,0))
    map('state',col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,mar=rep(0,4))
    Start<-input$Origin
    Des<-input$Destination
    
    for (i in (1:length(Des))){
      data=rbind(airline[Start,][,2:3],airline[Des[i],][,2:3]) %>% as.data.frame()
      inter <- gcIntermediate(c(as.numeric(data[1,1]),as.numeric(data[1,2])), 
                              c(as.numeric(data[2,1]),as.numeric(data[2,2])),n=50, addStartEnd=TRUE, breakAtDateLine=F)             
      lines(inter, col="slateblue", lwd=2)
      points(x=data$Longitude, y=data$Latitude,  cex=3, pch=20,col=c('#333274','#ff0043'))
      text(rownames(data), x=as.numeric(data$Longitude), y=as.numeric(data$Latitude),  col="slateblue", cex=1, pos=4)
    }
  })
  output$select_airline <- renderValueBox({
    valueBox(
      paste0(input$Airline), "Airlines", icon = icon("suitcase"),
      color = "purple"
    )
  })
  output$flight_num <- renderValueBox({
    valueBox(
      paste0(nrow(flight())), "Total Flights", icon = icon("plane"),
      color = "blue"
    )
  })
  output$delayed_num <- renderValueBox({
    valueBox(
      paste0(as.integer(nrow(flight()%>%subset(ArrDelay>0))/nrow(flight())*100),"%"), " Flights Delayed", icon = icon("bell"),
      color = "light-blue"
    )
  })
  output$cancel_num <- renderValueBox({
    valueBox(
      paste0(nrow(flight()%>%subset(CancellationCode%in%c('A','B','C')))), " Flights Cancelled", icon = icon("ban"),
      color = "red"
    )
  })
  output$avg_delay<-renderValueBox({
    valueBox(
      paste0(round(mean(flight()$ArrDelay,na.rm=TRUE),2)), " Average Delay(mins)", icon = icon("hourglass"),
      color = "maroon"
    )
  })
  output$ind_average<-renderValueBox({
    valueBox(
      paste0(round((mean(flight()$ArrDelay,na.rm=TRUE)/mean(industry()$ArrDelay,na.rm=TRUE)-1)*100,2),"%"),
      "Compared with Monthly Industry Avg", icon = icon("compass"),
      color = "maroon"
    )
  })
  output$bar_chart <- renderPlotly({
    plot_ly(dealy_weekday(), x = ~DayOfWeek, y = ~ArrDelay, type = 'bar')%>%layout(title = "Avg Delay Time by Weekday",
                                                                                   xaxis = list(title = "Day of Week(Mon-Sun)"),
                                                                                   yaxis = list(title = "Avg Delay(min)"))
  })
  output$pie_chart <- renderPlotly({
    plot_ly(as.data.frame(dealy_channel()), labels = ~rownames(as.data.frame(dealy_channel())), 
            values = ~as.data.frame(dealy_channel())$V1, type = 'pie') %>%
      layout(title = 'Composition of Flight Delay',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  output$flighttable<-DT::renderDataTable(
    {
      DT::datatable(data = table_dis(), 
                    options = list(pageLength = 10,scrollX = TRUE), 
                    rownames = TRUE)
    }
  )
}

shinyApp(ui, server)