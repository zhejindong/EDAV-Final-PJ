library(shiny)
library (ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(plotly)
library(shinythemes)
library(shinyWidgets)

#First read the csv file data
P = read.csv("/Users/Shimeng/Documents/Master_Columbia_2018/GR 5702/Final Project/Final_project/App-1/Pollution.csv", header = TRUE)
#P = read.csv("/Users/Shimeng/Documents/Master_Columbia_2018/GR 5702/Final Project/Final_project/App-1/SO2.csv", header = TRUE)

P = data.frame (P)


# Define UI for app that draws a histogram ----
ui <- fluidPage(theme = shinytheme("cerulean"),
  

  
  # App title ----
  titlePanel("Discovery of Pollution Across the United States"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
       sliderInput('range',
                  "Year:",
                 min = 2008, max = 2018,
                value = c(2008,2018)),
      
      
      #Input: check box that enable to select multiple state
      #selectInput
      
      selectInput(inputId = "pollutant",
                  label = "Choose a pollutant",
                  choices = c('TSO4',
                              "TNO3",
                              "TNH4",
                              "CA",
                              "MG",
                              "NA.",
                              "K",
                              "CL",
                              "NHNO3",
                              "WSO2",
                              "TOTAL_SO2",
                              "TOTAL_NO3"
                  )),
      prettyCheckboxGroup(inputId = "state",
                         label = "Choose a state",
                         selected = 'AL',
                         choices = c('AL',
                                     'AR',
                                     'CA',
                                     'CO',
                                     'CT',
                                     'FL',
                                     'GA',
                                     'IL',
                                     'IN',
                                     'KS',
                                     'KY',
                                     'MD',
                                     'ME',
                                     'MI',
                                     'MS',
                                     'NC',
                                     'NE',
                                     'NH',
                                     'NJ',
                                     'NY',
                                     'OH',
                                     'OK',
                                     'ON',
                                     'PA',
                                     'TN',
                                     'TX',
                                     'VA',
                                     'VT',
                                     'WI',
                                     'WV',
                                     'WY')
                         )
      
     
 ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      h2("Time Series"),
      h4("Preliminary Analysis Tool: The goal of this application is to support users to perform primary discovery
        over different states, pollutant, and time period. "),
      tabsetPanel(tabPanel("Time Series by Year", 
      helpText("User Guide: Please feel free to choose multiple states and corresponding pollutant. 
                           In addition, feel free to drag the timeline bar to eliminate the year(s)
                          that you are interested. Have Fun!"),plotOutput(outputId = "TimeSeries_year",
                          hover = hoverOpts(id = "plot_hover")),verbatimTextOutput("hover_info_1")), 
        tabPanel("Time Series by Month", 
  helpText("User Guide: Please feel free to choose multiple states and corresponding pollutant. 
                      In addition, feel free to drag the timeline bar to eliminate the year(s)
                      that you are interested, or hover over the graph to get the value of any point.
           Have Fun!!"),plotOutput(outputId = "TimeSeries_month",
                      hover = hoverOpts(id = "plot_hover")),verbatimTextOutput("hover_info")),
        tabPanel("Data by Year", helpText("User Guide: Interested in the data, here we go!"), tableOutput("tabledata"))
        
      ))))

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$tabledata<- renderTable({
    
    Input_pollutant = input$pollutant
    P_update <- P %>% 
      select (STATE, YEAR, MONTH, Input_pollutant)%>%
      filter (STATE ==input$state) 
    pollutant=  P_update %>% select (Input_pollutant)
    P_Final <- aggregate(pollutant,by=list(YEAR = P_update$YEAR,STATE = P_update$STATE), FUN = sum)
    P_Final_1 <- spread(P_Final, STATE, Input_pollutant)
    P_Final_1

  })
  
  
  output$TimeSeries_year <- renderPlot({
    #pre-processing data
    Input = input$pollutant
    start = input$range[1]
    end = input$range[2]
    P_update <- P %>% 
      select (STATE, YEAR, MONTH,Input)%>%
      filter (STATE == input$state) %>%
      filter (YEAR>start)%>%
      filter(YEAR < end)
    pollutant =  P_update %>% select (Input)
    P_Final <- aggregate(pollutant,by=list(YEAR = P_update$YEAR,STATE = P_update$STATE), FUN = sum)
    P_Final$YEAR <- as.Date(as.character(P_Final$YEAR),format = "%Y")
  
    #plot the time series
    ggplot(P_Final, aes(x=YEAR,color = STATE,group=STATE)) + geom_line(aes_string(y=Input)) + 
      ggtitle("Pollutant by Year by State") +
      labs (x = "10-years Time Span", y = "Pollutant Total Amount") +
      # scale_x_date (date_breaks = "1 year")+
      theme_grey(16) +
      theme(legend.title = element_blank(),legend.position = "bottom")+
      scale_x_date(labels = date_format("%Y"))+
      geom_point(aes_string(y=Input))
    
  })

  #Time series by Month 
  output$TimeSeries_month <- renderPlot({
    #pre-processing data
  
    Input = input$pollutant
    start = input$range[1]
    end = input$range[2]
   # P_month$TIME <- as.Date(as.factor(P_month$TIME),format = "%Y-%m-%d")
    
    
    P_month <- P %>%
      select (STATE,YEAR, TIME, Input)%>%
      filter (STATE == input$state) %>%
    filter (YEAR>start)%>%
      filter(YEAR<end)
    
    P_month$TIME <- as.Date(P_month$TIME, format = "%d/%m/%Y")


    
    ggplot(P_month, aes(x=TIME,color = STATE,group=STATE)) + geom_line(aes_string(y=Input)) + 
      ggtitle("Pollutant by Year by State") +
      labs (x = "10-years Time Span", y = "Pollutant Total Amount") +
      # scale_x_date (date_breaks = "1 year")+
      theme_grey(16) +
      theme(legend.title = element_blank(),legend.position = "bottom",
            axis.text.x = element_text(angle=60, hjust=1))+
      scale_x_date(date_breaks = "years",labels = date_format("%b-%Y"))+geom_point(aes_string(y=Input))


    })
  
  output$Time<- renderPrint({
    
  #For this modual, you are capable of selecting multiple state and discover why each 
  
   })
 
  
  output$hover_info <- renderPrint({
    Input = input$pollutant
    start = input$range[1]
    end = input$range[2]
    
    P_month <- P %>%
      select (STATE,YEAR, TIME, Input)%>%
      filter (STATE == input$state) %>%
      filter (YEAR>start)%>%
      filter(YEAR<end)
    
    P_month$TIME <- as.Date(P_month$TIME, format = "%d/%m/%Y")
    cat("Pollutant Value:\n")
    #str(input$plot_hover)
   # y <- nearPoints(P_month, input$plot_hover)
    #req(nrow(y) != 0)
    str(input$plot_hover$y)
    
  })
  
  output$hover_info_1 <- renderPrint({
    Input = input$pollutant
    start = input$range[1]
    end = input$range[2]
    
    P_month <- P %>%
      select (STATE,YEAR, TIME, Input)%>%
      filter (STATE == input$state) %>%
      filter (YEAR>start)%>%
      filter(YEAR<end)
    
    P_month$TIME <- as.Date(P_month$TIME, format = "%d/%m/%Y")
    cat("Pollutant Value:\n")
    #str(input$plot_hover)
    # y <- nearPoints(P_month, input$plot_hover)
    #req(nrow(y) != 0)
    str(input$plot_hover$y)
    
  })
  
  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)