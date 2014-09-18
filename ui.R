library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Visualization of USA NOAA Storm Database."),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel( 
        br(), 
        p("Visualization parameters :"),
        br(),
        selectInput("var", 
                    label = "Variable to display",
                    choices = c("Damages", "Injuries", "Fatalities"),
                    selected = "Damages"),
        
        sliderInput("year",
        "Year:",
        min = 1993,
        max = 2011,
        value = 2011),
        br(),
        
        selectInput("filter", 
        label = "Filter type",
        choices = c("Accumulative", "By year"),
        selected = "Accumulative"),
        br(),
        
        selectInput("graph", 
        label = "Graph type",
        choices = c("Bars", "USA map"),
        selected = "Bars")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      p("This application presents an interface to visualize the analysis of population health and economics damages of USA NOAA Storm Database created at the Reproducible Research course in Coursera (Jul 2014)."),
      p("In this report we have addressed questions about both public health and economic problems caused by severe weather events exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage."),
      p("For speed considerations in this application we only include data from year 1993 to 2011. The complete report is available at :"), a("http://rpubs.com/jagprieto/23612"),
      br(),
      strong("Summary : "),
      textOutput("summaryTitle"),
      br(),
      textOutput("summaryHead"),
      p("_____________________________"),
      textOutput("summaryMean"),
      textOutput("summarySd"),      
      textOutput("summaryMax"),
      textOutput("summaryMin"),
      plotOutput("barPlot", width = "100%", height = "700px")
    ),
    fluid = TRUE
  )
))
