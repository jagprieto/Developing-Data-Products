library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Visualization of USA NOAA Storm Database."),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel( 
      br(), 
      br(), 
      strong("Report options:"),
      br(), 
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
        
        selectInput("filter", 
        label = "Filter type",
        choices = c("Accumulative", "By year"),
        selected = "Accumulative"),
        
        selectInput("graph", 
        label = "Graph type",
        choices = c("Bars", "USA map"),
        selected = "Bars"), 
        
        br(), 
        strong("Report link :"),
        p("A complete report with the exploratory data analysis and the cleaning data process is available at : "), 
        a("http://rpubs.com/jagprieto/23612")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      p("This application presents an interface to visualize the analysis of population health and economics damages of USA NOAA Storm Database  created at the Reproducible Research course in Coursera (Jul 2014). This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage."),
      p("At the left panel the user has four options to filter the visualization and statistic summary. It is important to note that, because of speed considerations, in this application we only include data from year 1993 to 2011."),
      p("Note : Please, at the initial launch, wait until the databases are loaded on the report generator (about 4 seconds). "),      
      br(),
      strong("Summary : "),
      textOutput("summaryTitle"),
      br(),
      strong(textOutput("summaryHead")),
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
