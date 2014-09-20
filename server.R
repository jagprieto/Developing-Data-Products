library(shiny)
library(ggplot2)
library(maps)
source('plotBars.R')

## Read USA states capitols latitude and longitude.
states.relocations <- c('AK','HI','AS','PR','VI')
states.capitols.locations <- read.csv('state_locations.csv',  header=TRUE)
states.capitols.locations$State <- as.factor(states.capitols.locations$state)
states.capitols.locations[which(states.capitols.locations$state %in% states.relocations),]$longitude <- -120
latitude = 20
for (index in 1:length(states.relocations)) {
  states.capitols.locations$latitude[states.capitols.locations$state == states.relocations[index]] <- latitude
  latitude <- latitude + 2
}

## Read the USA states poligons maps.
states.poligon.data <- map_data("state")

# Read clean damages data
noaa.data.damages.1993.2012 <- read.csv('noaa.data.damages.1993.2012.csv',  header=TRUE)
noaa.data.damages.1993.2012$DATE <- as.Date(noaa.data.damages.1993.2012$DATE)
noaa.data.injuries.1993.2012 <- read.csv('noaa.data.injuries.1993.2012.csv',  header=TRUE)
noaa.data.injuries.1993.2012$DATE <- as.Date(noaa.data.injuries.1993.2012$DATE)
noaa.data.fatalities.1993.2012 <- read.csv('noaa.data.fatalities.1993.2012.csv',  header=TRUE)
noaa.data.fatalities.1993.2012$DATE <- as.Date(noaa.data.fatalities.1993.2012$DATE)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$summaryTitle <- renderText({ 
    if (input$filter == 'By year'){
      paste("NOOA ", input$var, " at year", input$year)
    }else{
      paste("NOOA accumulative ", input$var, " from 1993 to", input$year)
    }    
  })
  
  output$barPlot <- renderPlot({
    filter <- input$filter
    year <- input$year
    if (input$graph == 'Bars') {
      if (input$var == "Damages") { 
        if (input$filter == 'By year'){
          noaa.data.damages.filter <- noaa.data.damages.1993.2012[format(noaa.data.damages.1993.2012$DATE, "%Y") == year,]
        }else{
          filter_date <- as.Date(paste(strtoi(year),'-12-31', sep = ""), "%Y-%m-%d")    
          noaa.data.damages.filter <- noaa.data.damages.1993.2012[noaa.data.damages.1993.2012$DATE <= filter_date,]      
        }
        if (nrow(noaa.data.damages.filter) > 0) {
          noaa.data.damages.by.eventype <- aggregate(noaa.data.damages.filter$DAMAGES,by=list(noaa.data.damages.filter$EVTYPE), sum) 
          names(noaa.data.damages.by.eventype)[1]<-"Event"
          names(noaa.data.damages.by.eventype)[2]<-"Damages"
          
          nooa.mean <- round(mean(noaa.data.damages.by.eventype$Damages), 3)
          nooa.sd <- round(sd(noaa.data.damages.by.eventype$Damages), 3)
          nooa.max <- noaa.data.damages.by.eventype[which.max(noaa.data.damages.by.eventype$Damages), ]
          nooa.max.value <- round(nooa.max$Damages, 3)
          nooa.max.evtype <- as.character(nooa.max$Event)
          nooa.min <- noaa.data.damages.by.eventype[which.min(noaa.data.damages.by.eventype$Damages), ]
          nooa.min.value <- round(nooa.min$Damages, 3)
          nooa.min.evtype <- as.character(nooa.min$Event)
          output$summaryHead <- renderText({"Basic statistics by event type:"})
          output$summaryMean <- renderText({paste("Mean : ", nooa.mean ," $")})
          output$summarySd <- renderText({paste("Standar deviation : ", nooa.sd ," $")})
          output$summaryMax <- renderText({paste("Max : ", nooa.max.value, " $, Event : ", nooa.max.evtype)})
          output$summaryMin <- renderText({paste("Min : ", nooa.min.value, " $, Event : ", nooa.min.evtype)})
#           print (nooa.mean)
          g.plot.by.event.type <- ggplot(data=noaa.data.damages.by.eventype , aes(x=Event, y=Damages, colour=Event, fill=Event))
          g.plot.by.event.type <- g.plot.by.event.type + geom_bar(stat="identity") + labs(fill="Event type") + guides(colour=FALSE)
          g.plot.by.event.type <- g.plot.by.event.type + xlab("Event type") + ylab("Damages")  + ggtitle("Damages by event type  (in USA million dollars).") 
          g.plot.by.event.type
        }
      }else if (input$var == "Injuries") { 
        if (input$filter == 'By year'){
          noaa.data.injuries.filter <- noaa.data.injuries.1993.2012[format(noaa.data.injuries.1993.2012$DATE, "%Y") == year,]
        }else{
          filter_date <- as.Date(paste(strtoi(year),'-12-31', sep = ""), "%Y-%m-%d")    
          noaa.data.injuries.filter <- noaa.data.injuries.1993.2012[noaa.data.injuries.1993.2012$DATE <= filter_date,]      
        }
        if (nrow(noaa.data.injuries.filter) > 0) {
          noaa.data.injuries.by.eventype <- aggregate(noaa.data.injuries.filter$INJURIES,by=list(noaa.data.injuries.filter$EVTYPE), sum) 
          names(noaa.data.injuries.by.eventype)[1]<-"Event"
          names(noaa.data.injuries.by.eventype)[2]<-"Injuries"
        
          
          nooa.mean <- round(mean(noaa.data.injuries.by.eventype$Injuries), 3)
          nooa.sd <- round(sd(noaa.data.injuries.by.eventype$Injuries), 3)
          nooa.max <- noaa.data.injuries.by.eventype[which.max(noaa.data.injuries.by.eventype$Injuries), ]
          nooa.max.value <- round(nooa.max$Injuries, 3)
          nooa.max.evtype <- as.character(nooa.max$Event)
          nooa.min <- noaa.data.injuries.by.eventype[which.min(noaa.data.injuries.by.eventype$Injuries), ]
          nooa.min.value <- round(nooa.min$Injuries, 3)
          nooa.min.evtype <- as.character(nooa.min$Event)
          output$summaryHead <- renderText({"Basic statistics by event type:"})
          output$summaryMean <- renderText({paste("Mean : ", nooa.mean)})
          output$summarySd <- renderText({paste("Standar deviation : ", nooa.sd )})
          output$summaryMax <- renderText({paste("Max : ", nooa.max.value, ", Event : ", nooa.max.evtype)})
          output$summaryMin <- renderText({paste("Min : ", nooa.min.value, ", Event : ", nooa.min.evtype)})
          
          g.plot.by.event.type <- ggplot(data=noaa.data.injuries.by.eventype , aes(x=Event, y=Injuries, colour=Event, fill=Event))
          g.plot.by.event.type <- g.plot.by.event.type + geom_bar(stat="identity") + labs(fill="Event type") + guides(colour=FALSE)
          g.plot.by.event.type <- g.plot.by.event.type + xlab("Event type") + ylab("Injuries")  + ggtitle("Injuries by event type.") 
          g.plot.by.event.type  
        }
      } else{
        if (input$filter == 'By year'){
          noaa.data.fatalities.filter <- noaa.data.fatalities.1993.2012[format(noaa.data.fatalities.1993.2012$DATE, "%Y") == year,]
        }else{
          filter_date <- as.Date(paste(strtoi(year),'-12-31', sep = ""), "%Y-%m-%d")    
          noaa.data.fatalities.filter <- noaa.data.fatalities.1993.2012[noaa.data.fatalities.1993.2012$DATE <= filter_date,]      
        }
        if (nrow(noaa.data.fatalities.filter) > 0) {
          noaa.data.fatalities.by.eventype <- aggregate(noaa.data.fatalities.filter$FATALITIES,by=list(noaa.data.fatalities.filter$EVTYPE), sum) 
          names(noaa.data.fatalities.by.eventype)[1]<-"Event"
          names(noaa.data.fatalities.by.eventype)[2]<-"Fatalities"      
          
          nooa.mean <- round(mean(noaa.data.fatalities.by.eventype$Fatalities), 3)
          nooa.sd <- round(sd(noaa.data.fatalities.by.eventype$Fatalities), 3)
          nooa.max <- noaa.data.fatalities.by.eventype[which.max(noaa.data.fatalities.by.eventype$Fatalities), ]
          nooa.max.value <- round(nooa.max$Fatalities, 3)
          nooa.max.evtype <- as.character(nooa.max$Event)
          nooa.min <- noaa.data.fatalities.by.eventype[which.min(noaa.data.fatalities.by.eventype$Fatalities), ]
          nooa.min.value <- round(nooa.min$Fatalities, 3)
          nooa.min.evtype <- as.character(nooa.min$Event)
          output$summaryHead <- renderText({"Basic statistics by event type:"})
          output$summaryMean <- renderText({paste("Mean : ", nooa.mean)})
          output$summarySd <- renderText({paste("Standar deviation : ", nooa.sd )})
          output$summaryMax <- renderText({paste("Max : ", nooa.max.value, ", Event : ", nooa.max.evtype)})
          output$summaryMin <- renderText({paste("Min : ", nooa.min.value, ", Event : ", nooa.min.evtype)})
          
          g.plot.by.event.type <- ggplot(data=noaa.data.fatalities.by.eventype , aes(x=Event, y=Fatalities, colour=Event, fill=Event))
          g.plot.by.event.type <- g.plot.by.event.type + geom_bar(stat="identity") + labs(fill="Event type") + guides(colour=FALSE)
          g.plot.by.event.type <- g.plot.by.event.type + xlab("Event type") + ylab("Fatalities")  + ggtitle("Fatalities by event type.") 
          g.plot.by.event.type  
        }
      }
      
    }else{
      if (input$var == "Damages") { 
        if (input$filter == 'By year'){
          noaa.data.damages.filter <- noaa.data.damages.1993.2012[format(noaa.data.damages.1993.2012$DATE, "%Y") == year,]
        }else{
          filter_date <- as.Date(paste(strtoi(year),'-12-31', sep = ""), "%Y-%m-%d")    
          noaa.data.damages.filter <- noaa.data.damages.1993.2012[noaa.data.damages.1993.2012$DATE <= filter_date,]      
        }
        
        if (nrow(noaa.data.damages.filter) > 0) {
          noaa.data.damages.by.eventype.state <- aggregate(noaa.data.damages.filter$DAMAGES,by=list(noaa.data.damages.filter$STATE,noaa.data.damages.filter$EVTYPE), sum) 
          names(noaa.data.damages.by.eventype.state)[1]<-"State"
          names(noaa.data.damages.by.eventype.state)[2]<-"Event"
          names(noaa.data.damages.by.eventype.state)[3]<-"Damages"
          noaa.data.damages.by.eventype.state <- merge(noaa.data.damages.by.eventype.state, states.capitols.locations, by.x="State", by.y="State")
          
          
          noaa.data.damages.by.state <- aggregate(noaa.data.damages.filter$DAMAGES,by=list(noaa.data.damages.filter$STATE), sum)     
          names(noaa.data.damages.by.state)[1]<-"State"
          names(noaa.data.damages.by.state)[2]<-"Damages"          
          nooa.mean <- round(mean(noaa.data.damages.by.state$Damages), 3)
          nooa.sd <- round(sd(noaa.data.damages.by.state$Damages), 3)
          nooa.max <- noaa.data.damages.by.state[which.max(noaa.data.damages.by.state$Damages), ]
          nooa.max.value <- round(nooa.max$Damages, 3)
          nooa.max.state <- as.character(nooa.max$State)          
          nooa.min <- noaa.data.damages.by.state[which.min(noaa.data.damages.by.state$Damages), ]
          nooa.min.value <- round(nooa.min$Damages, 3)
          nooa.min.state <- as.character(nooa.min$State)
          output$summaryHead <- renderText({"Basic statistics by state:"})
          output$summaryMean <- renderText({paste("Mean : ", nooa.mean ," $")})
          output$summarySd <- renderText({paste("Standar deviation : ", nooa.sd ," $")})
          output$summaryMax <- renderText({paste("Max : ", nooa.max.value, " $, State : " , nooa.max.state)})
          output$summaryMin <- renderText({paste("Min : ", nooa.min.value, " $, State : " , nooa.min.state)})
          
          
          g.plot.damages.map <- ggplot()
          g.plot.damages.map <- g.plot.damages.map + geom_polygon(data=states.poligon.data , aes(x=long, y=lat, group = group), colour="white", fill="#eeeecc" ) 
          g.plot.damages.map <- g.plot.damages.map + geom_point(data=noaa.data.damages.by.eventype.state, aes(x=longitude, y=latitude, size=Damages, colour=Event), shape = 1) 
          g.plot.damages.map <- g.plot.damages.map + geom_text(data=noaa.data.damages.by.eventype.state, hjust=0.5, vjust=-0.5, aes(x=longitude, y=latitude, label=State), colour="#333333", size=4)
          g.plot.damages.map <- g.plot.damages.map + labs(size="Total damages" , color="Event type") + scale_size(range = c(5, 25)) # + theme(legend.text=element_text(size=4))
          g.plot.damages.map <- g.plot.damages.map + xlab("Longitude") + ylab("Latitude")  + ggtitle("State distribution of total damages (in USA million dollars) by event type.") 
          g.plot.damages.map
        }
      }else if (input$var == "Injuries") { 
        if (input$filter == 'By year'){
          noaa.data.injuries.filter <- noaa.data.injuries.1993.2012[format(noaa.data.injuries.1993.2012$DATE, "%Y") == year,]
        }else{
          filter_date <- as.Date(paste(strtoi(year),'-12-31', sep = ""), "%Y-%m-%d")    
          noaa.data.injuries.filter <- noaa.data.injuries.1993.2012[noaa.data.injuries.1993.2012$DATE <= filter_date,]      
        }
        
        if (nrow(noaa.data.injuries.filter) > 0) {
          noaa.data.injuries.by.eventype.state <- aggregate(noaa.data.injuries.filter$INJURIES,by=list(noaa.data.injuries.filter$STATE,noaa.data.injuries.filter$EVTYPE), sum) 
          names(noaa.data.injuries.by.eventype.state)[1]<-"State"
          names(noaa.data.injuries.by.eventype.state)[2]<-"Event"
          names(noaa.data.injuries.by.eventype.state)[3]<-"Injuries"
          noaa.data.injuries.by.eventype.state <- merge(noaa.data.injuries.by.eventype.state, states.capitols.locations, by.x="State", by.y="State")
          
          noaa.data.injuries.by.state <- aggregate(noaa.data.injuries.filter$INJURIES,by=list(noaa.data.injuries.filter$STATE), sum)     
          names(noaa.data.injuries.by.state)[1]<-"State"
          names(noaa.data.injuries.by.state)[2]<-"Injuries"          
          nooa.mean <- round(mean(noaa.data.injuries.by.state$Injuries), 3)
          nooa.sd <- round(sd(noaa.data.injuries.by.state$Injuries), 3)
          nooa.max <- noaa.data.injuries.by.state[which.max(noaa.data.injuries.by.state$Injuries), ]
          nooa.max.value <- round(nooa.max$Injuries, 3)
          nooa.max.state <- as.character(nooa.max$State)          
          nooa.min <- noaa.data.injuries.by.state[which.min(noaa.data.injuries.by.state$Injuries), ]
          nooa.min.value <- round(nooa.min$Injuries, 3)
          nooa.min.state <- as.character(nooa.min$State)
          output$summaryHead <- renderText({"Basic statistics by state:"})
          output$summaryMean <- renderText({paste("Mean : ", nooa.mean)})
          output$summarySd <- renderText({paste("Standar deviation : ", nooa.sd)})
          output$summaryMax <- renderText({paste("Max : ", nooa.max.value, ", State : " , nooa.max.state)})
          output$summaryMin <- renderText({paste("Min : ", nooa.min.value, ", State : " , nooa.min.state)})
          
          g.plot.injuries.map <- ggplot()
          g.plot.injuries.map <- g.plot.injuries.map + geom_polygon(data=states.poligon.data , aes(x=long, y=lat, group = group), colour="white", fill="#eeeecc" ) 
          g.plot.injuries.map <- g.plot.injuries.map + geom_point(data=noaa.data.injuries.by.eventype.state, aes(x=longitude, y=latitude, size=Injuries, colour=Event), shape = 1) 
          g.plot.injuries.map <- g.plot.injuries.map + geom_text(data=noaa.data.injuries.by.eventype.state, hjust=0.5, vjust=-0.5, aes(x=longitude, y=latitude, label=State), colour="#333333", size=4)
          g.plot.injuries.map <- g.plot.injuries.map + labs(size="Total injuries" , color="Event type") + scale_size(range = c(5, 25)) # + theme(legend.text=element_text(size=4))
          g.plot.injuries.map <- g.plot.injuries.map + xlab("Longitude") + ylab("Latitude")  + ggtitle("State distribution of total injuries (in USA million dollars) by event type.") 
          g.plot.injuries.map
        }
      }else{
        
        if (input$filter == 'By year'){
          noaa.data.fatalities.filter <- noaa.data.fatalities.1993.2012[format(noaa.data.fatalities.1993.2012$DATE, "%Y") == input$year,]
        }else{
          filter_date <- as.Date(paste(strtoi(input$year),'-12-31', sep = ""), "%Y-%m-%d")    
          noaa.data.fatalities.filter <- noaa.data.fatalities.1993.2012[noaa.data.fatalities.1993.2012$DATE <= filter_date,]      
        }
        
        if (nrow(noaa.data.fatalities.filter) > 0) {
          noaa.data.fatalities.by.eventype.state <- aggregate(noaa.data.fatalities.filter$FATALITIES,by=list(noaa.data.fatalities.filter$STATE,noaa.data.fatalities.filter$EVTYPE), sum) 
          names(noaa.data.fatalities.by.eventype.state)[1]<-"State"
          names(noaa.data.fatalities.by.eventype.state)[2]<-"Event"
          names(noaa.data.fatalities.by.eventype.state)[3]<-"Fatalities"
          noaa.data.fatalities.by.eventype.state <- merge(noaa.data.fatalities.by.eventype.state, states.capitols.locations, by.x="State", by.y="State")
          
          
          noaa.data.fatalities.by.state <- aggregate(noaa.data.fatalities.filter$FATALITIES,by=list(noaa.data.fatalities.filter$STATE), sum)     
          names(noaa.data.fatalities.by.state)[1]<-"State"
          names(noaa.data.fatalities.by.state)[2]<-"Fatalities"          
          nooa.mean <- round(mean(noaa.data.fatalities.by.state$Fatalities), 3)
          nooa.sd <- round(sd(noaa.data.fatalities.by.state$Fatalities), 3)
          nooa.max <- noaa.data.fatalities.by.state[which.max(noaa.data.fatalities.by.state$Fatalities), ]
          nooa.max.value <- round(nooa.max$Fatalities, 3)
          nooa.max.state <- as.character(nooa.max$State)          
          nooa.min <- noaa.data.fatalities.by.state[which.min(noaa.data.fatalities.by.state$Fatalities), ]
          nooa.min.value <- round(nooa.min$Fatalities, 3)
          nooa.min.state <- as.character(nooa.min$State)
          output$summaryHead <- renderText({"Basic statistics by state:"})
          output$summaryMean <- renderText({paste("Mean : ", nooa.mean)})
          output$summarySd <- renderText({paste("Standar deviation : ", nooa.sd)})
          output$summaryMax <- renderText({paste("Max : ", nooa.max.value, ", State : " , nooa.max.state)})
          output$summaryMin <- renderText({paste("Min : ", nooa.min.value, ", State : " , nooa.min.state)})
          
          g.plot.fatalities.map <- ggplot()
          g.plot.fatalities.map <- g.plot.fatalities.map + geom_polygon(data=states.poligon.data , aes(x=long, y=lat, group = group), colour="white", fill="#eeeecc" ) 
          g.plot.fatalities.map <- g.plot.fatalities.map + geom_point(data=noaa.data.fatalities.by.eventype.state, aes(x=longitude, y=latitude, size=Fatalities, colour=Event), shape = 1) 
          g.plot.fatalities.map <- g.plot.fatalities.map + geom_text(data=noaa.data.fatalities.by.eventype.state, hjust=0.5, vjust=-0.5, aes(x=longitude, y=latitude, label=State), colour="#333333", size=4)
          g.plot.fatalities.map <- g.plot.fatalities.map + labs(size="Total fatalities" , color="Event type") + scale_size(range = c(5, 25)) # + theme(legend.text=element_text(size=4))
          g.plot.fatalities.map <- g.plot.fatalities.map + xlab("Longitude") + ylab("Latitude")  + ggtitle("State distribution of total fatalities (in USA million dollars) by event type.") 
          g.plot.fatalities.map
        }
      }
    }
  })
})
