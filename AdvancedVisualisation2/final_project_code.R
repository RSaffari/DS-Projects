###Set Workdirectory-------------------------------------------------
setwd("/Users/tethys/Documents/Rmaktabkhooneh/Lecture3/Data_File_Code/Final Project")
getwd()

###Required Libraries------------------------------------------------

#For integrating and controling Leaflet maps
#install.packages("leaflet") 
library("leaflet")

#For creating elegant and complex plots 
library("ggplot2") 

##For creating interactive maps and plots 
#install.packages("plotly")
library("plotly")

#Tools for HTML generation and output
library("htmltools")

###Read Data from File------------------------------------------------
# Please load this files from your local address:
data <- read.csv("data_example_base.csv", header = T)
head(data)
#Shamsi Calendar
calendar <- read.csv("calendar.csv", header = T)
head(calendar)
tail(calendar)

#Z1 restricted zone
restrictedzone1 <- read.csv("restrictedzone.csv", header = T)
dim(restrictedzone1)

#Z2 restricted zone
restrictedzone2 <- read.csv("restrictedzone2.csv", header = T)
dim(restrictedzone2)

##Overview of Data---------------------------------------------------
dim(data)
colnames(data)
head(data,10)
tail(data,10)
View(head(data,10))

###Analysis of Demand Based on Time----------------------------------
##Data Preparation---------------------------------------------------

summary(data)

##Demand Analysis----------------------------------------------------
#Define New Variables-----------------------------------------------

#Date
data$date <- as.Date(data$time, "%m/%d/%Y")
head(data)

#Week
#Sunday 0, Monday 1, Tuesday 2, Wednesday 3, Thursday 4, Friday 5, Saturday 6
data$week_day <- format(data$date, "%w")
head(data)
tail(data,10)
data$w_d <- data$week_day

# Making a column with day names
daynum <- c("0", "1", "2", "3", "4", "5", "6")
dayname <- c("Sunday","Monday","Tuesday", "Wednesday","Thursday","Friday","Saturday")
for (i in daynum) {
  data$w_d[data$week_day == i] <- dayname[as.numeric(i)+1]
}

# to check
head(data)
tail(data)
class(data$w_d)
table(data$week_day)
table(data$w_d)

#Hour

data$hour <- format(as.POSIXct(data$time, tryFormats = "%m/%d/%Y %H:%M"),"%H")
head(data)
tail(data)
class(data$hour)
table(data$hour)

#shamsy_date
head(calendar)
data$shamsy_date <- calendar[match(data$date,as.Date(calendar$Ch_Date,"%m/%d/%Y")),'Sh_Date']
head(data)
tail(data)
table(data$shamsy_date)
class(data$shamsy_date)
data$shamsy_date <- factor(data$shamsy_date,levels = paste0("1398-Tir-",1:21))
table(data$shamsy_date)

#shamsy_month
data$shamsy_month <- calendar[match(data$shamsy_date,calendar$Sh_Date),'Sh_Month']
head(data)
tail(data)
table(data$shamsy_month)

#shamsy_week
data$shamsy_week <- calendar[match(data$shamsy_date,calendar$Sh_Date),'Year_Week']
head(data)
tail(data)
table(data$shamsy_week)
data$shamsy_week <- factor(data$shamsy_week,levels = paste0("98W",15:17))
table(data$shamsy_week)


#Find if a Point inside Polygon--------------------------------------
restrictedzone1 <- read.csv("restrictedzone.csv", header = TRUE)
restrictedzone2 <- read.csv("restrictedzone2.csv", header = TRUE)
dim(restrictedzone1)
dim(restrictedzone2)

polyFinder <- function(pointLat, pointLong, zone_data){
  polyLong <- zone_data$lon    #It's equivalent to x axis in cartesian coordinate
  polyLat <- zone_data$lat    #It's equivalent to y axis in cartesian coordinate
  i = 1
  j = dim(zone_data)[1]    #Number of polygon corners
  nodes <- 0
  
  #***Note: start point and end point should be put together successively
  #**Note: The limit is set on points with higher latitude
  #**Note: if we can sort these coordinates in clockwise we can remove the former condition in first if statement
  
  while(i <= dim(zone_data)[1]){
    if ((polyLat[i] < pointLat & pointLat <= polyLat[j]) 
        | (polyLat[j] < pointLat & pointLat <= polyLat[i])
        & (polyLong[i] <= pointLong | polyLong[j] <= pointLong)){
      if ( (((pointLat - polyLat[i])/(polyLat[j] - polyLat[i]))*(polyLong[j]-polyLong[i])) + polyLong[i] < pointLong) nodes <- nodes + 1
    }
    j = i
    i = i+1
  }
  if (nodes%%2 != 0) {u <- 1}
  else {u <- 0}
  u
}

### Determining Different Zones in Dataset----------
data$z1 <- as.numeric(NA)
data$z1 <- mapply(polyFinder, data$pickup_lat, data$pickup_lon, MoreArgs = list(zone_data = restrictedzone1))
data$IFz2 <- as.numeric(NA)
data$IFz2 <- mapply(polyFinder, data$pickup_lat, data$pickup_lon, MoreArgs = list(zone_data = restrictedzone2))
data$z2 <- ifelse(data$IFz2 == 1 & data$z1 == 0, 1, 0)
data$z3 <- ifelse(data$z2 == 0 & data$z1 == 0, 1, 0)

data$zone <- NA
data$zone[data$z1 == 1 & data$z2 == 0 & data$z3 == 0] <- "Restriction"
data$zone[data$z1 == 0 & data$z2 == 1 & data$z3 == 0] <- "Pollution"
data$zone[data$z1 == 0 & data$z2 == 0 & data$z3 == 1] <- "Outermost"
head(data)
tail(data)

# to avoid repeated calculations we write this data frame as "zone_based_data.csv"
write.csv(data, file = "zone_based_data.csv", row.names = FALSE)
# and to read
#data <- read.csv("zone_based_data.csv", header = T)


#Hourly Analysis in Restricted Zone----------------------------------

# Preparing for Plotly
#Weekly zone_based demand in terms of hour

# Now we make a new data frame composed of "weeks as: data$shamsy_week",
# "hours as: data$hour" and "districted zones as: zone"

hwz <- as.data.frame(table(data$shamsy_week, data$zone, data$hour))
colnames(hwz) <- c("week", "zone", "hour", "demand")
View(hwz)

comparision_plot <- ggplot(data = hwz, aes(x = hour, 
                                           y=demand , 
                                           colour= interaction(week, zone), 
                                           group= interaction(week, zone))) +
  geom_line() +
  ggtitle("Hourly Demand Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Hour") + 
  ylab("Demand Capacity")

comparision_plotly <- ggplotly(comparision_plot)
comparision_plotly
# We can look at any curve in this complex just by double clicking on its legend

###Shiny App------------------------------------------------

# for this app we need another column "days as: data$w_d" so we make another data frame 
hwdz <- as.data.frame(table(data$shamsy_week, data$w_d, data$zone, data$hour))
colnames(hwdz) <- c("week", "day", "zone", "hour", "demand")
View(hwdz)


library(shiny)

# Preparing Data for Shiny App

# Determining Popup positions
lon = c(restrictedzone1[restrictedzone1[2] == max(restrictedzone1[2]),1], 
        restrictedzone2[restrictedzone2[2] == max(restrictedzone2[2]),1], 
        restrictedzone2[restrictedzone2[2] == max(restrictedzone2[2]),1] - 0.15)
lat = c(max(restrictedzone1[2]), 
        max(restrictedzone2[2]), 
        max(restrictedzone2[2]) - 0.05)
zon = c("Restriction", "Pollution", "Outermost")
data_loc = data.frame(lon, lat, zon)

# Binding restriction zones as a data frame 
restrictedzone1$zone <- "Restriction"
restrictedzone2$zone <- "Pollution"
restriction <- rbind(restrictedzone1, restrictedzone2)

### Shiny input
ui <- fluidPage(titlePanel("Hourly Demand"),
                sidebarLayout(
                  sidebarPanel(h3("Input Parameters:"), 
                               radioButtons("weeks", 
                                            "Which week ... ?", 
                                            choices = c("98W15","98W16","98W17"), 
                                            selected = "98W15"),
                               br(),
                               selectInput("days", 
                                           "Which day ... ?" ,
                                           choices = c("Saturday","Sunday","Monday",
                                                       "Tuesday","Wednesday","Thursday","Friday")),
                               br(),
                               radioButtons("zones", 
                                            "Which zone ... ?", 
                                            choices = c("Restriction","Pollution","Outermost"), 
                                            selected = "Restriction")),
                  mainPanel(
                    h3("Output Results:"),
                    plotOutput("plot"),
                    br(),br(),
                    leafletOutput("mymap"),
                    br(),br(),
                    tableOutput("results"))
                ))
# Shiny server
server <- function(input, output) {
  output$plot <- renderPlot({
    filtered_data <- hwdz[hwdz$week == input$weeks & 
                            hwdz$day == input$days & 
                            hwdz$zone == input$zones,]
    ggplot(data = filtered_data, aes(x = hour, y = 100*demand/sum(demand), group= interaction(week, day, zone))) +
      geom_line(size = 1.2 ,colour = "red") +
      xlab("Hour") +
      ylab("Demand Percentage")
  })
  output$results <- renderTable({hwdz[hwdz$week == input$weeks & 
                                        hwdz$day == input$days & 
                                        hwdz$zone == input$zones,4:5]})
  output$mymap <- renderLeaflet({
    mymap <- leaflet() %>%
      addTiles() %>%
      setView(lat=35.6892, lng= 51.3890, zoom = 11) %>%
      addPolygons(lat = restrictedzone1$lat, 
                  lng = restrictedzone1$lon, 
                  color = "blue", group = "Restriction", fill = F) %>%
      addPolygons(lat = restrictedzone2$lat, 
                  lng = restrictedzone2$lon, 
                  color = "red", group = "Pollution", fill = F) %>%
      #addPolygons(lat = restriction$lat[restriction$zone == input$zones], 
      #            lng = restriction$lon[restriction$zone == input$zones], 
      #            color = "blue", fill = F) %>%
      addPopups(data_loc$lon[data_loc$zon == input$zones], 
                data_loc$lat[data_loc$zon == input$zones], 
                paste(sep = "<br/>", input$zones, "Total Demand: ", 
                      sum(hwdz[hwdz$week == input$weeks & 
                                 hwdz$day == input$days & 
                                 hwdz$zone == input$zones,5])), 
                options = popupOptions(closeButton = FALSE))
  })
}
# Shiny app
shinyApp(ui = ui, server = server)
