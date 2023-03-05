###Advanced Data Visualization w/ R----------------------------------
###Set Workdirectory-------------------------------------------------
getwd()
setwd("/Users/tethys/Desktop/Data_File_Code")
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

#data <- read.csv("data_example_base.csv", header = T)
data <- read.csv("prepared_data.csv", header = T)

#Shamsi Calendar
calendar <- read.csv("calendar.csv", header = T)
head(calendar)
tail(calendar)

##Overview of Data---------------------------------------------------
dim(data)
colnames(data)
head(data,10)
tail(data,10)
View(head(data,10))

###Analysis of Demand Based on Time----------------------------------
##Data Preparation---------------------------------------------------

summary(data)

#Define New Variables-----------------------------------------------

#Date
class(data$time)
data$date <- as.Date(data$time, "%m/%d/%Y")
head(data)
tail(data)
class(data$date)
table(data$date)

#Week
#Sunday 0, Monday 1, Tuesday 2, Wednesday 3, Thursday 4, Friday 5, Saturday 6
data$week_day <- format(data$date, "%w")
head(data)
tail(data)
class(data$week_day)
table(data$week_day)

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


##Demand Analysis----------------------------------------------------

#Daily Demand

table(data$date)
daily_demand <- as.data.frame(table(data$date))
daily_demand
colnames(daily_demand) <- c("date","demand")
daily_demand$sh_date <- calendar[match(as.Date(daily_demand$date),as.Date(calendar$Ch_Date,"%m/%d/%Y")),'Sh_Date']
head(daily_demand)
tail(daily_demand)

#Plot Daily Demand
plot(daily_demand$date,daily_demand$demand)

daily_demand$sh_date <- factor(daily_demand$sh_date,levels = paste0("1398-Tir-",1:21))
plot(daily_demand$sh_date,daily_demand$demand)

#Plot
plot(daily_demand$demand,  type = "l",col = "red", main = "Daily Demand", xlab = "", ylab = "Demand", lwd = 3, cex.axis = 1.1, xaxt="n")
axis(1, at = daily_demand$date , daily_demand$sh_date , las=2, cex.axis = 0.8)

#Plot  Based on Week Day Demand

week_day_demand <- as.data.frame(table(data$week_day))
week_day_demand
#Sunday 0, Monday 1, Tuesday 2, Wednesday 3, Thursday 4, Friday 5, Saturday 6
colnames(week_day_demand) <- c("week_day","demand")
week_day_demand
week_day_demand$percentage <- round(100 * week_day_demand$demand/sum(week_day_demand$demand),1)    
week_day_demand$name <- c("Sunday","Monday","Tuesday", "Wednesday","Thursday","Friday","Saturday")
week_day_demand$name <- factor(week_day_demand$name, levels = c("Saturday","Sunday","Monday","Tuesday", "Wednesday","Thursday","Friday"))

week_day_demand <- week_day_demand[order(week_day_demand$name),]
week_day_demand

#Plot with Normal R Function
plot_week_day_demand <- barplot(week_day_demand$percentage, main= "Weekly Demand Distribution", xlab = "", ylab = "Percentage", ylim = c(0,25), col = "#009999", cex.main = 1.5 )
axis(1, at = plot_week_day_demand, week_day_demand$name, cex.axis = 0.9, las=2)


#Plot with ggplot2

ggplot(week_day_demand, aes(name, percentage)) + 
  geom_bar(stat="identity") + 
  ggtitle("Demand Distribution") +
  xlab("Week Day") + 
  ylab("Demand(%)")

#Compare Two Weeks

head(data)
table(data$shamsy_week)
# 98W15 Campaign Week
# 98W17 Normal Week

# Campaign Week
week_day_demand_w15 <- as.data.frame(table(data$week_day[data$shamsy_week == "98W15"]))
colnames(week_day_demand_w15) <- c("week_day","demand")
week_day_demand_w15$percentage <- round(100 * week_day_demand_w15$demand/sum(week_day_demand_w15$demand),1)    
week_day_demand_w15$name <- c("Sunday","Monday","Tuesday", "Wednesday","Thursday","Friday","Saturday")
week_day_demand_w15$name <- factor(week_day_demand_w15$name, levels = c("Saturday","Sunday","Monday","Tuesday", "Wednesday","Thursday","Friday"))
week_day_demand_w15
week_day_demand_w15$group <- "campaign week"
week_day_demand_w15

#Normal Week
week_day_demand_w17 <- as.data.frame(table(data$week_day[data$shamsy_week == "98W17"]))
colnames(week_day_demand_w17) <- c("week_day","demand")
week_day_demand_w17$percentage <- round(100 * week_day_demand_w17$demand/sum(week_day_demand_w17$demand),1)    
week_day_demand_w17$name <- c("Sunday","Monday","Tuesday", "Wednesday","Thursday","Friday","Saturday")
week_day_demand_w17$name <- factor(week_day_demand_w17$name, levels = c("Saturday","Sunday","Monday","Tuesday", "Wednesday","Thursday","Friday"))
week_day_demand_w17$group <- "normal week"
week_day_demand_w17

week_day_demand_comp <- rbind(week_day_demand_w17,week_day_demand_w15)

#Comparision Plot

comparision_plot <- ggplot(week_day_demand_comp, aes(name, percentage, fill = group)) + 
                    geom_bar(stat="identity", position = "dodge") + 
                    ggtitle("Demand Distribution") +
                    xlab("Week Day") + 
                    ylab("Demand(%)") +
                    scale_fill_brewer(palette = "Set1")

comparision_plot <- comparision_plot + theme(
                                        plot.title = element_text(hjust = 0.5, color="green", size=24, face="bold.italic"),
                                        axis.title.x = element_text(color="blue", size=16, face="bold"),
                                        axis.title.y = element_text(color="#993333", size=16, face="bold"),
                                        axis.text.x = element_text(angle = 90, hjust=1, size = 20),
                                        axis.text.y = element_text(size = 20),
                                        panel.background = element_rect(fill = "lightblue"),
                                        panel.grid.major = element_line(size = 1.5,  linetype = "solid",color = "white"), 
                                        panel.grid.minor = element_line(size = 0.25, linetype = "dashed",color = "white")
                                      ) #too much cognitive load


comparision_plot <- comparision_plot + theme(
                    plot.title = element_text(hjust = 0.5, color="black", size=24, face="plain"),
                    legend.title = element_text(size=16),
                    legend.text = element_text(size=16),
                    axis.title.x = element_text(color = "black", size=16, face="plain"),
                    axis.title.y = element_text(color = "black", size=16, face="plain"),
                    axis.text.x = element_text(angle = 90, hjust=1, size = 16),
                    axis.text.y = element_text(size = 16),
                    panel.background = element_blank(),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank()
                  ) #less cognitive load


#Plot based on Absolute Demand 
ggplot(week_day_demand_comp, aes(name, demand, fill = group)) + 
      geom_bar(stat="identity", position = "dodge") + 
      ggtitle("Demand Distribution") +
      xlab("Week Day") + 
      ylab("Demand") +
      scale_fill_brewer(palette = "Set1")

#Hourly Demand Distribution

table(data$hour)
#Try it yourself!

#Heatmap Hour vs Weekday

table(data$hour,data$week_day)

hour_weekday_demand <- as.data.frame(table(data$hour,data$week_day))
colnames(hour_weekday_demand) <- c("hour","weekday","demand")

#Plot Heatmap
ggplot(hour_weekday_demand, aes(hour, weekday, fill= demand)) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="red") +
  ggtitle("Demand Distribution \n based on Hour and Weekday") +
  scale_y_discrete(labels=c("Sunday","Monday","Tuesday", "Wednesday","Thursday","Friday","Saturday")) +
  theme(plot.title = element_text(hjust = 0.5, size=20))

#Average Hourly Demand

hour_day_demand <- as.data.frame(table(data$hour,data$date))
head(hour_day_demand)
colnames(hour_day_demand) <- c("hour","date","demand")

tapply(hour_day_demand$demand,hour_day_demand$hour, mean)

ave_hour_day_demand <- as.data.frame(tapply(hour_day_demand$demand,hour_day_demand$hour, mean))
colnames(ave_hour_day_demand) <- "average_demand"
ave_hour_day_demand
ave_hour_day_demand$hour <- rownames(ave_hour_day_demand) 
ave_hour_day_demand

#Box Plot

head(hour_day_demand)
ggplot(hour_day_demand, aes(x = hour, y = demand)) + 
  geom_boxplot() +
  ggtitle("Hourly Demand Distribution") +
  theme(plot.title = element_text(hjust = 0.5, size=20)) +
  stat_summary(fun.y = mean, color = "red", geom = "point", shape = 18, size = 2)

#Outliers

d <- hour_day_demand[hour_day_demand$hour == "09",]
quantile(d$demand, probs = c(0.25,0.75))
iqr <- IQR(d$demand)
iqr
2076 + 1.5 * iqr
1320 - 1.5 * iqr
sum(d$demand > 2076 + 1.5 * iqr)
sum(d$demand < 1320 - 1.5 * iqr)

###Analysis of Demand Based on Geographical Distribution-------------
#Distribution graph for lat and lon

hist(data$pickup_lat, breaks = 50)
abline(v = mean(data$pickup_lat), col = "red", lwd = 3)

hist(data$pickup_lon, breaks = 50)
abline(v = mean(data$pickup_lon), col = "red", lwd = 3)


##Introduction to Leaflet--------------------------------------------

#Initialize Map
library("leaflet")
map <- leaflet()
map

#Add Map
map <- map %>% 
       addTiles()
map
#Set View
map <- map %>%
       setView(lat=35.6892, lng= 51.3890, zoom = 11)
map

#Add Azadi Square

map <- map %>%
       addMarkers(lat= 35.699730,lng = 51.338118, label = "Azadi Square")
map
#First 1000 Datapoints

head(data)

map1 <- leaflet() %>%
        addTiles() %>%
        setView(lat=35.6892, lng= 51.3890, zoom = 11)
  
map1 <- map1 %>%
        addCircleMarkers(lat= data$pickup_lat[1:1000],lng = data$pickup_lon[1:1000], label = as.character(data$id[1:1000]), color = "red", weight = 2)

map1

#Add Polygon

restrictedzone1 <- read.csv("restrictedzone.csv", header = TRUE)
restrictedzone2 <- read.csv("restrictedzone2.csv", header = TRUE)
dim(restrictedzone1)
dim(restrictedzone2)

map1 <- map1 %>%
        addPolygons(lat = restrictedzone2$lat, lng = restrictedzone2$lon, color = "blue")

map1
#Add Layer Control

map2 <- leaflet() %>%
        addTiles() %>%
        setView(lat=35.6892, lng= 51.3890, zoom = 11) %>%
        addCircleMarkers(lat= data$pickup_lat[1:1000],lng = data$pickup_lon[1:1000], label = as.character(data$id[1:1000]), color = "red", weight = 2, group = "Datapoints") %>%
        addPolygons(lat = restrictedzone1$lat, lng = restrictedzone1$lon, color = "blue", group = "Restricted Zone") %>%
        addLayersControl(overlayGroups = c("Datapoints","Restricted Zone"),options = layersControlOptions(collapsed = FALSE))

map2 
#save file as html

#Find if a Point inside Polygon--------------------------------------

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

data$z1 <- as.numeric(NA)
data$z1 <- mapply(polyFinder, data$pickup_lat, data$pickup_lon, MoreArgs = list(zone_data = restrictedzone1))

#Test polyFinder
polyFinder(data$pickup_lat[data$id == 413993],data$pickup_lon[data$id == 413993],restrictedzone)
polyFinder(data$pickup_lat[data$id == 414660],data$pickup_lon[data$id == 414660],restrictedzone)


table(data$shamsy_week)

data_w17 <- data[data$shamsy_week == "98W17",]
dim(data_w17)
for (i in 1:dim(data_w17)[1]) {
  data_w17$ifrestzone[i] <- polyFinder(data_w17$pickup_lat[i],data_w17$pickup_lon[i],restrictedzone)
}

head(data_w17)
tail(data_w17)
sum(is.na(data_w17$ifrestzone))
sum(data_w17$ifrestzone == 1)/dim(data_w17)[1] * 100

#Hourly Analysis in Restricted Zone----------------------------------

hourly_demand_w17 <- as.data.frame(table(data_w17$hour[data_w17$ifrestzone == 0]))
colnames(hourly_demand_w17) <- c("hour","demand")
hourly_demand_w17
hourly_demand_w17$percentage <- round(100 * hourly_demand_w17$demand/sum(hourly_demand_w17$demand),1)
hourly_demand_w17
hourly_demand_w17$group <- "out of restricted zone"

hourly_demand_w17_restz <- as.data.frame(table(data_w17$hour[data_w17$ifrestzone == 1]))
colnames(hourly_demand_w17_restz) <- c("hour","demand")
hourly_demand_w17_restz$percentage <- round(100 * hourly_demand_w17_restz$demand/sum(hourly_demand_w17_restz$demand),1)
hourly_demand_w17_restz$group <- "restricted zone"
hourly_demand_w17_restz

hourly_demand_w17_plot <- rbind(hourly_demand_w17,hourly_demand_w17_restz)

#Comparision Plot

ggplot(data = hourly_demand_w17_plot, aes(x = hour, y= percentage, group = group)) +
  geom_line(aes(color = group), size = 1.5) +
  geom_point(aes(color = group)) +
  ggtitle("Hourly Demand Distribution")

###Interactive Graphs/ plotly----------------------------------------

#Daily Demand Plot
head(daily_demand)
plot_ly(data = daily_demand, x = ~ sh_date, y = ~ demand)

plot_daily_demand <- ggplot(data = daily_demand, aes(x = sh_date, y = demand, group =1)) +
                     geom_line(color = "red", size = 1.5) +
                     ggtitle("Demand Distribution") +
                     xlab("Date") + 
                     ylab("Demand") +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1))

#ggplot2
plot_daily_demand
#plotly
plotly_daily_demand <- ggplotly(plot_daily_demand)
plotly_daily_demand

#Add annotation

plotly_daily_demand <-  plotly_daily_demand %>%
                        layout(annotations = list(x = 3,
                                                  y = daily_demand$demand[daily_demand$sh_date == "1398-Tir-3"],
                                                  text = "Campaing Day",
                                                  showarrow = TRUE,
                                                  arrowhead = 2,
                                                  ax = 50,
                                                  ay = -20))
plotly_daily_demand

#Comparision Plot
comparision_plot <- ggplot(week_day_demand_comp, aes(name, percentage, fill = group)) + 
                    geom_bar(stat="identity", position = "dodge") + 
                    ggtitle("Demand Distribution") +
                    xlab("Week Day") + 
                    ylab("Demand(%)") +
                    scale_fill_brewer(palette = "Set1")

##ggplot2
comparision_plot
#plotly
comparision_plotly <- ggplotly(comparision_plot)
comparision_plotly
#Save File in HTML Format
#More Material on:
#https://plotly-r.com/

###Work with Shiny---------------------------------------------------

#install.packages("shiny")
library(shiny)

runExample("01_hello")

#Examples:
#http://www.showmeshiny.com/

#Shiny app basics

#1) a web page that shows the app to the user (User Interface) 
#2) a computer that powers the app (Server)

#Create an empty Shiny app
ui <- fluidPage()
server <- function(input, output) {}
shinyApp(ui = ui, server = server)

#mtcars
mtcars

#Add plain text to the UI
ui <- fluidPage("my first shiny app",
                "mtcars")
server <- function(input, output) {}
shinyApp(ui = ui, server = server)

#Add formatted text and other HTML elements
ui <- fluidPage(h1("my first shiny app"),
                br(),
                strong("mtcars"),
                div("this is blue", style = "color: blue;"))
server <- function(input, output) {}
shinyApp(ui = ui, server = server)

#Add title
ui <- fluidPage(titlePanel("MTCARS Dashboard"))
server <- function(input, output) {}
shinyApp(ui = ui, server = server)

#Add a layout
ui <- fluidPage(titlePanel("MTCARS Dashboard"),
                sidebarLayout(
                  sidebarPanel(h2("Input")),
                  mainPanel(h2("Output"))
                ))
server <- function(input, output) {}
shinyApp(ui = ui, server = server)

print(ui)

#Add inputs to the UI
mtcars
range(mtcars$mpg)

#Slider Input
ui <- fluidPage(titlePanel("MTCARS Dashboard"),
                sidebarLayout(
                  sidebarPanel(h2("Input"), 
                               sliderInput("mpg", "MPG", min = 10, max = 35, step = 1, round = TRUE, value = 20)),
                  mainPanel(h2("Output"))
                ))
server <- function(input, output) {}
shinyApp(ui = ui, server = server)

#Radio Buttons
unique(mtcars$gear)

ui <- fluidPage(titlePanel("MTCARS Dashboard"),
                sidebarLayout(
                sidebarPanel(h2("Input"), 
                               sliderInput("mpg", "MPG", min = 10, max = 35, step = 1, round = TRUE, value = 20),
                               radioButtons("gears", "# of Gears",choices = c(3,4,5), selected = 4)),
                mainPanel(h2("Output"))
                ))
server <- function(input, output) {}
shinyApp(ui = ui, server = server)

#Select Input

ui <- fluidPage(titlePanel("MTCARS Dashboard"),
                sidebarLayout(
                  sidebarPanel(h2("Input"), 
                               sliderInput("mpg", "MPG", min = 10, max = 35, step = 1, round = TRUE, value = 20),
                               radioButtons("gears", "# of Gears",choices = c(3,4,5), selected = 4),
                               selectInput("vs", "VS Type",choices = c(0,1))),
                  mainPanel(h2("Output"))
                ))
server <- function(input, output) {}
shinyApp(ui = ui, server = server)

#Output

ui <- fluidPage(titlePanel("MTCARS Dashboard"),
                sidebarLayout(
                  sidebarPanel(h2("Input"), 
                               sliderInput("mpg", "MPG", min = 10, max = 35, step = 1, round = TRUE, value = 20),
                               radioButtons("gears", "# of Gears",choices = c(3,4,5), selected = 4),
                               selectInput("vs", "VS Type",choices = c(0,1))),
                  mainPanel(
                    h2("Output"),
                    plotOutput("plot"),
                    br(),br(),
                    tableOutput("results")
                    )
                ))
server <- function(input, output) {}
shinyApp(ui = ui, server = server)

#Implement Server Logic to Create Outputs

ui <- fluidPage(titlePanel("MTCARS Dashboard"),
                sidebarLayout(
                  sidebarPanel(h2("Input"), 
                               sliderInput("mpg", "MPG", min = 10, max = 35, step = 1, round = TRUE, value = 20),
                               radioButtons("gears", "# of Gears",choices = c(3,4,5), selected = 4),
                               selectInput("vs", "VS Type",choices = c(0,1))),
                  mainPanel(
                    h2("Output"),
                    plotOutput("plot"),
                    br(),br(),
                    tableOutput("results")
                  )
                ))
server <- function(input, output) {
  output$plot <- renderPlot({
    plot(rnorm(100))
  })
}
shinyApp(ui = ui, server = server)

#Making an Output React to an Input

ui <- fluidPage(titlePanel("MTCARS Dashboard"),
                sidebarLayout(
                  sidebarPanel(h2("Input"), 
                               sliderInput("mpg", "MPG", min = 10, max = 35, step = 1, round = TRUE, value = 20),
                               radioButtons("gears", "# of Gears",choices = c(3,4,5), selected = 4),
                               selectInput("vs", "VS Type",choices = c(0,1))),
                  mainPanel(
                    h2("Output"),
                    plotOutput("plot"),
                    br(),br(),
                    tableOutput("results")
                  )
                ))
server <- function(input, output) {
  output$plot <- renderPlot({
    plot(rnorm(input$mpg))
  })
}
shinyApp(ui = ui, server = server)

#Building the Plot Output

mtcars

ui <- fluidPage(titlePanel("MTCARS Dashboard"),
                sidebarLayout(
                  sidebarPanel(h2("Input"), 
                               sliderInput("mpg", "MPG", min = 10, max = 35, step = 1, round = TRUE, value = 20),
                               radioButtons("gears", "# of Gears",choices = c(3,4,5), selected = 4),
                               selectInput("vs", "VS Type",choices = c(0,1))),
                  mainPanel(
                    h2("Output"),
                    plotOutput("plot"),
                    br(),br(),
                    tableOutput("results")
                  )
                ))
server <- function(input, output) {
  output$plot <- renderPlot({
    
    filtered_data <- mtcars[mtcars$mpg <= input$mpg &
                            mtcars$gear == input$gears &
                            mtcars$vs == input$vs,]
    
    ggplot(data = filtered_data, aes(x = wt, y = mpg)) +
    geom_point()
  })
}
shinyApp(ui = ui, server = server)

#Building the Table Output

ui <- fluidPage(titlePanel("MTCARS Dashboard"),
                sidebarLayout(
                  sidebarPanel(h2("Input"), 
                               sliderInput("mpg", "MPG", min = 10, max = 35, step = 1, round = TRUE, value = 20),
                               radioButtons("gears", "# of Gears",choices = c(3,4,5), selected = 3),
                               selectInput("vs", "VS Type",choices = c(0,1))),
                  mainPanel(
                    h2("Output"),
                    plotOutput("plot"),
                    br(),br(),
                    tableOutput("results")
                  )
                ))
server <- function(input, output) {
  output$plot <- renderPlot({
    
    filtered_data <- mtcars[mtcars$mpg <= input$mpg &
                              mtcars$gear == input$gears &
                              mtcars$vs == input$vs,]
    
    ggplot(data = filtered_data, aes(x = wt, y = mpg)) +
      geom_point()
  })
  output$results <- renderTable({
    
    filtered_data <- mtcars[mtcars$mpg <= input$mpg &
                              mtcars$gear == input$gears &
                              mtcars$vs == input$vs,]
    
    filtered_data
  })
}
shinyApp(ui = ui, server = server)
########End of Code ########


