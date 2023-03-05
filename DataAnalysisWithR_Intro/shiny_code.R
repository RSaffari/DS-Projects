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
