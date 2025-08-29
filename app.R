library(shiny)
library(ggplot2)

sim <- function(N0, Sigma){
  Y <- 50 # Number of years the model will run
  K <- 100
  # Make a vector for the state variable
  N <- numeric(Y)
  # Define parameter values
  N[1] <- N0 # Initial population size
  for (t in 1:(Y - 1)){
    r <- rnorm(1,0.2,Sigma)
    N[t + 1] <- ifelse(N[t] == 0, 0, 
                       round(N[t] + N[t] * r * (1 - N[t] / K)))
    N[t + 1] <- ifelse(N[t + 1] < 0, 0, N[t + 1])
  }
  df <- data.frame(N)
  df$Time <- 0:(Y-1)
  return(df)
}

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Demographic stochasticity"),

  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("N0", "N0:",
                  min = 0, max = 20, value = 10),
      sliderInput("Sigma", "Sigma:",
                  min = 0, max = 0.5, value = 0.1),
      actionButton("run", "Project forward")
    ),
    mainPanel(
      plotOutput("popPlot")
    )
  )
)

# Define server logic required to draw a plot
server <- function(input, output) {
  observeEvent(input$run, {
    df <- sim(input$N0, input$Sigma)
    output$popPlot <- renderPlot({
      ggplot(df) + 
        geom_line(aes(x=Time, y=N)) +
        labs(x = 'Time', 
             y = 'N') +
        ylim(0,105) +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.title.y = element_text(angle=90, vjust = 2, size = rel(1.2)),
              axis.title.x = element_text(size = rel(1.2)),
              axis.text = element_text(size = rel(1.2)),
              axis.line = element_line(colour="black"),
              axis.ticks = element_line(colour="black"),
              axis.ticks.length = unit(.25, "cm"),
              legend.position = "none")
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)