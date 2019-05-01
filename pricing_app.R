library(shiny)
source(file = "Black_Scholes_functions.R")



ui <- fluidPage(
  titlePanel("Black-Scholes visualisation"),
  sidebarLayout(
    sidebarPanel(
      h4("Trajectories for the underlying asset"),
      numericInput("init_value", "Initial value", value = 100,min=0,step=0.01) ,
      numericInput("r", "r", value = 0.01,min=0,step=0.0001) ,
      numericInput("sigm", "Volatility", value = 0.05,min=0,step=0.0001) ,
      numericInput("n", "Time step", value = 100,min=1) ,
      numericInput("N_sim", "Number of simulations", value = 1,min=1) ,
      
      h4("Specific parameters of the european options"),
      numericInput("K", "Strike", value = 110,min=1,step=0.01),
      numericInput("t", "Expiration date", value = 1,min=1)
      
    ),
    mainPanel(
      h3("Trajectories simulations"),
      plotOutput("trajectories"),
      h3("European options prices"),
      "Call :",
      textOutput("call"),
      "Put :",
      textOutput("put")
    )
  )
)

server <- function(input,output){
  output$trajectories <- renderPlot({
    plot_MC_diffusion(input$init_value,input$r,input$sigm,input$n,input$N_sim)
  })
  
  
  
  output$call <- renderPrint({
    price <- eur_options(input$init_value,input$K,input$t,input$r,input$sigm)
    price[1]
    })
  output$put <- renderPrint({
    price <- eur_options(input$init_value,input$K,input$t,input$r,input$sigm)
    price[2]
  })

}

shinyApp( ui = ui , server = server )