
# Sandbox for Stats Exercises Dashboard
#######################################

# Carlo Knotz

library(MASS)
  library(shiny)
  library(shinyjs)

cor(data$V1,data$V2)

ui <- fluidPage(
  shinyjs::useShinyjs(),
    titlePanel("What is the correlation coefficient?"),
    sidebarLayout(
      sidebarPanel(
        actionButton(inputId = "sim",
                     label = "Gimme some numbers!"),
        disabled(actionButton(inputId = "solution",
                     label = "What's the solution?"))
      ),
      mainPanel(
        tableOutput(outputId = "tab"),
        textOutput(outputId = "result")
      )
    )
)

server <- function(input,output,session){
  
  vals <- reactiveValues()
  
observeEvent(input$sim, {
  
  rho <- runif(n=1,
               min=0,
               max=1)
  
  vals$data <- as.data.frame(mvrnorm(n=10,
                                mu = c(0,0),
                                Sigma = matrix(c(1,rho,rho,1),ncol = 2),
                                empirical = T))
  
  vals$data$X <- round((vals$data$V1 - min(vals$data$V1))*10+10, digits = 0)
  vals$data$Y <- round((vals$data$V2 - min(vals$data$V2))*20+20, digits = 0)  
  output$tab <- renderTable(vals$data[,c("X","Y")],
                            digits = 0)
  enable("solution")
 
})
  
observeEvent(input$solution,{
  res <- isolate(round(cor(vals$data$V1,vals$data$V2),digits=2))
  
  output$result <- renderText(
    paste0("The correlation coefficient is: ",res))
})  
  
}

shinyApp(ui = ui, server = server)