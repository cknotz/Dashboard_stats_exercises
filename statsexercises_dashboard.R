
# Sandbox for Stats Exercises Dashboard
#######################################

# Carlo Knotz, UiS
# Start date: Feb 22, 2021

library(MASS)
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(dashboardthemes)
  library(shinyjs)
  library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title="Statistics Exercises"),
  dashboardSidebar(collapsed = T,
    sidebarMenu(
      menuItem("Start",tabName = "start"),
      menuItem("Measures of central tendency",tabName = "cent"),
      menuItem("Measures of spread",tabName = "spread"),
      menuItem("Chi-squared test",tabName = "chi"),
      menuItem("Difference of means test",tabName = "ttest"),
      menuItem("Correlation",tabName = "corr", selected = T),
      menuItem("Linear regression",tabName = "ols")
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    shinyDashboardThemes(theme="grey_light"),
    tabItems(
      tabItem(tabName = "start"),
      tabItem(tabName = "corr",
              fluidRow(
                column(width = 4,
                box(width=NULL,title = "Correlation coefficient",collapsible = F,
                    solidHeader = F,
                    HTML("<p>The correlation coefficient is a measure of how strongly
                         two metric (or continuous, linear) variables are associated
                         with each other. To start, click on the 'Gimme some numbers'-
                         button to generate some data.</p>
                         
                         <p>The box below provides a detailed step-by-step explanation for how to
                         compute the correlation coefficient. If you want to see that, just
                         expand the box by clicking on the 'plus' symbol on the right.</p>")),
                box(width=NULL,title = "Controls",collapsible = F,solidHeader = F,
                    actionBttn(inputId = "sim",
                               label = "Gimme some numbers!",
                               style="material-flat",
                               color="success",
                               size = "sm"),
                    br(),br(),
                    disabled(actionBttn(inputId = "solution",
                                        label = "Gimme the solution!",
                                        style = "material-flat",
                                        color = "warning",
                                        size = "sm"))
              ),
              box(width = NULL,title = "Result",collapsible = F,solidHeader = F,
                  textOutput(outputId = "result")
                  )
              ),
              column(width=8,
              box(width=NULL,title = "Data",collapsible = F,solidHeader = F,
                  column(3,tableOutput(outputId = "tab")),
                  column(9,plotOutput(outputId = "plot"))
                  )
              )),
              fluidRow(
                box(width = 12,title = "The detailed solution",collapsible = T,
                    collapsed = T,solidHeader = F,
                    HTML("<p><strong>Step 1:</strong></p>"))
              )
    )
  )
))

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
  
  output$plot <- renderPlot({
    ggplot(vals$data,aes(x=X,y=Y)) +
      geom_point() +
      geom_smooth(method='lm',se=F,color="gray",linetype="dashed")
  })
 
})
  
observeEvent(input$solution,{
  res <- isolate(round(cor(vals$data$V1,vals$data$V2),digits=2))
  
  output$result <- renderText(
    paste0("The correlation coefficient is: ",res))
})  
  
}

shinyApp(ui = ui, server = server)