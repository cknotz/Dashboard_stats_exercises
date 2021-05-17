
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
                         with each other. In this exercise, you calculate some correlation
                         coefficients by hand.</p>
                         
                         <p>The computer will generate two variables for you with randomly
                         varying levels of correlation between them. To start, click on the 'Gimme some numbers!' 
                         button. To generate new variables, just click the button again.</p>
                         
                         <p>You can find the formula to compute the correlation coefficient between
                         two linear variables in Kellstedt & Whitten (2013, Chapter 7.4.3).</p> 
                         
                         <p>The computer will show you the correct solution if you click on the 
                         'Show me the solution!' button.</p>
                         
                         <p>If you want a more detailed step-by-step explanation, you can expand the box below 
                         by clicking on the 'plus' symbol on the right. See also the explanation in 
                         Kellstedt & Whitten.</p>")),
                box(width=NULL,title = "Controls",collapsible = F,solidHeader = F,
                    actionBttn(inputId = "sim",
                               label = "Gimme some numbers!",
                               style="material-flat",
                               color="success",
                               size = "xs"),
                    br(),br(),
                    disabled(actionBttn(inputId = "solution",
                                        label = "Gimme the solution!",
                                        style = "material-flat",
                                        color = "warning",
                                        size = "xs"))
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
                    collapsed = F,solidHeader = F,id = "det",
                    uiOutput("cor_detail1"),
                    uiOutput("cor_detail2"),
                    uiOutput("cor_detail3"),
                    uiOutput("cor_detail4"),
                    tableOutput("cor_tab1"),
                    uiOutput("cor_detail5"),
                    tableOutput("cor_tab2"),
                    uiOutput("cor_detail6"),
                    uiOutput("cor_detail7"),
                    uiOutput("cor_detail8"),
                    uiOutput("cor_detail9")
                    )
                
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
                            digits = 0,
                            rownames = T)
  enable("solution")
  
  output$plot <- renderPlot({
    ggplot(vals$data,aes(x=X,y=Y)) +
      geom_point() +
      geom_smooth(method='lm',se=F,color="gray",linetype="dashed")
  })
 
})
  
observeEvent(input$solution,{

# Simple solution
  res <- isolate(round(cor(vals$data$V1,vals$data$V2),digits=2))
  
  t <- round((res*sqrt(10-2))/(sqrt(1-res^2)),digits=3)
  
  p_val <- round(dt(t,df = 10-2),digits = 3)
  
  output$result <- renderText(
    paste0("The correlation coefficient is: ",res,", and its t-value is: ",t,".\n
            The corresponding p-value is: ",p_val))
  
  
# Detailed solution  
output$cor_detail1 <- renderUI({
  withMathJax(helpText("The first step is to calculate the
                          covariance between X and Y. The formula to calculate the
                          covariance is the following: $$cov_{X,Y} = \\frac{\\sum_{i=1}^n (X_i - \\bar X)(Y_i - \\bar Y)}{n}$$"))
})

output$cor_detail2 <- renderUI({
  HTML("<p>You may notice some similarities between this formula and the formula for
                          the variance, which was covered earlier: The variance is calculated as the
                          deviation of each observation within a variable from that variable's mean divided by
                          the overall number of observations (minus 1). The calculation of the covariance is 
                          similar, but we now have to do a few extra steps.</p>")
})

output$cor_detail3 <- renderUI({
  HTML(paste0("<p>First we calculate the mean values for X and Y, which are:</p>
       
       <p><strong>Mean of X: ",isolate(round(mean(vals$data$X),digits = 1)),"</strong></p>
       
       <p><strong>Mean of Y: ",isolate(round(mean(vals$data$Y),digits = 1)),"</strong></p>"))  
})

output$cor_detail4 <- renderUI({
  HTML(paste0("<p>Next we calculate the deviations of each observation from the mean values.
                  For example, for the first observation, we get the following results:</p>
              
              
              <p><strong>Deviation from the mean of X: ",isolate(vals$data[1,c("X")])," - ",isolate(round(mean(vals$data$X),digits = 1)), " = ",isolate(round(vals$data[1,3] - round(mean(vals$data$X),digits = 1),digits=1)),"</strong></p>
              <p><strong>Deviation from the mean of X: ",isolate(vals$data[1,c("Y")])," - ",isolate(round(mean(vals$data$Y),digits = 1)), " = ",isolate(round(vals$data[1,4] - round(mean(vals$data$Y),digits = 1),digits=1)),
              "</strong></p>
              
              <p>The following table displays the deviations for each of the observations in our
              dataset.</p>"))
  
})

vals$cordata <- vals$data

  vals$cordata$meanX <- isolate(round(mean(vals$cordata$X),digits = 1))
  vals$cordata$meanY <- isolate(round(mean(vals$cordata$Y),digits = 1)) 
  
  vals$cordata$deviationX <- isolate(vals$cordata[,c("X")] - round(vals$cordata[,c("meanX")],1))
  vals$cordata$deviationY <- isolate(vals$cordata[,c("Y")] - round(vals$cordata[,c("meanY")],1))
    
output$cor_tab1 <- renderTable(vals$cordata[,c("X","Y","meanX","meanY","deviationX","deviationY")],
                             digits = 1,
                             rownames = T)
  
output$cor_detail5 <- renderUI({
  HTML(paste0("<p>In the next step, we multiply the two deviation scores for each of the observations in our data.</p>
              
              <p>We start again with the first observation:</p>
              
              <p><strong>Deviation from X times Deviation from Y: ",isolate(round(vals$cordata[1,c("deviationX")],digits=1))," x ",isolate(round(vals$cordata[1,c("deviationY")],digits=1))," = ",round(round(vals$cordata[1,c("deviationX")],digits=1)*round(vals$cordata[1,c("deviationY")],digits=1),digits=1),"</strong></p>
              
              <p>And, again, we show this for all observation in a table:</p>"))
})

vals$cordata$DevX_times_DevY <- round(vals$cordata[c("deviationX")],digits=1)*round(vals$cordata[c("deviationY")],digits=1)


output$cor_tab2 <- renderTable(vals$cordata[,c("X","Y","meanX","meanY","deviationX","deviationY","DevX_times_DevY")],
                               digits = 1,
                               rownames = T)

output$cor_detail6 <- renderUI({
  HTML(paste0("<strong>Almost done!</strong> Now that we have the multiplied deviation scores, we simply
              sum them up over all observations and divide by the number of observations (10).</p>
              
              <p><strong>The sum of all the scores (the rightmost column in the table above) is: ",round(sum(vals$cordata[,c("DevX_times_DevY")]),digits=1),"</strong></p>
              
              <p><strong>And this divided by the number of observations - the covariance - is: ",round(round(sum(vals$cordata[,c("DevX_times_DevY")]),digits=1)/10,digits=1),"</strong></p>"))
})

output$cor_detail7 <- renderUI({
  withMathJax(helpText("Now we have the covariance - but we actually want the correlation! To 
                       calculate the correlation coefficient (r), we take the covariance and divide
                       it by the square root of the product of the variances of the two variables 
                       (we will not go in detail over how the variance is calculated):
                       $$r = \\frac{cov_{X,Y}}{\\sqrt{var_X \\times var_Y}}$$"))
})

output$cor_detail8 <- renderUI({
  HTML(paste0("<p>In our example, <strong>the variance of X is: ",round(var(isolate(vals$cordata[,c("X")])),digits = 1),", and the variance of Y is: ",round(var(isolate(vals$cordata[,c("Y")])),digits = 1),"</strong></p>
              
              <p>If we now plug these values into the equation above, we get:</p>"))
})

output$cor_detail9 <- renderUI({
  HTML(paste0("<p> r = <math><mfrac><mn>",round(round(sum(vals$cordata[,c("DevX_times_DevY")]),digits=1)/10,digits=1),"</mn><msqrt><mn>",round(var(isolate(vals$cordata[,c("X")])),digits = 1),"</mn><mo>*</mo><mn>",round(var(isolate(vals$cordata[,c("X")])),digits = 1),"</mn></msqrt></mfrac></math> =",round(round(round(sum(vals$cordata[,c("DevX_times_DevY")]),digits=1)/10,digits=1)/(sqrt(round(var(isolate(vals$cordata[,c("X")])),digits = 1)*round(var(isolate(vals$cordata[,c("Y")])),digits = 1))),digits = 2),"</p>"))
})

})
  
}

shinyApp(ui = ui, server = server)