
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
      menuItem("Mathematical notation", tabName = "math"),
      menuItem("Measures of central tendency",tabName = "cent"),
      menuItem("Measures of spread",tabName = "spread"),
      menuItem("The Central Limit Theorem", tabName = "clt"),
      menuItem("Statistical distributions", tabName = "dist", selected = T),
      menuItem("Chi-squared test",tabName = "chi"),
      menuItem("Difference of means test",tabName = "ttest"),
      menuItem("Correlation",tabName = "corr"),
      menuItem("Linear regression",tabName = "ols")
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    shinyDashboardThemes(theme="grey_light"),
    tabItems(
      tabItem(tabName = "start"),
      tabItem(tabName = "dist",
              fluidRow(
                column(width = 4,
                       box(width = NULL, title = "Statistical distributions",
                           collapsible = F,solidHeader = F,
                           HTML("<p>When you do statistical tests, you always work with different
                                statistical distributions: the normal distribution, the <i>t</i>-distribution,
                                or the &chi;&sup2;-distribution.</p>
                                
                                <p>Here you can visualize these three distributions (for different degrees of
                                freedom, where applicable) as well as the location of critical values for
                                your chosen level of significance.</p>
                                
                                <p>If you like, you can also enter a test value (from a t- or chi-squared test)
                                into the box below. This indicates where your test result is relative to the 
                                distribution - which should you help you make sense of your test result.</p>")),
                       box(width=NULL,title = "Controls",collapsible = F,solidHeader = F,
                           selectInput(inputId = "dist_distselect",
                                       label = "Select a distribution",
                                       choices = c("Normal","t","Chi-squared")),
                           selectInput(inputId = "dist_signselect",
                                       label = "Select a level of significance",
                                       choices = c(0.1,0.05,0.025,0.01,0.005),
                                       selected = 0.05),
                           selectInput(inputId = "dist_hypselect",
                                       label = "Select type of hypothesis",
                                       choices = c("Two-sided","Larger than","Smaller than")),
                           numericInput(inputId = "dist_dfselect",
                                        label = "Enter your degrees of freedom",
                                        value = 3,
                                        min = 1,
                                        step = 1),
                           numericInput(inputId = "dist_valselect",
                                        label = "Enter your test statistic (optional)",
                                        value = NULL)
                           )),
                column(width = 8,
                       box(width = NULL,title = "", collapsible = F,solidHeader = T,
                           plotOutput("distplot")
                           ))
              )
              
              ),
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
                    collapsed = T,solidHeader = F,
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
                    uiOutput("cor_detail9"),
                    uiOutput("cor_detail10")
                    )
                
              )
    )
  )
))

server <- function(input,output,session){
  
  vals <- reactiveValues()

  
# Statistical distributions
  output$distplot <- renderPlot({
  
  if(input$dist_distselect=="Normal"){
    disable(id = "dist_dfselect")
    enable(id = "dist_hypselect")
    if(input$dist_hypselect=="Two-sided"){
    ggplot(NULL, aes(c(-4,4))) + 
      geom_area(stat = "function", fun = dnorm, fill = "#d3d3d3",
                xlim = c(-4, qnorm(as.numeric(input$dist_signselect)/2)), color = "black") +
      geom_area(stat = "function", fun = dnorm, fill = "grey30", 
                xlim = c(qnorm(as.numeric(input$dist_signselect)/2),qnorm(1-(as.numeric(input$dist_signselect)/2))), color = "black") +
      geom_area(stat = "function", fun = dnorm, fill = "#d3d3d3",
                xlim = c(qnorm(1-(as.numeric(input$dist_signselect)/2)),4), color = "black") +
      annotate("segment", x = qnorm(as.numeric(input$dist_signselect)/2), xend = qnorm(1-as.numeric(input$dist_signselect)/2), 
               y = dnorm(qnorm(as.numeric(input$dist_signselect)/2)), yend = dnorm(-qnorm(as.numeric(input$dist_signselect)/2)), arrow = arrow(ends='both'),
               size = 1.5, color = "#d3d3d3") +
      annotate("text", x=0, y=dnorm(qnorm(as.numeric(input$dist_signselect)/2))+.015,label = paste0(100*(1-as.numeric(input$dist_signselect)),"% of data"), color="#d3d3d3", fontface = "bold") +
      geom_vline(xintercept = qnorm(as.numeric(input$dist_signselect)/2), color = "#d3d3d3", linetype = "dashed",
                 size=1.5) +
      geom_vline(xintercept = qnorm(1-as.numeric(input$dist_signselect)/2), color = "#d3d3d3", linetype = "dashed",
                 size=1.5) +
      geom_vline(xintercept = as.numeric(input$dist_valselect), color = "red", linetype = "dashed",
                 size=1.5) +
      labs(x = "", y = "Density",
           title = paste0("Normal distribution critical values for a ",as.numeric(input$dist_signselect)," significance level (two-sided): ",
                          round(qnorm(as.numeric(input$dist_signselect)/2), digits = 3)," & ",
                          round(qnorm(1-as.numeric(input$dist_signselect)/2), digits = 3))) +
      theme_bw() +
      theme(axis.text = element_text(size=12))
    }
    else if(input$dist_hypselect=="Larger than"){
      ggplot(NULL, aes(c(-4,4))) + 
        geom_area(stat = "function", fun = dnorm, fill = "grey30", 
                  xlim = c(-4,qnorm(1-(as.numeric(input$dist_signselect)))), color = "black") +
        geom_area(stat = "function", fun = dnorm, fill = "#d3d3d3",
                  xlim = c(qnorm(1-(as.numeric(input$dist_signselect))),4), color = "black") +
        geom_vline(xintercept = qnorm(1-as.numeric(input$dist_signselect)), color = "#d3d3d3", linetype = "dashed",
                   size=1.5) +
        geom_vline(xintercept = as.numeric(input$dist_valselect), color = "red", linetype = "dashed",
                   size=1.5) +
        annotate("segment", x = -3.99, xend = qnorm(1-as.numeric(input$dist_signselect)), 
                 y = dnorm(qnorm(as.numeric(input$dist_signselect)))/2, yend = dnorm(-qnorm(as.numeric(input$dist_signselect)))/2, arrow = arrow(ends='both'),
                 size = 1.5, color = "#d3d3d3") +
        annotate("text", x=0, hjust = 1, 
                 y=dnorm(qnorm(as.numeric(input$dist_signselect)/2))+.015,label = paste0(100*(1-as.numeric(input$dist_signselect)),"% of data"), color="#d3d3d3", fontface = "bold") +
        labs(x = "", y = "Density",
             title = paste0("Normal distribution critical value for a ",as.numeric(input$dist_signselect)," significance level (larger than): ",
                            round(qnorm(1-as.numeric(input$dist_signselect)), digits = 3))) +
        theme_bw() +
        theme(axis.text = element_text(size=12))
    }
    else if(input$dist_hypselect=="Smaller than"){
      ggplot(NULL, aes(c(-4,4))) + 
        geom_area(stat = "function", fun = dnorm, fill = "#d3d3d3", 
                  xlim = c(-4,qnorm((as.numeric(input$dist_signselect)))), color = "black") +
        geom_area(stat = "function", fun = dnorm, fill = "grey30",
                  xlim = c(qnorm((as.numeric(input$dist_signselect))),4), color = "black") +
        geom_vline(xintercept = qnorm(as.numeric(input$dist_signselect)), color = "#d3d3d3", linetype = "dashed",
                   size=1.5) +
        geom_vline(xintercept = as.numeric(input$dist_valselect), color = "red", linetype = "dashed",
                   size=1.5) +
        annotate("segment", x = 3.99, xend = qnorm(as.numeric(input$dist_signselect)),
                 y = dnorm(qnorm(as.numeric(input$dist_signselect)))/2, yend = dnorm(-qnorm(as.numeric(input$dist_signselect)))/2, arrow = arrow(ends='both'),
                 size = 1.5, color = "#d3d3d3") +
        annotate("text", x=0, hjust = 0, 
                 y=dnorm(qnorm(as.numeric(input$dist_signselect)/2))+.015,label = paste0(100*(1-as.numeric(input$dist_signselect)),"% of data"), color="#d3d3d3", fontface = "bold") +
        labs(x = "", y = "Density",
             title = paste0("Normal distribution critical value for a ",as.numeric(input$dist_signselect)," significance level (smaller than): ",
                            round(qnorm(as.numeric(input$dist_signselect)), digits = 3))) +
        theme_bw() +
        theme(axis.text = element_text(size=12))
    }
      
  }else if(input$dist_distselect=="t"){
    enable(id = "dist_dfselect")
    enable(id = "dist_hypselect")
    if(input$dist_hypselect=="Two-sided"){
      
      ggplot(NULL, aes(c(-4,4))) + 
        geom_area(stat = "function", fun = dt, args = list(df=as.numeric(input$dist_dfselect)), fill = "#d3d3d3",
                  xlim = c((qt(as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)))-5,
                           qt(as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect))), color = "black") +
        geom_area(stat = "function", fun = dt, args = list(df=as.numeric(input$dist_dfselect)), fill = "grey30",
                  xlim = c(qt(as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)),
                           qt(1-as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect))), color = "black") +
        geom_area(stat = "function", fun = dt, args = list(df=as.numeric(input$dist_dfselect)), fill = "#d3d3d3",
                  xlim = c(qt(1-as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)),
                           qt(1-as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect))+5), color = "black") +
        annotate("segment", x = qt(as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)), 
                 xend = qt(1-as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)),
                 y = dt(qt(as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)), df=as.numeric(input$dist_dfselect)),
                 yend = dt(-qt(as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)), df=as.numeric(input$dist_dfselect)), arrow = arrow(ends='both'),
                 size = 1.5, color = "#d3d3d3") +
        annotate("text", x=0, y=dt(qt(as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)), df=as.numeric(input$dist_dfselect))+0.015,
                 label = paste0(100*(1-as.numeric(input$dist_signselect)),"% of data"), color="#d3d3d3", fontface = "bold") +
        geom_vline(xintercept = qt(as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)),
                   color = "#d3d3d3", linetype = "dashed",
                   size=1.5) +
        geom_vline(xintercept = qt(1-as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)), 
                   color = "#d3d3d3", linetype = "dashed",
                   size=1.5) +
        geom_vline(xintercept = as.numeric(input$dist_valselect), color = "red", linetype = "dashed",
                   size=1.5) +
        labs(x = "", y = "Density",
             title = paste0("t-distribution critical values for a ",as.numeric(input$dist_signselect)," significance level (two-sided; df = ",as.numeric(input$dist_dfselect),"): ",
                            round(qt(as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)), digits = 3)," & ",
                            round(qt(1-as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)), digits = 3))) +
        theme_bw() +
        theme(axis.text = element_text(size=12))
    }
    else if(input$dist_hypselect=="Larger than"){
      ggplot(NULL, aes(c(-4,4))) + 
        geom_area(stat = "function", fun = dt, args = list(df=as.numeric(input$dist_dfselect)), fill = "grey30",
                  xlim = c((qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)))-5,
                           qt(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect))), color = "black") +
        geom_area(stat = "function", fun = dt, args = list(df=as.numeric(input$dist_dfselect)), fill = "#d3d3d3",
                  xlim = c(qt(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)),
                           qt(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect))+5), color = "black") +
        annotate("segment", x = (qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)))-4.99,
                 xend = qt(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)),
                 y = dt(qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), df=as.numeric(input$dist_dfselect)),
                 yend = dt(-qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), df=as.numeric(input$dist_dfselect)), arrow = arrow(ends='both'),
                 size = 1.5, color = "#d3d3d3") +
        annotate("text", x=0, hjust=1,
                 y=dt(qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), df=as.numeric(input$dist_dfselect))+0.015,
                 label = paste0(100*(1-as.numeric(input$dist_signselect)),"% of data"), color="#d3d3d3", fontface = "bold") +
        geom_vline(xintercept = qt(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), 
                   color = "#d3d3d3", linetype = "dashed",
                   size=1.5) +
        geom_vline(xintercept = as.numeric(input$dist_valselect), color = "red", linetype = "dashed",
                   size=1.5) +
        labs(x = "", y = "Density",
             title = paste0("t-distribution critical value for a ",as.numeric(input$dist_signselect)," significance level (larger than; df = ",as.numeric(input$dist_dfselect),"): ",
                            round(qt(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), digits = 3))) +
        theme_bw() +
        theme(axis.text = element_text(size=12))
    }
    else if(input$dist_hypselect=="Smaller than"){
      ggplot(NULL, aes(c(-4,4))) + 
        geom_area(stat = "function", fun = dt, args = list(df=as.numeric(input$dist_dfselect)), fill = "#d3d3d3",
                  xlim = c((qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)))-5,
                           qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect))), color = "black") +
        geom_area(stat = "function", fun = dt, args = list(df=as.numeric(input$dist_dfselect)), fill = "grey30",
                  xlim = c(qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)),
                           qt(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect))+5), color = "black") +
        annotate("segment", x = (qt(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)))+4.99,
                 xend = qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)),
                 y = dt(qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), df=as.numeric(input$dist_dfselect)),
                 yend = dt(-qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), df=as.numeric(input$dist_dfselect)), arrow = arrow(ends='both'),
                 size = 1.5, color = "#d3d3d3") +
        annotate("text", x=0, hjust=0,
                 y=dt(qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), df=as.numeric(input$dist_dfselect))+0.015,
                 label = paste0(100*(1-as.numeric(input$dist_signselect)),"% of data"), color="#d3d3d3", fontface = "bold") +
        geom_vline(xintercept = qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), 
                   color = "#d3d3d3", linetype = "dashed",
                   size=1.5) +
        geom_vline(xintercept = as.numeric(input$dist_valselect), color = "red", linetype = "dashed",
                   size=1.5) +
        labs(x = "", y = "Density",
             title = paste0("t-distribution critical value for a ",as.numeric(input$dist_signselect)," significance level (smaller than; df = ",as.numeric(input$dist_dfselect),"): ",
                            round(qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), digits = 3))) +
        theme_bw() +
        theme(axis.text = element_text(size=12))
    }
    
  }else if(input$dist_distselect=="Chi-squared"){
    enable(id = "dist_dfselect")
    disable(id = "dist_hypselect")
    
    ggplot(NULL, aes(c(0,5))) + 
      geom_area(stat = "function", fun = dchisq, fill = "grey30", color = "black",
                xlim = c(0, qchisq(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect))), 
                args = list(df=as.numeric(input$dist_dfselect))) +
      geom_area(stat = "function", fun = dchisq, fill = "#d3d3d3", color = "black",
                xlim = c(qchisq(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), 
                                                                            qchisq(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect))+.5*qchisq(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect))), 
                args = list(df=as.numeric(input$dist_dfselect))) +
      geom_vline(xintercept = qchisq(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), color = "#d3d3d3", linetype = "dashed", size = 1.25) +
      geom_vline(xintercept = as.numeric(input$dist_valselect), color = "red", linetype = "dashed",
                 size=1.5) +
      annotate("segment", arrow = arrow(ends = "both"), size = 1.5, color = "#d3d3d3",
               x = 0, xend = qchisq(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)),
               y = dchisq(qchisq(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), df = as.numeric(input$dist_dfselect)),
               yend = dchisq(qchisq(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), df = as.numeric(input$dist_dfselect))) +
      # annotate("text", color = "#d3d3d3", fontface = "bold",
      #          y = dchisq(qchisq(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), df = as.numeric(input$dist_dfselect)) + dchisq(qchisq(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), df = as.numeric(input$dist_dfselect)),
      #          x = qchisq(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect))/2,
      #          label = paste0(100*(1-as.numeric(input$dist_signselect))," % of data")) +
      labs(y = "Density",
           caption = paste0("Gray arrow indicates ",100*(1-as.numeric(input$dist_signselect))," % of data"),
           title = paste0("Critical value = ",round(qchisq(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)),digits = 3),
                          " for df=",as.numeric(input$dist_dfselect)," and a ",as.numeric(input$dist_signselect)," level of confidence")) +
      xlab(~ paste(chi ^ 2, "-value")) +
      theme_bw() +
      theme(axis.text = element_text(size=14))
  }
  })
  
  
  
# Correlation coefficient
observeEvent(input$sim, {
  
  rho <- runif(n=1,
               min=-1,
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
  res <- isolate(round(cor(vals$data$X,vals$data$Y,
                           method = "pearson"),digits=2))
  
  t <- round((res*sqrt(10-2))/(sqrt(1-res^2)),digits=3)
  
  p_val <- format(round(2 * pt(abs(t), 8, lower.tail = F),digits = 3), nsmall = 3)
  
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
       
       <p><strong>Mean of X: ",isolate(round(mean(vals$data$X),digits = 3)),"</strong></p>
      
       <p><strong>Mean of Y: ",isolate(round(mean(vals$data$Y),digits = 3)),"</strong></p>"))  
})

output$cor_detail4 <- renderUI({
  HTML(paste0("<p>Next we calculate the deviations of each observation from the mean values.
                  For example, for the first observation, we get the following results:</p>
              
              
              <p><strong>Deviation from the mean of X: ",isolate(vals$data[1,c("X")])," - ",isolate(round(mean(vals$data$X),digits = 3)), " = ",isolate(round(vals$data[1,3] - round(mean(vals$data$X),digits = 3),digits=3)),"</strong></p>
              <p><strong>Deviation from the mean of X: ",isolate(vals$data[1,c("Y")])," - ",isolate(round(mean(vals$data$Y),digits = 3)), " = ",isolate(round(vals$data[1,4] - round(mean(vals$data$Y),digits = 3),digits=3)),
              "</strong></p>
              
              <p>The following table displays the deviations for each of the observations in our
              dataset.</p>"))
  
})

vals$cordata <- vals$data

  vals$cordata$meanX <- isolate(round(mean(vals$cordata$X),digits = 3))
  vals$cordata$meanY <- isolate(round(mean(vals$cordata$Y),digits = 3)) 
  
  vals$cordata$deviationX <- isolate(vals$cordata[,c("X")] - round(vals$cordata[,c("meanX")],3))
  vals$cordata$deviationY <- isolate(vals$cordata[,c("Y")] - round(vals$cordata[,c("meanY")],3))
    
output$cor_tab1 <- renderTable(vals$cordata[,c("X","Y","meanX","meanY","deviationX","deviationY")],
                             digits = 3,
                             rownames = T)
  
output$cor_detail5 <- renderUI({
  HTML(paste0("<p>In the next step, we multiply the two deviation scores for each of the observations in our data.</p>
              
              <p>We start again with the first observation:</p>
              
              <p><strong>Deviation from X times Deviation from Y: ",isolate(round(vals$cordata[1,c("deviationX")],digits=3))," x ",isolate(round(vals$cordata[1,c("deviationY")],digits=3))," = ",round(round(vals$cordata[1,c("deviationX")],digits=3)*round(vals$cordata[1,c("deviationY")],digits=3),digits=3),"</strong></p>
              
              <p>And, again, we show this for all observation in a table:</p>"))
})

vals$cordata$DevX_times_DevY <- round(vals$cordata[c("deviationX")],digits=3)*round(vals$cordata[c("deviationY")],digits=3)


output$cor_tab2 <- renderTable(vals$cordata[,c("X","Y","meanX","meanY","deviationX","deviationY","DevX_times_DevY")],
                               digits = 3,
                               rownames = T)

output$cor_detail6 <- renderUI({
  HTML(paste0("<strong>Almost done!</strong> Now that we have the multiplied deviation scores, we simply
              sum them up over all observations and then divide by the number of observations minus 1. <strong>Important:</strong> In the calculation here,
              we divide by n-1 rather than n; this is because that is the way R computes it, and also because we are dealing here with a quite small sample of 10 observations.
              In a larger dataset (several hundreds or thousands of observations, this should not make a difference but here it does).</p>
              
              <p><strong>The sum of all the scores (the rightmost column in the table above) is: ",round(sum(vals$cordata[,c("DevX_times_DevY")]),digits=3),"</strong></p>
              
              <p><strong>And this divided by the number of observations minus 1 - the covariance - is: ",round(round(sum(vals$cordata[,c("DevX_times_DevY")]),digits=3)/9,digits=3),"</strong></p>"))
})

output$cor_detail7 <- renderUI({
  withMathJax(helpText("Now we have the covariance - but we actually want the correlation! To 
                       calculate the correlation coefficient (r), we take the covariance and divide
                       it by the square root of the product of the variances of the two variables 
                       (we will not go in detail over how the variance is calculated):
                       $$r = \\frac{cov_{X,Y}}{\\sqrt{var_X \\times var_Y}}$$"))
})

output$cor_detail8 <- renderUI({
  HTML(paste0("<p>In our example, <strong>the variance of X is: ",round(var(isolate(vals$cordata[,c("X")])),digits = 3),", and the variance of Y is: ",round(var(isolate(vals$cordata[,c("Y")])),digits = 3),"</strong></p>
              
              <p>If we now plug these values into the equation above, we get:</p>"))
})

output$cor_detail9 <- renderUI({
  HTML(paste0("<p><strong> r = <math><mfrac><mn>",round(round(sum(vals$cordata[,c("DevX_times_DevY")]),digits=3)/9,digits=3),"</mn><msqrt><mn>",round(var(isolate(vals$cordata[,c("X")])),digits = 3),"</mn><mo>*</mo><mn>",round(var(isolate(vals$cordata[,c("Y")])),digits = 3),"</mn></msqrt></mfrac></math> =",round(round(round(sum(vals$cordata[,c("DevX_times_DevY")]),digits=3)/9,digits=3)/(sqrt(round(var(isolate(vals$cordata[,c("X")])),digits = 3)*round(var(isolate(vals$cordata[,c("Y")])),digits = 3))),digits = 2),"</strong></p>"))
})

output$cor_detail10 <- renderUI({
  HTML(paste0("<p>Now that we have the correlation coefficient, we also want to know: Did we get this result simply by random chance?
       (Technically, the answer is of course yes - the computer generated some random numbers for us. But we will just pretend for now that we have
       some real data with actual meaning in front of us.)</p>
       
       To perform a formal test, we calculate the <strong>t-statistic</strong> for our correlation coefficient. The formula
       for this calculation looks as follows:</p>
       
       <p> <math> <msub><mi>t</mi><mi>r</mi></msub> <mo>=</mo>  <mfrac><mrow> <mi>r</mi><msqrt><mi>n</mi><mo>-</mo><mn>2</mn></msqrt> </mrow> <mrow> <msqrt><mn>1</mn><mo>-</mo><msup><mi>r</mi><mn>2</mn></sup></msqrt></mrow></mfrac></math></p>
              
       <p>With our values plugged into the formula, we get:</p>
              
       <p> <math> <msub><mi>t</mi><mi>r</mi></msub> <mo>=</mo>  <mfrac><mrow><mn>",isolate(round(cor(vals$cordata$X,vals$cordata$Y,
                                                                                                 method = "pearson"),digits=2)),"</mn><msqrt><mn>10</mn><mo>-</mo><mn>2</mn></msqrt> </mrow> <mrow> <msqrt><mn>1</mn><mo>-</mo><msup><mn>",isolate(round(cor(vals$cordata$X,vals$cordata$Y,
                                                                                                                                                                                                                                                            method = "pearson"),digits=2)),"</mn><mn>2</mn></sup></msqrt></mrow></mfrac> <mo>=</mo><mn>",
              round(isolate(round(cor(vals$cordata$X,vals$cordata$Y,
                                method = "pearson"),digits=2))*sqrt(10-2)/(sqrt(1-(isolate(round(cor(vals$cordata$X,vals$cordata$Y,
                                                                                                      method = "pearson"),digits=2)))^2)),digits = 3),"</mn></math></p>
              
        <p>R tells us that the corresponding <i>p</i>-value is ",p_val,". Can you confirm that this
              value makes sense using the table of critical t-values in Appendix B of Kellstedt and Whitten (2013)? (Careful: Their table
              shows only some selected t- and their corresponding p-values! T-values that are relatively low - i.e. below what is required for a p-value of 0.1 or lower - are not shown.)</p>"))
})

})
  
# 


}

shinyApp(ui = ui, server = server)