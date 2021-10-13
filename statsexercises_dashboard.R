
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
  library(tidyverse)
  library(xtable)


ui <- dashboardPage(
  dashboardHeader(title="Statistics Exercises"),
  dashboardSidebar(collapsed = T,
    sidebarMenu(
      menuItem("Start",tabName = "start"),
      menuItem("Mathematical notation", tabName = "math"),
      menuItem("Measures of central tendency",tabName = "cent"),
      menuItem("Measures of spread",tabName = "spread", selected = T),
      menuItem("Statistical distributions", tabName = "dist"),
      menuItem("The Central Limit Theorem", tabName = "clt"),
      menuItem("Confidence intervals", tabName = "ci"),
      menuItem("Chi-squared test",tabName = "chi"),
      menuItem("Difference of means test",tabName = "ttest"),
      menuItem("Correlation",tabName = "corr")
      #menuItem("Linear regression",tabName = "ols")
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    shinyDashboardThemes(theme="grey_light"),
    tabItems(
      tabItem(tabName = "start"),
      ###############
      
      ###############
      
      tabItem(tabName = "cent",
      ##############
              fluidRow(
                column(width = 4,
                       box(width = NULL, title = "Measures of central tendency",
                           collapsible = T, collapsed = F, solidHeader = F,
                           HTML("<p>Measures of central tendency are statistics used 
                                to describe where most of the values of a variable 
                                are located.</p>
                                <p>The <strong>mean</strong> or 'average' is probably the
                                most familiar, but there are also the <strong>median</strong>
                                and the <strong>mode</strong>.</p>
                                <p>These statistics are used in many more advanced procedures,
                                so a thorough understanding of them is essential. Fortunately,
                                they are also easy to understand.</p>
                                <p>This module allows you to calculate the mean and median
                                of random sets of numbers. (Why not also the mode? Because it is not really difficult:
                                The mode is simply the most frequent value observed in a set of data.)")),
                       box(width = NULL, title = "Controls", collapsible = T, collapsed = F,
                           solidHeader = F,
                           actionBttn(inputId = "cent_sim",
                                      label = "Give me some data!",
                                      style="material-flat",
                                      color="success",
                                      size = "xs"),
                           br(),br(),
                           disabled(actionBttn(inputId = "cent_solution",
                                               label = "Show me the solution!",
                                               style = "material-flat",
                                               color = "warning",
                                               size = "xs")))),
                column(width = 8,
                       box(width = NULL, title = "Data", collapsible = F, solidHeader = F,
                           HTML("<p>If you click on the green button on the left, you 
                           will get a set of numbers. Can you calculate the mean and 
                                median of this set of numbers?</p>"),
                           br(),
                           textOutput("centvals")),
                       box(width = NULL, title = "Solution", collapsible = F, solidHeader = F,
                           uiOutput("cent_sol")),
                       box(width = NULL, title = "Detailed solution", collapsible = T, solidHeader = F,
                           collapsed = T,
                           uiOutput("cent_sol_det")))
              )
      
              ),
      ##############
      
      tabItem(tabName = "spread",
      ##############        
              fluidRow(
                column(width = 4,
                       box(width = NULL, title = "Measures of spread",
                           collapsible = T, collapsed = T, solidHeader = F,
                           HTML("<p>Measures of spread are statistics that we use
                                to see how spread out or 'dispersed' our data are.</p>
                                <p>The two most important ones of these are the <strong>variance</strong>
                                and the <strong>standard deviation</strong>. These two statistics are not only
                                important when we want to describe our data, they are also
                                key ingredients in many of the more advanced procedures 
                                (e.g., the covariance and correlation, or confidence intervals).
                                It is therefore very important that you get a solid understanding 
                                of what the variance and standard deviation are and how they are
                                calculated.</p>
                                <p>This module allows you to practice this. As in the other modules,
                                you create a set of numbers with the green button and can call up the 
                                correct result with the orange button. A detailed solution is also 
                                available if you want.</p>")),
                       box(width = NULL, title = "Controls", collapsible = T, collapsed = F,
                           solidHeader = F,
                           actionBttn(inputId = "spread_sim",
                                      label = "Give me some data!",
                                      style="material-flat",
                                      color="success",
                                      size = "xs"),
                           br(),br(),
                           disabled(actionBttn(inputId = "spread_solution",
                                               label = "Show me the solution!",
                                               style = "material-flat",
                                               color = "warning",
                                               size = "xs")))),
                column(width = 8,
                       box(width = NULL, title = "Data", collapsible = F, solidHeader = F,
                           HTML("<p>If you click on the green button on the left, you 
                           will get a set of numbers. Can you calculate the variance and 
                                standard deviation of this set of numbers?</p>"),
                           br(),
                           textOutput("spreadvals")),
                       box(width = NULL, title = "Solution", collapsible = F,
                           solidHeader = F,
                           uiOutput("spread_sol")),
                       box(width = NULL, title = "Detailed solution", collapsible = T,
                           collapsed = F, solidHeader = F,
                           uiOutput("spread_sol_det1"),
                           tableOutput("spread_sol_det2"),
                           uiOutput("spread_sol_det3")))
              )
              ),
      #############
      
      tabItem(tabName = "clt",
      ###############      
              fluidRow(
                column(width = 4,
                       box(width = NULL, title = "The Central Limit Theorem",
                           collapsible = T,solidHeader = F, collapsed = F,
                           HTML("<p>The Central Limit Theorem is a central concept
                           in most areas of applied statistics. Understanding it is
                           therefore obviously important &mdash; but can also be
                                challenging.</p>
                                <p>This module allows you to approach the Central
                                Limit Theorem via a simulation (of the ideological
                                left-right self-placement of a fictional population of 125 individuals).</p>
                                <p>Specifically, you can simulate drawing samples
                                from a hypothetical population and calculating a 
                                sample mean. You can adjust the number of samples
                                that are drawn simultaneously and the size of each
                                sample to see how the result (the sampling distribution)
                                changes.</p>")),
                       box(width = NULL, title = "Controls",
                           collapsible = T, solidHeader = F, collapsed = T,
                           sliderInput("clt_size",
                                       "Size of each sample:",
                                       min = 5,
                                       max = 125,
                                       value = 18,
                                       ticks = F),
                           sliderTextInput(
                             inputId = "clt_samples",
                             label = "Number of samples:", 
                             choices = c(1, 10, 100, 1000, 10000, 100000),
                             grid = T),
                           actionButton("button_clt",
                                        "Simulate")
                           )),
                column(width = 8,
                       box(width = NULL, title = "", collapsible = F, solidHeader = F,
                           plotOutput("clt_popplot",
                                      height = "200px"),
                           plotOutput("clt_distPlot")
                           ))
              )),
      ###############
      
      tabItem(tabName = "ci",
      ###############
              fluidRow(
                column(width = 4,
                       box(width = NULL,title = "Confidence intervals", collapsible = T, 
                           collapsed = F, solidHeader = F,
                           HTML("<p>Confidence intervals are a very important tool in
                                statistical analysis. Unfortunately, they are also
                                difficult to really understand &mdash; or, to put
                                it differently, they are very easy to misunderstand
                                and misinterpret.</p>
                                <p>This module allows you to visualize a confidence
                                interval around a fixed sample mean (using simulated data
                                about people's left-right self-placement) 
                                and how the confidence interval is related to 
                                sampling distributions.</p>
                                <p>You can also change the size of the sample that
                                you work with or the level of confidence to see how
                                this changes the size of the confidence interval.</p>")),
                       box(width = NULL, title = "Controls", collapsible = T,
                           collapsed = T,
                           sliderInput("ci_size",
                                       "Size of each sample",
                                       min = 5,
                                       max = 125,
                                       value = 18,
                                       ticks = F),
                           sliderInput("ci_diff",
                                       "Move location of true population mean",
                                       min = -25,
                                       max = 25,
                                       value = 0,
                                       step = 1,
                                       ticks = F),
                           radioButtons("ci_level",
                                        label = "Level",
                                        choices = c("90%" = 1.645,
                                                    "95%" = 1.960,
                                                    "99%" = 2.576),
                                        selected = 1.960,
                                        inline = T),
                           radioButtons(inputId = "show_ci",
                                        label = "Show confidence interval",
                                        choices = c("No" = F,
                                                    "Yes" = T),
                                        selected = F,
                                        inline = T))),
                column(width = 8,
                       box(width = NULL, title = "Making sense of what you see", collapsible = T, solidHeader = T,
                           collapsed = T,
                           HTML("<p>If you move the slider on the left ('Move location of population mean')
                                to the left and right,
                                you can simulate different scenarios for where the 'true'
                                population mean is located. As you move the slider, 
                                ask yourself: If the 'true' population mean were located
                                at this point, would it be likely or unlikely that we
                                measured our given sample mean (given the chosen confidence level)?</p>
                                <p>If you then let the graph show the confidence interval, 
                                you should notice that this interval corresponds to
                                those possible true population means where you said they are
                                plausible given our sample mean and the sampling distribution. In other words,
                                you should notice that the confidence interval includes the 
                                <strong>range of potential 'true' population values
                                that are plausible, given the sample size and level of confidence.</strong></p>")),
                       plotOutput("ci_plot"))
              )
              ),
      
      ###############
      
      tabItem(tabName = "dist",
      ###############
              fluidRow(
                column(width = 4,
                       box(width = NULL, title = "Statistical distributions",
                           collapsible = T,solidHeader = F, collapsed = F,
                           HTML("<p>When you do statistical tests, you always work with different
                                statistical distributions: the normal distribution, the <i>t</i>-distribution,
                                or the &chi;&sup2;-distribution.</p>
                                
                                <p>Here you can visualize these three distributions (for different degrees of
                                freedom, where applicable) as well as the location of critical values for
                                your chosen level of significance.</p>
                                
                                <p>If you like, you can also enter a test value (from a t- or chi-squared test)
                                into the box below. This indicates where your test result is relative to the 
                                distribution - which should you help you make sense of your test result.</p>")),
                       box(width=NULL,title = "Controls",collapsible = T,solidHeader = F, collapsed = T,
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
      ###############
      
      tabItem(tabName = "ttest",
      ###############
              fluidRow(
                column(width = 4,
                       box(width = NULL, title = "Difference-of-means t-test",
                           collapsible = T, collapsed = F, solidHeader = F,
                           HTML("<p>We use the difference-of-means test when we want to
                                see if two groups are significantly different in some 
                                numeric attribute &mdash; for example, if men and women differ
                                significantly in how much they earn (the notorious 'gender wage gap').</p>
                                <p>When we do a difference-of-means test, we can test different hypotheses: 
                                a) the two group means are <strong>different</strong>; b) the mean of one group is 
                                <strong>larger</strong> than that of the other; and c) the mean of one
                                group is <strong>smaller</strong> than that of the other group.</p>
                                <p>You can generate a scenario by clicking on the green button below. To see the
                                result, click on the orange button. If you want a detailed step-by-step solution,
                                you can expand the box below by clicking on the '+' sign.</p>")),
                       box(width = NULL, collapsible = T, collapsed = F,
                           solidHeader = F, title = "Controls",
                           actionBttn(inputId = "tt_sim",
                                      label = "Give me some Data!",
                                      style="material-flat",
                                      color="success",
                                      size = "xs"),
                           br(),br(),
                           disabled(actionBttn(inputId = "tt_solution",
                                               label = "Show me the solution!",
                                               style = "material-flat",
                                               color = "warning",
                                               size = "xs")))),
                column(width = 8,
                       box(width = 0, title = "Data", collapsible = F, solidHeader = F,
                           tableOutput("tt_table")
                           ),
                       box(width = NULL, title = "The result in brief", collapsible = F, 
                           solidHeader = F,
                           uiOutput("tt_result_brief")))
              )
              ),
      ###############
      
      tabItem(tabName = "corr",
      ###############        
              fluidRow(
                column(width = 4,
                box(width=NULL,title = "Correlation coefficient",collapsible = T,
                    solidHeader = F, collapsed = F,
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
                box(width=NULL,title = "Controls",collapsible = T,solidHeader = F, collapsed = T,
                    actionBttn(inputId = "cor_sim",
                               label = "Give me some data!",
                               style="material-flat",
                               color="success",
                               size = "xs"),
                    br(),br(),
                    disabled(actionBttn(inputId = "cor_solution",
                                        label = "Show me the solution!",
                                        style = "material-flat",
                                        color = "warning",
                                        size = "xs"))
              )
              ),
              column(width=8,
              box(width=NULL,title = "Data",collapsible = F,solidHeader = F,
                  column(3,tableOutput(outputId = "tab")),
                  column(9,plotOutput(outputId = "plot"))
                  ),
              box(width = NULL,title = "Result",collapsible = F,solidHeader = F,
                  textOutput(outputId = "result")
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
      ###############
  )
))

server <- function(input,output,session){
  
  vals <- reactiveValues()
  
  
# Central tendency - Data
observeEvent(input$cent_sim,{
  enable("cent_solution")
output$centvals <- renderText({
  
  set.seed(NULL)
  vals$cent <- sample(seq(1,50,1),
                      size = 9)
  
  paste0("X = (",paste0(vals$cent,collapse = "; "),")")
  
})
})

# Central tendency - Solution
observeEvent(input$cent_solution,{
  
cent <- isolate(vals$cent)
  
output$cent_sol_det <- renderUI({
  HTML(paste0("<p>Calculating the mean ('average') should be easy: You calculate the sum
              of all the values in X and then divide by the overall number of values (9):</p>",
              paste0(cent, collapse = " + ")," = ",sum(cent),
              br(),br(),
              sum(cent),"/9 = ",round(sum(cent)/9, digits = 1),br(),br(),
              "<p>Identifying the median is a bit more interesting: You first have to arrange all the values
              from lowest to highest:</p>",
              br(),
              "(",paste0(sort(cent), collapse = "; "),")",
              br(),br(),
              "<p>Then you identify the value in the middle &mdash; the one that divides the data in half.
              In this case, this is: ",paste0(median(cent)),".</p>
              <p><strong>Note:</strong> We are working here with an <i>uneven</i> number of values, 9! If we would have an
              even number such as 10 or 6, we would take the two middle values and calculate the average of these
              two (see also Kellstedt & Whitten 2018, 133).</p>"))
})

output$cent_sol <- renderUI({
  HTML(paste0("<p>The mean of X is: ",round(mean(cent), digits = 1),".</p>
              <p>The median of X is: ",median(cent),".</p>"))
})
  
})


# Measures of spread - Data
observeEvent(input$spread_sim,{
  enable("spread_solution")
  
  output$spreadvals <- renderText({
    set.seed(NULL)
    vals$spread <- sample(seq(1,50,1),
                          size = 10)
    paste0("X = (",paste0(vals$spread,collapse = "; "),")")
  })
  
  
})

# Measures of spread - Solution
observeEvent(input$spread_solution,{
  spread <- isolate(vals$spread)
  
  spreadmat <- data.frame(X = spread,
                          meanX = rep(mean(spread),length(spread)))
  
  spreadmat %>% 
    mutate(diff = X - meanX,
           diff_squared = diff^2) -> spreadmat
  
  sumdiffsq <- sum(spreadmat$diff_squared)
  spread_var <- sumdiffsq/(length(spread)-1)
  
output$spread_sol <- renderUI({
  HTML(paste0("<p>The variance is: ",round(var(spread), digits = 1),".</p>
              <p>The standard deviation is: ",round(sd(spread), digits = 1),".</p>"))
  
})

output$spread_sol_det1 <- renderUI({
  withMathJax(helpText("We start by calculating the variance of X. The formula is as follows: 
                       $$s^2 = \\frac{\\sum_{i=1}^N (X_i - \\bar{X})^2}{N-1}$$
                       In human language: We calculate the mean of X, and then we calculate the 
                       difference of each value in X from this mean. Then we square each of the
                       resulting numbers. Finally, we add them all up and then divide the
                       result by N-1. The calculation is a bit tedious and easier to follow
                       when it is presented in a table:"))
})

output$spread_sol_det2 <- renderTable({
  spreadmat
}, rownames = T, include.colnames = F, width = "100%",hover = T,
add.to.row = list(pos = list(0),
                  command = " <tr> <th> </th><th>X</th><th>X&#772;</th><th>(X<sub>i</sub> - X&#772;)</th><th>(X<sub>i</sub> - X&#772;)<sup>2</sup></th> </tr>"))

output$spread_sol_det3 <- renderUI({
  HTML(paste0("<p>If we now calculate the sum of all the values in the last table column
              (the one furthest to the right), we get the sum of the squared differences
              from the mean: ",round(sumdiffsq,digits=1),".</p>
              <p>Then we divide this by 9 (the number of values in X minus 1).
              The result is the <strong>variance</strong>: ",round(spread_var, digits=1),"</p>
              <p>You should now also see that the variance is <i>the average
              squared deviation from the mean</i> in the data. Obviously, this number is difficult
              to interpret in a meaningful way &mdash; what are 'squared differences'?</p>
              <p>But we can take the square root (&radic;) of the variance to get to the
              <i>average deviation from the mean</i>: the <strong>standard deviation</strong>. 
              This statistic is much more intuitive to interpret.</p>
              <p>In our case, this is: <math><msqrt><mn>",paste0(round(spread_var, digits=1)),"</mn></msqrt></math> = ",round(sd(spread),digits=1),"</p>"))
})
  
})


  
# Central Limit Theorem - population data
set.seed(42)
vals$cltpop <- 10*sample(seq(1,10,1),
                 125,
                 replace = T,
                 prob = c(.02,.20,.29,.13,.10,.09,.09,.04,.03,0.01))  

# "True" population - plot
output$clt_popplot <- renderPlot({
  
  vals$cltdata <- data.frame(pop = vals$cltpop,
                     idno = seq(1,length(vals$cltpop),1))
  
  vals$cltdata %>% 
    group_by(pop) %>% 
    summarize(n = n()) %>% 
    ggplot(aes(x=pop,y=n)) +
    geom_bar(stat = "identity") +
    geom_vline(xintercept = mean(vals$cltpop), color = "#d95f02", size = 1.25) +
    scale_x_continuous(breaks = seq(10,100,10),
                       limits = c(5,105)) +
    labs(x = "Left-right self-placement",
         y = "Frequency",
         title = "The 'true' population (N=125) with our target: the population mean",
         caption = paste0("The orange line indicates the 'true' mean: ",round(mean(vals$cltpop), digits = 2))) +
    theme_bw() +
    theme(aspect.ratio=1/8)
})

  
# Simulation graph, CLT
observeEvent(input$button_clt,{
  
  if(input$clt_samples>=10000){
    showModal(modalDialog("Simulation is running, please wait...", footer=NULL)) 
  }
  
  # Simulate repeat sampling
  vals$means <- sapply(seq(1,input$clt_samples,1),
                  function(x){
                    sample <- sample(vals$cltpop,
                                     size = input$clt_size,
                                     replace = F)
                    return(mean(sample))
                  })
  
  isolate(sims <- data.frame(means = vals$means,
                             draws = seq(1,length(vals$means),1)))
  
  if(input$clt_samples>=10000){
    removeModal()
  }
  
  output$clt_distPlot <- renderPlot({
    p <- sims %>%
      ggplot(mapping = aes(x=means)) +
      geom_bar(stat = "count",
               width = 1) +
      geom_vline(xintercept = mean(vals$cltpop),
                 color = "#d95f02", size = 1.25) +
      ylab("Number of samples") +
      xlab("Sample mean(s)") +
      labs(title = "Our measurement(s) of the population mean: Dark gray line(s)",
           caption = paste0("The orange line indicates the 'true' mean: ",round(mean(vals$cltpop), digits = 2))) +
      scale_x_continuous(limits = c(5,105),
                         breaks = seq(10,100,10)) +
      theme_bw()
    
    if(input$clt_samples<30){
      p <- p + scale_y_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))
    }
    p
  })
})


# Simulation graph, CI
observeEvent(input$ci_size,{
  
  # Simulate repeat sampling
  vals$means <- sapply(seq(1,2500,1),
                  function(x){
                    sample <- sample(vals$cltpop,
                                     size = input$ci_size,
                                     replace = F)
                    return(mean(sample))
                  })
  
  sims <- data.frame(means = vals$means-10,
                     draws = seq(1,length(vals$means),1))
  
  sims_sd <- sd(sims$means)
  sims_mean <- mean(sims$means)
  
  
  
  isolate(sims)
  #rm(vals$means)
  
  observeEvent(input$ci_level,{
    sims$within <- ifelse(sims$means<=sims_mean + as.numeric(input$ci_level)*sims_sd & 
                            sims$means>= sims_mean - as.numeric(input$ci_level)*sims_sd,
                          "Yes","No")
    observeEvent(input$ci_diff,{
      sims$means <- sims$means+input$ci_diff    
      
      output$ci_plot <- renderPlot({
        g <- sims %>% 
          ggplot(mapping = aes(x=means,fill=within)) +
          geom_bar(stat = "count",
                   width = 1) +
          scale_fill_manual(values = c("orange","gray30"),
                            labels = c(paste0("Outer ",round(200*(1-pnorm(as.numeric(input$ci_level))),digits = 1),"%")
                                       ,paste0("Inner ",100-round(200*(1-pnorm(as.numeric(input$ci_level))),digits = 1),"%"))) +
          geom_vline(xintercept = 34.1,
                     color = "#1b9e77", size = 1.25, linetype = "dashed") +
          geom_vline(xintercept = mean(sims$means),
                     color = "gray", size = 1.25) +
          scale_x_continuous(limits = c(5,105),
                             breaks = seq(10,100,10)) +
          labs(x = "Left-right self-placement",
               y = "Number of samples",
               caption = paste0("The gray solid line indicates the POSSIBLE true population mean: ",round(mean(sims$means), digits = 2),
                                "\n The green dashed line indicates the MEASURED sample mean: 34.1")) +
          theme_bw() +
          theme(legend.title = element_blank(),
                legend.position = "bottom")
        
        if(input$show_ci=="TRUE"){
          g +geom_errorbarh(size = 1, height = 10, aes(y=30,
                                xmin = 34.1 - as.numeric(input$ci_level)*(sd(sims$means)/sqrt(length(input$ci_size))),
                                xmax = 34.1 + as.numeric(input$ci_level)*(sd(sims$means)/sqrt(length(input$ci_size)))
          ))
        }else{
          g
        }
      })        
    })
  })
})



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
  
  
# t-test - data
observeEvent(input$tt_sim,{
  enable("tt_solution")
set.seed(NULL)
n_1 <- format(sample(seq(75,150,1),1), nsmall = 0) # "sample" sizes
n_2 <- format(sample(seq(75,150,1),1), nsmall = 0)

m_1 <- sample(seq(100,500,.1),1)
m_2 <- sample(c(m_1 + sample(seq(0.3,92.17,.1),1),
                m_1 - sample(seq(0.3,92.17,.1),1)),
              1)
format(m_1, nsmall = 1)
format(m_2, nsmall = 1)

sd_1 <- sample(seq(75,225,.1),1) # "sample" SDs
sd_2 <- sd_1 + round(runif(1),digits = 1)
format(sd_1, nsmall = 1)
format(sd_2, nsmall = 1)

vals$mat <- matrix(data = c(n_1,n_2,m_1,m_2,sd_1,sd_2),
              nrow = 2,byrow = F)
colnames(vals$mat) <- c("Observations","Mean","Standard deviation")
rownames(vals$mat) <- c("Group 1", "Group 2")

output$tt_table <- renderTable({
  vals$mat
},align = c('c'), rownames = T, colnames = T)

})

# t-test - solution, brief
observeEvent(input$tt_solution,{
  
# Calculation
ttdiff <- as.numeric(vals$mat[1,2]) - as.numeric(vals$mat[2,2]) # difference
  
tt_se <- sqrt(((as.numeric(vals$mat[1,1])-1)*as.numeric(vals$mat[1,3])^2 + (as.numeric(vals$mat[2,1])-1)*as.numeric(vals$mat[2,3])^2)/(as.numeric(vals$mat[1,1]) + as.numeric(vals$mat[2,1]) - 2)) * sqrt((1/as.numeric(vals$mat[1,1])) + (1/as.numeric(vals$mat[2,1])))

tt_tval <- ttdiff/tt_se

tt_df <- as.numeric(vals$mat[1,1]) + as.numeric(vals$mat[2,1]) - 2

tt_pval <- 2*pt(abs(tt_tval), df = tt_df,
                lower.tail = F)

tt_pval_sm <- pt(tt_tval, df = tt_df,
                 lower.tail = T)

tt_pval_la <- pt(tt_tval, df = tt_df,
                 lower.tail = F)
  
  output$tt_result_brief <- renderUI({
    HTML(paste0("The difference between the two group means is: ",vals$mat[1,2]," - ",vals$mat[2,2]," = ",round(ttdiff,digits = 1),".\n
           The standard error of this difference is: ",round(tt_se, digits = 3),", and the t-value is accordingly ",format(round(tt_tval,digits = 2),nsmall=2),".",br(),br(),
           "The corresponding p-value for a two-tailed test (whether or not the two group means are equal or not) is: ",
           format(round(tt_pval, digits=3), nsmall = 3)," (df = ",tt_df,").",br(),br(),
           "If we would instead do a one-sided test if the mean in Group 1 is ",strong("smaller")," than the mean in Group 2, the
           p-value would be: ",format(round(tt_pval_sm, digits=3), nsmall = 3),".",br(),br(),
           "And if we would test the opposite hypothesis that the mean in Group 1 is really ",strong("larger")," than the mean in Group 2, the 
           corresponding one-sided p-value would be: ",format(round(tt_pval_la, digits=3), nsmall = 3),".",br(),br(),
           "To see more clearly where these p-values come from and what we do when we test 'in different directions', you can
           plug the values you get here into the 'Statistical distributions' module and play with the type of hypothesis."))
  })
})
  
# Correlation coefficient
observeEvent(input$cor_sim, {
  
  set.seed(NULL)
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
  enable("cor_solution")
  
  output$plot <- renderPlot({
    ggplot(vals$data,aes(x=X,y=Y)) +
      geom_point() +
      geom_smooth(method='lm',se=F,color="gray",linetype="dashed") +
      theme_bw()
  })
 
})
  
observeEvent(input$cor_solution,{

# Simple solution
res <- isolate(round(cor(vals$data$X,vals$data$Y,
                         method = "pearson"),digits=2))

t <- round((res*sqrt(10-2))/(sqrt(1-res^2)),digits=3)

p_val <- format(round(2 * pt(abs(t), 8, lower.tail = F),digits = 3), nsmall = 3)

output$result <- renderText(
  paste0("The correlation coefficient is: ",res,", and its t-value is: ",t,".\n
          The corresponding p-value (two-sided) is: ",p_val))
  
  
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
  


}

shinyApp(ui = ui, server = server)