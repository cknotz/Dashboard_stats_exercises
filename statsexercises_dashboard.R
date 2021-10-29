
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

# Matching color scheme for graphs
theme_darkgray <- function(){
  
  theme_minimal() %+replace%
    
    theme(panel.background = element_rect(fill = "#343e48",color = "#d3d3d3"),
          plot.background = element_rect(fill="#343e48", color = "#343e48"),
          panel.grid.major = element_line(color="#d3d3d3", size = .1),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "#d3d3d3"),
          axis.title = element_text(color = "#d3d3d3"),
          plot.caption = element_text(color="#d3d3d3"),
          plot.title = element_text(color = "#d3d3d3"),
          legend.text = element_text(color = "#d3d3d3")
      
    )
}

ui <- dashboardPage(
  dashboardHeader(title="Practice Statistics!"),
  dashboardSidebar(collapsed = F,
    sidebarMenu(
      menuItem("Start",tabName = "start", selected = T),
      menuItem("Mathematical notation", tabName = "math"),
      menuItem("Measures of central tendency",tabName = "cent"),
      menuItem("Measures of spread",tabName = "spread"),
      menuItem("Statistical distributions", tabName = "dist"),
      menuItem("The Central Limit Theorem", tabName = "clt"),
      menuItem("Confidence intervals", tabName = "ci"),
      menuItem("Chi-squared test",tabName = "chi"),
      menuItem("Difference of means test",tabName = "ttest"),
      menuItem("Correlation",tabName = "corr"),
      menuItem("Contact & feedback",tabName = "contact")
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    shinyDashboardThemes(theme="grey_dark"),
    tags$style(type="text/css", "text {font-family: sans-serif}"),
    tags$style(type="text/css",".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, 
              .js-irs-0 .irs-bar {background: #ff9900;border-color: #ff9900;}
              .js-irs-0 .irs-max {font-family: 'arial'; color: white;}
              .js-irs-0 .irs-grid-text {font-family: 'arial'; color: white;}
              .js-irs-0 .irs-grid-pol {display: none;}
              .js-irs-0 .irs-min {font-family: 'arial'; color: white;}"),
    tags$style(type="text/css",".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, 
              .js-irs-1 .irs-bar {background: #ff9900;border-color: #ff9900;}
              .js-irs-1 .irs-max {font-family: 'arial'; color: white;}
              .js-irs-1 .irs-grid-text {font-family: 'arial'; color: white;}
              .js-irs-1 .irs-min {font-family: 'arial'; color: white;}"),
    tags$style(type="text/css",".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, 
              .js-irs-2 .irs-bar {background: #ff9900;border-color: #ff9900;}
              .js-irs-2 .irs-max {font-family: 'arial'; color: white;}
              .js-irs-2 .irs-grid-text {font-family: 'arial'; color: white;}
              .js-irs-2 .irs-min {font-family: 'arial'; color: white;}"),
    tags$style(type="text/css",".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, 
              .js-irs-3 .irs-bar {background: #ff9900;border-color: #ff9900;}
              .js-irs-3 .irs-max {font-family: 'arial'; color: white;}
              .js-irs-3 .irs-grid-text {font-family: 'arial'; color: white;}
              .js-irs-3 .irs-min {font-family: 'arial'; color: white;}"),
    tabItems(
      tabItem(tabName = "start",
      ###############
              fluidRow(
                column(width = 12,
                       box(width = NULL, title = "The age of data", collapsible = T, collapsed = T,
                           solidHeader = T,
                           HTML("<p><strong>We live in an age of data.</strong>
                                Many important societal questions such as whether or not there is a 'gender wage gap' or if there
                                is discrimination and bias against immigrants or minorities are nowadays answered with experiments and
                                statistical analyses. </p>
                                <p>In addition, the internet is creating an ever increasing amount of
                                data about human behavior than wants to be analyzed and used. Data science and machine learning methods,
                                which build to a large extent on statistical methods, are now routinely used by many businesses, and NGOs active in the
                                aid and development sector are also increasingly relying on data scientists to do and evalute their
                                project work (e.g., <a target='_blank' href='https://correlaid.org/'>correlaid.org</a> or
                                <a target='_blank' href='https://data.org/'>data.org</a>). <strong>Clearly, being able to use statistical methods is an extremely powerful skill &mdash; now and in the future.</strong></p>
                                <p>Unfortunately, statistics is also something many students (especially in the social sciences)
                                are not exactly looking forward to engaging with. Some may see statistics as irrelevant to them, but
                                many others are simply afraid of the math. Many students, again especially in the social sciences,
                                see themselves as 'not a math' person, an attitude that might stem from bad experiences in school.</p>")),
                       box(width = NULL, solidHeader = T, collapsible = T, collapsed = T,
                           title = "Statistics can be learned",
                                HTML("<p><strong>Statistics can be learned and mastered, even by people who see themselves as 'not a math person'.</strong>
                                The idea that someone is simply unable to do math is <i>wrong</i> (according to <a target = '_blank'
                                href='https://www.aft.org/sites/default/files/periodicals/willingham.pdf'>cognitive scientists</a>!).
                                But what many people who struggle with mathematical concepts and procedures might need more of is simply: <i>practice</i>.</p>
                                <p>Many statistics textbooks are typically focused on explaining the basic ideas behind the various statistics and 
                                methods &mdash; the <i>conceptual</i> side of things. In statistics courses, teachers likewise emphasize conceptual
                                understanding and often even leave out all equations, in the hope that this helps the 'not a math person'
                                students.</p>
                                <p>To be clear, developing a sound conceptual understanding is very important &mdash; but it is also important
                                to practice actually 'doing the math'. Here is why: Research has shown that students can really improve their understanding of 
                                a particular method or concepts simply by 'crunching the numbers' a few times. By doing calculations, you literally force
                                your brain to engage deeply with the material you are studying, and this can help you to better understand the logic behind
                                a particular procedure or concepts.</po>
                                <p>Also, after a few calculations, equations that at first sight seemed impenetrable
                                and perhaps even scary become manageable and intuitive. You learn that you can actually understand and master seemingly complicated material.
                                In consequence, you gain confidence that will help you tackle the more complicated concepts and procedures.</p>")),
                       box(width = NULL, solidHeader = T, collapsible = T, collapsed = T,
                           title = "The purpose of this application",
                                HTML("<p><strong>And this is the purpose of this application: To let you practice</strong> beginner-level statistical
                                methods by hand. It will give you brief instructions and then keeping spitting out new numbers for
                                you to crunch until you feel that you really understand a given technique. In addition, it features a module
                                to simulate the logic behind the Central Limit Theorem and confidence intervals. Finally, you can visualize 
                                central statistical distributions, which can help you to understand how to interpret the results of statistical
                                tests.</p>")
              ))
              )),
      ###############
      
      ###############
      
      tabItem(tabName = "math",
      ###############
              fluidRow(
                column(width = 6,
                       box(width = NULL, collapsible = T, collapsed = T, solidHeader = F,
                           title = HTML("&Sigma;"),
                           HTML("<p>The symbol &Sigma; is the Greek letter 'Sigma' &mdash; or the Greek
                                large S. In mathematical equations, it stands for 'Sum'.</p>
                                <p>For example, assume we have a set of numbers such as (3, 7, 8, 5). Let's call this
                                set of numbers X.</p>
                                <p>&Sigma;(X) would simply be the sum of all the numbers in X:</p>
                                <p>&Sigma;(X) = 3 + 7 + 8 + 5 = 23</p>")),
                       box(width = NULL, collapsible = T, collapsed = T, solidHeader = F,
                           title = HTML("X&#772;"),
                           HTML("<p>A little horizontal bar usually indicates that we are talking about 
                           the <i>mean</i>. For example, X&#772; ('X bar') would be the mean of a variable X.</p>")),
                       box(width = NULL, collapsible = T, collapsed = T, solidHeader = F,
                           title = HTML("&#177;"),
                           HTML("<p>The &#177; is the 'plus-minus' symbol. As its name suggests, it means that we
                                first add two numbers and then subtract them. This produces two results.</p>
                                <p>For example:</p>
                                <p>3 &#177; 2 </p>
                                <p>= 3 + 2 = 5</p>
                                <p> & </p>
                                <p>= 3 - 2 = 1</p>"))),
                column(width = 6,
                       box(width = NULL, collapsible = T, collapsed = T, solidHeader = F,
                           title = HTML("&radic;"),
                           HTML("<p>You probably know the square root symbol (&radic;) from high school: It is, simply put, the opposite of
                                the square of a number (a number multiplied with itself).</p>
                                <p>To illustrate:</p>
                                <p>2 x 2 = 2<sup>2</sup> = 4</p>
                                <p></p>
                                <p>The square root is the same in reverse: </p>
                                <p><span style='white-space: nowrap; font-size:larger'>&radic;<span style='text-decoration:overline;'>&nbsp;4&nbsp;</span></span> = 2</p>")),
                       box(width = NULL, collapsible = T, collapsed = T, solidHeader = F,
                           title = HTML("Y&#770;"),
                           HTML("<p>A little hat symbol on top of a letter usually indicates that we are dealing with
                                an <i>estimated value</i> (e.g., a prediction from a statistical model).</p>
                                <p>Y&#770; ('Y hat') is the estimated value of Y.</p>")),
                       box(width = NULL, collapsible = T, collapsed = T, solidHeader = F,
                           title = "|x|",
                           HTML("<p>Two vertical bars indicate that we are talking about the <i>absolute value</i> of a number.</p>
                                <p>For example: |-2| = 2 and |2| = 2.</p>")))
              )
              ),
      ##############
      
      tabItem(tabName = "cent",
      ##############
              fluidRow(
                column(width = 4,
                       box(width = NULL, title = "Measures of central tendency",
                           collapsible = T, collapsed = T, solidHeader = F,
                           HTML("<p>Measures of central tendency are statistics used 
                                to describe where most of the values of a variable 
                                are located.</p>
                                <p>The <strong>mean</strong> or 'average' is probably the
                                most familiar, but there are also the <strong>median</strong>
                                and the <strong>mode</strong>.</p>
                                <p>These statistics are used in many more advanced procedures,
                                so a thorough understanding of them is essential. Fortunately,
                                they are also easy to understand.</p>
                                <p>This module allows you to generate some numbers to practice
                                calculating the the mean and median. (Why not also the mode? 
                                Because it is not really difficult:
                                The mode is simply the most frequent value observed in a set of data.)")),
                       box(width = NULL, title = "Controls", collapsible = T, collapsed = F,
                           solidHeader = F,
                           actionBttn(inputId = "cent_sim",
                                      label = "Give me some data!",
                                      style="material-flat",
                                      color="danger",
                                      size = "xs"),
                           br(),br(),
                           disabled(actionBttn(inputId = "cent_solution",
                                               label = "Show me the solution!",
                                               style = "material-flat",
                                               color = "warning",
                                               size = "xs")))),
                column(width = 8,
                       box(width = NULL, title = "Can you calculate the mean and median?", collapsible = F, solidHeader = F,
                           # HTML("<p>If you click on the green button on the left, you 
                           # will get a set of numbers. Can you calculate the mean and 
                           #      median of this set of numbers?</p>"),
                           # br(),
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
                                you can create a set of numbers, calculate the variance and standard deviation,
                                and then let the computer show you the correct result. A detailed solution is also 
                                available if you want.</p>")),
                       box(width = NULL, title = "Controls", collapsible = T, collapsed = F,
                           solidHeader = F,
                           actionBttn(inputId = "spread_sim",
                                      label = "Give me some data!",
                                      style="material-flat",
                                      color="danger",
                                      size = "xs"),
                           br(),br(),
                           disabled(actionBttn(inputId = "spread_solution",
                                               label = "Show me the solution!",
                                               style = "material-flat",
                                               color = "warning",
                                               size = "xs")))),
                column(width = 8,
                       box(width = NULL, title = "Can you calculate the variance & standard deviation?", collapsible = F, solidHeader = F,
                           # HTML("<p>If you click on the green button on the left, you 
                           # will get a set of numbers. Can you calculate the variance and 
                           #      standard deviation of this set of numbers?</p>"),
                           # br(),
                           textOutput("spreadvals")),
                       box(width = NULL, title = "Solution", collapsible = F,
                           solidHeader = F,
                           uiOutput("spread_sol")),
                       box(width = NULL, title = "Detailed solution", collapsible = T,
                           collapsed = T, solidHeader = F,
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
                           collapsible = T,solidHeader = F, collapsed = T,
                           HTML("<p>The Central Limit Theorem is a central concept
                           in most areas of applied statistics. Understanding it is
                           therefore obviously important &mdash; but can also be
                                challenging.</p>
                                <p>This module allows you to approach the Central
                                Limit Theorem via a simulation (of the ideological
                                left-right self-placement of a fictional population).</p>
                                <p>Specifically, you can simulate drawing samples
                                from a hypothetical population and calculating a 
                                sample mean. You can adjust the number of samples
                                that are drawn simultaneously and the size of each
                                sample to see how the result (the sampling distribution)
                                changes.</p>")),
                       box(width = NULL, title = "Controls",
                           collapsible = T, solidHeader = F, collapsed = F,
                           actionButton("button_pop",
                                        "Create population data", 
                                        class = "btn-secondary"),
                           br(),br(),
                           sliderInput("clt_size",
                                       "Size of each sample:",
                                       min = 5,
                                       max = 125,
                                       value = 18,
                                       ticks = F),
                           sliderTextInput(
                             inputId = "clt_samples",
                             label = "Number of samples drawn simultaneously:", 
                             choices = c(1, 10, 100, 1000, 10000, 100000),
                             selected = 100,
                             grid = T),
                           actionButton("button_clt",
                                        "Simulate drawing samples")
                           )),
                column(width = 8,
                       box(width = NULL, title = "Simulate repeated sampling from a population", collapsible = F, solidHeader = F,
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
                           collapsed = T, solidHeader = F,
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
                           collapsed = F,
                           sliderInput("ci_size",
                                       "Size of each sample",
                                       min = 5,
                                       max = 125,
                                       value = 18,
                                       ticks = F),
                           sliderInput("ci_diff",
                                       "Move location of potential true population mean",
                                       min = -50,
                                       max = 50,
                                       value = 0,
                                       step = 1,
                                       ticks = F),
                           radioGroupButtons("ci_level",
                                        label = "Confidence level",
                                        choices = c("90%" = 1.645,
                                                    "95%" = 1.960,
                                                    "99%" = 2.576),
                                        selected = 1.960,
                                        justified = T,
                                        checkIcon = list(
                                          yes = icon("ok", 
                                                     lib = "glyphicon"))
                                        ),
                           radioGroupButtons(inputId = "show_ci",
                                        label = "Show confidence interval",
                                        choices = c("No" = F,
                                                    "Yes" = T),
                                        selected = F,
                                        justified = T,
                                        checkIcon = list(
                                          yes = icon("ok",
                                                     lib = "glyphicon")))
                           )),
                column(width = 8,
                       plotOutput("ci_plot"),
                       br(),
                       box(width = NULL, title = "Making sense of what you see", collapsible = T, solidHeader = T,
                           collapsed = T,
                           HTML("<p>If you move the slider on the left ('Move location of population mean')
                                to the left and right,
                                you can simulate different scenarios for where the 'true'
                                population mean is located. As you move the slider, 
                                ask yourself: If the 'true' population mean were located
                                at this point, would it be likely or unlikely that we
                                measured our given sample mean (given the sample size and chosen confidence level)?</p>
                                <p>If you then let the graph show the confidence interval, 
                                you should notice that this interval corresponds to
                                those possible true population means where you said they are
                                plausible given our sample mean and the sampling distribution. In other words,
                                you should notice that the confidence interval includes the 
                                <strong>range of potential 'true' population values
                                that are plausible, given the sample size and level of confidence.</strong></p>")))
              )
              ),
      
      ###############
      
      tabItem(tabName = "dist",
      ###############
              fluidRow(
                column(width = 4,
                       box(width = NULL, title = "Statistical distributions",
                           collapsible = T,solidHeader = F, collapsed = T,
                           HTML("<p>When you do statistical tests, you always work with different
                                statistical distributions: the normal distribution, the <i>t</i>-distribution,
                                or the &chi;&sup2;-distribution.</p>
                                
                                <p>Here you can visualize these three distributions (for different degrees of
                                freedom, where applicable) as well as the location of critical values for
                                your chosen level of significance.</p>
                                
                                <p>If you like, you can also enter a test value (from a t- or chi-squared test)
                                into the box below. This indicates where your test result is relative to the 
                                distribution - which should you help you make sense of your test result.</p>")),
                       box(width=NULL,title = "Controls",collapsible = T,solidHeader = F, collapsed = F,
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
                           collapsible = T, collapsed = T, solidHeader = F,
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
                                      label = "Give me some data!",
                                      style="material-flat",
                                      color="danger",
                                      size = "xs"),
                           br(),br(),
                           disabled(actionBttn(inputId = "tt_solution",
                                               label = "Show me the solution!",
                                               style = "material-flat",
                                               color = "warning",
                                               size = "xs")))),
                column(width = 8,
                       box(width = 0, title = "Is there a significant difference?", collapsible = F, solidHeader = F,
                           tableOutput("tt_table")
                           ),
                       box(width = NULL, title = "Solution", collapsible = T, 
                           solidHeader = F,
                           uiOutput("tt_result_brief")),
                       box(width = NULL, title = "Detailed solution", collapsible = T, collapsed = T,
                           solidHeader = T,
                           uiOutput("tt_result_det"))
                       )
              )
              ),
      ###############
      
      tabItem(tabName = "chi",
      ##############        
              fluidRow(
                column(width = 4,
                       box(width = NULL, solidHeader = F, collapsible = T, collapsed = T,
                           title = HTML("The &#x1D6D8;<sup>2</sup> test"),
                           HTML("<p>The &#x1D6D8;<sup>2</sup> ('chi-squared') test is a statistical
                                test for relationships between two categorical variables. The underlying logic
                                of this test &mdash; comparing the observed patterns in a cross-table with 
                                a hypothetical one &mdash; can be difficult to grasp at first, but calculating
                                a few example tests by hand can really help with this.</p>
                                <p>As in the other panels, you can let the computer generate some random
                                example data for you to work with. You can then reveal a brief and a detailed
                                step-by-step solution.</p>")),
                       box(width = NULL, solidHeader = F, collapsible = T, collapsed = F,
                           title = "Controls",
                           actionBttn(inputId = "chi_sim",
                                      label = "Give me some data!",
                                      style="material-flat",
                                      color="danger",
                                      size = "xs"),
                           br(),br(),
                           disabled(actionBttn(inputId = "chi_solution",
                                               label = "Show me the solution!",
                                               style = "material-flat",
                                               color = "warning",
                                               size = "xs")))),
                column(width = 8,
                       box(width = NULL, solidHeader = F, collapsible = F, 
                           title = "Is there a significant relationship in the data?",
                           tableOutput("chitab")),
                       box(width = NULL, solidHeader = F, collapsible = F,
                           title = "Solution",
                           uiOutput("chires_brief")),
                       box(width = NULL, solidHeader = F, collapsible = T, collapsed = T, 
                           title = "Detailed solution"))
              )
              ),
      ##############
      
      tabItem(tabName = "corr",
      ###############        
              fluidRow(
                column(width = 4,
                box(width=NULL,title = "Correlation coefficient",collapsible = T,
                    solidHeader = F, collapsed = T,
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
                box(width=NULL,title = "Controls",collapsible = T,solidHeader = F, collapsed = F,
                    actionBttn(inputId = "cor_sim",
                               label = "Give me some data!",
                               style="material-flat",
                               color="danger",
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
              box(width=NULL,title = "Is there a significant correlation?",collapsible = F,solidHeader = F,
                  column(3,tableOutput(outputId = "tab")),
                  column(9,plotOutput(outputId = "plot"))
                  ),
              box(width = NULL,title = "Solution",collapsible = F,solidHeader = F,
                  textOutput(outputId = "result")),
              box(width = NULL,title = "The detailed solution",collapsible = T,
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
                    uiOutput("cor_detail10"))
              )
    )),
      ###############
    
      tabItem(tabName = "contact",
      ##############
              fluidRow(
                column(width = 12,
                       box(width = NULL, title = "Questions & Feedback",
                           collapsible = F, solidHeader = T,
                           HTML("<p>I hope you find this dashboard useful to practice and therefore
                                better understand statistics and the theory behind it.</p>
                                <p>Should you have any questions or suggestions for further improvement,
                                please feel free to reach out to me by e-mail (<a href='mailto:carlo.knotz@uis.no' style='color:orange;'>carlo.knotz@uis.no</a>).</p>")),
                       box(width = NULL, title = "Want to contribute?",
                           collapsible = F, solidheader = T,
                           HTML("<p>If you feel that this application lacks some functionality or could be improved
                                in some way (which it probably can!), you can access and 'fork' the code on GitHub
                                (<a href='' target='_blank'>LINK TO GITHUB REPO WHEN READY</a>).</p>"))
              ))
              )
      ##############
  
)))

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
              of all the values in X and then divide the result by the overall number of values (9):</p>",
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
  withMathJax("We start by calculating the variance of X. The formula is as follows: 
                       $$s^2 = \\frac{\\sum_{i=1}^N (X_i - \\bar{X})^2}{N-1}$$
                       In human language: We calculate the mean of X, and then we calculate the 
                       difference of each value in X from this mean. Then we square each of the
                       resulting numbers. Finally, we add them all up and then divide the
                       result by N-1. The calculation is a bit tedious and easier to follow
                       when it is presented in a table:")
})

output$spread_sol_det2 <- renderTable({
  spreadmat
}, rownames = T, include.colnames = F, 
width = "100%",hover = T,digits = 1,
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
set.seed(NULL)
lambda <- sample(seq(1,10,1),
                 1,
                 replace = F)

pois <- 10*rpois(100,lambda)

vals$cltpop <- sample(pois[which(pois<=100)], 2000, replace = T)


observeEvent(input$button_pop,{
  
# "True" population - plot
output$clt_popplot <- renderPlot({
  set.seed(NULL)
  lambda <- sample(seq(1,10,1),
                   1,
                   replace = F)
  
  pois <- 10*rpois(100,lambda)
  
  vals$cltpop <- sample(pois[which(pois<=100)], 2000, replace = T)
  
  
  vals$cltdata <- data.frame(pop = vals$cltpop,
                     idno = seq(1,length(vals$cltpop),1))
  
  vals$cltdata %>% 
    group_by(pop) %>% 
    summarize(n = n()) %>% 
    ggplot(aes(x=pop,y=n)) +
    geom_bar(stat = "identity", fill = "#d3d3d3") +
    geom_vline(xintercept = mean(vals$cltpop), color = "#b34e24", size = 1.25) +
    scale_x_continuous(breaks = seq(10,100,10),
                       limits = c(5,105)) +
    labs(x = "Left-right self-placement",
         y = "Frequency",
         title = "The 'true' population with our target: the population mean",
         caption = paste0("The orange line indicates the 'true' population mean: ",round(mean(vals$cltpop), digits = 2))) +
    theme_darkgray()
  # +
  #   theme(aspect.ratio=1/8)
})
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
               width = 1, fill = "#d3d3d3") +
      geom_vline(xintercept = mean(vals$cltpop),
                 color = "#b34e24", size = 1.25) +
      ylab("Number of samples") +
      xlab("Sample mean(s)") +
      labs(title = "Our measurement(s) of the population mean: Light gray line(s)",
           caption = paste0("The orange line indicates the 'true' mean: ",round(mean(vals$cltpop), digits = 2))) +
      scale_x_continuous(limits = c(5,105),
                         breaks = seq(10,100,10)) +
      theme_darkgray()
    
    if(input$clt_samples<30){
      p <- p + scale_y_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))
    }
    p
  })
})

# Simulation graph, CI

set.seed(42)
vals$cipop <- 10*sample(seq(1,10,1),
                 125,
                 replace = T,
                 prob = c(.02,.20,.29,.13,.10,.09,.09,.04,.03,0.01))

observeEvent(input$ci_size,{
  
  
  
  # Simulate repeat sampling
  vals$means <- sapply(seq(1,2500,1),
                  function(x){
                    sample <- sample(vals$cipop,
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
          scale_fill_manual(values = c("#b34e24","#d3d3d3"),
                            labels = c(paste0("Outer ",round(200*(1-pnorm(as.numeric(input$ci_level))),digits = 1),"%")
                                       ,paste0("Inner ",100-round(200*(1-pnorm(as.numeric(input$ci_level))),digits = 1),"%"))) +
          geom_vline(xintercept = 34.1,
                     color = "#1b9e77", size = 1.25, linetype = "dashed") +
          geom_vline(xintercept = mean(sims$means),
                     color = "grey15", size = 1.25) +
          scale_x_continuous(limits = c(5,105),
                             breaks = seq(10,100,10)) +
          labs(x = "Left-right self-placement",
               y = "Number of samples",
               caption = paste0("The gray solid line indicates the POSSIBLE true population mean: ",round(mean(sims$means), digits = 2),
                                "\n The green dashed line indicates the MEASURED sample mean: 34.1")) +
          theme_darkgray() +
          theme(legend.title = element_blank(),
                legend.position = "bottom")
        
        if(input$show_ci=="TRUE"){
          g +geom_errorbarh(size = 1, height = 10, color = "#1b9e77", aes(y=30,
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
      geom_area(stat = "function", fun = dnorm, fill = "#b34e24",
                xlim = c(-4, qnorm(as.numeric(input$dist_signselect)/2)), color = "black") +
      geom_area(stat = "function", fun = dnorm, fill = "#d3d3d3", 
                xlim = c(qnorm(as.numeric(input$dist_signselect)/2),qnorm(1-(as.numeric(input$dist_signselect)/2))), color = "black") +
      geom_area(stat = "function", fun = dnorm, fill = "#b34e24",
                xlim = c(qnorm(1-(as.numeric(input$dist_signselect)/2)),4), color = "black") +
      annotate("segment", x = qnorm(as.numeric(input$dist_signselect)/2), xend = qnorm(1-as.numeric(input$dist_signselect)/2), 
               y = dnorm(qnorm(as.numeric(input$dist_signselect)/2)), yend = dnorm(-qnorm(as.numeric(input$dist_signselect)/2)), arrow = arrow(ends='both'),
               size = 1.5, color = "grey15") +
      annotate("text", x=0, y=dnorm(qnorm(as.numeric(input$dist_signselect)/2))+.015,label = paste0(100*(1-as.numeric(input$dist_signselect)),"% of data"), 
               color="grey15", fontface = "bold") +
      # geom_vline(xintercept = qnorm(as.numeric(input$dist_signselect)/2), color = "#ff9900", linetype = "dashed",
      #            size=1.5) +
      # geom_vline(xintercept = qnorm(1-as.numeric(input$dist_signselect)/2), color = "#ff9900", linetype = "dashed",
      #            size=1.5) +
      geom_vline(xintercept = as.numeric(input$dist_valselect), color = "black", linetype = "dashed",
                 size=1.5) +
      labs(x = "", y = "Density",
           title = paste0("Normal distribution critical values for a ",as.numeric(input$dist_signselect)," significance level (two-sided): ",
                          round(qnorm(as.numeric(input$dist_signselect)/2), digits = 3)," & ",
                          round(qnorm(1-as.numeric(input$dist_signselect)/2), digits = 3))) +
      theme_darkgray() +
      theme(axis.text = element_text(size=12))
    }
    else if(input$dist_hypselect=="Larger than"){
      ggplot(NULL, aes(c(-4,4))) + 
        geom_area(stat = "function", fun = dnorm, fill = "#d3d3d3", 
                  xlim = c(-4,qnorm(1-(as.numeric(input$dist_signselect)))), color = "black") +
        geom_area(stat = "function", fun = dnorm, fill = "#b34e24",
                  xlim = c(qnorm(1-(as.numeric(input$dist_signselect))),4), color = "black") +
        # geom_vline(xintercept = qnorm(1-as.numeric(input$dist_signselect)), color = "#d3d3d3", linetype = "dashed",
        #            size=1.5) +
        geom_vline(xintercept = as.numeric(input$dist_valselect), color = "black", linetype = "dashed",
                   size=1.5) +
        annotate("segment", x = -3.99, xend = qnorm(1-as.numeric(input$dist_signselect)), 
                 y = dnorm(qnorm(as.numeric(input$dist_signselect)))/2, yend = dnorm(-qnorm(as.numeric(input$dist_signselect)))/2, arrow = arrow(ends='both'),
                 size = 1.5, color = "grey15") +
        annotate("text", x=0, hjust = 1, 
                 y=dnorm(qnorm(as.numeric(input$dist_signselect)/2))+.015,label = paste0(100*(1-as.numeric(input$dist_signselect)),"% of data"), 
                 color="grey15", fontface = "bold") +
        labs(x = "", y = "Density",
             title = paste0("Normal distribution critical value for a ",as.numeric(input$dist_signselect)," significance level (larger than): ",
                            round(qnorm(1-as.numeric(input$dist_signselect)), digits = 3))) +
        theme_darkgray() +
        theme(axis.text = element_text(size=12))
    }
    else if(input$dist_hypselect=="Smaller than"){
      ggplot(NULL, aes(c(-4,4))) + 
        geom_area(stat = "function", fun = dnorm, fill = "#b34e24", 
                  xlim = c(-4,qnorm((as.numeric(input$dist_signselect)))), color = "black") +
        geom_area(stat = "function", fun = dnorm, fill = "#d3d3d3",
                  xlim = c(qnorm((as.numeric(input$dist_signselect))),4), color = "black") +
        # geom_vline(xintercept = qnorm(as.numeric(input$dist_signselect)), color = "#d3d3d3", linetype = "dashed",
        #            size=1.5) +
        geom_vline(xintercept = as.numeric(input$dist_valselect), color = "black", linetype = "dashed",
                   size=1.5) +
        annotate("segment", x = 3.99, xend = qnorm(as.numeric(input$dist_signselect)),
                 y = dnorm(qnorm(as.numeric(input$dist_signselect)))/2, yend = dnorm(-qnorm(as.numeric(input$dist_signselect)))/2, arrow = arrow(ends='both'),
                 size = 1.5, color = "grey15") +
        annotate("text", x=0, hjust = 0, 
                 y=dnorm(qnorm(as.numeric(input$dist_signselect)/2))+.015,label = paste0(100*(1-as.numeric(input$dist_signselect)),"% of data"), 
                 color="grey15", fontface = "bold") +
        labs(x = "", y = "Density",
             title = paste0("Normal distribution critical value for a ",as.numeric(input$dist_signselect)," significance level (smaller than): ",
                            round(qnorm(as.numeric(input$dist_signselect)), digits = 3))) +
        theme_darkgray() +
        theme(axis.text = element_text(size=12))
    }
      
  }else if(input$dist_distselect=="t"){
    enable(id = "dist_dfselect")
    enable(id = "dist_hypselect")
    if(input$dist_hypselect=="Two-sided"){
      
      ggplot(NULL, aes(c(-4,4))) + 
        geom_area(stat = "function", fun = dt, args = list(df=as.numeric(input$dist_dfselect)), fill = "#b34e24",
                  xlim = c((qt(as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)))-5,
                           qt(as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect))), color = "black") +
        geom_area(stat = "function", fun = dt, args = list(df=as.numeric(input$dist_dfselect)), fill = "#d3d3d3",
                  xlim = c(qt(as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)),
                           qt(1-as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect))), color = "black") +
        geom_area(stat = "function", fun = dt, args = list(df=as.numeric(input$dist_dfselect)), fill = "#b34e24",
                  xlim = c(qt(1-as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)),
                           qt(1-as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect))+5), color = "black") +
        annotate("segment", x = qt(as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)), 
                 xend = qt(1-as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)),
                 y = dt(qt(as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)), df=as.numeric(input$dist_dfselect)),
                 yend = dt(-qt(as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)), df=as.numeric(input$dist_dfselect)), arrow = arrow(ends='both'),
                 size = 1.5, color = "grey15") +
        annotate("text", x=0, y=dt(qt(as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)), df=as.numeric(input$dist_dfselect))+0.015,
                 label = paste0(100*(1-as.numeric(input$dist_signselect)),"% of data"), color="grey15", fontface = "bold") +
        # geom_vline(xintercept = qt(as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)),
        #            color = "#d3d3d3", linetype = "dashed",
        #            size=1.5) +
        # geom_vline(xintercept = qt(1-as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)), 
        #            color = "#d3d3d3", linetype = "dashed",
        #            size=1.5) +
        geom_vline(xintercept = as.numeric(input$dist_valselect), color = "black", linetype = "dashed",
                   size=1.5) +
        labs(x = "", y = "Density",
             title = paste0("t-distribution critical values for a ",as.numeric(input$dist_signselect)," significance level (two-sided; df = ",as.numeric(input$dist_dfselect),"): ",
                            round(qt(as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)), digits = 3)," & ",
                            round(qt(1-as.numeric(input$dist_signselect)/2, df=as.numeric(input$dist_dfselect)), digits = 3))) +
        theme_darkgray() +
        theme(axis.text = element_text(size=12))
    }
    else if(input$dist_hypselect=="Larger than"){
      ggplot(NULL, aes(c(-4,4))) + 
        geom_area(stat = "function", fun = dt, args = list(df=as.numeric(input$dist_dfselect)), fill = "#d3d3d3",
                  xlim = c((qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)))-5,
                           qt(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect))), color = "black") +
        geom_area(stat = "function", fun = dt, args = list(df=as.numeric(input$dist_dfselect)), fill = "#b34e24",
                  xlim = c(qt(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)),
                           qt(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect))+5), color = "black") +
        annotate("segment", x = (qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)))-4.99,
                 xend = qt(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)),
                 y = dt(qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), df=as.numeric(input$dist_dfselect)),
                 yend = dt(-qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), df=as.numeric(input$dist_dfselect)), arrow = arrow(ends='both'),
                 size = 1.5, color = "grey15") +
        annotate("text", x=0, hjust=1,
                 y=dt(qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), df=as.numeric(input$dist_dfselect))+0.015,
                 label = paste0(100*(1-as.numeric(input$dist_signselect)),"% of data"), color="grey15", fontface = "bold") +
        # geom_vline(xintercept = qt(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), 
        #            color = "#d3d3d3", linetype = "dashed",
        #            size=1.5) +
        geom_vline(xintercept = as.numeric(input$dist_valselect), color = "black", linetype = "dashed",
                   size=1.5) +
        labs(x = "", y = "Density",
             title = paste0("t-distribution critical value for a ",as.numeric(input$dist_signselect)," significance level (larger than; df = ",as.numeric(input$dist_dfselect),"): ",
                            round(qt(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), digits = 3))) +
        theme_darkgray() +
        theme(axis.text = element_text(size=12))
    }
    else if(input$dist_hypselect=="Smaller than"){
      ggplot(NULL, aes(c(-4,4))) + 
        geom_area(stat = "function", fun = dt, args = list(df=as.numeric(input$dist_dfselect)), fill = "#b34e24",
                  xlim = c((qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)))-5,
                           qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect))), color = "black") +
        geom_area(stat = "function", fun = dt, args = list(df=as.numeric(input$dist_dfselect)), fill = "#d3d3d3",
                  xlim = c(qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)),
                           qt(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect))+5), color = "black") +
        annotate("segment", x = (qt(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)))+4.99,
                 xend = qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)),
                 y = dt(qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), df=as.numeric(input$dist_dfselect)),
                 yend = dt(-qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), df=as.numeric(input$dist_dfselect)), arrow = arrow(ends='both'),
                 size = 1.5, color = "grey15") +
        annotate("text", x=0, hjust=0,
                 y=dt(qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), df=as.numeric(input$dist_dfselect))+0.015,
                 label = paste0(100*(1-as.numeric(input$dist_signselect)),"% of data"), color="grey15", fontface = "bold") +
        # geom_vline(xintercept = qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), 
        #            color = "#d3d3d3", linetype = "dashed",
        #            size=1.5) +
        geom_vline(xintercept = as.numeric(input$dist_valselect), color = "black", linetype = "dashed",
                   size=1.5) +
        labs(x = "", y = "Density",
             title = paste0("t-distribution critical value for a ",as.numeric(input$dist_signselect)," significance level (smaller than; df = ",as.numeric(input$dist_dfselect),"): ",
                            round(qt(as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), digits = 3))) +
        theme_darkgray() +
        theme(axis.text = element_text(size=12))
    }
    
  }else if(input$dist_distselect=="Chi-squared"){
    enable(id = "dist_dfselect")
    disable(id = "dist_hypselect")
    
    ggplot(NULL, aes(c(0,5))) + 
      geom_area(stat = "function", fun = dchisq, fill = "#d3d3d3", color = "black",
                xlim = c(0, qchisq(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect))), 
                args = list(df=as.numeric(input$dist_dfselect))) +
      geom_area(stat = "function", fun = dchisq, fill = "#b34e24", color = "black",
                xlim = c(qchisq(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), 
                                                                            qchisq(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect))+.5*qchisq(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect))), 
                args = list(df=as.numeric(input$dist_dfselect))) +
      # geom_vline(xintercept = qchisq(1-as.numeric(input$dist_signselect), df=as.numeric(input$dist_dfselect)), color = "#d3d3d3", linetype = "dashed", size = 1.25) +
      geom_vline(xintercept = as.numeric(input$dist_valselect), color = "black", linetype = "dashed",
                 size=1.5) +
      annotate("segment", arrow = arrow(ends = "both"), size = 1.5, color = "grey15",
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
      theme_darkgray() +
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

vals$mat <- isolate(matrix(data = c(n_1,n_2,m_1,m_2,sd_1,sd_2),
              nrow = 2,byrow = F))
colnames(vals$mat) <- c("Observations","Mean","Standard deviation")
rownames(vals$mat) <- c("Group 1", "Group 2")

output$tt_table <- renderTable({
  vals$mat
},align = c('c'), rownames = T, colnames = T)

})

# t-test - solution
observeEvent(input$tt_solution,{
  
# Calculation
  
t_m1 <- as.numeric(vals$mat[1,2])
t_m2 <- as.numeric(vals$mat[2,2])
  
t_n1 <- as.numeric(vals$mat[1,1])
t_n2 <- as.numeric(vals$mat[2,1])

t_sd1 <- as.numeric(vals$mat[1,3])
t_sd2 <- as.numeric(vals$mat[2,3])

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

# Brief solution  
output$tt_result_brief <- renderUI({
    HTML(paste0("The difference between the two group means is: ",t_m1," - ",t_m2," = ",round(ttdiff,digits = 1),".\n
           The standard error of this difference is: ",round(tt_se, digits = 3),", and the t-value is accordingly ",format(round(tt_tval,digits = 2),nsmall=2),".",br(),br(),
           "The corresponding p-value for a two-tailed test (whether or not the two group means are equal or not) is: ",
           format(round(tt_pval, digits=3), nsmall = 3)," (df = ",tt_df,").",br(),br(),
           "If we would instead do a one-sided test if the mean in Group 1 is ",strong("smaller")," than the mean in Group 2, the
           p-value would be: ",format(round(tt_pval_sm, digits=3), nsmall = 3),".",br(),br(),
           "And if we would test the opposite hypothesis that the mean in Group 1 is really ",strong("larger")," than the mean in Group 2, the 
           corresponding one-sided p-value would be: ",format(round(tt_pval_la, digits=3), nsmall = 3),"."))
  })

# Detailed solution
output$tt_result_det <- renderUI({
  withMathJax(paste0("The first step in calculating a difference-of-means t-test is very simple: We calculate the
              difference between the two group means: ",t_m1," - ",t_m2," = ",round(ttdiff,digits = 1),"$$ $$",
                     "Once we have done that simple first step, things get (a bit) more serious. We now need to
                     calculate the standard error of this difference - our measurement of how much 'noise' is in the data. 
                     The standard error is calculated with this impressive-looking formula (which is actually less complicated that it might
                     seem at first): $$SE_{\\bar{Y}_1 - \\bar{Y}_2} = \\sqrt{\\left(\\frac{(N_{Y_1}-1)\\times s^2_{Y_1} + (N_{Y_2}-1)\\times s^2_{Y_2}}{N_{Y_1} + N_{Y_2}-2} \\right)} \\times \\sqrt{\\left(\\frac{1}{N_{Y_1}} + \\frac{1}{N_{Y_2}} \\right)}$$
                     All we do is just plug in the numbers we have into the formula: 
                     $$SE_{\\bar{Y}_1 - \\bar{Y}_2} = \\sqrt{\\left(\\frac{(",t_n1,"-1)\\times ",t_sd1,"^2 + (",t_n2,"-1)\\times ",t_sd2,"^2}{",t_n1," + ",t_n2,"-2} \\right)} \\times \\sqrt{\\left(\\frac{1}{",t_n1,"} + \\frac{1}{",t_n2,"} \\right)}$$
                     And then we do the math, step by step (ideally with a calculator, of course):
                     $$SE_{\\bar{Y}_1 - \\bar{Y}_2} = \\sqrt{\\left(\\frac{",format((t_n1-1)*t_sd1^2,nsmall = 2)," + ",format((t_n2-1)*t_sd2^2,nsmall=2),"}{",t_n1+t_n2-2,"} \\right)} \\times \\sqrt{\\left(",format(round(1/t_n1,digits=4),nsmall=4)," + ",format(round(1/t_n2,digits=4),nsmall=4)," \\right)}$$
                     ...and on we go...
                     $$SE_{\\bar{Y}_1 - \\bar{Y}_2} = \\sqrt{",format(((t_n1-1)*t_sd1^2 + (t_n2-1)*t_sd2^2)/(t_n1 + t_n2 - 2),nsmall=2),"} \\times \\sqrt{",format(1/t_n1 + 1/t_n2,nsmall=4),"}$$
                     ...until finally:
                     $$SE_{\\bar{Y}_1 - \\bar{Y}_2} = ",format(sqrt(((t_n1-1)*t_sd1^2 + (t_n2-1)*t_sd2^2)/(t_n1 + t_n2 - 2)) * sqrt(1/t_n1 + 1/t_n2),nsmall=4),"$$
                    Now we have both the difference (the 'signal') and its standard error (the 'noise'), and we can calculate the t-statistic as the ratio of the two:
                     $$t = \\frac{",ttdiff,"}{",format(tt_se,nsmall=4),"} = ",format(round(ttdiff/tt_se,digits=2),nsmall=2),"$$
                     Finally, we need to determine the degrees of freeom, which is simply the total number of observations minus 2:
                     $$df = N_{Y_1} + N_{Y_2} - 2 = ",t_n1 + t_n2 - 2,"$$
                     This completes the boring math part. And now comes the last (and perhaps most challenging part): We have to decide if the test is significant! The first thing
                     we need to do here is to decide what type of hypothesis we want to test. Are we simply interested in whether the two means are
                     different, or is the hypothesis that one mean is larger or smaller than the other (normally, this depends on the theory we test)?
                     $$$$
                     First, we consider whether or not we can conclude that the means are different - the 'equal or not' or 'two-sided' hypothesis. 
                     You can note down the number of degrees of freedom and the t-statistic on a piece of paper and navigate to the 'Statistical distributions' panel. There,
                     select the t-distribution, and the two-sided hypothesis, adjust the degrees of freedom, and enter the t-statistic in the field at the bottom of the Controls-panel. 
                     Finally, select a significance level.
                     $$$$
                     Does your t-statistic fall within the light-gray shaded area, or does it fall into the orange areas (or even further out)?
                     If it is in the gray area, this means the test is not significant - we cannot reject the Null hypothesis that the two means are really equal. If you now look at
                     the brief solution, you should not that the p-value for the two-sided test is high. If, however, 
                     your t-statistic is in the orange areas or further away from 0, then the test is significant - we can say that the true difference is probably not 0, and thus reject the Null hypothesis. This should
                     correspond to a low p-value.
                     $$$$
                     The logic is similar if we do one-sided ('larger-than' or 'smaller-than') hypothesis tests. The difference is only that we then
                     consider only if our t-statistic is significantly higher ('larger-than') or lower ('smaller-than'). If you go back to the 'Statistical distributions' panel 
                     and play with the hypothesis option, you should see the direction of the test logic changing."))
})
})


# Chi-squared test
observeEvent(input$chi_sim,{
  set.seed(NULL)
  enable("chi_solution")
  
# Generate data (based on: https://gsverhoeven.github.io/post/simulating-fake-data/)
rho <- runif(n=1,min = -.5,max = .5)

nobs <- sample(250:750,
               1)

m_1 <- runif(n=1,min = .3, max = .7)
m_2 <- runif(n=1,min = .3, max = .7)
  
cov.mat <- matrix(c(1,rho,rho,1),
                  nrow = 2)

vals$dfdat <- data.frame(MASS::mvrnorm(n=nobs,
                               mu = c(0,0),
                               Sigma = cov.mat))

vals$dfdat$B1 <- ifelse(vals$dfdat$X1 <qnorm(m_1),1,0)
vals$dfdat$B2 <- ifelse(vals$dfdat$X2 <qnorm(m_2),1,0)

dftab <- table(vals$dfdat$B1,vals$dfdat$B2)
rownames(dftab) <- c("Prefers chocolate","Prefers vanilla")
colnames(dftab) <- c("Morning person","Night person")
dftab <- addmargins(dftab)


# Generate table
output$chitab <- renderTable({
  as.data.frame.matrix(dftab)
},rownames = T, digits = 0)

})

observeEvent(input$chi_solution,{
chires <- chisq.test(table(vals$dfdat$B1,vals$dfdat$B2),
                     correct = F)


output$chires_brief <- renderUI({
  HTML(paste0("The &#x1D6D8;<sup>2</sup> value is ",format(round(chires$statistic, digits = 3),nsmall = 3),". At 1 degree
              of freedom, this corresponds to a <i>p</i>-value of ",format(round(chires$p.value,digits=3),nsmall = 3),"."))
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
      geom_point(color = "#d3d3d3", size = 3, shape = 4, stroke = 2) +
      geom_smooth(method='lm',se=F,color="#b34e24",linetype="dashed") + #
      theme_darkgray()
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
  withMathJax("The first step is to calculate the
                          covariance between X and Y. The formula to calculate the
                          covariance is the following: $$cov_{X,Y} = \\frac{\\sum_{i=1}^n (X_i - \\bar X)(Y_i - \\bar Y)}{n}$$")
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
  withMathJax("Now we have the covariance - but we actually want the correlation! To 
                       calculate the correlation coefficient (r), we take the covariance and divide
                       it by the square root of the product of the variances of the two variables 
                       (we will not go in detail over how the variance is calculated):
                       $$r = \\frac{cov_{X,Y}}{\\sqrt{var_X \\times var_Y}}$$")
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