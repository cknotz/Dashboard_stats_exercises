
# App to simulate Central Limit Theorem & Confidence Intervals
##############################################################

# Carlo Knotz (carlo.knotz@uis.no)

# Checks if packages installed, and installs if not
if(!require(shiny)){
    install.packages("shiny")
}
if(!require(shinyWidgets)){
    install.packages("shinyWidgets")
}
if(!require(tidyverse)){
    install.packages("tidyverse")
}

# Loads packages
library(shiny)
library(shinyWidgets)
library(tidyverse)


# User interface
ui <- navbarPage("Simulations",

    # Application title
    #titlePanel(: A simulation"),
    tabPanel("The Central Limit Theorem",
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("size",
                        "Size of each sample:",
                        min = 5,
                        max = 125,
                        value = 18,
                        ticks = F),
            sliderTextInput(
                inputId = "samples",
                label = "Number of samples:", 
                choices = c(1, 10, 100, 1000, 10000, 100000),
                grid = T),
            actionButton("button_clt",
                         "Simulate")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("popplot",
                       height = "200px"), 
            plotOutput("distPlot")
        )
    )
    ),
    navbarMenu("What if we do not know the true mean?",
                tabPanel("Where could it be?",
                         sidebarLayout(
                             sidebarPanel(
                                 sliderInput("loc_size",
                                             "Size of each sample",
                                             min = 5,
                                             max = 125,
                                             value = 18,
                                             ticks = F),
                                 sliderInput("loc_diff",
                                             "Distance to sample mean",
                                             min = -2.5,
                                             max = 2.5,
                                             value = 0,
                                             step = 0.1,
                                             ticks = F)
                             ),
                             mainPanel(
                                 plotOutput("locplot")
                             )
                         )
                         ),
    tabPanel("More explicit: Confidence intervals",
             sidebarLayout(
                 sidebarPanel(
                     sliderInput("ci_size",
                                 "Size of each sample",
                                 min = 5,
                                 max = 125,
                                 value = 18,
                                 ticks = F),
                     sliderInput("ci_diff",
                                 "Distance to sample mean",
                                 min = -2.5,
                                 max = 2.5,
                                 value = 0,
                                 step = 0.1,
                                 ticks = F),
                     radioButtons(inputId = "show_ci",
                                  label = "Use CI",
                                  choices = c("No" = F,
                                              "Yes" = T),
                                  selected = F,
                                  inline = T),
                     radioButtons("ci_level",
                                  label = "Level",
                                  choices = c("90%" = 1.645,
                                              "95%" = 1.960,
                                              "99%" = 2.576),
                                  selected = 1.960,
                                  inline = T)
                 ),
                 mainPanel(
                     plotOutput("ciplot")
                 )
             )))
)

# Server
server <- function(input, output, session) {
    
    # New true population, simulated
    pop <- sample(seq(1,10,1),
                  125,
                  replace = T,
                  prob = c(.02,.11,.17,.29,.14,.10,.09,.04,.03,0.01))
    
    # Plot for population distribution, CLT tab
    output$popplot <- renderPlot({
        
        data <- data.frame(pop = pop,
                           idno = seq(1,length(pop),1))
        
        data %>% 
            group_by(pop) %>% 
            summarize(n = n()) %>% 
            ggplot(aes(x=pop,y=n)) +
                geom_bar(stat = "identity") +
                geom_vline(xintercept = mean(pop), color = "red") +
            scale_x_continuous(breaks = seq(1,10,1),
                               limits = c(.5,10.5)) +
            labs(x = "Left-right self-placement",
                 y = "Frequency",
                 title = "The 'true' population data with our target: the population mean",
                 caption = paste0("The red line indicates the 'true' mean: ",round(mean(pop), digits = 2))) +
            theme_bw() +
            theme(aspect.ratio=1/8)
    })
    
    # Simulation & plot output, CLT tab
    observeEvent(input$button_clt,{
        
        if(input$samples>=10000){
            showModal(modalDialog("Simulation is running, please wait...", footer=NULL)) 
        }
        
        # Simulate repeat sampling
        means <- sapply(seq(1,input$samples,1),
                        function(x){
                            sample <- sample(pop,
                                             size = input$size,
                                             replace = F)
                            return(mean(sample))
                        })
        
        isolate(sims <- data.frame(means = means,
                                   draws = seq(1,length(means),1)))
        rm(means)

    if(input$samples>=10000){
        removeModal()
    }
        
    output$distPlot <- renderPlot({
        p <- sims %>%
          ggplot(mapping = aes(x=means)) +
            geom_bar(stat = "count",
                     width = 0.05,
                     position = position_dodge(width=0.01)) +
            geom_vline(xintercept = mean(pop),
                       color = "red") +
            ylab("Number of samples") +
            xlab("Sample mean(s)") +
            labs(title = "Our measurement(s) of the population mean",
                 caption = paste0("The red line indicates the 'true' mean: ",round(mean(pop), digits = 2))) +
            scale_x_continuous(limits = c(.5,10.5),
                               breaks = seq(1,10,1)) +
            theme_bw()
          
          if(input$samples<30){
              p <- p + scale_y_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))
          }
        p
    })
    })
    
    # Simulation & plot output, "where" tab
    observeEvent(input$loc_size,{
        # Simulate repeat sampling
        means <- sapply(seq(1,1000,1),
                        function(x){
                            sample <- sample(pop,
                                             size = input$loc_size,
                                             replace = F)
                            return(mean(sample))
                        })
        
        isolate(sims <- data.frame(means = means,
                                   draws = seq(1,length(means),1)))
        rm(means)
        
        observeEvent(input$loc_diff,{
            sims$means <- sims$means+input$loc_diff
            
            output$locplot <- renderPlot({
                sims %>% 
                    ggplot(mapping = aes(x=means)) +
                    geom_bar(stat = "count",
                             width = 0.05, alpha = .75,
                             position = position_dodge(width=0.01)) +
                    geom_vline(xintercept = mean(pop),
                               color = "#0F52BA", size = 1.25) +
                    geom_vline(xintercept = mean(sims$means),
                               color = "red", size = 1.25) +
                    scale_x_continuous(limits = c(.5,10.5),
                                       breaks = seq(1,10,1)) +
                    labs(x = "Left-right self-placement",
                         y = "Number of samples",
                         caption = paste0("The red line indicates the TRUE population mean: ",round(mean(sims$means), digits = 2),
                                          "\n The blue line indicates the MEASURED mean: ",round(mean(pop), digits=2))) +
                    theme_bw()

            })
        
        })
    })
    
    # Simulation & plot output, CI tab
    observeEvent(input$ci_size,{
        
        # Simulate repeat sampling
        means <- sapply(seq(1,1000,1),
                        function(x){
                            sample <- sample(pop,
                                             size = input$ci_size,
                                             replace = F)
                            return(mean(sample))
                        })
        
        isolate(sims <- data.frame(means = means,
                                   draws = seq(1,length(means),1)))
        rm(means)
        
        observeEvent(input$ci_diff,{
            sims$means <- sims$means+input$ci_diff
        
        output$ciplot <- renderPlot({
        g <- sims %>% 
            ggplot(mapping = aes(x=means)) +
            geom_bar(stat = "count",
                     width = 0.05, alpha = .75,
                     position = position_dodge(width=0.01)) +
            geom_vline(xintercept = mean(pop),
                       color = "#0F52BA", size = 1.25) +
            geom_vline(xintercept = mean(sims$means),
                       color = "red", size = 1.25) +
            scale_x_continuous(limits = c(.5,10.5),
                               breaks = seq(1,10,1)) +
            labs(x = "Left-right self-placement",
                 y = "Number of samples",
                 caption = paste0("The red line indicates the TRUE population mean: ",round(mean(sims$means), digits = 2),
                                  "\n The blue line indicates the MEASURED mean: ",round(mean(pop), digits=2))) +
            theme_bw()
        
        if(input$show_ci=="TRUE"){
            g +geom_errorbarh(aes(y=30,
                                  xmin = mean(sims$means) - as.numeric(input$ci_level)*(sd(sims$means)/sqrt(length(input$ci_size))),
                                  xmax = mean(sims$means) + as.numeric(input$ci_level)*(sd(sims$means)/sqrt(length(input$ci_size)))
            ))
        }else{
        g
        }
    })        
    })
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
