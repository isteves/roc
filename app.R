#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

calc_one <- function(dat, x) {
    tibble(a_neg = sum(dat$type == "a" & dat$val < x), #below
           a_pos = sum(dat$type == "a" & dat$val >= x), #above
           b_neg = sum(dat$type == "b" & dat$val < x),
           b_pos = sum(dat$type == "b" & dat$val >= x))
}

check_equal <- function(x, y, tolerance = .01) {
    out <- all.equal(x, y, tolerance = tolerance)
    if(isTRUE(out)) return(TRUE)
    else return(FALSE)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("ROC Curve"),
    
    # Sidebar with a slider input 
    sidebarLayout(
        sidebarPanel(
            sliderInput("mean_a",
                        "Mean - A",
                        min = 0,
                        max = 1,
                        value = .7),
            sliderInput("mean_b",
                        "Mean - B",
                        min = 0,
                        max = 1,
                        value = .4),
            plotOutput("roc", width = "300px", height = "300px")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            sliderInput("thresh",
                        "Threshold",
                        min = 0,
                        max = 1,
                        step = .05,
                        value = .7),
            plotOutput("distr")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distr <- renderPlot({
        set.seed(1)
        a <- rnorm(5000, mean = input$mean_a, sd = .1)
        b <- rnorm(5000, mean = input$mean_b, sd = .1)
        dat <- tibble(type = rep(c("a", "b"), each = 5000),
                      val = c(a, b))
        
        # draw the histogram with the specified number of bins
        ggplot(dat, aes(val, fill = type)) +
            geom_density(alpha = .5) +
            geom_vline(xintercept = input$thresh,
                       size = 1, color = "red") +
            xlim(0, 1)
    })
    
    output$roc <- renderPlot({
        set.seed(1)
        a <- rnorm(5000, mean = input$mean_a, sd = .1)
        b <- rnorm(5000, mean = input$mean_b, sd = .1)
        dat <- tibble(type = rep(c("a", "b"), each = 5000),
                      val = c(a, b))
        
        tbl_roc <- tibble(x = seq(0, 1, by = 0.05)) %>% 
            mutate(roc = purrr::map(x, ~calc_one(dat, .x))) %>% 
            unnest(roc) %>% 
            mutate(tpr = a_pos/(a_pos + a_neg),
                   fpr = b_pos/(b_pos + b_neg)) %>%
            mutate(fpr = replace_na(fpr, 0),
                   tpr = replace_na(tpr, 0)) 
        
        lgl_thresh <- map_lgl(tbl_roc$x, 
                              ~check_equal(.x, input$thresh, 
                                           tolerance = .01))
        x_thresh <- tbl_roc$fpr[lgl_thresh]
        y_thresh <- tbl_roc$tpr[lgl_thresh]
        
        ggplot(tbl_roc, aes(fpr, tpr)) +
            geom_point() +
            geom_line() +
            geom_point(aes(x = x_thresh, y = y_thresh),
                       color = "red") +
            xlim(0, 1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
