#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load packages ----
library(shiny)  # Shiny web app
library(shinydashboard)  # Shiny Dashboards
library(shinydashboardPlus)  # additionaly dashboard features
library(shinythemes)  # additional themes for Shiny Apps
library(shinyWidgets)  # additional functionality
library(dashboardthemes)  # additional themes for Shiny Dashboards
library(DT)  # alternative way of handling data tables
library(tidyverse)  # data wrangling and exploration
library(ggplot2)  # visualisation


# Source required files ----
source("functions/basic_functions.R")
source("functions/first_sims.R")
source("functions/methods_comparison.R")

# Load example_data
example_data <- 
    read.csv(file = "data/betas.csv",
             header = TRUE,
             sep = ",",
             quote = "\"",
             check.names = FALSE) %>% 
    .[, -1]


# Define UI for application ----
ui <- dashboardPage(
    dashboardHeader(title = "Gini Anker",
                    titleWidth = 350),
    dashboardSidebar(
        # Remove the sidebar toggle element
        tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].
                       style.visibility = 'hidden';")),
        width = 350,
        collapsed = TRUE,
        minified = FALSE),
    dashboardBody(
        shinyDashboardThemes(
            theme = "grey_light"
        ),
        source("tabs/tab_main.R", 
               local = TRUE, encoding = "utf-8")[1]
    )
)
    

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Original Coefficients Table ----
    
    # create data frame with example .csv file
    dat <- reactiveValues(df = example_data)
    
    # create reactive set
    df <- reactive({dat$df})
    
    # table containing original beta coefficients
    output$beta_table <- DT::renderDataTable({
        
        DT::datatable(df(), rownames = FALSE,
                      selection = "none",
                      editable = TRUE,
                      options = list(dom = 'plt', 
                                     scrollX = TRUE,
                                     pageLength = 25,
                                     lengthMenu = c(10, 25, 50)))
    })
    
    # edit cells of df
    observeEvent(input$beta_table_cell_edit, {
        
        info <- input$beta_table_cell_edit
        str(info)
        row   <- info$row
        col   <- info$col + 1L
        value <- info$value %>% as.numeric()
        
        isolate(dat$df[row, col] <- value)
    })
    
    # create data frame from uploaded .csv file
    observeEvent(input$upload_button, {
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                tmp <- read.csv(input$file$datapath,
                                header = input$header,
                                sep = input$sep,
                                quote = input$quote,
                                check.names = FALSE)
                
                # remove column containing row names
                if (colnames(tmp)[1] == "") {
                    tmp <- tmp[, -1]
                }
                
                dat$df <- tmp
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
    })
    
    # add row
    observeEvent(input$add_row, {
        tmp <- data.frame(0, 0, 0)
        colnames(tmp) <- colnames(dat$df)
        dat$df <- rbind(dat$df,
                        tmp)
    })
    
    # remove row
    observeEvent(input$remove_row, {
        dat$df <- dat$df[-nrow(dat$df), ]
    })
    
    # gini_all with selecatble rows ----
    
    # create reactive variable with selected row
    selected_row <- reactiveVal(NULL)
    observeEvent(input$all_rows_selected, {
        selected_row(input$all_rows_selected)
    }, ignoreNULL = FALSE)
    
    # create reactive data frame with all methods maxima
    gini_all <- reactive({
        temp <- all_methods_maxima(beta1 = df()[, 1],
                                   beta2 = df()[, 2],
                                   beta3 = df()[, 3])$All
        temp$Gini_Sum <- round(temp$Gini_Sum, digits = 2)
        return(temp %>% as.data.frame())
    })
    
    # Gini Output ----
    
    # plot of the beta coefficients
    output$gini_plot <- renderPlot({
        if (is.null(selected_row())) {
            ggplot_betas(df(), shifts = c(0, 0, 0))
        } else {
            ggplot_betas(df(), 
                         shifts = gini_all() %>% 
                             slice(selected_row()) %>% 
                             select(starts_with("c")))
        }
        
    })
    
    # tables showing output of all_methods_maxima() function
    output$all <- DT::renderDataTable({
        DT::datatable(gini_all(), rownames = FALSE, selection = "single",
                      options = list(dom = 'plt', 
                                     scrollX = TRUE,
                                     pageLength = 25,
                                     lengthMenu = c(10, 25, 50)))
    })
    
    output$reference <- DT::renderDataTable({
        temp <- all_methods_maxima(beta1 = df()[, 1],
                                   beta2 = df()[, 2],
                                   beta3 = df()[, 3])$Reference
        temp$Gini_Sum <- round(temp$Gini_Sum, digits = 2)
        DT::datatable(temp, rownames = FALSE, selection = "none",
                      options = list(dom = 'plt', 
                                     scrollX = TRUE,
                                     pageLength = 25,
                                     lengthMenu = c(10, 25, 50)))
    })
    
    output$sequential <- DT::renderDataTable({
        temp <- all_methods_maxima(beta1 = df()[, 1],
                                   beta2 = df()[, 2],
                                   beta3 = df()[, 3])$Sequential
        temp$Gini_Sum <- round(temp$Gini_Sum, digits = 2)
        DT::datatable(temp, rownames = FALSE, selection = "none",
                      options = list(dom = 'plt', 
                                     scrollX = TRUE,
                                     pageLength = 25,
                                     lengthMenu = c(10, 25, 50)))
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
