#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load packages ----
pacman::p_load(
    shiny,  # Shiny web app
    shinydashboard,  # Shiny Dashboards
    shinythemes,  # additional themes for Shiny Dashboards
    DT,  # alternative way of handling data tables
    tidyverse,  # data wrangling and exploration
    ggplot2,  # visualisation
    vroom  # data import
)

# Source required files ----
source("functions/basic_functions.R")
source("functions/first_sims.R")
source("functions/methods_comparison.R")


# Define UI for application ----
ui <- navbarPage(
    "Gini Anker Prototype v0.1", theme = shinytheme("lumen"),
    
    # First Tab                
    tabPanel(
        "Home",
        fluid = TRUE,
        icon = icon("desktop"),
        source("tabs/tab_main.R", 
               local = TRUE, encoding = "utf-8")[1]
    ),
    
    # Second Tab
    tabPanel(
        "Info",
        fluid = TRUE,
        icon = icon("info-circle"),
        source("tabs/tab_info.R", 
               local = TRUE, encoding = "utf-8")[1]
    )
)
    

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # General ----
    
    # create data frame from uploaded .csv file
    df <- eventReactive(input$upload_button, {
        
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
                    
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
    })
    
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
    
    # Original Beta Coefficients Table ----
    
    # table containing original beta coefficients
    output$beta_table <- DT::renderDataTable({
        
        # input$file will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.

        if(input$disp == "head") {
            df() %>% 
                head() %>%
                DT::datatable(rownames = input$rownames,
                              options = list(dom = 't',
                                             scrollY = "400px")) 
                   
        } else {
            DT::datatable(df(), rownames = input$rownames,
                          options = list(dom = 't',
                                         scrollY = "400px"))
        }
    })
    
    
    # Gini Output ----
    
    # plot of the beta coefficients
    output$gini_plot <- renderPlot({
        req(input$file)
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
                      options = list(dom = 't'))
    })
    
    output$reference <- DT::renderDataTable({
        temp <- all_methods_maxima(beta1 = df()[, 1],
                                   beta2 = df()[, 2],
                                   beta3 = df()[, 3])$Reference
        temp$Gini_Sum <- round(temp$Gini_Sum, digits = 2)
        DT::datatable(temp, rownames = FALSE, selection = "none",
                      options = list(dom = 't'))
    })
    
    output$sequential <- DT::renderDataTable({
        temp <- all_methods_maxima(beta1 = df()[, 1],
                                   beta2 = df()[, 2],
                                   beta3 = df()[, 3])$Sequential
        temp$Gini_Sum <- round(temp$Gini_Sum, digits = 2)
        DT::datatable(temp, rownames = FALSE, selection = "none",
                      options = list(dom = 't'))
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
