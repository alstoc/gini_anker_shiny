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

# Define comparison function
compare <- function(main, comparison) {
    rows <- list(NA)
    df1 <- main %>% 
        select(c1, c2, c3) %>% 
        mutate(content = NA)
    df2 <- comparison %>% 
        select(c1, c2, c3) %>% 
        mutate(content = NA)
    
    df1$content <- apply(df1[, 1:3], 1, paste0, sep = "", collapse = "")
    df2$content <- apply(df2[, 1:3], 1, paste0, sep = "", collapse = "")
    
    for (i in 1:nrow(df1)) {
        if (df2$content %in% df1$content[i] %>% any()) {
            # store row numbers of rows with same values
            same_rows <- df2$content %in% df1$content[i] %>% which()
            rows[[i]] <- same_rows
        } else {
            rows[[i]] <- NA
        }
    }
    print(rows)
    return(rows)
}

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
    df <- reactive({
        dat$df %>% 
            mutate(Item = 1:nrow(dat$df)) %>% 
            select(Item, everything())
    })
    
    # table containing original beta coefficients
    output$beta_table <- DT::renderDataTable({
        #print(df())
        DT::datatable(isolate(df()), rownames = FALSE,
                      selection = "none",
                      editable = TRUE,
                      options = list(dom = 't', 
                                     scrollX = TRUE,
                                     pageLength = 100,
                                     ordering = FALSE))
    })
    
    proxy <- dataTableProxy("beta_table")
    
    observe({
        proxy %>% replaceData(df(), rownames = FALSE)
    })
    
    # edit cells of df
    observeEvent(input$beta_table_cell_edit, {
        
        info <- input$beta_table_cell_edit
        #str(info)
        row   <- info$row
        col   <- info$col
        value <- info$value %>% as.numeric()
        
        dat$df[row, col] <- value
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
    
    # create reactive list with all_methods_maxima results
    gini_all <- reactive({
        temp <- all_methods_maxima(beta1 = df()[, 2],
                                   beta2 = df()[, 3],
                                   beta3 = df()[, 4])
        temp$All <- temp$All %>%
            as.data.frame() %>% 
            arrange(desc(Gini_Sum)) %>% 
            rename("Gini Sum" = Gini_Sum)
        temp$All$"Gini Sum" <- round(temp$All$"Gini Sum", digits = 2)
        
        temp$Reference <- temp$Reference %>% 
            as.data.frame() %>% 
            arrange(desc(Gini_Sum)) %>% 
            rename("Gini Sum" = Gini_Sum)
        temp$Reference$"Gini Sum" <- round(temp$Reference$"Gini Sum", digits = 2)
        
        temp$Sequential <- temp$Sequential %>% 
            as.data.frame() %>% 
            arrange(desc(Gini_Sum)) %>% 
            rename("Gini Sum" = Gini_Sum)
        temp$Sequential$"Gini Sum" <- round(temp$Sequential$"Gini Sum", digits = 2)
        
        temp$All <- temp$All %>%
            mutate(same_reference = compare(temp$All, temp$Reference),
                   same_sequential = compare(temp$All, temp$Sequential))
        
        return(temp)
    })
    
    # Gini Output ----
    
    # plot of the beta coefficients
    output$gini_plot <- renderPlot({
        temp <- gini_all()$All %>% as.data.frame()
        if (is.null(selected_row())) {
            ggplot_betas(df()[, -1], shifts = c(0, 0, 0))
        } else {
            ggplot_betas(df()[, -1], 
                         shifts = temp %>% 
                             slice(selected_row()) %>% 
                             select(starts_with("c")))
        }
        
    })
    
    # tables showing output of all_methods_maxima() function
    output$all <- DT::renderDataTable({
        temp <- gini_all()$All %>% as.data.frame()
        DT::datatable(temp, rownames = FALSE, selection = "single",
                      options = list(dom = 't', 
                                     scrollX = TRUE,
                                     pageLength = 1000,
                                     order = list(4, "desc"),
                                     ordering = FALSE))
    })
    
    output$reference <- DT::renderDataTable({
        temp <- gini_all()$Reference %>% as.data.frame()
        if (is.null(selected_row())) {
            highlight <- NA
        } else {
            highlight <- gini_all()$All$same_reference[[selected_row()]]
        }
        DT::datatable(temp, rownames = FALSE, 
                      selection = list(mode = "multiple",
                                       selected = highlight),
                      options = list(dom = 't', 
                                     scrollX = TRUE,
                                     pageLength = 100,
                                     order = list(5, "desc"),
                                     ordering = FALSE)) 
    })
    
    output$sequential <- DT::renderDataTable({
        temp <- gini_all()$Sequential %>% as.data.frame()
        if (is.null(selected_row())) {
            highlight <- NA
        } else {
            highlight <- gini_all()$All$same_sequential[[selected_row()]]
        }
        DT::datatable(temp, rownames = FALSE, 
                      selection = list(mode = "multiple",
                                       selected = highlight),
                      options = list(dom = 't', 
                                     scrollX = TRUE,
                                     pageLength = 100,
                                     order = list(6, "desc"),
                                     ordering = FALSE))
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
