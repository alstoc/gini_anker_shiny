fluidPage(
    sidebarLayout(
        # Menu for uploading .csv file ----
        sidebarPanel(width = 3,
            # Input: Select a file ----
            fileInput("file", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            h4("File Properties"),
            checkboxInput("header", "Header", TRUE),
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Upload button ----
            actionButton(inputId = "upload_button",
                         label = "Upload CSV file"),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            h4("Data Frame Settings"),
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            
            checkboxInput("rownames", "Show Row Names", FALSE)
        ),
        
        
        # Table containing beta coefficients ----
        mainPanel(
            
            # Output: Table of .csv file and diagram with beta coefficients ----
            fluidRow(
                column(3, DT::dataTableOutput("beta_table")),
                column(9, offset = 0.5, plotOutput("gini_plot"))
            ),
            
            br(), 
            
            # Output: Data tables showing output of all_methods_maxima() ----
            fluidRow(
                column(4,
                       tabsetPanel(
                           type = "tabs",
                           tabPanel("All", DT::dataTableOutput("all"))
                       )
                ),
                column(4, 
                       tabsetPanel(
                           type = "tabs",
                           tabPanel("Reference", DT::dataTableOutput("reference"))
                       )
                ),
                column(4, 
                       tabsetPanel(
                           type = "tabs",
                           tabPanel("Sequential", DT::dataTableOutput("sequential"))
                       )
                )
            )
        )
            
    )
)
    

        


