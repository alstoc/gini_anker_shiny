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
            helpText("Please upload a .csv file containing beta coefficients for 3 groups."),
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
                         selected = "all"),
            
            checkboxInput("rownames", "Show Row Names", FALSE)
        ),
        
        
        # Table containing beta coefficients ----
        mainPanel(
            
            # Output: Table of .csv file and diagram with beta coefficients ----
            fluidRow(
                box(width = 3, h3("Original Coefficients"),
                    DT::dataTableOutput("beta_table")),
                box(width = 9, h3("Beta Coefficients"), 
                    plotOutput("gini_plot"))
            ),
            
            br(), 
            
            # Output: Data tables showing output of all_methods_maxima() ----
            fluidRow(
                box(width = 4,
                    h3("All"),
                    h4("Update the diagram by clicking on a row."),
                    DT::dataTableOutput("all")),
                box(width = 4,
                    h3("Reference"),
                    br(),
                    DT::dataTableOutput("reference")),
                box(width = 4,
                    h3("Sequential"),
                    br(),
                    DT::dataTableOutput("sequential"))
            ),
            
            br(),
            br()
            
        )
            
    )
)
    

        


