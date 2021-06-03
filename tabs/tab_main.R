fluidPage(
    
    # Table containing beta coefficients ----
    fluidRow(
        box(
            width = 3,
            title = "Original Coefficients",
            helpText("Double-click on cells to edit. Press Enter to confirm change."),
            DT::dataTableOutput("beta_table"),
            
            # box dropdown ----
            dropdownMenu = boxDropdown(
                boxDropdownItem("Add row", id = "add_row", icon = icon("plus")),
                boxDropdownItem("Remove row", id = "remove_row", icon = icon("minus")),
            ),
            
            # box sidebar ----
            sidebar = boxSidebar(
                id = "beta_sidebar",
                width = 100,
                style = 
                    "padding-left:30px;
                padding-right:40px;",
                background = "#F8F8F8",
                br(),
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
            )
        ),
        box(
            width = 9,
            title = "Beta Coefficients", 
            plotOutput("gini_plot")
        )
    ),
    
    # Output: Data tables showing output of all_methods_maxima() ----
    fluidRow(
        box(width = 4,
            status = "danger",
            title = "All",
            helpText("Update the diagram by clicking on a row."),
            DT::dataTableOutput("all")),
        box(width = 4,
            title = "Reference",
            br(),
            DT::dataTableOutput("reference")),
        box(width = 4,
            title = "Sequential",
            br(),
            DT::dataTableOutput("sequential"))
    ),
    
    br(),
    br()
)





