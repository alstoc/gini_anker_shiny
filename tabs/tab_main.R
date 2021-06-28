fluidPage(
    
    # Table containing beta coefficients ----
    fluidRow(
        box(
            width = 3,
            status = "primary",
            solidHeader = TRUE,
            title = "True Item Parameters",
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
                tags$hr()
            )
        ),
        box(
            width = 9,
            status = "danger",
            solidHeader = TRUE,
            title = "Anchored Estimated Item Parameters", 
            plotOutput("gini_plot")
        )
    ),
    
    # Output: Data tables showing output of all_methods_maxima() ----
    fluidRow(
        box(width = 4,
            status = "danger",
            solidHeader = TRUE,
            title = "All Possible Maxima",
            helpText("Update the diagram by clicking on a row.",
                     br(),
                     "(If the same solution can be found by the other methods, too, 
                     the respective row is highlighted in the other tables.)"),
            DT::dataTableOutput("all")),
        box(width = 4,
            status = "info",
            solidHeader = TRUE,
            title = "Maxima Identified by Reference Group Method",
            br(),
            DT::dataTableOutput("reference")),
        box(width = 4,
            status = "info",
            solidHeader = TRUE,
            title = "Maxima Identified by Sequential Method",
            br(),
            DT::dataTableOutput("sequential"))
    ),
    
    br(),
    br()
)





