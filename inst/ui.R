require(shiny)

ui <- shiny::fluidPage(
  
  # App title
  shiny::titlePanel("Visual Exploratory Data Analysis for the USDA ARMS Dataset"),
  
  # Add Nav Bar
  shiny::navbarPage("EDA Tool",
                    
                    # Add Get Data Tab
                    shiny::tabPanel("Get Data",
                                    
                                    # Sidebar layout with input and output definitions
                                    shiny::sidebarLayout(
                                      
                                      # Sidebar panel for inputs
                                      shiny::sidebarPanel(
                                        
                                        # Button: Refresh Meta-data
                                        shiny::actionButton(inputId = "refresh_metadata",
                                                            label = "Refresh Meta-data",
                                                            icon = shiny::icon("sync")),
                                        
                                        # Input: Multi-Select Drop-down Box for 'year'
                                        shiny::selectInput(inputId = "year",
                                                           label = "Years of Data to Pull (choose at least one)",
                                                           choices = as.list(metadata$years),
                                                           selected = metadata$years[1],
                                                           multiple = TRUE,
                                                           selectize = FALSE),
                                        
                                        # Input: Multi-Select Drop-down Box for 'report'
                                        shiny::selectInput(inputId = "report",
                                                           label = "Reports to Pull (choose at least one)",
                                                           choices = as.list(metadata$reports),
                                                           selected = metadata$reports[1],
                                                           multiple = TRUE,
                                                           selectize = FALSE),
                                        
                                        # Input: Multi-Select Drop-down Box for 'state'
                                        shiny::selectInput(inputId = "states",
                                                           label = "States Filter",
                                                           choices = as.list(metadata$states$name),
                                                           multiple = TRUE),
                                        
                                        # Input: Multi-Select Drop-down Box for 'farmtype'
                                        shiny::selectInput(inputId = "farmtype",
                                                           label = "Farmtype Filter",
                                                           choices = as.list(metadata$farmtypes$name),
                                                           multiple = TRUE),      
                                        
                                        # Button: Pull Data
                                        shiny::actionButton(inputId = "pull_data",
                                                            label = "Pull Data",
                                                            icon = shiny::icon("sync")),         
                                      ),
                                      
                                      # Main panel for displaying outputs
                                      mainPanel(
                                        
                                        # Output: Text based on API call
                                        textOutput(outputId = "data_pull")
                                        
                                      )
                                    )
                    ),
                    
                    # Add Visualise Tab
                    tabPanel("Visualise",
                             
                             # Sidebar layout with input and output definitions
                             shiny::sidebarLayout(
                               
                               # Sidebar panel for inputs
                               shiny::sidebarPanel(
                                 
                                 # Input: Multi-Select Drop-down Box for 'variable'
                                 shiny::uiOutput(outputId = "variables_from_data"),            
                                 
                                 # Input: Check Box Group for 'average by' variables
                                 shiny::uiOutput(outputId = "avg_by_vars"),
                                 
                                 # Input: Radio Box to identify Analysis Variable
                                 shiny::uiOutput(outputId = "analysis_variable"),
                                 
                                 # Input: Multi-Select Drop-down Box for 'year'
                                 shiny::uiOutput(outputId = "years_from_data"),          
                                 
                                 # Input: Multi-Select Drop-down Box for 'state'
                                 shiny::uiOutput(outputId = "states_from_data"),
                                 
                                 # Input: Multi-Select Drop-down Box for 'farmtype'
                                 shiny::uiOutput(outputId = "farmtypes_from_data"),      
                                 
                                 # Input: Single-Select Drop-down Box for 'category'
                                 shiny::uiOutput(outputId = "categories_from_data"), 
                                 
                                 # Input: Multi-Select Drop-down Box for 'category_values'
                                 shiny::uiOutput(outputId = "cat_vals_from_data"),                                 
                                 
                                 # Add Note
                                 shiny::uiOutput(outputId = "note"),
                                 
                                 # Button: Corr Plot
                                 shiny::uiOutput(outputId = "corr_plot_button"),
                                 
                                 # Button: Pair Plot
                                 shiny::uiOutput(outputId = "pair_plot_button")
                                 
                               ),
                               
                               # Main panel for displaying outputs
                               mainPanel(
                                 
                                 # Output: Plots
                                 plotOutput(outputId = "plots")
                                 
                               )
                             )
                    ),
                    
                    # Add Pulled Data Tab
                    tabPanel("Pulled Data",
                             
                             # Output: Table
                             dataTableOutput(outputId = "pulled_data")
                             
                    )
  )
)