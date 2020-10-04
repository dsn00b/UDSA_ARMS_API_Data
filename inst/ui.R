require(shiny)

ui <- shiny::fluidPage(
  
  # App title
  shiny::titlePanel("Visual Exploratory Data Analysis for the USDA ARMS Dataset"),
  
  # Add Nav Bar
  shiny::navbarPage("EDA Tool",
                    
    # Add Get Data Tab
    shiny::tabPanel("Get Data",
                    
      # Button: Refresh Meta-data
      shiny::actionButton(inputId = "refresh_metadata", label = "Refresh Meta-data"),
      
      # Input: Multi-Select Drop-down Box for 'year'
      shiny::uiOutput(outputId = "year_pull_data"),
      
      # Input: Multi-Select Drop-down Box for 'report'
      shiny::uiOutput(outputId = "report_pull_data"),
      
      # Input: Multi-Select Drop-down Box for 'state'
      shiny::uiOutput(outputId = "states_pull_data"),
      
      # Input: Multi-Select Drop-down Box for 'farmtype'
      shiny::uiOutput(outputId = "farmtype_pull_data"),      
      
      # Button: Pull Data
      shiny::actionButton(inputId = "pull_data", label = "Pull Data")        
    ),
    
    # Add Visualise Tab
    shiny::tabPanel("Visualise",
                    
      # Sidebar layout with input and output definitions
      shiny::sidebarLayout(
        
        # Sidebar panel for inputs
        shiny::sidebarPanel(
          
          # Input: Multi-Select Drop-down Box for 'variable'
          shiny::uiOutput(outputId = "variables_from_data"),            
          
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

          # Input: Check Box Group for 'average by' variables
          shiny::uiOutput(outputId = "avg_by_vars"),
          
          # Add Note
          shiny::uiOutput(outputId = "note"),
          
          # Input: Radio Box to identify Analysis Variable
          shiny::uiOutput(outputId = "analysis_variable"),
          
          # Button: Corr Plot
          shiny::uiOutput(outputId = "corr_plot_button"),
          
          # Button: Pair Plot
          shiny::uiOutput(outputId = "pair_plot_button")
          
        ),
        
        # Main panel for displaying outputs
        shiny::mainPanel(
          
          # Output: Plots
          shiny::plotOutput(outputId = "plots"),
          
          # Output: Legend
          shiny::dataTableOutput(outputId = "legend")
          
        )
      )
    ),
    
    # Add Pulled Data Tab
    shiny::tabPanel("Pulled Data",
                    
      # Output: Table
      shiny::dataTableOutput(outputId = "pulled_data"),
      
      # Button: Download as CSV
      shiny::actionButton(inputId = "download_csv",
                          label = "Download Data as CSV")
                    
    )
  )
)