#' @import shiny
#' @import dplyr

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
          
          # Input: Multi-Select Drop-down Box for 'year'
          shiny::uiOutput(outputId = "years_from_data"),
          
          # Input: Multi-Select Drop-down Box for 'variable'
          shiny::uiOutput(outputId = "variables_from_data"),  
          
          # Input: Radio Box to identify Analysis Variable
          shiny::uiOutput(outputId = "analysis_variable"),
          
          # Input: Multi-Select Drop-down Box for 'state'
          shiny::uiOutput(outputId = "states_from_data"),
          
          # Input: Multi-Select Drop-down Box for 'farmtype'
          shiny::uiOutput(outputId = "farmtypes_from_data"),      
          
          # Input: Multi-Select Drop-down Box for 'category'
          shiny::uiOutput(outputId = "categories_from_data"), 
          
          # Button: Plot
          shiny::uiOutput(outputId = "plot_button")
          
        ),
        
        # Main panel for displaying outputs
        mainPanel(
          
          # Output: Pair Plots
          plotOutput(outputId = "pair_plot")
          
        )
      )
    ),
    
    # Add Pulled Data Tab
    tabPanel("Pulled Data",
             
      # Output: Table
      tableOutput(outputID = "pulled_data")
    
    )
  )
)

server <- function(input, output) {

  ### cover functionality in "Pull Data" tab
  
  # refresh metadata
  shiny::observeEvent(input$refresh_metadata, {metadata <<- refresh_metadata()})
  
  # pull data based on user selection and render an output message
  shiny::observeEvent(input$pull_data, {
    
    # data pull
    state <- if (length(input$state) > 0) {"all"} 
      else {metadata$states[metadata$states$name %in% input$state, "code"]}
    
    farmtype <- if (length(input$farmtype) > 0) {input$farmtype} else {"all"}
    
    pulled_data <<- pull_data(year = input$year, report = input$report, 
                             state = state, farmtype = farmtype)
    # render output message
    if (class(pulled_data) == "character") {
      
      output$data_pull <- shiny::renderText(pulled_data)
      
    } else {
      
      output$data_pull <- shiny::renderText("API Data loaded for Visualisation")
      
    }
    
    ### cover functionality in "Visualise" tab
 
    # populate 'variables' widget
    output$variables_from_data <- shiny::renderUI({
      shiny::selectInput(inputId = "variables_visualisation",
                         label = "Select Variable(s) to Visualise",
                         choices = list(unique(sliced_data[, var_rep])),
                         multiple = TRUE,
                         selectize = FALSE)
    })
    
    # create remaining functionality in the 'Visualisation' tab, based on user input
    shiny::observeEvent(input$variables_visualisation, {
      
      # subset data
      
      exclude_columns <- c("category2", "category2_value", "variable_id", "variable_name",
                           "variable_sequence", "variable_level", "variable_group",
                           "variable_group_id", "variable_unit", "variable_description",
                           "variable_is_invalid", "median", "statistic", "rse",
                           "unreliable_estimate", "decimal_display")

      sliced_data <<- pulled_data[pulled_data$report %in% input$variables_visualisation, 
                                  !(colnames(pulled_data) %in% (exclude_columns))]
      
      # populate 'analysis variable' radio button
      output$analysis_variable <- shiny::renderUI({
        
        shiny::radioButtons(inputId = "analysis_variable_selected",
                            label = "Select an Analysis Variable",
                            choices = list(c("Year", "State", "Farm Type", "Category", "None")),
                            inline = TRUE,
                            selected = "None")
        
      })
      
      # populate 'years' widget
      output$years_from_data <- shiny::renderUI({
        shiny::selectInput(inputId = "years_visualisation",
                           label = "Filter by Year(s)",
                           choices = as.list(unique(sliced_data$year)),
                           multiple = TRUE,
                           selectize = FALSE)
      })

      # populate 'states' drop-down
      output$states_from_data <- shiny::renderUI({
        
        shiny::selectInput(inputId = "states_visualisation",
                           label = "Filter by State(s)",
                           choices = list(unique(sliced_data$state)),
                           multiple = TRUE)
        
      })
      
      # populate 'farmtype' drop-down
      output$farmtypes_from_data <- shiny::renderUI({
        
        shiny::selectInput(inputId = "farmtype_visualisation",
                           label = "Select Farm Type(s) to Drill Down into",
                           choices = list(unique(sliced_data[, farmtype])),
                           selected = "All Farms")
        
      })
      
      # populate 'category' drop-down
      output$categories_from_data <- shiny::renderUI({
        
        shiny::selectInput(inputId = "farmtype_visualisation",
                           label = "Select Category to Drill Down into",
                           choices = list(unique(sliced_data[, category])),
                           selected = "All Farms")
        
      })
      
      ## populate other drop-downs on the basis of 'analysis variable' selected by the user
      
      shiny::observeEvent(input$analysis_variable_selected & input$variables_visualisation, {
        
      })
      
    })
    
  })
  
}

shinyApp(ui = ui, server = server)