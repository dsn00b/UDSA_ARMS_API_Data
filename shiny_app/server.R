#' @import shiny
#' @import dplyr
#' @import data.table
#' @import stringr
#' @impoty GGally

server <- function(input, output) {
  
  ### cover functionality in "Pull Data" tab
  
  # refresh metadata
  shiny::observeEvent(input$refresh_metadata, {metadata <<- refresh_metadata()})
  
  # pull data based on user selection and render an output message
  shiny::observeEvent(input$pull_data, {
    
    # data pull
    
    state <- if (length(input$states) > 0) {
      metadata$states[metadata$states$name %in% input$states, "code"]
    } else {"all"}
    
    farmtype <- if (length(input$farmtype) > 0) {input$farmtype} else {"all"}
    
    pulled_data <<- pull_data(year = input$year, report = input$report, 
                             state = state, farmtype = farmtype)
    
    # render output message
    if (class(pulled_data) == "character") {
      
      output$data_pull <- shiny::renderText(pulled_data)
      
    } else {
      
      output$data_pull <- shiny::renderText("API Data loaded for Visualisation")
      
    }
    
    ### populate raw data in the "Pulled Data" tab
    
    output$pulled_data <- shiny::renderDataTable(pulled_data)
    
    ### cover functionality in "Visualise" tab
    
    # populate 'variables' widget
    output$variables_from_data <- shiny::renderUI({
      shiny::selectInput(inputId = "variables_visualisation",
                         label = "Select Variable(s) to Visualise",
                         choices = as.list(unique(sliced_data[, var_rep])),
                         multiple = TRUE,
                         selectize = FALSE)
    })
    
    # create remaining functionality in the 'Visualisation' tab, based on user input
    shiny::observeEvent(input$variables_visualisation, {
      
      # process data
      exclude_columns <- c("category2", "category2_value", "variable_id", "variable_name",
                           "variable_sequence", "variable_level", "variable_group",
                           "variable_group_id", "variable_unit", "variable_description",
                           "variable_is_invalid", "median", "statistic", "rse",
                           "unreliable_estimate", "decimal_display", "report")
      
      sliced_data <- pulled_data[pulled_data$var_rep %in% input$variables_visualisation, 
                                 !(colnames(pulled_data) %in% (exclude_columns))]
      
      sliced_data <- data.table::dcast(data.table::as.data.table(sliced_data),
                                       year + state + farmtype + category + category_value ~ var_rep,
                                       value.var = "estimate")
      
      # populate 'avg_by_vars' check box group
      output$avg_by_vars <- shiny::renderUI({
        
        shiny::checkboxGroupInput(inputId = "avg_by_vars_visualisation",
                                  label = "Select more Average Variables by (leave all unselected 
                                          if you don't want to average the variables)",
                                  choiceNames = as.list(c("Year", "State", "Farm Type", "Category")),
                                  choiceValues = 
                                    as.list(c("year", "state", "farmtype", "category_value")),
                                  inline = TRUE)
        
      })
      
      # populate 'analysis variable' radio button
      shiny::observeEvent(input$avg_by_vars_visualisation, {
        
        output$analysis_variable <- shiny::renderUI({
          
          var_names <- gsub("category_value", "Category", input$avg_by_vars_visualisation)
          var_names <- stringr::str_to_title(var_names)
          
          shiny::selectInput(inputId = "analysis_variable_visualisation",
                             label = "Select upto one Analysis Variable",
                             choiceNames = as.list(var_names),
                             choiceValues = as.list(input$avg_by_vars_visualisation),
                             inline = TRUE)
          
        })
        
      })
      
      # populate 'years' widget
      output$years_from_data <- shiny::renderUI({
        
        shiny::selectInput(inputId = "years_visualisation",
                           label = "Filter by Year(s)",
                           choices = as.list(unique(sliced_data$year)),
                           multiple = TRUE)
      })
      
      # populate 'states' drop-down
      output$states_from_data <- shiny::renderUI({
        
        shiny::selectInput(inputId = "states_visualisation",
                           label = "Filter by State(s)",
                           choices = as.list(unique(sliced_data$state)),
                           multiple = TRUE)
        
      })
      
      # populate 'farmtype' drop-down
      output$farmtypes_from_data <- shiny::renderUI({
        
        shiny::selectInput(inputId = "farmtype_visualisation",
                           label = "Filter by Farm Type(s)",
                           choices = as.list(unique(sliced_data$farmtype)))
        
      })
      
      # populate 'category' drop-down
      output$categories_from_data <- shiny::renderUI({
        
        shiny::selectInput(inputId = "category_visualisation",
                           label = "Filter by Category",
                           choices = as.list(unique(sliced_data$category)),
                           selected = "All Farms")
        
      })
      
      # populate 'category_value' drop-down
      shiny::observeEvent(input$category_visualisation, {
        
        output$cat_vals_from_data <- shiny::renderUI({
          
          shiny::selectInput(inputId = "cat_vals_visualisation",
                             label = "Filter by Sub Category(ies)",
                             choices = as.list(unique(
                               sliced_data[sliced_data$category == input$category_visualisation,
                                           "category_value"])))             
          
        })
     
      })
      
      # add note
      output$note <- shiny::renderText("Note: If any filters are to be applied, they will be done
                                       before any aggregation is done (if at all)")
      
      # show plot_button
      output$plot_button <- shiny::renderUI({
        
        shiny::actionButton(inputId = "plot_clicked", label = "Plot")
        
      })
      
      # process data and plot graphs if plot_button is clicked
      shiny::observeEvent(input$plot_clicked, {
        
        ## process data
        
        # filter by category      
        plot_data <- sliced_data[sliced_data$category == input$category_visualisation, 
                                 colnames(sliced_data) != "category"] # filter by category & remove category; it no longer serves any purpose
        # filter by year
        if (length(input$years_visualisation) > 0) {
          
          plot_data <- plot_data[plot_data$year %in% input$years_visualisation, ]
          
        }
        
        # filter by state
        if (length(input$states_visualisation) > 0) {
          
          plot_data <- plot_data[plot_data$state %in% input$states_visualisation, ]
          
        }
        
        # filter by farmtype
        if (length(input$farmtype_visualisation) > 0) {
          
          plot_data <- plot_data[plot_data$farmtype %in% input$farmtype_visualisation, ]
          
        }
        
        # filter by category_value
        if (length(input$cat_vals_visualisation) > 0) {
          
          plot_data <- plot_data[plot_data$category_value %in% input$cat_vals_visualisation, ]
          
        }
        
        # trim and aggregate data
        plot_data <- plot_data %>% 
          dplyr::select(c(input$avg_by_vars_visualisation, input$variables_visualisation))
        
        if (length(input$avg_by_vars_visualisation) > 0) {
          
          plot_data <- plot_data %>% 
            dplyr::group_by_at(input$avg_by_vars_visualisation) %>% 
            dplyr::summarise_at(input$variables_visualisation, mean)

        }
        
        # plot
        
        if (length(input$analysis_variable_visualisation) > 0) {
          
          output$pair_plot <- shiny::renderPlot({
            
            GGally::ggpairs(plot_data[, 
                        c(input$analysis_variable_visualisation, input$variables_visualisation)], 
                        ggplot2::aes(colour=input$analysis_variable_visualisation)) 
            
          })
          
        } else {
          
          output$pair_plot <- shiny::renderPlot({
            
            GGally::ggpairs(plot_data[, input$variables_visualisation]) 
            
          })
          
        }

      })
      
    })
    
  })
  
}