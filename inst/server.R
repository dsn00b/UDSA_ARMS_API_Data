require(shiny)
require(dplyr)
require(data.table)
require(stringr)
require(GGally) 
require(magrittr)

server <- function(input, output) {
  
  ### 1. Set up "Get Data" tab
  
  ## 1.1. check if metadata exists, if not, get it populated
  if (!exists("usda.arms.api.env")) {usda.arms.api.env <<- new.env()}
  
  if(!exists("metadata", envir = usda.arms.api.env)) {
    
    usda.arms.api.env$metadata <<- refresh_metadata()
    
    if (class(usda.arms.api.env$metadata) == "character") {
      
      shiny::showModal(modalDialog(title = "Error", paste(usda.arms.api.env$metadata, 
        ". Please try again")))
      
    } else {
      
      shiny::showModal(modalDialog(title = "Success", "Latest metadata has been fetched
        and the app will be ready to use when you dismiss this message"))
      
    }
  
  }
  
  ## 1.2. populate widgets on 'Pull Data' tab
  
  if(exists("metadata", envir = usda.arms.api.env)) {
  
    # 1.2.1. 'year' widget
    
    output$year_pull_data <- renderUI({
      
      shiny::selectInput(inputId = "year",
                         label = "Years of Data to Pull (choose at least one)",
                         choices = as.list(usda.arms.api.env$metadata$years),
                         selected = usda.arms.api.env$metadata$years[1],
                         multiple = TRUE,
                         selectize = FALSE)
      
    })
    
    # 1.2.2. 'report' widget
    output$report_pull_data <- renderUI({
      
      shiny::selectInput(inputId = "report",
                         label = "Reports to Pull (choose at least one)",
                         choices = as.list(usda.arms.api.env$metadata$reports),
                         selected = usda.arms.api.env$metadata$reports[1],
                         multiple = TRUE,
                         selectize = FALSE)
      
    })
    
    # 1.2.3. 'states' widget
    output$states_pull_data <- renderUI({
      
      shiny::selectInput(inputId = "states",
                         label = "States Filter",
                         choices = as.list(usda.arms.api.env$metadata$states$name),
                         multiple = TRUE)
      
    })
    
    # 1.2.4. 'farmtype' widget
    output$farmtype_pull_data <- renderUI({
      
      shiny::selectInput(inputId = "farmtype",
                         label = "Farmtype Filter",
                         choices = as.list(usda.arms.api.env$metadata$farmtypes$name),
                         multiple = TRUE)
      
    })
    
    # 1.2.5. refresh metadata button
    shiny::observeEvent(input$refresh_metadata, {
      
      usda.arms.api.env$metadata <<- refresh_metadata()
      
      if (class(usda.arms.api.env$metadata) == "character") {
        
        shiny::showModal(modalDialog(title = "Error", paste(usda.arms.api.env$metadata, 
                                                            ". Please try again")))
        
      } else {
        
        shiny::showModal(modalDialog(title = "Success", "Latest metadata has been fetched
        and the app will be ready to use when you dismiss this message"))
        
      }
      
    })
    
    ## 1.3. events triggered by pull data button 
    shiny::observeEvent(input$pull_data, {
      
      # 1.3.1 (re)-initialise app-level variables
      if ("pulled_data" %in% ls(usda.arms.api.env)) {
        
        rm("pulled_data", envir = usda.arms.api.env)
        
      }
      
      # 1.3.2. stage the data pull
      state <- if (length(input$states) > 0) {
        
        usda.arms.api.env$metadata$states[
          usda.arms.api.env$metadata$states$name %in% input$states, "code"]
        
      } else {"all"}
      
      farmtype <- if (length(input$farmtype) > 0) {input$farmtype} else {"all"}
      
      usda.arms.api.env$pulled_data <<- pull_data(year = input$year,
                                                  report = input$report, state = state, farmtype = farmtype)
      
      # 1.3.3. handle data pull success / failure
      if (class(usda.arms.api.env$pulled_data) == "character") { # failure
        
        # 1.3.3.1. display error message
        shiny::showModal(modalDialog(title = "Error Message", pulled_data))
        
        # 1.3.3.2. invalidate all widgets on the 'visualise' and 'pulled data' tab
        output$variables_from_data <- shiny::renderText("")
        output$years_from_data <- shiny::renderText("")
        output$states_from_data <- shiny::renderText("")
        output$farmtypes_from_data <- shiny::renderText("")
        output$categories_from_data <- shiny::renderText("")
        output$cat_vals_from_data <- shiny::renderText("")
        output$avg_by_vars <- shiny::renderText("")
        output$note <- shiny::renderText("")
        output$analysis_variable <- shiny::renderText("")
        output$corr_plot_button <- shiny::renderText("")
        output$pair_plot_button <- shiny::renderText("")
        output$plots <- shiny::renderText("")
        output$legend <- shiny::renderText("")
        output$pulled_data <- shiny::renderText("")
        
      } else { # success
        
        # 1.3.3.3. set up 'visualise' tab
        
        # 1.3.3.3.1. populate "Variables" widget
        output$variables_from_data <- shiny::renderUI({
          
          shiny::selectInput(inputId = "variables_visualisation",
                             label = "Select Variable(s) to Visualise",
                             choices = as.list(unique(usda.arms.api.env$pulled_data$var_rep)),
                             multiple = TRUE,
                             selectize = FALSE)
          
        })
        
        # 1.3.3.3.2. invalidate all other widgets on the tab
        output$years_from_data <- shiny::renderText("")
        output$states_from_data <- shiny::renderText("")
        output$farmtypes_from_data <- shiny::renderText("")
        output$categories_from_data <- shiny::renderText("")
        output$cat_vals_from_data <- shiny::renderText("")
        output$avg_by_vars <- shiny::renderText("")
        output$note <- shiny::renderText("")
        output$analysis_variable <- shiny::renderText("")
        output$corr_plot_button <- shiny::renderText("")
        output$pair_plot_button <- shiny::renderText("")
        output$plots <- shiny::renderText("")
        output$legend <- shiny::renderText("")
        
        # 1.3.3.4. set up 'pulled data' tab
        output$pulled_data <- shiny::renderDataTable(usda.arms.api.env$pulled_data)
        
        # 1.3.3.5. show user success message
        shiny::showModal(modalDialog(
          title = "Success!", 
          "API Data loaded for Visualisation. See the 'Visualise' tab or 
        for some options to visually explore data variables (OR) view/download
        the loaded API data on the 'Pulled Data' tab"))
        
      }
      
    })
    
    ### 2. Set up "Visualise" tab
    shiny::observeEvent(input$variables_visualisation, {
      
      # 2.1. cull down a copy of pulled_data for leaner operations downstream
      exclude_columns <- c("category2", "category2_value", "variable_id",
                           "variable_name", "variable_sequence", "variable_level", "variable_group",
                           "variable_group_id", "variable_unit", "variable_description",
                           "variable_is_invalid", "median", "statistic", "rse", "unreliable_estimate",
                           "decimal_display", "report")
      
      usda.arms.api.env$sliced_data <<- usda.arms.api.env$pulled_data[
        usda.arms.api.env$pulled_data$var_rep %in% input$variables_visualisation, 
        !(colnames(usda.arms.api.env$pulled_data) %in% (exclude_columns))]
      
      usda.arms.api.env$sliced_data <<- data.table::dcast(
        data.table::as.data.table(usda.arms.api.env$sliced_data),
        year + state + farmtype + category + category_value ~ var_rep, 
        value.var = "estimate")
      
      # 2.2. set up the drop-down widgets and plot buttons
      
      # 2.2.1. 'years_from_data' drop-down
      output$years_from_data <- shiny::renderUI({
        
        shiny::selectInput(inputId = "years_visualisation",
                           label = "Filter by Year(s)",
                           choices = as.list(unique(usda.arms.api.env$sliced_data$year)),
                           multiple = TRUE)
      })
      
      # 2.2.2. 'states_from_data' drop-down
      output$states_from_data <- shiny::renderUI({
        
        shiny::selectInput(inputId = "states_visualisation",
                           label = "Filter by State(s)",
                           choices = as.list(unique(usda.arms.api.env$sliced_data$state)),
                           multiple = TRUE)
      })
      
      # 2.2.3. 'farmtypes_from_data' drop-down
      output$farmtypes_from_data <- shiny::renderUI({
        
        shiny::selectInput(inputId = "farmtype_visualisation",
                           label = "Filter by Farm Type(s)",
                           choices = as.list(unique(usda.arms.api.env$sliced_data$farmtype)),
                           multiple = TRUE)
      })   
      
      # 2.2.4. 'categories_from_data' drop-down
      output$categories_from_data <- shiny::renderUI({
        
        shiny::selectInput(inputId = "category_visualisation",
                           label = "Filter by Category",
                           choices = as.list(unique(usda.arms.api.env$sliced_data$category)),
                           selected = unique(usda.arms.api.env$sliced_data$category)[1])
      })    
      
      # 2.2.5. 'cat_vals_from_data' drop-down
      shiny::observeEvent(input$category_visualisation, {
        
        cat_val_list <- as.list(unique(usda.arms.api.env$sliced_data[
          usda.arms.api.env$sliced_data$category == 
            input$category_visualisation, "category_value"]))
        
        if (length(cat_val_list[[1]]) == 1) {
          
          names(cat_val_list) <- cat_val_list[[1]]
          
        }
        
        output$cat_vals_from_data <- shiny::renderUI({
          
          shiny::selectInput(inputId = "cat_vals_visualisation",
                             label = "Filter by Sub Category(ies)",
                             choices = cat_val_list,
                             multiple = TRUE)             
          
        })
        
      })
      
      # 2.2.6. 'average by variables' check-box
      output$avg_by_vars <- shiny::renderUI({
        
        shiny::checkboxGroupInput(inputId = "avg_by_vars_visualisation",
                                  label = "Select one or more fields to Average Variables by (leave 
          all unselected if you don't want to average the variables)",
                                  choiceNames = as.list(c("Year", "State", "Farm Type", "Category")),
                                  choiceValues =  as.list(c("year", "state", "farmtype", "category_value")),
                                  inline = TRUE)
        
      })
      
      # 2.2.7. 'note'
      output$note <- shiny::renderText("Note: If any filters are to be applied, 
      they will be done before any aggregation is done (if at all)")
      
      # 2.2.8. 'analysis_variable' drop-down
      shiny::observeEvent(input$avg_by_vars_visualisation, {
        
        if (length(input$avg_by_vars_visualisation) > 0) {
          
          possible_an_var <- vector()
          
          for (i in input$avg_by_vars_visualisation) {
            
            if (length(unlist(unique(subset(
              usda.arms.api.env$sliced_data, select = i)))) > 1) {
              
              possible_an_var <- c(possible_an_var, i)
              
            }
            
          }

          if (length(possible_an_var) > 0) {
            
            var_names <- gsub("category_value", "Category", possible_an_var)
            var_names <- stringr::str_to_title(var_names)
            
            output$analysis_variable <- shiny::renderUI({
              
              shiny::radioButtons(inputId = "analysis_variable_visualisation",
                                  label = "Select upto one Analysis Variable",
                                  choiceNames = as.list(c(var_names, "None")),
                                  choiceValues = as.list(c(possible_an_var, "None")),
                                  selected = "None",
                                  inline = TRUE)
              
            })
            
          } else {
            
            output$analysis_variable <- shiny::renderText("")
            
          }
          
        } else {
          
          output$analysis_variable <- shiny::renderText("")
          
        }
        
      })
      
      # 2.2.9. 'corr_plot_button'
      output$corr_plot_button <- shiny::renderUI({
        
        shiny::actionButton(inputId = "corr_plot_clicked", 
                            label = "Generate Correlation Plots")
        
      })
      
      # 2.2.10. 'pair_plot_button'
      output$pair_plot_button <- shiny::renderUI({
        
        shiny::actionButton(inputId = "pair_plot_clicked",
                            label = "Generate Pair Plots")
        
      })
      
      # 2.3. set up plots
      
      # 2.3.1. generate plot for corr plot button click
      shiny::observeEvent(input$corr_plot_clicked, {
        
        # 2.3.1.1. cull down a copy of sliced_data for leaner operations downstream
        
        # 2.3.1.1.1. filter by category, because one (and only one) category must be selected
        usda.arms.api.env$plot_data <<- shiny::isolate(
          usda.arms.api.env$sliced_data[
          usda.arms.api.env$sliced_data$category == input$category_visualisation, ])
        
        # 2.3.1.1.2. filter by year
        if (nrow(usda.arms.api.env$plot_data) > 0 & 
            length(input$years_visualisation) > 0) {
          
          usda.arms.api.env$plot_data <<- shiny::isolate(
            usda.arms.api.env$plot_data[
            usda.arms.api.env$plot_data$year %in% input$years_visualisation, ])
          
        }
        
        # 2.3.1.1.3. filter by state
        if (nrow(usda.arms.api.env$plot_data) > 0 &
            length(input$states_visualisation) > 0) {
          
          usda.arms.api.env$plot_data <<- shiny::isolate(
            usda.arms.api.env$plot_data[
            usda.arms.api.env$plot_data$state %in% input$states_visualisation, ])
          
        }
        
        # 2.3.1.1.4. filter by farmtype
        if (nrow(usda.arms.api.env$plot_data) > 0 &
            length(input$farmtype_visualisation) > 0) {
          
          usda.arms.api.env$plot_data <<- shiny::isolate(
            usda.arms.api.env$plot_data[
            usda.arms.api.env$plot_data$farmtype %in% input$farmtype_visualisation, ])
          
        }
        
        # 2.3.1.1.5. filter by category_value
        if (nrow(usda.arms.api.env$plot_data) > 0 &
            length(input$cat_vals_visualisation) > 0) {
          
          usda.arms.api.env$plot_data <<- shiny::isolate(
            usda.arms.api.env$plot_data[
            usda.arms.api.env$plot_data$category_value %in% 
              input$cat_vals_visualisation, ])
          
        }
        
        # 2.3.1.1.6. trim further to only keep variables and avg_by_vars (if any) & aggregate (if needed)
        if (nrow(usda.arms.api.env$plot_data) > 0) {
          
          if (length(input$avg_by_vars_visualisation) > 0) {
            
            usda.arms.api.env$plot_data <<- shiny::isolate(
              usda.arms.api.env$plot_data %>% 
              dplyr::select(c(input$avg_by_vars_visualisation, 
              input$variables_visualisation)) %>% 
              dplyr::group_by_at(input$avg_by_vars_visualisation) %>% 
              dplyr::summarise_at(input$variables_visualisation, mean))
            
          }
          
          else {
            
            usda.arms.api.env$plot_data <<- shiny::isolate(
              usda.arms.api.env$plot_data %>% 
              dplyr::select(input$variables_visualisation))
              
          }
          
        }
        
        # 2.3.1.1.7. make variable names readable
        usda.arms.api.env$plot_data <<- shiny::isolate(
          data.table::setnames(usda.arms.api.env$plot_data,
          old = input$variables_visualisation,
          new = paste0("Var_", 1:(length(input$variables_visualisation)))))
        
        # 2.3.1.2. generate corr plot
        if (nrow(usda.arms.api.env$plot_data) > 0) {
          
          output$plots <- shiny::renderPlot({
            
            GGally::ggcorr(isolate(subset(usda.arms.api.env$plot_data, 
              select = paste0("Var_", 1:(length(input$variables_visualisation))))))
            
          })
          
        } else {
          
          output$plots <- shiny::renderText("Filters are too restrictive; 
              relax one or more filters")
          
        }

        # 2.3.1.3. generate legend
        if (nrow(usda.arms.api.env$plot_data) > 0) {
          
          usda.arms.api.env$legend <<- isolate(cbind(
            Variable = paste0("Var_", 1:(length(input$variables_visualisation))),
            Legend = input$variables_visualisation))
          
          output$legend <- shiny::renderDataTable(usda.arms.api.env$legend)
          
        }
        
      })
      
      # 2.3.2. plot for pair plot button click
      shiny::observeEvent(input$pair_plot_clicked, {
        
        # 2.3.2.1. cull down a copy of sliced_data for leaner operations downstream
        
        # 2.3.2.1.1. filter by category, because one (and only one) category must be selected
        usda.arms.api.env$plot_data <<- shiny::isolate(
          usda.arms.api.env$sliced_data[
          usda.arms.api.env$sliced_data$category == input$category_visualisation, ])
        
        # 2.3.2.1.2. filter by year
        if (nrow(usda.arms.api.env$plot_data) > 0 & 
            length(input$years_visualisation) > 0) {
          
          usda.arms.api.env$plot_data <<- shiny::isolate(
            usda.arms.api.env$plot_data[
            usda.arms.api.env$plot_data$year %in% input$years_visualisation, ])
          
        }
        
        # 2.3.2.1.3. filter by state
        if (nrow(usda.arms.api.env$plot_data) > 0 &
            length(input$states_visualisation) > 0) {
          
          usda.arms.api.env$plot_data <<- shiny::isolate(
            usda.arms.api.env$plot_data[
            usda.arms.api.env$plot_data$state %in% input$states_visualisation, ])
          
        }
        
        # 2.3.2.1.4. filter by farmtype
        if (nrow(usda.arms.api.env$plot_data) > 0 &
            length(input$farmtype_visualisation) > 0) {
          
          usda.arms.api.env$plot_data <<- shiny::isolate(
            usda.arms.api.env$plot_data[
            usda.arms.api.env$plot_data$farmtype %in% 
            input$farmtype_visualisation, ])
          
        }
        
        # 2.3.2.1.5. filter by category_value
        if (nrow(usda.arms.api.env$plot_data) > 0 &
            length(input$cat_vals_visualisation) > 0) {
          
          usda.arms.api.env$plot_data <<- shiny::isolate(
            usda.arms.api.env$plot_data[
            usda.arms.api.env$plot_data$category_value %in% 
              input$cat_vals_visualisation, ])
          
        }
        
        # 2.3.2.1.6. trim further to only keep variables and avg_by_vars (if any) and aggregate data (if needed)
        if (nrow(usda.arms.api.env$plot_data) > 0) {
          
          if (length(input$avg_by_vars_visualisation) > 0) {
            
            usda.arms.api.env$plot_data <<- shiny::isolate(
              usda.arms.api.env$plot_data %>% 
              dplyr::select(c(input$avg_by_vars_visualisation, 
              input$variables_visualisation)) %>% 
              dplyr::group_by_at(input$avg_by_vars_visualisation) %>% 
              dplyr::summarise_at(input$variables_visualisation, mean))
            
          }
          
          else {
            
            usda.arms.api.env$plot_data <<- shiny::isolate(
              usda.arms.api.env$plot_data %>% 
              dplyr::select(input$variables_visualisation))          
          }
          
        }
        
        # 2.3.2.1.7. make variable names readable
        usda.arms.api.env$plot_data <<- shiny::isolate(
          data.table::setnames(usda.arms.api.env$plot_data,
          old = input$variables_visualisation,
          new = paste0("Var_", 1:(length(input$variables_visualisation)))))
        
        # 2.3.2.2. generate pair plots
        if (nrow(usda.arms.api.env$plot_data) > 0) {
          
          if (length(input$analysis_variable_visualisation) > 0) {
            
            if (input$analysis_variable_visualisation!= "None") {
              
              output$plots <- shiny::renderPlot({
                
                GGally::ggpairs(shiny::isolate(subset(usda.arms.api.env$plot_data,
                  select = paste0("Var_", 1:(length(input$variables_visualisation))))), 
                  ggplot2::aes(colour=input$analysis_variable_visualisation))
                
              })
              
            } else {
              
              output$plots <- shiny::renderPlot({
                
                GGally::ggpairs(shiny::isolate(subset(usda.arms.api.env$plot_data, 
                  select = paste0("Var_", 1:(length(input$variables_visualisation))))))
                
              })
              
            }
            
          } else {
            
            output$plots <- shiny::renderPlot({
              
              GGally::ggpairs(isolate(subset(usda.arms.api.env$plot_data, 
                select = paste0("Var_", 1:(length(input$variables_visualisation))))))
              
            })
            
          }
          
        } else {
          
          output$plots <- shiny::renderText("Filters are too restrictive; 
          relax one or more filters")
          
        }
        
        # 2.3.2.3. generate legend
        if (nrow(usda.arms.api.env$plot_data) > 0) {
          
          usda.arms.api.env$legend <<- isolate(cbind(
            Variable = paste0("Var_", 1:(length(input$variables_visualisation))),
            Legend = input$variables_visualisation))
          
          output$legend <- shiny::renderDataTable(usda.arms.api.env$legend)
          
        }
        
      })
      
    })
    
    ### 3. Set up "Pulled Data" tab
    
    # 3.1. 'download_csv' button
    shiny::observeEvent(input$download_csv, {
      
      if ("pulled_data" %in% ls(usda.arms.api.env)) {
        
        write.csv(usda.arms.api.env$pulled_data, 
          paste("download_", Sys.time(), ".csv"))
        shiny::showModal(modalDialog(title = "Success", "Data Downloaded as CSV"))
        
      } else {
        
        shiny::showModal(modalDialog(title = "Error", "Get data using the 'Get Data'
        tab before attempting to download API data in CSV format"))
        
      }
      
    })
    
  }
  
}