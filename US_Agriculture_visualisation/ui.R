


fluidPage(
    
    sidebarLayout(
  
    sidebarPanel(
        selectizeInput('report', 'Report type', choices = unique(full_data_28Sep_2004$report), multiple = FALSE),
        
        uiOutput("secondaryFilter"),
        uiOutput("tertiaryFilter")
        

    ),
    mainPanel(
        verbatimTextOutput('values')
    )
), title = 'Options groups for select(ize) input')
