

function(input, output, session) {

    output$values <- renderPrint({
        list(x1 = input$report, x2 = input$category, x3 = input$x3)
    })
    
    output$secondaryFilter <- renderUI({
        choices1 <- unique(full_data_28Sep_2004[full_data_28Sep_2004$report==input$report,]$category)
        selectizeInput('category', 'X2', choices1)
    })

    output$tertiaryFilter <- renderUI({
        choices2 <- unique(full_data_28Sep_2004[(full_data_28Sep_2004$report==input$report) & (full_data_28Sep_2004$category==input$category),]$category_value)
        selectizeInput('x3', 'X3', choices2)
    })
}