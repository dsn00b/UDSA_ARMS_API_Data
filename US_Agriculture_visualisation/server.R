# function(input, output, session) {
#     
#     updateSelectizeInput(session, 'x2', choices = list(
#         Eastern = c(`Rhode Island` = 'RI', `New Jersey` = 'NJ'),
#         Western = c(`Oregon` = 'OR', `Washington` = 'WA'),
#         Middle = list(Iowa = 'IA')
#     ), selected = 'IA')
#     
#     output$values <- renderPrint({
#         list(x1 = input$x1, x2 = input$x2, x3 = input$x3, x4 = input$x4)
#     })
# }

function(input, output, session) {
    reactive({
        updateSelectizeInput(session, 'x2', 
                             choices = unique(full_data_28Sep_2004[full_data_28Sep_2004$report==input$report,]$category))
    })
    output$values <- renderPrint({
        list(x1 = input$report, x2 = input$x2)
    })
}