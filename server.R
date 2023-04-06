

################################################################################
# define the server function

server <- function(input,output,session) {
  
  # Reactivity:
    # As a general rule, use reactive() when you just want to update something based on a new value (e.g. a user input)
    # and use reactiveVal or reactiveValues when you have an object that you want to maintain a state
  
    # # Reactive objects:
    # text_labels_input <- reactive({
    #   req(input$submarket_labels_input)
    #   paste('You entered: ',input$submarket_labels_input,sep = '',collapse = '')
    #   })
    # output$text_labels_output <- renderText({text_labels_input()})

    # # Reactive objects:
    # market_cpc_input_text <- reactive({
    #   req(input$market_cpcs_input)
    #   paste(input$market_cpcs_input,sep = '',collapse = ', ')
    #   })
    # output$market_cpc_output <- renderText({market_cpc_input_text()})
    
    
    # Reactive values and observers
    # r_list <- reactiveValues(txt = 'You entered: ',txt2 = 'You typed: ')
    # observe({
    #   output$text_labels_output <- renderText({
    #     paste(r_list$txt, input$submarket_labels_input,sep = '',collapse = '')
    #   })
    #   })
  
  
    # # ObserveEvent
    # r_list <- reactiveValues(txt = 'You entered: ',txt2 = 'You typed: ')
    # observeEvent(input$generate_competitive_positioning,{
    #   r_list$txt <- paste(r_list$txt, input$submarket_labels_input,sep = '',collapse = '')
    # })
    # output$text_labels_output <- renderText({
    #   r_list$txt
    # })
    
  
    # # Debugging - browser()
    #   r_list <- reactiveValues(txt = 'You entered: ',txt2 = 'You typed: ')
    #   observeEvent(input$generate_competitive_positioning,{
    #     browser()
    #     temp <- 1
    #     print(temp*5)
    #     r_list$txt <- paste(r_list$txt, input$submarket_labels_input,sep = '',collapse = '')
    # 
    #   })
    #   output$text_labels_output <- renderText({
    #     r_list$txt
    #   })

  
}


