`%then%` <- shiny:::`%OR%`

server <- function(input, output, session) {
  
  data <- reactive({
    validate(
      need(input$decisionTableFile != "", "Please select an input file") %then%
        need(file_ext(input$decisionTableFile$datapath) %in% 
               c(csv = "csv", excel = "xls", excel_xml = "xlsx", excel_xmlm = "xlsm", excel_xmlb = "xlsb", rds = "rds", rda = "rda"),
             "Unrecognized file type")
    )
    
    data = switch(file_ext(input$decisionTableFile$datapath),
                  "csv" = fread(input$decisionTableFile$datapath),
                  "xls" = read_excel(input$decisionTableFile$datapath),
                  "xlsx" = read_excel(input$decisionTableFile$datapath),
                  "xlsm" = read_excel(input$decisionTableFile$datapath),
                  "xlsb" = read_excel(input$decisionTableFile$datapath),
                  "rds" = readRDS(file = input$decisionTableFile$datapath),
                  "rda" = readRDS(file = input$decisionTableFile$datapath)
    )
    
    return(data)
  })
  
  decisionTable = reactive({

    data = data()
    req(data)
    
    if ("InformationTable" %in% class(data)) {
      data$decisionTable
    } else {
      data
    }
  })
  
  # values <- reactiveValues()
  # observeEvent(eventExpr = input$decisionTableFile, handlerExpr = {
  #   
  #   data = data()
  #   req(data)
  #   
  #   if ("InformationTable" %in% class(data)) {
  #     metaData = data$metaData
  #   } else {
  #     decisionTable = data
  #     attributeCount = ncol(decisionTable)
  #     
  #     # Create default meta-data:
  #     metaData = data.frame(
  #       name = names(decisionTable),
  #       type = c('object', rep('dominance', attributeCount - 2), 'decision'),
  #       alpha = rep(NA_real_, attributeCount),
  #       beta = rep(NA_real_, attributeCount)
  #     )
  #   }
  #   
  #   values$metaData = metaData
  # })
  
  metaData = reactive({
    
    #data = data()
    
    if (is.null(input$metaDataHOT)) {
      data = data()
      req(data)
      
      if ("InformationTable" %in% class(data)) {
        metaData = data$metaData
      } else {
        decisionTable = data
        attributeCount = ncol(decisionTable)
        
        # Create default meta-data:
        metaData = data.frame(
          name = names(decisionTable),
          type = c('object', rep('dominance', attributeCount - 2), 'decision'),
          alpha = rep(NA_real_, attributeCount),
          beta = rep(NA_real_, attributeCount)
        )
      }
      
      currentMetaData = metaData
    } else {
      currentMetaData = hot_to_r(input$metaDataHOT)
    }
    
    currentMetaData
  })
  
  IT = reactive({
    #req(input$appartment)
    #req(input$stayRange)
    
    IT = tryCatch(
      expr = {
        IT = InformationTable$new(decisionTable(), metaData())
        output$metaDataErrors = renderText("")
        IT
      }, 
      error = function(cond) {
        output$metaDataErrors = renderText({ paste0("ERROR: cannot create InformationTable instance. \n\n", cond) })
        NULL
      }
    )
    
    IT
  })
  
  output$dowloadITBTN <- downloadHandler(
    filename = function() {
      paste('IT', ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(IT(), file)
    }
  )
  
  P = reactive({
    decisionTable = decisionTable()
    req(decisionTable)
    
    IT = IT()
    req(IT)
    
    P = names(decisionTable)
    P = IT$partitionAttributes(P)
    P = c(P$ind, P$sim, P$dom)
    P
  })
  
  output$decisionTableDT = renderDT({
    
    decisionTable = decisionTable()
    req(decisionTable)
    
    datatable(decisionTable, options = list(
      scrollX = TRUE, 
      pageLength = 10
    ), selection = 'none')
  })
  
  output$metaDataHOT = renderRHandsontable({

    metaData = metaData()
    req(metaData)

    rhandsontable(metaData, useTypes = T) %>%
      hot_col("name", readOnly = TRUE)
      
  })
  
  output$classUnionsDT = renderDT({
    
    IT = IT()
    req(IT)
    
    classUnions = IT$classUnions()
    upward = convertMatrixToList(classUnions$upward, IT$objects)
    downward = convertMatrixToList(classUnions$downward, IT$objects)
    
    decisionsDecoded = IT$decodeDecisions(1:nrow(classUnions$upward))
    
    df = data.frame(Class = decisionsDecoded, 
                    Upward = map_chr(upward, ~ paste(., collapse = ", ")),
                    Downward = map_chr(downward, ~ paste(., collapse = ", ")))
    
    datatable(df, options = list(
      scrollX = TRUE, 
      pageLength = 10
    ), selection = 'none')
  })
  
  output$upwardApproxDT = renderDT({
    
    IT = IT()
    P = P()
    req(IT)
    req(P)
    
    classUnions = IT$classUnions()
    roughSets = IT$roughSets(P)
    boundary = IT$boundaryRegions(roughSets)
    
    
    upper = convertMatrixToList(roughSets$upward_U, IT$objects)
    lower = convertMatrixToList(roughSets$upward_L, IT$objects)
    boundary = convertMatrixToList(boundary$upward, IT$objects)
    
    decisionsDecoded = IT$decodeDecisions(1:nrow(classUnions$upward))
    acc = IT$accuracyOfApproximation(roughSets)
    
    df = data.frame(Class = decisionsDecoded, 
                    Accuracy = round(acc$upward, digit = 2),
                    Upper = map_chr(upper, ~ paste(., collapse = ", ")),
                    Lower = map_chr(lower, ~ paste(., collapse = ", ")),
                    Boundary = map_chr(boundary, ~ paste(., collapse = ", "))
    )
    
    datatable(df, options = list(
      scrollX = TRUE, 
      pageLength = 10
    ), selection = 'none')
  })
  
  output$downwardApproxDT = renderDT({
    
    IT = IT()
    P = P()
    req(IT)
    req(P)
    
    classUnions = IT$classUnions()
    roughSets = IT$roughSets(P)
    boundary = IT$boundaryRegions(roughSets)
    
    
    upper = convertMatrixToList(roughSets$downward_U, IT$objects)
    lower = convertMatrixToList(roughSets$downward_L, IT$objects)
    boundary = convertMatrixToList(boundary$downward, IT$objects)
    
    decisionsDecoded = IT$decodeDecisions(1:nrow(classUnions$downward))
    acc = IT$accuracyOfApproximation(roughSets)
    
    df = data.frame(Class = decisionsDecoded, 
                    Accuracy = round(acc$downward, digit = 2),
                    Upper = map_chr(upper, ~ paste(., collapse = ", ")),
                    Lower = map_chr(lower, ~ paste(., collapse = ", ")),
                    Boundary = map_chr(boundary, ~ paste(., collapse = ", "))
    )
    
    datatable(df, options = list(
      scrollX = TRUE, 
      pageLength = 10
    ), selection = 'none')
  })
  
  output$decisionRulesDT = renderDT({
    
    IT = IT()
    P = P()
    req(IT)
    req(P)
    
    domlem = DOMLEM$new(it = IT, P = P)
    domlem$main()
    rules = c(domlem$rules$STAT1, domlem$rules$STAT2, domlem$rules$STAT3)
    
    df = map_dfr(rules, function(d) d$toList(IT))
    
    datatable(df, options = list(
      scrollX = TRUE, 
      pageLength = 10
    ), selection = 'none')
  })
}