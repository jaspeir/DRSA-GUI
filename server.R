server <- function(input, output, session) {
  
  decisionTable = reactive({
    data('warehouseIT', package = 'DRSA')
    warehouseIT$decisionTable
  })
  
  metaData = reactive({
    
    if (is.null(input$metaDataHOT)) {
      currentMetaData = warehouseIT$metaData
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
  
  P = reactive({
    IT = IT()
    req(IT)
    
    P = names(IT$decisionTable)
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