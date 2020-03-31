require(shinycssloaders)

sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem("File Import", icon = icon('file'), tabName = "fileImportTab"),
    menuItem("Decision Table", icon = icon("table"), tabName = "decisionTableTab"),
    menuItem("Meta-data", icon = icon("sliders-h"), tabName = "metaDataTab"),
    tags$hr(),
    menuItem("Class unions", icon = icon("greater-than-equal"), tabName = "classUnionsTab"),
    menuItem("Rough sets", icon = icon("not-equal"), tabName = "roughSetsTab"),
    menuItem("Decision rules", icon = icon("long-arrow-alt-right"), tabName = "domlemTab"),
    tags$hr(),
    menuItem("Save results", icon = icon('save'), tabName = "exportTab")
  )
)

body = dashboardBody(
  tabItems(
    
    tabItem(tabName = "fileImportTab", 
            fileInput(inputId = "decisionTableFile", label = "Decision table (raw data) or Information Table object", accept = c("xlsx", "xls", "xlsb", "xlsm", "csv", "rds", "rda")),
    ),
    
    tabItem(tabName = "decisionTableTab", 
            DTOutput(outputId = "decisionTableDT") %>% withSpinner()
    ),
    
    tabItem(tabName = "metaDataTab", 
            rHandsontableOutput(outputId = "metaDataHOT"),
            span(h4(textOutput(outputId = 'metaDataErrors')), style = "color:red")
    ),
    
    tabItem(tabName = "classUnionsTab", 
            DTOutput(outputId = "classUnionsDT")
    ),
    
    tabItem(tabName = "roughSetsTab", 
      tabsetPanel(id = 'roughSetsTabPanel',
        tabPanel(title = "Upward", 
          DTOutput(outputId = "upwardApproxDT") %>% withSpinner()
        ),
        tabPanel(title = "Downward", 
                 DTOutput(outputId = "downwardApproxDT") %>% withSpinner()
        )
      )
    ),
    
    tabItem(tabName = "domlemTab", 
            DTOutput(outputId = "decisionRulesDT") %>% withSpinner()
    ),
    
    tabItem(tabName = "exportTab", 
            downloadButton("dowloadITBTN", label = "Download information table")
    )
  )
)

ui <- dashboardPage(
  title = "DRSA",
  skin = "blue",
  header = dashboardHeader(),
  sidebar = sidebar,
  body = body
)