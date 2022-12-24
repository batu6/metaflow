library(shiny)
library(fcexpr)
library(tidyverse)
library(DT)

fd <- list()
md <- list()

# Option to filter out side files or select groups in the beginning
# Uppercase insensitive filtering.


stimulation <- c("unstim\nCD28|328\nCD3", "unstim\nCD3+CD28\nCD3") 
Cell_Type <- c("CD4\nCD8", "CD4\nCD8")
ug_ml <- c("unstim\n01\n10", "0 ug/ml\n0.1 ug/ml\n10 ug/ml")
dilution <- c("unstim\nd1\nd2\nd3\nd4\nd5\nd6\nd7\nd8\nd9\nd10", "unstim\nd1\nd2\nd3\nd4\nd5\nd6\nd7\nd8\nd9\nd10")
presetList <- list(stimulation, Cell_Type, ug_ml, dilution)
names(presetList) <- c("stimulation", "Cell_Type", "ug_ml", "dilution")


ui <- fluidPage(
  
  ## Upload WSPs
  sidebarPanel(width = 2,
               fileInput("files", "Choose Wsp File", accept = ".wsp", multiple = T)
  ),
  
  mainPanel(
    verbatimTextOutput("exps"),
    div(id = "expID"),
    fluidRow(
      column(4,
             div(id = "group"),
             div(id = "vars")
      ),
      
      column(8,
             dataTableOutput("metatable")
      )
    )
    
    
    
    
    
    
    
  )
  
  
)


server <- function(input, output, session) {
  
  
  ## read wsp file(s)
  
  wss <- reactiveVal(1)
  dataf <- reactiveVal()
  
  flowdata <- eventReactive(input$files,{
    
    #print(input$files)
    #print(input$files[1,4])
    
    
    for(i in 1:nrow(input$files)){
      
      fd[[i]] <- wsx_get_popstats(ws = input$files[i,4])
      
      #length_input(length_input() + 1)
    }
    
    return(fd)
  })
  
  
  ## Display wsp files and take input for experiment ID
  
  
  observeEvent(flowdata(), {
    
    
    for(i in 1:length(flowdata())){
      wss(c(unique(flowdata()[[i]][[1]]$ws), wss()) )
    }
    
    wss(wss()[-length(wss())])
    
    
    #print(unique(dataf()[[length_input()]][[1]]$ws))
    #
    insertUI(
      selector = '#expID',
      where = "beforeEnd",
      ui = tagList(
        textInput(inputId = "ini", 
                  label = tags$em("Initials")),
        textInput(inputId = "idi", 
                  label = tags$em("Experiment IDs")),
        actionButton("submitID", "Submit",class="btn btn-info"))
    )
    
    
    
  })
  
  
  output$exps <- renderText({
    
    if(wss()[1]!= 1){
      
      wss()  
    }
    
    
  })
  
  metadata <- reactiveVal()
  
  observeEvent(input$submitID, { 
    
    removeUI(selector = "div:has(> #submitID)", immediate = T)
    removeUI(selector = "div:has(> #ini)", immediate = T)
    removeUI(selector = "div:has(> #idi)", immediate = T)
    
    
    for(i in 1:length(flowdata())){
      md[[i]] <- flowdata()[[i]][[1]] %>%
        select("FileName") %>%
        distinct(FileName, .keep_all = T) %>%
        mutate(FileName = str_replace_all(FileName, "%20", " "),
               Experiment = paste0(input$ini,str_split(input$idi, pattern = " ")[[1]][i]),
               FileName2 = paste(FileName, Experiment, sep = "_")) %>%
        filter(!str_detect(FileName, "Compensation"))
      
    }
    
    md <- do.call(bind_rows, md)
    
    metadata(md)
    
    print(metadata)
  })
  
  
  
  ## combine with each single wsp file. 1st col: file names 2nd col: exp ID, 3rd col paste
  
  ## row bind
  
  ## display the first column with file names.
  
  output$metatable <- renderDataTable({
    
    if(!is.null(metadata())){
      metadata() %>%
        select(-"FileName2")
    }
    
    
  },options = list(pageLength = 100))
  
  ## take identifier, add input text box (comma separated multiple inputs etc) = unique identifiers for population groups
  
  observeEvent(metadata(), {
    
    insertUI(
      selector = '#group',
      where = "beforeEnd",
      ui = tagList(
        actionButton("updateMeta",class="btn btn-info", 
                     label = tags$em("Update Table")),
        selectInput('preset', 'Presets', c(Choose='', names(presetList)), selectize=TRUE),
        textInput(inputId = "varName", 
                  label = tags$em("Variable Name")))
    )
    
    insertUI(
      selector = '#vars',
      where = "beforeEnd",
      ui = tagList(
        column(11,
               column(5,textAreaInput(inputId = "var",resize = "none", width = '450px', height = '220px', 
                                      label = tags$em("String"))),
               column(1, h3(">", align = "center")),
               column(5,textAreaInput(inputId = "var2",resize = "none", width = '450px', height = '220px', 
                                      label = tags$em("Label")) ))
      )
      
    )
    
    
    
  },ignoreInit = T, once = T)
  
  #  output$strText <- renderText({
  #    
  #    str_replace_all(input$var, " ", "\n")
  #  })
  
  
  
  observeEvent(c(input$preset, input$var),{
    
    if(input$preset == ""){
      updateTextAreaInput(session, "var2",
                          value = input$var)
    } else if(input$preset != ""){
      updateTextAreaInput(session, "var",
                          value = presetList[[input$preset]][1])
      updateTextAreaInput(session, "var2",
                          value = presetList[[input$preset]][2])
      updateTextInput(session, "varName",
                      value = input$preset)
      
    }
    
    
    
    
  })
  
  
  counter <- reactiveVal(0)
  
  
  
  ## Apply button.
  
  ## Display updated table.
  
  observeEvent(input$updateMeta,{
    
    index <- 1:length(str_split(input$var, pattern = "\\n")[[1]])
    ix <- c(index, rep(index[length(index)], 12 - length(index)) )
    
    x <- metadata() %>%
      mutate(!!input$varName := case_when(
        str_detect(FileName, regex(str_split(input$var, pattern = "\\n")[[1]][ix[1]], ignore_case = T)) ~ str_split(input$var2, pattern = "\\n")[[1]][ix[1]],
        str_detect(FileName, regex(str_split(input$var, pattern = "\\n")[[1]][ix[2]], ignore_case = T)) ~ str_split(input$var2, pattern = "\\n")[[1]][ix[2]],
        str_detect(FileName, regex(str_split(input$var, pattern = "\\n")[[1]][ix[3]], ignore_case = T)) ~ str_split(input$var2, pattern = "\\n")[[1]][ix[3]],
        str_detect(FileName, regex(str_split(input$var, pattern = "\\n")[[1]][ix[4]], ignore_case = T)) ~ str_split(input$var2, pattern = "\\n")[[1]][ix[4]],
        str_detect(FileName, regex(str_split(input$var, pattern = "\\n")[[1]][ix[5]], ignore_case = T)) ~ str_split(input$var2, pattern = "\\n")[[1]][ix[5]],
        str_detect(FileName, regex(str_split(input$var, pattern = "\\n")[[1]][ix[6]], ignore_case = T)) ~ str_split(input$var2, pattern = "\\n")[[1]][ix[6]],
        str_detect(FileName, regex(str_split(input$var, pattern = "\\n")[[1]][ix[7]], ignore_case = T)) ~ str_split(input$var2, pattern = "\\n")[[1]][ix[7]],
        str_detect(FileName, regex(str_split(input$var, pattern = "\\n")[[1]][ix[8]], ignore_case = T)) ~ str_split(input$var2, pattern = "\\n")[[1]][ix[8]],
        str_detect(FileName, regex(str_split(input$var, pattern = "\\n")[[1]][ix[9]], ignore_case = T)) ~ str_split(input$var2, pattern = "\\n")[[1]][ix[9]],
        str_detect(FileName, regex(str_split(input$var, pattern = "\\n")[[1]][ix[10]], ignore_case = T)) ~ str_split(input$var2, pattern = "\\n")[[1]][ix[10]],
        str_detect(FileName, regex(str_split(input$var, pattern = "\\n")[[1]][ix[11]], ignore_case = T)) ~ str_split(input$var2, pattern = "\\n")[[1]][ix[11]],
        str_detect(FileName, regex(str_split(input$var, pattern = "\\n")[[1]][ix[12]], ignore_case = T)) ~ str_split(input$var2, pattern = "\\n")[[1]][ix[12]]
      )
      )
    
    
    
    metadata(x)
    # remove text boxes and bring back. Instead create two text boxes. Left string_ right label (automaticall copied)
    #click enter and it resets the text boxes and print a text to show you. CD3 > CD3 ie. 
    # so I dont create new text boxes. They will stay there. DO it in a new copy
    
    updateTextAreaInput(session, "var",
                        value = "")
    updateTextInput(session, "varName",
                    value = "")
    updateSelectizeInput(session, "preset",
                         selected = "")
    
  })
  
  # select option for pre determined groupings, it updates the text boxes
  # option to select the column of str_detect
  
  ## repeat.
  
  ## reset button.
  
  ## download.
  
}


shinyApp(ui, server)