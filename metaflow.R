library(shiny)
library(fcexpr)
library(tidyverse)
library(DT)

fd <- list()
md <- list()


stimulation <- c("unstim\nCD28|328\nCD3", "unstim\nCD3+CD28\nCD3") 
Cell_Type <- c("CD4\nCD8", "CD4\nCD8")
ug_ml <- c("01\n10", "0 ug/ml\n0.1 ug/ml\n10 ug/ml")
presetList <- list(stimulation, Cell_Type, ug_ml)
names(presetList) <- c("stimulation", "Cell_Type", "ug_ml")


ui <- fluidPage(
  
  ## Upload WSPs
  sidebarPanel(width = 1.5,
               fileInput("files", "Choose Wsp File", accept = ".wsp", multiple = T)
  ),
  
  mainPanel(
    textOutput("exps"),
    verbatimTextOutput("strText"),
    div(id = "expID"),
    div(id = "group"),
    div(id = "groupString"),
    dataTableOutput("metatable"),
    div(id = "addString"),
    tags$script(HTML("$(function(){ 
      $(document).keyup(function(e) {
      if (e.which == 13) {
        $('#addClick').click()
      }
      });
      })"))
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
               FileName2 = paste(FileName, Experiment, sep = "_")) 
      
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
                  label = tags$em("Variable Name")),
        textAreaInput(inputId = "var",resize = "none", 
                      label = tags$em("String")),
        textAreaInput(inputId = "var2",resize = "none", 
                      label = tags$em("Label")))
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
    ix <- c(index, rep(index[length(index)], 6 - length(index)) )
    
    x <- metadata() %>%
      mutate(!!input$varName := case_when(
        str_detect(FileName, str_split(input$var, pattern = "\\n")[[1]][ix[1]]) ~ str_split(input$var2, pattern = "\\n")[[1]][ix[1]],
        str_detect(FileName, str_split(input$var, pattern = "\\n")[[1]][ix[2]]) ~ str_split(input$var2, pattern = "\\n")[[1]][ix[2]],
        str_detect(FileName, str_split(input$var, pattern = "\\n")[[1]][ix[3]]) ~ str_split(input$var2, pattern = "\\n")[[1]][ix[3]],
        str_detect(FileName, str_split(input$var, pattern = "\\n")[[1]][ix[4]]) ~ str_split(input$var2, pattern = "\\n")[[1]][ix[4]],
        str_detect(FileName, str_split(input$var, pattern = "\\n")[[1]][ix[5]]) ~ str_split(input$var2, pattern = "\\n")[[1]][ix[5]],
        str_detect(FileName, str_split(input$var, pattern = "\\n")[[1]][ix[6]]) ~ str_split(input$var2, pattern = "\\n")[[1]][ix[6]]
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