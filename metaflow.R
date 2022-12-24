library(shiny)
library(fcexpr)
library(tidyverse)
library(DT)

fd <- list()
md <- list()

ui <- fluidPage(
  
  ## Upload WSPs
  sidebarPanel(
    fileInput("files", "Choose Wsp File", accept = ".wsp", multiple = T)
  ),
  
  mainPanel(
    textOutput("exps"),
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
        textInput(inputId = "var", 
                  label = tags$em("Group")))
    )
    
  },ignoreInit = T)
  
  observeEvent(metadata(), {
    
    insertUI(
      selector = '#addString',
      where = "beforeEnd",
      ui = tagList(
        actionButton("addClick",class="btn btn-info", label = "", style='font-size:0.2%'))
    )
  
    
  },ignoreInit = T)
  
  
  counter <- reactiveVal(0)
  
  observeEvent(input$addClick, {
    
    counter(1 + counter())
    
  },ignoreInit = T)
  
  observeEvent(input$addClick, {
    
    insertUI(
      selector = '#groupString',
      where = "beforeEnd",
      ui = tagList(
        textInput(inputId = paste0("str",counter()), 
                  label = tags$em("String")))
    )
    
},ignoreInit = T)

  ## Apply button.
  
  ## Display updated table.
  
  observeEvent(input$updateMeta,{
    
    a <- input$var
    
    x <- metadata() %>%
      mutate(a = case_when(
        str_detect(FileName, input$str1) ~ input$str1,
        str_detect(FileName, input$str2) ~ input$str2 
      )
      )
    
    metadata(x)
  # remove text boxes and bring back. Instead create two text boxes. Left string_ right label (automaticall copied)
    #click enter and it resets the text boxes and print a text to show you. CD3 > CD3 ie. 
    # so I dont create new text boxes. They will stay there. DO it in a new copy
    
    
  })
  
  ## repeat.
  
  ## reset button.
  
  ## download.
  
  }


shinyApp(ui, server)