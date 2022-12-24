library(shiny)
library(fcexpr)
library(tidyverse)
library(DT)

fd <- list()
md <- list()

# --- Option to filter out side files or select groups in the beginning
# --- option to select the column of str_detect, not so necessary I think.
# --- reset button ?
# --- download button 

# Presets
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
    # Displays the uploaded file names
    verbatimTextOutput("exps"),
    # Annotate the experiment with initials and numbers of the individual experiments
    div(id = "expID"),
    
    fluidRow(
      # Group name, string input and their names UI
      column(4,
             div(id = "group"),
             div(id = "vars")
      ),
      
      column(8,
             # metadata table
             dataTableOutput("metatable")
      )
    )
    
  )
  
  
)


server <- function(input, output, session) {
  
  flowdata <- eventReactive(input$files,{
    
    #print(input$files)
    #print(input$files[1,4])
    
    ## read wsp file(s) and save it in flowdata()
    
    for(i in 1:nrow(input$files)){
      
      fd[[i]] <- wsx_get_popstats(ws = input$files[i,4])
      
    }
    
    return(fd)
  })
  
  
  ## Display wsp files and take input for experiment ID
  wss <- reactiveVal(1)
  
  observeEvent(flowdata(), {
    
    
    for(i in 1:length(flowdata())){
      wss(c(unique(flowdata()[[i]][[1]]$ws), wss()) )
    }
    
    wss(wss()[-length(wss())])
    

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
  
  ## Display the wsp files if any file uploaded
  output$exps <- renderText({
    
    if(wss()[1]!= 1){
      
      wss()  
    }
    
  })
  
  
  ## Remove the experiment ID input UIs
  ## Design a metadata table including specific IDs for the experiment.
  
  metadata <- reactiveVal()
  
  observeEvent(input$submitID, { 
    
    removeUI(selector = "div:has(> #submitID)", immediate = T)
    removeUI(selector = "div:has(> #ini)", immediate = T)
    removeUI(selector = "div:has(> #idi)", immediate = T)
    
    
    for(i in 1:length(flowdata())){
      md[[i]] <- flowdata()[[i]][[1]] %>%
        select("FileName") %>% 
        distinct(FileName, .keep_all = T) %>%
        mutate(FileName = str_replace_all(FileName, "%20", " "), # remove %20 
               Experiment = paste0(input$ini,str_split(input$idi, pattern = " ")[[1]][i]), 
               FileName2 = paste(FileName, Experiment, sep = "_")) %>%
        filter(!str_detect(FileName, "Compensation")) # remove compensation files
      
    }
    
    md <- do.call(bind_rows, md) # combine all wsps.
    
    metadata(md) # save it to metadata()
    
    #print(metadata)
  })
  
  ## display the metadata 'seed'
  ### ------------------ Maybe keep FileName2 here instead of FileName?
  output$metatable <- renderDataTable({
    
    if(!is.null(metadata())){
      metadata() %>%
        select(-"FileName2") 
    }
    
    
  },options = list(pageLength = 100)) # show as much as possible
  
  ## Take input for group names(cols) and string identifiers to mine meta data from sample names (FileName).
  # updateMeta: applies the given inputs to update the metadata()
  # preset: allows you to select presets for data grouping.
  # varName: name of the group (column name for the variable)
  # var: string input for mining the text
  # var2: name for the mined text under the varName column
  
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
    
    
    
  },ignoreInit = T, once = T) # I don't want to generate it again when the metadata() updates.
  
  # If no preset selected update the var2 according to the inputs from var (string).
  # One can manually change the experimental group names (var2) also.
  # If preset selected, shows you how it looks like at textAreas and it applies it when you click update.
    # not changeable as long as the preset is selected.
    # ------------ Maybe if I use isolate somewhere it wont update it?
  
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

  # Upon clicking update Table it mines the FileName according to the given input and creates a new column.
  
  
  observeEvent(input$updateMeta,{
    
    # If the given input is shorter than 12 (e.g. 3 values entered.)
    # 1 2 3 + and nine 3s more to complete it to 12.
    # So str_detect will choose the last entered value and apply it again, which will  not affect the generated column.
    # I did this to allow input of various lengths.
    # Since I added 12 str_detects in case_when(), it can max take 12 different groups. It should be enough for most cases but can be updated.
    
    index <- 1:length(str_split(input$var, pattern = "\\n")[[1]])
    ix <- c(index, rep(index[length(index)], 12 - length(index)) )
    
    # case insensitive.
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
    
    metadata(x) # save updated metadata
    
    # reset the inputs to null
    updateTextAreaInput(session, "var",
                        value = "")
    updateTextInput(session, "varName",
                    value = "")
    updateSelectizeInput(session, "preset",
                         selected = "")
    
  })
  
}


shinyApp(ui, server)