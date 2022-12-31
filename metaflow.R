library(shiny)
library(fcexpr)
library(tidyverse)
library(DT)
library(writexl)

fd <- list()
md <- list()
filter_data <- list()
pd <- list()
sd <- list()
new_presets <- list()
namesss <- c()

# when you dont have it in the same channel it becomes a problem naturally...
# --- Option to filter out side files or select groups in the beginning
# --- option to select the column of str_detect, not so necessary I think.
# --- reset button ?
# --- download button 
# --- shinyjs disable
# --- var var2 update problem presets overwrite
# --- a 4th tab unstim negative positive control pasting before pivot wider

# Presets
stimulation <- c("unstim\nCD28|328\nCD3", "unstim\nCD3+CD28\nCD3") 
Cell_Type <- c("CD4\nCD8", "CD4\nCD8")
ug_ml <- c("unstim\n01\n10", "0 ug/ml\n0.1 ug/ml\n10 ug/ml")
dilution <- c("unstim\nd1\nd2\nd3\nd4\nd5\nd6\nd7\nd8\nd9\nd10\nno inhibitor", "unstim\nd1\nd2\nd3\nd4\nd5\nd6\nd7\nd8\nd9\nd10\nNo Inhibitor")

presetList<- list(stimulation, Cell_Type, ug_ml, dilution)
names(presetList) <- c("stimulation", "Cell_Type", "ug_ml", "dilution")


ui <- fluidPage(
  
  ## Upload WSPs
  sidebarPanel(width = 2,
               fileInput("files", "Choose Wsp File", accept = ".wsp", multiple = T),
               fileInput("presetUpdate", label = "Upload Preset", accept = ".rds",multiple = F),
  
               hr(),
               
               checkboxGroupInput("filtergroup", label = "Filter groups", choices = character(0)),
               actionButton("filtergroupb",label = "Keep"),
               checkboxGroupInput("percentage", label = "Percentage gates", choices = character(0)),
               actionButton("get_perc",label = "Get data percentage!")
  ),
  
  mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel("First panelll", 

      # Displays the uploaded file names
      #verbatimTextOutput("exps"),
      fluidRow(
        
        column(5,
               # Annotate the experiment with initials and numbers of the individual experiments
               div(id = "expID"),
        )
      
      ),
      
      fluidRow(
        column(2,offset = 9,
               downloadButton("dm", "Download Meta",class="btn btn-info btn-sm")
        )

      ),

                     
      fluidRow(
        # Group name, string input and their names UI
        column(5,
               div(id = "group"),
               div(id = "vars"),
               div(id = "combmeta")
        ),
        
        column(7,

               # metadata table
               dataTableOutput("metatable")
        )
        
      )
      
    ),
    tabPanel("Percentage Pannel", 
             
             fluidRow(
               column(10,
                      checkboxGroupInput("arrcol", label = "Arrange", choices = character(0),inline = T)
                      ),
               column(2,
                      downloadButton("dp","Download",class="btn btn-info btn-sm")
                      )
             ),
             
             
             dataTableOutput("perctab")
    
             ),
    tabPanel("Stats Pannel", 
             
             fluidRow(
               column(10,
                      checkboxGroupInput("arrcol2", label = "Arrange", choices = character(0),inline = T)
               ),
               column(2,
                      downloadButton("ds","Download",class="btn btn-info btn-sm")
               )
             ),
             

             dataTableOutput("stattab")
             
    ),
    
    tabPanel("Stats Pannel 2", 
             fluidRow(
               column(4,
                      radioButtons("colnp", label = "Column", choices = "")
               ),
               column(4,
                      radioButtons("negc2", label = "negative control", choices = "")
               ),
               column(3,
                      radioButtons("posc2", label = "positive control", choices = "")
               ),
               column(1,
                      actionButton("rearrange", label = "Rearrange")
                      )
             ),
             
             fluidRow(
               column(10,
                      checkboxGroupInput("arrcol3", label = "Arrange", choices = character(0),inline = T)
               ),
               column(2,
                      downloadButton("ds2","Download",class="btn btn-info btn-sm")
               )
             ),
             
             
             dataTableOutput("stattab2")
             
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
    
    print(input$files)
    return(fd)
  })
  
  
  ## Display wsp files and take input for experiment ID
  wss <- reactiveVal(1)
  
  observeEvent(flowdata(), {
    
    
    for(i in 1:length(flowdata())){
      #w1 <- str_extract(unique(flowdata()[[i]][[1]]$FilePath), "[^\\/]*$")
      w1 <- unique(flowdata()[[i]][[1]]$ws)
      w1 <- str_replace_all(w1, "%20", " ")   
      
      wss(c(w1, wss())) 
    }
    
    wss(wss()[-length(wss())])
    

    insertUI(
      selector = '#expID',
      where = "beforeEnd",
      ui = tagList(
        
        fluidRow(
          verbatimTextOutput("exps")
        ),
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
      
      rev(paste(wss(), collapse = "\n") )  
    }
    
  })
  
  
  ## Remove the experiment ID input UIs
  ## Design a metadata table including specific IDs for the experiment.
  
  metadata <- reactiveVal()
  percentdata <- reactiveVal()
  filterdata <- reactiveVal()
  statdata <- reactiveVal()
  
  observeEvent(input$submitID, { 
    
    removeUI(selector = "div:has(> #submitID)", immediate = T)
    removeUI(selector = "div:has(> #ini)", immediate = T)
    removeUI(selector = "div:has(> #idi)", immediate = T)
    
    
    for(i in 1:length(flowdata())){
      md[[i]] <- flowdata()[[i]][[1]] %>%
        select(c(FileName, group)) %>% 
        distinct(FileName, .keep_all = T) %>%
        mutate(FileName = str_replace_all(FileName, "%20", " "), # remove %20 
               Experiment = paste0(input$ini,str_split(input$idi, pattern = " ")[[1]][i]), 
               FileName2 = paste(FileName, Experiment, sep = "_")) %>%
        filter(!str_detect(FileName, "Compensation")) # remove compensation files
      
    }
    
    md <- do.call(bind_rows, md) # combine all wsps.
    
    metadata(md) # save it to metadata()
    
    # Have the filter data separately by considering the first file only. The target gate names should be same also in other files
    # i.e. it should be CD4 in all but not cd_4 in the other file.
    for(i in 1:length(flowdata())){
      
      filter_data[[i]] <- flowdata()[[i]][[1]] %>%
        select(c(FileName, Population, group)) %>%
        mutate(FileName = str_replace_all(FileName, "%20", " ")) %>% #required for adding to statdata
        filter(!str_detect(FileName, "Compensation")) 
      
    }

    
    filterdata(filter_data)
    
    
    for(i in 1:length(flowdata())){
      pd[[i]] <- flowdata()[[i]][[1]] %>%
        select(FileName, Population, FractionOfParent, group) %>% 
        mutate(FileName = str_replace_all(FileName, "%20", " "), # remove %20 
               Experiment = paste0(input$ini,str_split(input$idi, pattern = " ")[[1]][i]), 
               FileName2 = paste(FileName, Experiment, sep = "_"),
               FractionOfParent = round(FractionOfParent,2)) %>%
        filter(!str_detect(FileName, "Compensation")) # remove compensation files
      
    }
    
    pd <- do.call(bind_rows, pd) # combine all wsps.
    
    percentdata(pd)
    
    # gives an error when there is no stat calculation
    if(!is.null(flowdata()[[1]][[2]])){
    
      
      for(i in 1:length(flowdata())){
        sd[[i]] <- flowdata()[[i]][[2]] %>%
          select(FileName, PopulationFullPath, statistic, channel, value) %>% 
          mutate(FileName = str_replace_all(FileName, "%20", " "), # remove %20 
                 Experiment = paste0(input$ini,str_split(input$idi, pattern = " ")[[1]][i]), 
                 FileName2 = paste(FileName, Experiment, sep = "_"),
                 Population = str_extract(PopulationFullPath, "[^\\/]*$"),
                 value = round(value,0)) %>%
          filter(!str_detect(FileName, "Compensation")) %>% # remove compensation files
          select(-PopulationFullPath) %>%
          left_join(filter_data[[i]] %>% distinct(FileName, .keep_all = T) %>% select(FileName, group),by= "FileName" )
      }

      sd <- do.call(bind_rows, sd) # combine all wsps.
      
      statdata(sd)
    
    } 
    #print(per_data)
  })
  
  observeEvent(filterdata(),{
    dc_filterdata <- do.call(bind_rows, filterdata())
    
    updateCheckboxGroupInput(inputId = "filtergroup",choices = unique(dc_filterdata$group) )
    updateCheckboxGroupInput(inputId = "percentage",choices = unique(dc_filterdata$Population) )
    
  })
  
  observeEvent(input$filtergroupb,{
    mfilter <- metadata() %>%
                filter(group %in% input$filtergroup)
    metadata(mfilter)
    
    pfilter <- percentdata() %>%
      filter(group %in% input$filtergroup)
    percentdata(pfilter)
    

    if(!is.null(flowdata()[[1]][[2]])){
      sfilter <- statdata() %>%
        filter(group %in% input$filtergroup)
      statdata(sfilter)
    }
  
  })
  
  observeEvent(input$get_perc,{
    xget_perc <- percentdata() %>%
      filter(Population %in% input$percentage)
    
    #xget_perc <- percentdata() %>%
    #  filter(Population %in% input$percentage) %>%
    #    pivot_wider(names_from = Population, values_from = FractionOfParent)
  
    #xget_perc <- percentdata() %>%
    #  filter(Population %in% input$percentage) %>%
    #    select(-FileName2) %>%
    #      pivot_wider(names_from = Experiment, values_from = FractionOfParent)
    
    #print(xget_perc)
    percentdata(xget_perc)
    
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

  
  presetListN <- eventReactive(input$presetUpdate,{
    
    newp <- readRDS(input$presetUpdate[1,4])
    
    newp <- do.call(bind_rows, newp)
    
    new_names <- pull(newp[,1])
    

    
    for(i in 1:nrow(newp)){
      new_presets[[i]] <- c(pull(newp[i,2]),pull(newp[i,3]))
    }
    
    
    
    final_presets <- c(presetList, new_presets)
    
    names(final_presets) <- c(names(presetList),new_names)
    
    #print(final_presets)
    return(final_presets)
    
  })
  
  observeEvent(presetListN(),{
    
    if(!is.null(presetListN()) ){
      presetList <<- presetListN()
      
    }
    
  })

  
  
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

        column(12,
               column(5,textAreaInput(inputId = "var",resize = "none", width = '550px', height = '220px', 
                                      label = tags$em("String"))),
               column(1, h3(">", align = "center")),
               column(5,textAreaInput(inputId = "var2",resize = "none", width = '550px', height = '220px', 
                                      label = tags$em("Label")) ))
        
        
      )

      
    )
    
    
    
  },ignoreInit = T, once = T) # I don't want to generate it again when the metadata() updates.
  
  observeEvent(metadata(), {
    
    
    insertUI(
      selector = '#combmeta',
      where = "beforeEnd",
      ui = tagList(
        column(2,
               
               actionButton("combine", "Combine",class="btn btn-success")
               ),
        column(2,offset = 1,
               
               downloadButton("dpreset", "Download Preset",class="btn btn-success")
        )
      )
      
      
    )
    
    
  },ignoreInit = T, once = T)
  
  
  # If no preset selected update the var2 according to the inputs from var (string).
  # One can manually change the experimental group names (var2) also.
  # If preset selected, shows you how it looks like at textAreas and it applies it when you click update.
    # not changeable as long as the preset is selected.
    # ------------ Maybe if I use isolate somewhere it wont update it?
  
   # separated the two below so that you can edit the presets now on the go.
    #still a problemmm

  observeEvent(input$var,{
    
    updateTextAreaInput(session, "var2",
                        value = input$var)
    
  })
  
  observeEvent(input$preset,{
    
    if(input$preset != ""){
      
      updateTextAreaInput(session, "var",
                          value = presetList[[input$preset]][1])
      updateTextAreaInput(session, "var2",
                          value = presetList[[input$preset]][2])
      updateTextInput(session, "varName",
                      value = input$preset)
      
    }
    
  })

  # Upon clicking update Table it mines the FileName according to the given input and creates a new column.
  
  savePreset <- reactiveVal()
  
  observeEvent(input$updateMeta,{
    
    savep <- list(c(varName = input$varName, var = input$var, var2= input$var2))
    
    savePreset(c(savep, savePreset()))
    
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
  
  percCombine <- reactiveVal()
  statCombine <- reactiveVal()
  
  observeEvent(input$combine,{
    
    pc <- left_join(percentdata(),metadata(), by=c("FileName2", "FileName", "Experiment", "group") )
    
    xpc <- colnames(pc)[!(colnames(pc) %in% c("FractionOfParent", "Experiment", "FileName", "FileName2")) ]
    
    pc <- pc %>%
      select(-c(FileName,FileName2) ) %>%
      group_by(across(c(all_of(xpc), "Experiment"))) %>%
      mutate(n = row_number()) %>%
      pivot_wider(id_cols = c(n, all_of(xpc)),
                  names_from = Experiment, values_from = FractionOfParent)%>%
      select(-1)
    
    percCombine(pc)

    updateCheckboxGroupInput(inputId = "arrcol", choices = colnames(percCombine()),inline = T)
    
    ### stat part
    
    if(!is.null(statdata())){
      
      #print(statdata())
      sc <- left_join(statdata(),metadata(), by=c("FileName2", "FileName", "Experiment", "group") )
      
      xsc <- colnames(sc)[!(colnames(sc) %in% c("value", "Experiment", "FileName", "FileName2")) ]
      
      sc <- sc %>%
        select(-c(FileName,FileName2) ) %>%
        group_by(across(c(all_of(xsc), "Experiment"))) %>%
        mutate(n = row_number()) %>%
        pivot_wider(id_cols = c(n, all_of(xsc)),
                    names_from = Experiment, values_from = value)%>%
        select(-1)
      
      statCombine(sc)
      
      updateCheckboxGroupInput(inputId = "arrcol2", choices = colnames(statCombine()),inline = T)
    }


    
  })
  
  output$perctab <- renderDataTable({
    
    if(!is.null(percCombine())){
      
        p1 <- percCombine() %>%
          arrange(across(all_of(input$arrcol)))

        percCombine(p1)
        
        p1
    }

  },options = list(pageLength = 100))
  
  output$stattab <- renderDataTable({
    
    if(!is.null(statCombine()) ){
      
      p2 <- statCombine() %>%
        arrange(across(all_of(input$arrcol2)))
      
      statCombine(p2)
      
      p2
      
    }
    
  },options = list(pageLength = 100))

  
  statCombine2 <- reactiveVal()
  
  observeEvent(statCombine(),{

    if(!is.null(statCombine())){
      updateRadioButtons(inputId = "colnp", choices = colnames(statCombine())[5:(ncol(statCombine())-isolate({length(flowdata())})) ])

    }

    
    print("b")
  },ignoreNULL = T,once = T)
  
  observeEvent(input$colnp,{
    if(input$colnp != ""){
      updateRadioButtons(inputId = "negc2", choices = unique(pull(statCombine(),input$colnp)) )
      updateRadioButtons(inputId = "posc2", choices = unique(pull(statCombine(),input$colnp)) )
    }

  })
  
  observeEvent(input$rearrange,{
    
    if(input$negc2 != ""){
    
    exp_vector <- unique(isolate({metadata()$Experiment}))
    slong <- statCombine() %>% pivot_longer(cols = all_of(exp_vector),names_to = "Experiment") %>% 
      ungroup() # IMPORTANTTTTTTTTTT
  
    
    user_added <- colnames(slong)[5:(ncol(slong)-2)]
    
    negativec <- slong %>%
      filter(.data[[input$colnp]] %in% input$negc2 ) %>%
      select(-c(all_of(user_added))) %>% # user added columns remove
      rename(value_negative = "value")

     slong2 <- slong %>% ungroup() %>% left_join(negativec)
    
      if(input$posc2 != ""){
        positivec <- slong %>%
          filter(.data[[input$colnp]] %in% input$posc2 ) %>%
          select(-c(all_of(user_added))) %>% # user added columns remove
          rename(value_positive = "value")
        
        slong2 <- slong2 %>% ungroup() %>%left_join(positivec)
      }
   

    slong3 <- slong2 %>%
      mutate(value_norm = value - value_negative)
    
    if(input$posc2 != ""){
      
    slong3 <- slong3 %>% 
        mutate(perc_change = (100*value_norm)/value_positive)
      
    }
    
  #  b <- colnames(slong3)[!(colnames(slong3) %in% c("value_negative")) ] 
  #  
  # print(b)  
  #
  #  slong4 <- slong3 %>%
  #    select(-c(value, value_negative)) %>%
  #    group_by(across(all_of(b))) %>%
  #    mutate(n = row_number()) %>%
  #    pivot_wider(  id_cols = all_of(c(n, colnames(slong3)[!(colnames(slong3) %in% c("value_negative", "Experiment")) ])),
  #                  names_from = Experiment, names_sep = ".", values_from = c(value_negative))

    #print(slong3)
    if(input$posc2 != ""){
      
      b2 <- colnames(slong3)[!(colnames(slong3) %in% c("value_negative", "value_positive", "value", "value_norm", "perc_change")) ]
      b3 <- colnames(slong3)[!(colnames(slong3) %in% c("value_negative", "value_positive","Experiment" ,"value", "value_norm", "perc_change")) ]
      
      slong4 <- slong3 %>%
        select(-c(value, value_negative, value_positive)) %>%
        group_by(across(all_of(b2))) %>%
        mutate(n = row_number()) %>%
        pivot_wider(  id_cols = c(n, all_of(b3)),
                      names_from = Experiment, names_sep = ".", values_from = c(value_norm, perc_change))
      
      
    }
    
    statCombine2(slong4)
    
    }
  })
  
  output$stattab2 <- renderDataTable({
    
    if(!is.null(statCombine2()) ){
      
      p2 <- statCombine2() %>%
        arrange(across(all_of(input$arrcol3)))
      
      statCombine2(p2)
      
      p2
      
    }
    
  },options = list(pageLength = 100))
    
  output$dm <- downloadHandler(
    filename = function() {"metadata.xlsx"},
    content = function(file) {write_xlsx(metadata(), path = file)}
  )
  
  output$dp <- downloadHandler(
    filename = function() {"dataperc.xlsx"},
    content = function(file) {write_xlsx(percCombine(), path = file)}
  )
  
  output$ds <- downloadHandler(
    filename = function() {"datastat.xlsx"},
    content = function(file) {write_xlsx(statCombine(), path = file)}
  )
  
  output$ds2 <- downloadHandler(
    filename = function() {"datastat_norm.xlsx"},
    content = function(file) {write_xlsx(statCombine2(), path = file)}
  )
  
  output$dpreset <- downloadHandler(
    filename = function() {"presets.rds"},
    content = function(file) {saveRDS(object = savePreset(),file)}
  )
  
}


shinyApp(ui, server)
