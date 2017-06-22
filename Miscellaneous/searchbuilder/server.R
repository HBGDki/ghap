shinyServer(function(input, output, session) {
  
  sessionvalues <- reactiveValues()
  network <- reactiveValues()
  
  sessionvalues$data<-loadData('')
  
  observe({
    if (length(input$queryBuilderSQL)>0)
      sessionvalues$data<-loadData(input$queryBuilderSQL)
  })
  
  output$sqlQuery<-renderText({
    sql<-''
    if (length(input$queryBuilderSQL)>0) {
      if (input$queryBuilderSQL!='')
        sql<-paste0('where ',input$queryBuilderSQL)
    }
    paste0('select * from datatable ',sql)
  })
  
  output$queryBuilderWidget<-renderQueryBuildR({
    data<-sessionvalues$data
    load('filters.Rdata')
    rules<-NULL
    queryBuildR(rules,filters)
  })
  
  output$table<-renderDataTable({
    data<-sessionvalues$data
    colnames(data)<-as.vector(sapply(colnames(data),idToName))
    action <- dataTableAjax(session, data,rownames=F)
    
    DT::datatable(data, rownames=F, 
                  extensions = c('Buttons', 'Scroller', 'ColReorder', 'FixedColumns'),
                  filter = 'top', 
                  options = list(
                    dom= c('Bfrtip'),
                    ajax = list(url = action),
                    deferRender = TRUE, 
                    scrollX = TRUE, 
                    pageLength = 50, 
                    scrollY = pmin(100+500*(nrow(data)/50),500), 
                    scroller = TRUE, 
                    colReorder = TRUE, 
                    fixedColumns = TRUE, 
                    buttons = c('copy', 'csv', 'colvis')
                  )
    )
  }, server = TRUE)
  
  observeEvent(input$btn_copy,{
    y <- loadData(input$queryBuilderSQL)
    
    y <- y%>%dplyr::count(STUDY_TYPE,STUDY_ID)
    
    y_copy <- y$STUDY_ID
    
    if(length(input$study_select_rows_selected)>0) y_copy<-y_copy[input$study_select_rows_selected]
    
    writeClipboard(y_copy)
  })
  
  observeEvent(input$queryBuilderSQL,{

    y <- loadData(input$queryBuilderSQL)

    if(nrow(y)==0){
      output$zero_out<-shiny::renderText('Query matched zero rows')
    }else{
      output$zero_out<-shiny::renderText('')
      if(nrow(y)<nrow(meta_ghap)){
        
        output$table_tag<-renderUI({
          list(shiny::tags$h3('Studies Query Output'),
               shiny::helpText('Table contains unique rows of (Study Type and Study ID) and
                               columns containing the STUDY VARIABLE DESCRIPTION [STUDY VARIABLE NAME] that are result of the query. 
                               A value of 1 indicates that the study contains this column.'),
               shiny::helpText('The table may be searched globally (search field above the table to the right) or by column (search fields above each row),
                               and it can be copied to the clipboard or exported to a csv file (application must be running in a web browser)')
               )
        })
        
        output$btn_copy<-renderUI({
          list(shiny::actionButton(inputId = 'btn_copy',label = 'Copy List of Studies to clipboard'),
               shiny::helpText('Use this button to copy to the clipboard the list of studies seen on 
                               the Studies Query Output table, if any rows are clicked/highlighted on 
                               the table only the highlighted ones will be copied')
               )
        })
        
        output$study_select <- 
          
          renderDataTable({
            
            out <- y %>% 
              dplyr::mutate(COLS = sprintf("%s\n[%s]",STUDY_VARIABLE_DESCRIPTION,STUDY_VARIABLE),val=1) %>% 
              reshape2::dcast(STUDY_TYPE + STUDY_ID + DOMAIN ~ COLS,value.var='val')
            
            if(input$get_n){
              study_n<-get_study_n(y)
              out<-out%>%left_join(study_n,by=c('STUDY_TYPE','DOMAIN','STUDY_ID'))
            }
            
            
            if(input$complete)
              out <- out %>% 
                dplyr::filter_(~complete.cases(.))
            
            DT::datatable(out,
                          extensions = c('Buttons', 'Scroller', 'ColReorder', 'FixedColumns'), 
                          filter = 'top',
                          options = list(
                            deferRender = TRUE, 
                            scrollX = TRUE, 
                            pageLength = 50, 
                            scrollY = pmin(100+500*(nrow(out)/50),500), 
                            scroller = TRUE, 
                            dom = 'Bfrtip', 
                            colReorder = TRUE, 
                            fixedColumns = TRUE, 
                            buttons = c('copy', 'csv', 'colvis')
                          ))
            
          }) 
        }      
    }
  })
  
  output$chk_complete_rows<-renderUI({
    list(shiny::checkboxInput('complete','Show only complete cases'),
         shiny::helpText('Use this checkbox to filter the Studies Query Output table to show only studies that have all the columns'))
  })
  
  output$chk_n<-renderUI({
    if(dir.exists('../data'))
      list(shiny::checkboxInput('get_n','Show number of subjects per study conditional on query result',value = FALSE),
           shiny::helpText('Use this checkbox to add an additional column to the Studies Query Output table that shows how many unique subjects are in the columns indicated for each study')
      )
  })
  
  output$chk_tree<-renderUI({
    if(dir.exists(ghap::get_git_base_path()))
      list(shiny::hr(),
           shiny::checkboxInput('chk_tree','Visualization of Study Repository Contents',value = FALSE)
      )
  })
  
  observeEvent(input$tree_update,{
    current_selection<-input$tree_update$.current_tree
    if(!is.null(current_selection)) network$tree <- jsonlite::fromJSON(current_selection)
  })
  
  observeEvent(input$study_tree,{

    basepath=normalizePath(ghap::get_git_base_path(),winslash = '/')
    dirOutput<-file.path(basepath,'HBGD',input$study_tree)
    dirGit<- file.path(dirOutput,'.git')
    
    
    
    if(!dir.exists(dirGit)){
      output$btn_tree<-renderUI({
        list(shiny::actionButton('btn_tree','Fetch Study'),
             shiny::helpText('Press the Fetch Study button to retrieve 
                             the file directory structure of the study repository 
                             chosen in the field above.')
        )
      })
      
      output$tree_show<-renderUI({
        shiny::p('')
      })
      
      
    }else{
      output$btn_tree<-renderUI({
        list(shiny::actionButton('btn_tree','Update Study'),
             shiny::helpText('Navigate the tree by clicking on folders to open them or using 
                              the search field above the tree, 
                              and choose which files to retrieve from the GHAP repository by 
                              checking next to a folder or a file. 
                              If there are already files in fetched from the repository 
                              they will be prechecked for you, uncheck them to remove files.
                              Press on the Update Study button to invoke the update.'))
      })
      
      
      
      tips.folders=c(adam='Analysis-Ready data derived form SDTM data sets (USERS: ALL)',
                     docs='Study documentation (USERS: ALL)',
                     fmt='SAS format files (USERS: Data Management)',
                     import='Raw metadata submitted by Prinicipal Investigators (USERS: Data Management)',
                     jobs='SAS programs for creating SDTM datasets',
                     raw='Data submitted by Prinicipal Investigators (USERS: Data Management)',
                     sdtm='Data in HBGDki Standardized format (USERS: Data Scientists)'
      )
      
      basepath=normalizePath(ghap::get_git_base_path(),winslash = '/')
      dirOutput<-file.path(basepath,'HBGD',input$study_tree)
      dirGit<- file.path(dirOutput,'.git')

        output$tree <- jsTree::renderJsTree({
          path=file.path(basepath,'HBGD',input$study_tree)
          if(dir.exists(path)){
          obj=vcs::ls_remote(path = path,vcs='git')
          jsTree::jsTree(obj = obj,
                         remote_repo = input$study_tree,vcs='git',
                         tooltips = tips.folders,
                         nodestate = vcs::diff_head(path,vcs='git',show = FALSE))
          }else{
            shiny::p('')
          }
        })
        
        output$tree_show<-renderUI({
          jsTree::jsTreeOutput(outputId = 'tree')
        })
    }
    
    
  })
  
  observeEvent(sessionvalues$data,{

    output$study_choose<-renderUI({
      study<-sessionvalues$data%>%count(STUDY_ID_SHORT,STUDY_REPOSITORY_NAME)
      study_split<-split(study$STUDY_REPOSITORY_NAME,study$STUDY_ID_SHORT)
      shiny::selectInput(inputId = 'study_tree',label = 'Select Study to Preview',choices = study_split,selected = study_split[1])
    })    
  })

  observeEvent(input$btn_tree,{
    basepath<-normalizePath(ghap::get_git_base_path(),winslash = '/')
    dirOutput<-file.path(basepath,'HBGD',input$study_tree)
    dirGit<- file.path(dirOutput,'.git')
    study<-sessionvalues$data%>%count(STUDY_ID_SHORT,STUDY_REPOSITORY_NAME)
    study_name<-study$STUDY_ID_SHORT[which(study$STUDY_REPOSITORY_NAME==input$study_tree)]
    f2<-'*.txt'

    if(length(f2)>0){
      if(dir.exists(dirGit)){
        f2<-gsub(sprintf('%s/%s',input$study_tree,'master'),'',network$tree)
        ghap::use_study(study_name,
                        queries=f2,
                        create = !'sparse-checkout'%in%basename(dir(dirGit,recursive = TRUE)),
                        append = FALSE)
      }else{
        ghap::use_study(study_name,queries=f2)
      }
    }
  })
  
  
})
