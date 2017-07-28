createDB<-function(MYDIR) {

  if(file.exists(file.path(MYDIR,'filters.Rdata'))) load(file.path(MYDIR,'filters.Rdata'))
  nm=names(meta_ghap)
  meta_ghap=meta_ghap[,c('STUDY_TYPE',nm[nm!='STUDY_TYPE'])]
  col_opts <- sapply(names(meta_ghap)[sapply(meta_ghap,class)=='character'],function(x) list(type='select',plugin='selectize'),simplify = FALSE)
  filters<-getFiltersFromTable(data = meta_ghap,column_opts = col_opts)
  save(file=file.path(MYDIR,'filters.Rdata'),filters)

  datadb<-dbConnect(RSQLite::SQLite(), file.path(MYDIR,"data/data.db"))
  dbWriteTable(datadb,"datatable",meta_ghap,row.names=F,overwrite=TRUE)
  dbDisconnect(datadb)
}

loadData<-function(sql,MYDIR) {
  if (sql!="") sql<-paste0("where ",sql)
  datadb<-dbConnect(RSQLite::SQLite(), file.path(MYDIR,"data/data.db"))
  datacontent<-dbGetQuery(datadb,paste0("select * from datatable ",sql))
  dbDisconnect(datadb)
  datacontent
}

get_study_n<-function(current_query){
  
  n_summ<-current_query%>%
    select(STUDY_TYPE,DOMAIN,STUDY_ID,VARIABLE=STUDY_VARIABLE)%>%distinct%>%
    group_by(STUDY_TYPE,DOMAIN,STUDY_ID)%>%
    summarise_at(funs(paste0(sprintf('%s IS NOT NULL',.),collapse=' AND ')),.vars=vars(VARIABLE))%>%
    group_by(STUDY_TYPE,DOMAIN,VARIABLE)%>%
    summarise_at(funs(paste0(sprintf("'%s'",.),collapse=',')),.vars=vars(STUDY_ID))
  
  
  if(file.exists('../data/ghap_longitudinal.sqlite3')) long_db<-dbConnect(RSQLite::SQLite(), "../data/ghap_longitudinal.sqlite3")
  if(file.exists('../data/ghap_cross_sectional.sqlite3')) cross_db<-dbConnect(RSQLite::SQLite(), "../data/ghap_cross_sectional.sqlite3")
  
  
  get_n<-n_summ%>%ddply(.(STUDY_TYPE,DOMAIN,VARIABLE),.fun=function(x){
    q <- sprintf("select STUDYID as STUDY_ID, count(DISTINCT SUBJID) as SUBJID_N from %s WHERE %s AND STUDY_ID IN (%s) GROUP BY STUDY_ID",x$DOMAIN,x$VARIABLE,x$STUDY_ID)
    
    if(x$STUDY_TYPE=='Longitudinal'){
      DBI::dbGetQuery(conn=long_db,q)
    }else{
      DBI::dbGetQuery(conn=cross_db,q)
    }
    
  },.progress = 'text')
  
  dbDisconnect(long_db)
  dbDisconnect(cross_db)
  
  get_n%>%select(-VARIABLE)
}


#' @title Shinyapp to navigate and maintain GHAP repositories
#' @description Shinyapp to navigate and maintain GHAP repositories run from console or launch from addin menu 
#' @param viewer Where to open the application can be dialogViewer, browserViewer or paneViewer , Default: shiny::dialogViewer()
#' @return nothing
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  searchbuilder()
#'  }
#' }
#' @rdname searchbuilder
#' @export 
#' @import dplyr
#' @importFrom jsonlite fromJSON
#' @importFrom jsTree renderJsTree jsTree jsTreeOutput
#' @importFrom miniUI miniPage gadgetTitleBar miniTitleBarButton miniContentPanel
#' @importFrom reshape2 dcast
#' @import shiny
#' @importFrom vcs ls_remote diff_head
searchbuilder <- function(viewer = shiny::dialogViewer(dialogName = 'GHAP',width = 3000,height = 2000)){
  
  MYDIR <- file.path(tempdir(),'mydir')
  
  if(!dir.exists(MYDIR)){
    dir.create(MYDIR)
    dir.create(file.path(MYDIR,'data'))
    dbConnect(RSQLite::SQLite(), file.path(MYDIR,"data/data.db"))
  }
  
  createDB(MYDIR)
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar('GHAP Search Builder',
                           left = miniUI::miniTitleBarButton(inputId = "qt","Quit",primary = TRUE),
                           right=NULL),
    miniUI::miniContentPanel(
      shiny::fluidPage(
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::h3('Define and apply filters'),
            queryBuildROutput('queryBuilderWidget',height='100%'),
            shiny::actionButton('queryApply', label = 'Apply filters'),
            shiny::tags$script("
                               function getSQLStatement() {
                               var sql = $('#queryBuilderWidget').queryBuilder('getSQL', false);
                               Shiny.onInputChange('queryBuilderSQL', sql);
                               };
                               document.getElementById('queryApply').onclick = function() {getSQLStatement()}
                               "),
            shiny::tags$h4('Query string applied to Study Meta Information'),
            shiny::textOutput('sqlQuery'),
            shiny::span(shiny::uiOutput('zero_out'),style="color:red"),
            shiny::hr(),
            shiny::uiOutput('chk_n'),
            shiny::uiOutput('chk_complete_rows'),
            shiny::uiOutput('btn_copy'),
            shiny::uiOutput('chk_tree'),
            shiny::conditionalPanel('input.chk_tree==true',
                                    shiny::uiOutput('study_choose'),
                                    shiny::uiOutput('btn_tree'),
                                    shiny::uiOutput('tree_show')
            ),
            
            width=4
            ),
          shiny::mainPanel(
            shiny::fluidRow(
              shiny::uiOutput('table_tag'),
              DT::dataTableOutput('study_select'),
              shiny::h1(''),
              shiny::tags$h3('Study Meta Information'),
              shiny::tags$h4('Use this table to select columns and search terms for the query builder in the left side panel.'),
              shiny::helpText('This table contains unique rows by (Study Type, Domain, Study ID, Study Variable). 
                              Additionally there is meta-information for Study Variables, Studies and Repositories in the additional columns.'),
              shiny::helpText('The table may be searched globally (search field above the table to the right) or by column (search fields above each row),
                              and it can be copied to the clipboard or exported to a csv file (application must be running in a web browser)'),
              DT::dataTableOutput('table'),
              shiny::tags$div(class='extraspace')
              
              ),
            width=8
            )
        )
          )
      
      
    ))
 
  
  server <- function(input, output,session) {
    
    sessionvalues <- reactiveValues()
    network <- reactiveValues()
    
    sessionvalues$data<-loadData(sql = '',MYDIR = MYDIR)
    
    observe({
      if (length(input$queryBuilderSQL)>0)
        sessionvalues$data<-loadData(input$queryBuilderSQL,MYDIR = MYDIR)
    })
    
    output$sqlQuery<-renderText({
      sql<-''
      if (length(input$queryBuilderSQL)>0) {
        if (input$queryBuilderSQL!='')
          sql<-paste0('where ', input$queryBuilderSQL)
      }
      paste0('select * from datatable ',sql)
    })
    
    output$queryBuilderWidget<-renderQueryBuildR({
      data<-sessionvalues$data
      load(file.path(MYDIR,'filters.Rdata'))
      rules<-NULL
      queryBuildR(filters)
    })
    
    output$table<-DT::renderDataTable({
      data<-sessionvalues$data
      colnames(data)<-as.vector(sapply(colnames(data),function(x) gsub('[_.]',' ',x)))
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
      y <- loadData(input$queryBuilderSQL,MYDIR = MYDIR)
      
      y <- y%>%dplyr::count(STUDY_TYPE,STUDY_ID)
      
      y_copy <- y$STUDY_ID
      
      if(length(input$study_select_rows_selected)>0) y_copy<-y_copy[input$study_select_rows_selected]
      
      writeClipboard(y_copy)
    })
    
    observeEvent(input$queryBuilderSQL,{
      
      y <- loadData(input$queryBuilderSQL,MYDIR = MYDIR)
      
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
            
            DT::renderDataTable({
              
              out <- y %>% 
                dplyr::mutate(COLS = sprintf("%s\n[%s]",STUDY_VARIABLE_DESCRIPTION,STUDY_VARIABLE),val=1) %>% 
                reshape2::dcast(STUDY_TYPE + STUDY_ID + DOMAIN ~ COLS,value.var='val')
              
             
              
              if(dir.exists('../data')){
              if(input$get_n){
                study_n<-get_study_n(y)
                out<-out%>%left_join(study_n,by=c('STUDY_TYPE','DOMAIN','STUDY_ID'))
              }}
              
              
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
      if(dir.exists(get_git_base_path()))
        list(shiny::hr(),
             shiny::checkboxInput('chk_tree','Visualization of Study Repository Contents',value = FALSE)
        )
    })
    
    observeEvent(input$tree_update,{
      current_selection<-input$tree_update$.current_tree
      if(!is.null(current_selection)) network$tree <- jsonlite::fromJSON(current_selection)
    })
    
    observeEvent(input$study_tree,{
      
      basepath=normalizePath(get_git_base_path(),winslash = '/')
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
          
          basepath=normalizePath(get_git_base_path(),winslash = '/')
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
      basepath<-normalizePath(get_git_base_path(),winslash = '/')
      dirOutput<-file.path(basepath,'HBGD',input$study_tree)
      dirGit<- file.path(dirOutput,'.git')
      study<-sessionvalues$data%>%count(STUDY_ID_SHORT,STUDY_REPOSITORY_NAME)
      study_name<-study$STUDY_ID_SHORT[which(study$STUDY_REPOSITORY_NAME==input$study_tree)]
      f2<-'*.txt'
      
      if(length(f2)>0){
        if(dir.exists(dirGit)){
          f2<-gsub(sprintf('%s/%s',input$study_tree,'master'),'',network$tree)
          use_study(study_name,
                          queries=f2,
                          create = !'sparse-checkout'%in%basename(dir(dirGit,recursive = TRUE)),
                          append = FALSE)
        }else{
          use_study(study_name,queries=f2)
        }
      }
    })
    
    shiny::observeEvent(input$qt,{
      unlink(MYDIR,recursive = TRUE)
      shiny::stopApp()
      })
      }
  shiny::runGadget(ui, server, viewer = viewer)
}