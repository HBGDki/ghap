library(shiny)
library(vcs)
library(ghap)
library(jsTree)
# 
studies=ghap::get_study_list()

tips.folders=c(adam='Analysis-Ready data derived form SDTM data sets (USERS: ALL)',
               docs='Study documentation (USERS: ALL)',
               fmt='SAS format files (USERS: Data Management)',
               import='Raw metadata submitted by Prinicipal Investigators (USERS: Data Management)',
               jobs='SAS programs for creating SDTM datasets',
               raw='Data submitted by Prinicipal Investigators (USERS: Data Management)',
               sdtm='Data in HBGDki Standardized format (USERS: Data Scientists)'
)

fields=c('study_description','subject_count','grant_folder','study_type','intervention_type','country','population','study_url')

fields.title=gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", gsub('_',' ',fields), perl=TRUE)

tips.studies.name=t(apply(studies[,fields[c(1)]],1,function(x){
  paste(fields.title[c(1)],x,sep=': ')
}))

tips.studies.grant=tips.studies.name

names(tips.studies.name)=file.path(studies[['study_id']],'master')
names(tips.studies.grant)=file.path(studies[['grant_folder']],'master')

tips=c(tips.folders,tips.studies.name,tips.studies.grant)


basepath=normalizePath(get_git_base_path(),winslash = '/')

server <- function(input, output,session) {
  
  network <- reactiveValues()
  
  # tree.list<-reactive({
  #   unlist(sapply(dir(file.path(basepath,input$f0)),function(i) file.path(i,vcs::ls_remote(file.path(basepath,input$f0,i),vcs='git'))))
  # })
  
  observeEvent(input$tree_update,{
    current_selection<-input$tree_update$.current_tree
    if(!is.null(current_selection)) network$tree <- jsonlite::fromJSON(current_selection)
  })
  
  observeEvent(c(input$f0,input$f1,network$tree),{
    output$results <- renderPrint({
      str.out=''
      if(length(network$tree)>0) str.out=network$tree
      return(str.out)
    })    
  })
  
  observeEvent(c(input$f0,input$f1),{
    output$txt<-renderUI({
      textInput('dirOutput','',placeholder = 'path of checkout',value = file.path(basepath,input$f0,input$f1))
    })  
  })
  
  observeEvent(c(input$f0,input$f1),{
    output$btn<-renderUI({
      txt<-ifelse(dir.exists(file.path(basepath,input$f0,input$f1)),'Update sparse checkout','Create Sparse Checkout')
      actionButton('createRepo',txt)      
    })

  })
  
  observeEvent(input$f1,{
  output$tree <- jsTree::renderJsTree({
    obj=vcs::ls_remote(file.path(basepath,input$f0,input$f1),vcs='git')
    jsTree::jsTree(obj = obj,remote_repo = input$f1,vcs='git',tooltips = tips,vcs::diff_head(file.path(basepath,input$f0,input$f1),vcs='git',show = FALSE))
  })
  })
  
  observeEvent(c(input$createRepo),{
    f2<-gsub(sprintf('%s/%s',input$f1,'master'),'',network$tree)
    if(length(f2)>0){
      if(dir.exists(sprintf('%s/.git',input$dirOutput))){
        ghap::sparse_ghap(repo_url = sprintf('https://git.ghap.io/stash/scm/hbgd/%s.git',input$f1),
                          queries = f2,
                             repo = gsub(basepath,'',input$dirOutput),
                             create = !'sparse-checkout'%in%basename(dir(file.path(basepath,input$f0,input$f1,'.git'),recursive = TRUE)),
                             append = FALSE)
      }else{
        ghap::sparse_ghap(repo_url = sprintf('https://git.ghap.io/stash/scm/hbgd/%s.git',input$f1),
                             queries = f2,
                             repo = gsub(basepath,'',input$dirOutput),
                             create = TRUE)
      }
    }
  })
  
  observeEvent(input$f0,{
    output$repo<-renderUI({
      selectInput(inputId = 'f1',label = 'choose repo',choices = dir(file.path(basepath,input$f0)),selected = dir(file.path(basepath,input$f0))[1])  
    })
    
  })
  
  
}

ui <- fluidPage(
  selectInput(inputId = 'f0',label = 'choose directory',choices = dir(file.path(basepath))[!grepl('common',dir(file.path(basepath)),ignore.case = TRUE)],selected = 'HBGD'),
  uiOutput('repo'),
  uiOutput('btn'),
  uiOutput('txt'),
  verbatimTextOutput(outputId = "results"),
  jsTree::jsTreeOutput(outputId = 'tree')
)

shinyApp(ui = ui, server = server)
