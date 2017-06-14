library(shiny)
library(vcs)
library(ghap)
library(jsTree)

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

tips.studies=t(apply(studies[,fields[c(1)]],1,function(x){
  paste(fields.title[c(1)],x,sep=': ')
}))


names(tips.studies)=file.path(studies[['study_id']],'master')

tips=c(tips.folders,tips.studies)

tree.list<-unlist(sapply(dir(file.path(get_git_base_path(),'HBGD')),function(i) file.path(i,vcs::ls_remote(file.path(get_git_base_path(),sprintf('HBGD/%s',i)),vcs='git'))))


server <- function(input, output,session) {
  
  network <- reactiveValues()
  
  observeEvent(input$tree_update,{
    current_selection<-input$tree_update$.current_tree
    if(!is.null(current_selection)) network$tree <- jsonlite::fromJSON(current_selection)
  })
  
  observeEvent(network$tree,{
    output$results <- renderPrint({
      str.out=''
      if(length(network$tree)>0) str.out=network$tree
      return(str.out)
    })    
  })
  
  observeEvent(input$f1,{
    output$txt<-renderUI({
      textInput('dirOutput','',placeholder = 'path of checkout',value = file.path(get_git_base_path(),sprintf('HBGD/%s',input$f1)))
    })  
  })
  
  observeEvent(input$f1,{
    output$btn<-renderUI({
      txt<-ifelse(dir.exists(file.path(get_git_base_path(),sprintf('HBGD/%s',input$f1))),'Update sparse checkout','Create Sparse Checkout')
      actionButton('createRepo',txt)      
    })

  })
  
  
  output$tree <- jsTree::renderJsTree({
    obj=vcs::ls_remote(file.path(get_git_base_path(),sprintf('HBGD/%s',input$f1)),vcs='git')
    jsTree::jsTree(obj = obj,remote_repo = input$f1,vcs='git',tooltips = tips,vcs::diff_head(file.path(get_git_base_path(),sprintf('HBGD/%s',input$f1)),vcs='git',show = FALSE))
  })
  
  observeEvent(c(input$createRepo),{
    f2<-gsub(sprintf('%s/%s',input$f1,'master'),'',network$tree)
    if(length(f2)>0){
      if(dir.exists(sprintf('%s/.git',input$dirOutput))){
        ghap::sparse_ghap(repo_url = sprintf('https://git.ghap.io/stash/scm/hbgd/%s.git',input$f1),
                          queries = f2,
                             repo = gsub(get_git_base_path(),'',input$dirOutput),
                             create = FALSE,
                             append = FALSE)
      }else{
        ghap::sparse_ghap(repo_url = sprintf('https://git.ghap.io/stash/scm/hbgd/%s.git',input$f1),
                             queries = f2,
                             repo = input$dirOutput,
                             create = TRUE)
      }
    }
  })
  
}

ui <- fluidPage(
  selectInput(inputId = 'f1',label = 'choose repo',choices = dir(file.path(get_git_base_path(),'HBGD')),selected = dir(file.path(get_git_base_path(),'HBGD'))[1]),
  uiOutput('btn'),
  uiOutput('txt'),
  verbatimTextOutput(outputId = "results"),
  jsTree::jsTreeOutput(outputId = 'tree')
)

shinyApp(ui = ui, server = server)
