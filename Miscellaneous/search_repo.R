library(shiny)
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
  
  output$tree <- jsTree::renderJsTree({
    jsTree::jsTree(obj = ciderhouse::ls_head(file.path(get_git_base_path(),'HBGD1',input$f1)))
  })
  
  observeEvent(c(input$createRepo),{
    f2<-gsub(sprintf('%s/%s',input$f1,'master'),'',network$tree)
    if(length(f2)>0){
      ghap::use_study(input$f1)
    }
  })
  
}

ui <- fluidPage(
  selectInput(inputId = 'f1',label = 'choose repo',choices = list.dirs(file.path(get_git_base_path(),'HBGD1'),recursive = FALSE,full.names = FALSE),selected = "Peru Zn"),
  actionButton('createRepo','create sparse checkout'),
  verbatimTextOutput(outputId = "results"),
  jsTree::jsTreeOutput(outputId = 'tree')
)

shinyApp(ui = ui, server = server)

