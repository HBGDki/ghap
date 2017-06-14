load('filters.Rdata')
shinyServer(function(input, output, session) {
  
  sessionvalues <- reactiveValues()
  
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
              options = list(
                dom= 'itp',
                ajax = list(url = action),
                deferRender = TRUE, 
                scrollX = TRUE, 
                pageLength = 50, 
                scrollY = 500, 
                scroller = TRUE, 
                colReorder = TRUE, 
                fixedColumns = TRUE, 
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print', 'colvis')
              )
    )
  }, server = TRUE)
  
  observeEvent(input$queryBuilderSQL,{
    
    y <- loadData(input$queryBuilderSQL)

    if(nrow(y)<nrow(meta_ghap)){
    
      output$study_select <- 
        
        renderDataTable({
          #browser()
          out <- y %>% 
            dplyr::mutate(COLS = sprintf("%s\n[%s]",LABEL,variable),val=1) %>% 
            reshape2::dcast(Study_Type + STUDYID + DOMAIN ~ COLS,value.var='val')
          
          if(input$complete)
            out <- out %>% 
            dplyr::filter_(~complete.cases(.))
          
          f<<-out
          
          DT::datatable(out,
                        extensions = c('Buttons', 'Scroller', 'ColReorder', 'FixedColumns'), 
                        filter = 'top', 
                        options = list(
                          deferRender = TRUE, 
                          scrollX = TRUE, 
                          pageLength = 50, 
                          scrollY = 500, 
                          scroller = TRUE, 
                          dom = 'Bfrtip', 
                          colReorder = TRUE, 
                          fixedColumns = TRUE, 
                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print', 'colvis')
                        ))
          
        }) 
    }
  })
  
})
