shiny::shinyUI(shiny::fluidPage(
   useShinyjs(),
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
  )
