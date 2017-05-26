query_db <- function(pattern, field = "LABEL", df = ghap::meta_ghap, ...) {
  
  if (!field %in% names(df)) 
    stop(sprintf("field : '%s' not in %s : '%s'", field, class(df), deparse(substitute(df))))
  
  df[grepl(pattern = pattern, x = df[, field], ...), ]
  
}


#' @title api to search ghap respositories
#' @description use regular expression to search fields in ghap study meta data
#' @param query character, search expression
#' @param field character, column of meta data to search on, Default: 'LABEL'
#' @param complete boolean, only show complete cases of query result, Default: FALSE
#' @param ... parameters to pass to grepl
#' @return data.frame
#' @import dplyr
#' @importFrom DT datatable
#' @importFrom plyr ddply
#' @importFrom reshape2 dcast
#' @importFrom htmltools html_print
#' @export
#' @examples 
#' \donttest{search_ghap(c('^Birth','birth \\(days\\)$'),complete = FALSE)}
search_ghap <- function(query, field = "LABEL", complete = FALSE, ...) {
  y <- data.frame(query = query, stringsAsFactors = FALSE) %>% 
    plyr::ddply(c("query"), .fun = query_db, ...)
  
  out <- y %>% 
    dplyr::mutate_(COLS = paste0('sprintf("%s\n[%s]",', field,', variable)')) %>% 
    reshape2::dcast(Study_Type + STUDYID + DOMAIN ~ COLS, value.var = "query")
  
  if (complete) 
    out <- out %>% 
    dplyr::filter_(~complete.cases(.))
  
  htmltools::html_print(DT::datatable(out, 
                extensions = c("Buttons", "Scroller", "ColReorder", "FixedColumns"), 
                filter = "top", 
                options = list(deferRender = TRUE, 
                               scrollX = TRUE, 
                               pageLength = 50, 
                               scrollY = 500, 
                               scroller = TRUE, 
                               dom = "Bfrtip", 
                               colReorder = TRUE, 
                               fixedColumns = TRUE, 
                               buttons = c("copy", "csv", "excel", "pdf", "print", "colvis")
                               )
                ))
  
  invisible(out)
}