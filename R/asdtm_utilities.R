#' @title Parse docx tables to list of data.frames
#' @description Takes a docx as an imput and returns list of data.frames of all tables in document 
#' @param word_doc character, path to word docx file
#' @return list
#' @importFrom xml2 read_xml xml_ns xml_find_all xml_text
#' @importFrom utils unzip
#' @export
get_tbls <- function(word_doc) {
  
  tmpd <- tempdir()
  tmpf <- tempfile(tmpdir = tmpd, fileext = ".zip")
  
  file.copy(word_doc, tmpf)
  utils::unzip(tmpf, exdir = sprintf("%s/docdata", tmpd))
  
  doc <- xml2::read_xml(sprintf("%s/docdata/word/document.xml", tmpd))
  
  unlink(tmpf)
  unlink(sprintf("%s/docdata", tmpd), recursive = TRUE)
  
  ns <- xml2::xml_ns(doc)
  
  tbls <- xml2::xml_find_all(doc, ".//w:tbl", ns = ns)
  
  lapply(tbls, function(tbl) {
    
    cells <- xml2::xml_find_all(tbl, "./w:tr/w:tc", ns = ns)
    rows <- xml2::xml_find_all(tbl, "./w:tr", ns = ns)
    dat <- data.frame(matrix(xml2::xml_text(cells[-1]), 
                             ncol = (length(cells[-1])/length(rows[-1])), 
                             byrow = TRUE), stringsAsFactors = FALSE)
    colnames(dat) <- dat[1, ]
    dat <- dat[-1, ]
    rownames(dat) <- NULL
    dat
    
  })
}

#' @title Create sdtm structure files from ASDTM tables from docx file
#' @description Create files that summarize sdtm contents, files and domain variable meta information 
#' @param path character, path to word docx file
#' @return list
#' @import dplyr
#' @importFrom plyr ldply
#' @importFrom tools file_path_sans_ext
#' @export
parse_docs <- function(path) {
  data.dir <- sprintf("%s/HBGD/all%s/Main/sdtm", 
                      normalizePath(get_git_base_path(), winslash = "/"),
                      path)
  
  # list of Tables
  sdtm.tbls.list <- get_tbls(file.path(data.dir, "Define_ASDTM.docx"))
  names(sdtm.tbls.list) <- toupper(c("DataSet.Summary", sdtm.tbls.list[[1]]$Dataset))
  
  # Remove tables that arent in Longitudinal folder
  idx <- names(sdtm.tbls.list)[!grepl("[._]|SITES", names(sdtm.tbls.list))]
  
  # Convert to Dataframe
  sdtm.tbls.df <- plyr::ldply(sdtm.tbls.list[idx], .id = "DATASET", stringsAsFactors = F)
  
  # First table is the table of contents
  sdtm.contents <- data.frame(sdtm.tbls.list[[1]], stringsAsFactors = F) %>% 
    dplyr::rename_(.dots = c(DATASET='Dataset', DESCRIPTION='Description.of.dataset')) %>% 
    dplyr::filter_(~!grepl("[._]|SITES", DATASET))
  
  # File names
  sdtm.files <- data.frame(file = list.files(data.dir, pattern = "csv", full.names = TRUE), stringsAsFactors = F) %>% 
    dplyr::mutate(DATASET = tools::file_path_sans_ext(basename(file))) %>% 
    dplyr::rename_(FILE = 'file') %>% 
    dplyr::filter_(~!grepl("[._]|SITES", DATASET))
  
  
  # Join the tables
  
  sdtm.contents <- sdtm.contents %>% 
    dplyr::left_join(sdtm.files, by = "DATASET") %>% 
    dplyr::left_join(data.frame(file.info(sdtm.files$FILE)) %>% 
    dplyr::select_('size') %>% 
    dplyr::mutate(FILE = rownames(.), size = size/1e+06) %>% 
    dplyr::rename_(SIZE = 'size'), by = "FILE") %>% 
    dplyr::arrange(desc(SIZE)) %>% 
    dplyr::left_join(sdtm.tbls.df %>% 
    dplyr::count(DATASET) %>% 
    dplyr::rename_(NCOL = 'n') %>% 
    dplyr::mutate(DATASET = as.character(DATASET)), by = "DATASET")
  
  return(list(contents = sdtm.contents, files = sdtm.files, df = sdtm.tbls.df))
}