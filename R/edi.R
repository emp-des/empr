#' Download EDI Package Files
#' Download the latest revision of files in an EDI package
#' @param pkg_id The EDI Package ID
#' @param fnames A vector of file names in the package to download. Supports regex.
#' @param verbose If TRUE, display descriptive messages.
#' @return a list of dataframes
#' @export
get_edi_file = function(pkg_id, fnames, verbose = TRUE){
  # get revision
  revision_url = glue::glue('https://pasta.lternet.edu/package/eml/edi/{pkg_id}')
  all_revisions = readLines(revision_url, warn = FALSE) 
  latest_revision = tail(all_revisions, 1)
  if (verbose) {
    message('Latest revision: ', latest_revision)
  }
  # get entities 
  pkg_url = glue::glue('https://pasta.lternet.edu/package/data/eml/edi/{pkg_id}/{latest_revision}')
  all_entities = readLines(pkg_url, warn = FALSE)
  name_urls = glue::glue('https://pasta.lternet.edu/package/name/eml/edi/{pkg_id}/{latest_revision}/{all_entities}')
  names(all_entities) = purrr::map_chr(name_urls, readLines, warn = FALSE)
  if (verbose) {
    message('Package contains files:\n', 
            stringr::str_c('    ', names(all_entities), sep = '', collapse = '\n'))
  }
  # select entities that match fnames
  fname_regex = stringr::str_c(glue::glue('({fnames})'), collapse = '|')
  included_entities = all_entities[stringr::str_detect(names(all_entities), fname_regex)]
  if(length(included_entities) != length(fnames)){
    stop('Not all specified filenames are included in package')
  }
  # download data
  if (verbose) {
    message('Downloading files:\n',
            stringr::str_c('    ', names(included_entities), sep = '', collapse = '\n'))
  }
  dfs = purrr::map(glue::glue('https://portal.edirepository.org/nis/dataviewer?packageid=edi.{pkg_id}.{latest_revision}&entityid={included_entities}'),
                   readr::read_csv, guess_max = 1000000)
  names(dfs) = names(included_entities)
  return(dfs)
}

programs <- c('DWQ','CWQ','Benthic','Phyto','Zoop')
#' Get EMP EDI Package Names
#' Gives package ID for EMP data
#' @param program The program to retrieve the EDI ID for. Must be one of `r knitr::combine_words(programs, and = 'or ')`.
#' @export
get_edi_ids = function(program){
  program <- tolower(program)
  str_programs <- toString(programs)
  
  if (!program %in% programs) {
    stop(glue::glue("'program' argument must be one of: {str_programs}"))
  }
  
  edi_ids <- list(
    'dwq' = 458,
    'cwq' = 1177,
    'benthic' = 1036,
    'phyto' = 1320,
    'zoop' = 522
  )
  
  id <- edi_ids[program]
  
  return(id)
}