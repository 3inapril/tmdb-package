#' Get url to pull discover data from
#' 
#' @description This function generate url based on a list of condition users specify 
#'    to query qualified movie or tv
#' @usage get_discover_url(query_list=list(), search_cat='movie')
#' @param query_list The list of criterion that narrow the range of returned results. 
#'     For movie and tv there are respective legit variable to specify. To check the
#'     list, print object discover_query_strings to see.
#' @param search_cat The variable that clarify whether the query id for movie or tv
#' @return An url in format of character. 
#'
#' @examples
#' \dontrun{
#' sample_query_list <- list(year=2012, language='en-US', with_genres=12)
#' target_url <- get_discover_url(sample_query_list, 'movie')
#' }
#' @export
get_discover_url <- function(query_list=list(), search_cat='movie'){
  
  api_key <- TMDb_Env$api_key
  if (is.null(api_key)) stop('Need api_key authentication information. \n Please use function auth_key() first.')
  method <- 'discover/'
  url <- tryCatch({
    
    if(!search_cat %in% c('movie', 'tv') | length(search_cat) != 1)
      stop("search_cat must be either 'movie' or 'tv'")
    
    
    valid_strings <- discover_query_strings[[search_cat]]
    sub_idx <- which(names(query_list) %in% valid_strings)
    query_list_sel <- query_list[sub_idx]
    
    query_seg <- lapply(seq_along(query_list_sel), 
                        function(i) paste0(names(query_list_sel)[[i]],
                                           '=', query_list_sel[[i]]))
    
    query_seg2 <- unlist(query_seg)
    
    if(!any(grepl('language', query_seg2))){
      query_seg2 <- c("language=en-US", query_seg2)
    }
    
    query_part <- paste0(query_seg2, collapse = '&')
    
    rst <- URLencode(paste0('https://api.themoviedb.org/3/', method, search_cat,
                            '?api_key=', api_key, 
                            if(query_part!='') '&',
                            query_part))
    return(rst)
    
  }, error = function(err) {
    
    rst <- NULL
    return(rst)}, finally = {})
  
  return(url)
}


#' Given a query criterion, return multi-page of data using discover functionality
#' 
#' @description This function takes in a list of condition users specify and the 
#'     target they are query against(movie or tv) and return qualified target. 
#'     Noted that the the page can take multiple values here instead of one. Due 
#'     to the rate limit of API, page can be no longer than 40 integers.
#' @usage discover_multi_page(query_list=list(), search_cat='movie', verbose=T)
#' @param query_list The list of criterion that narrow the range of returned results. 
#'     For movie and tv there are respective legit variable to specify. To check the
#'     list, print object discover_query_strings to see.
#' @param search_cat The variable that clarify whether the query id for movie or tv
#' @param verbose A logic value that control whether the page pulled info should 
#'     be sent as a message
#' @return An object of class \code{tmdb_api} that consists quried data in a 
#'     data frame or a list and a metadata list 
#'
#' @examples
#' \dontrun{
#' sample_query_list <- list(year=2012, language='en-US', with_genres=12, page=c(2,3))
#' result_data <- discover_multi_page(sample_query_list, 'movie')
#' }
#' @export
discover_multi_page <- function(query_list=list(), search_cat='movie', verbose=T){
  
  api_key <- TMDb_Env$api_key
  if (is.null(api_key)) stop('Need api_key authentication information. \n Please use function auth_key() first.')
  
  if(!'page' %in% names(query_list)){
    query_list$page <- 1
  } else if(length(query_list$page)==1){
    query_list$page <- query_list$page  
  }

  if(length(query_list$page)==1){
    return(get_result(get_discover_url(query_list, search_cat)))
  }
  
  if (length(query_list$page) >1){
    
    if(length(query_list$page) <= 40){
      loop_page <- query_list$page
    } else {
      loop_page <- query_list$page[1:40]
      warnings('Exceed the upper limit of 40 pages, return as the first 40 pages as queried. Please query the rest of pages in 10 seconds')
    }
    
    temp_query_list <- query_list
    temp_query_list$page <- min(loop_page)
    
    temp_url <- get_discover_url(query_list=temp_query_list)
    if(verbose){temp_resp <- get_result(temp_url)} else {
      temp_resp <- suppressMessages(get_result(temp_url))
      }
    
    loop_page <- loop_page[loop_page <= content(temp_resp$response_detail)$total_pages]
    
    if(length(loop_page) == 1){

      temp_resp
      
    } else {
      
      returned_dat <- temp_resp$data
      
      for (i in 2:length(loop_page)){
        
        query_list$page <- loop_page[i]
        temp_url <- get_discover_url(query_list=query_list)
        if(verbose){temp_resp <- get_result(temp_url)} else {
          temp_resp <- suppressMessages(get_result(temp_url))}
        
        returned_dat <- rbind(returned_dat, temp_resp$data)
      }
      
      structure(
        list(
          data = returned_dat,
          response_detail=temp_resp$response_detail
        ),
        class = "tmdb_api")
      
    }
  }
}
