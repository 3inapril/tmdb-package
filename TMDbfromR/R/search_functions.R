#' Get url to pull data from using the search functionality
#' 
#' @description This function generate url mainly based on one or a series 
#'     of keywords users specify to query from one of various target categories. 
#'     To see the full list of legit search categories please print 
#'     search_query_string to see
#' @usage get_search_url(query_list=list(), search_cat='movie')
#' @param query_list The list of criterion that narrow the range of returned results. 
#'     To see the full list of legit search categories please print 
#'     search_query_string 
#' @param search_cat The variable that clarify search category
#' @return An url in format of character. 
#'
#' @examples
#' \dontrun{
#' sample_query_list <- list(language='en-US', query='kill bill')
#' target_url <- get_search_url(sample_query_list, 'movie')
#' }
#' @export
get_search_url <- function(query_list, search_cat='movie'){
  
  api_key <- TMDb_Env$api_key
  if (is.null(api_key)) stop('Need api_key authentication information. \n Please use function auth_key() first.')
  
  method <- 'search/'
  
  url <- tryCatch({
    
    if(!search_cat %in% c('company', 'collection', 'keyword',
                          'movie', 'multi', 'person', 'tv') |
       length(search_cat) != 1)
      stop("search_cat must be one and only one of the following values: 'company', 'collection', 'keyword', 'movie', 'multi', 'person', 'tv'")
    
    if(!'query' %in% names(query_list))
      stop("'query' element is required in query_list")
    
    valid_strings <- search_query_string[[search_cat]]
    sub_idx <- which(names(query_list) %in%  valid_strings)
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
                            '?api_key=', api_key, '&', query_part))
    return(rst)
    
  }, error = function(err) {
    
    rst <- NULL
    return(rst)
  }, finally = {})
  
  return(url)
}


#' Get multi-page of data using the search functionality
#' 
#' @description This function takes in a list of condition users specify and the 
#'     target they are query against and return qualified target. 
#'     Noted that the the page can take multiple values here instead of one. Due 
#'     to the rate limit of API, page can be no longer than 40 integers.
#' @usage search_multi_page(query_list=list(), search_cat='movie', verbose=T)
#' @param query_list The list of criterion that narrow the range of returned results. 
#'     For different search categories there are respective set of legit variables 
#'     to specify. To check the list, print object search_query_string.
#' @param search_cat The variable that clarify the search category
#' @param verbose A logic value that control whether the page pulled info should 
#'     be sent as a message
#' @return An object of class \code{tmdb_api} that consists quried data in a 
#'     data frame or a list and a metadata list 
#'
#' @examples
#' \dontrun{
#' sample_query_list <- list(language='en-US', query='blue', page=c(2,3))
#' result_data <- search_multi_page(sample_query_list, 'movie')
#' }
#' @export
search_multi_page <- function(search_cat='movie', query_list, verbose=T){
  
  api_key <- TMDb_Env$api_key
  if (is.null(api_key)) stop('Need api_key authentication information. \n Please use function auth_key() first.')
  
  if(!'page' %in% names(query_list)){
    query_list$page <- 1
  } else if(length(query_list$page)==1){
    query_list$page <- query_list$page  
  }
  
  
  if(length(query_list$page)==1){
    return(get_result(get_search_url(query_list, search_cat)))
  }
  
  if (length(query_list$page) >1){
    
    if(length(query_list$page) <= 40){
      
      loop_page <- query_list$page
    } else {
      
      loop_page <- query_list$page[1:40]
      warnings('Exceed the upper limit of 40 pages, return as the first 40 pages as queried')
    }
    
    temp_query_list <- query_list
    temp_query_list$page <- min(loop_page)
    
    temp_url <- get_search_url(query_list=temp_query_list)
    
    if(verbose){temp_resp <- get_result(temp_url)} else {
      temp_resp <- suppressMessages(get_result(temp_url))}
    
    loop_page <- loop_page[loop_page <= content(temp_resp$response_detail)$total_pages]
    
    if(length(loop_page) == 1){
      
      temp_resp
      
    } else {
      
      returned_dat <- temp_resp$data_frame
      
      for (i in 2:length(loop_page)){
        
        query_list$page <- loop_page[i]
        temp_url <- get_search_url(query_list=query_list)
        if(verbose){temp_resp <- get_result(temp_url)} else {
          temp_resp <- suppressMessages(get_result(temp_url))}
        
        returned_dat <- rbind(returned_dat, temp_resp$data_frame)
      }
      
      structure(
        list(
          data_frame = returned_dat,
          response_detail=temp_resp$response_detail
        ),
        class = "tmdb_api")
    }
  }
}
