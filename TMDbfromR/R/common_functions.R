#' store API key in global environment
#'
#' @description This is a function that store API key for the sake of functions 
#'     in the pakage
#' @usage auth_key(api_key='')
#' @param api_key A unque key given by TMDb for each account
#' @return An character object stored in the global environment
#' @export
auth_key <- function(api_key){
  #unlockBinding("api_key", env=globalenv())
  assign("api_key", api_key, envir = TMDb_pkg)
  #utils::globalVariables(names="api_key", package="TMDbfromR", add=FALSE)
}

#'  Query data using a URL
#'
#' @description This is a function that takes an assembled URL and query data from it, especially for the query where users specify which page to return. This function will generate message of total page so the user can modify their query accordingly.
#' @usage get_result(url)
#' @param url A TMDb url string to pull data from. url can be get using function 
#' in this package such as get_discover_url and get_search_url
#' @return An object of class \code{tmdb_api}. The object consists of two elements. 
#'    The first element called data_frame is a \code{data frame} if the requested 
#'    returnings can be easily converted to data frame witout confusion. The second 
#'    element called response_detail is a \code{list} that contains metadata of the
#'    query such as headers, url and status code. Raw data in json format is 
#'    also available in response_detail.
#' @import jsonlite httr DBI
#'
#' @examples
#' \dontrun{
#' url <- 'https://api.themoviedb.org/3/discover/movie?api_key={Put_API_Key_Here}&year=2011&with_genres=12,18&language=en-US&page=14'
#' result <- get_result(url)
#' }
#'
#' @export
get_result <- function(url){
  
  api_key <- TMDb_pkg$api_key
  resp=GET(url)
  
  if(length(content(resp)$total_pages) == 0) {
    
    raw_data <- jsonlite::fromJSON(content(resp, 'text'), simplifyVector = FALSE, simplifyDataFrame=T)
    data <- NULL
    
  } else if(length(content(resp)$results) < 1){
    
    # when result is NULL but $total_page is larger than 0, usually it is page quried > the last page
    stop('Max page is page ', content(resp)$total_pages, '. Please modify page value in your query_list')
    raw_data <- jsonlite::fromJSON(content(resp, 'text'), simplifyVector = FALSE, simplifyDataFrame=T)
    data <- NULL
    
  } else {
    
    raw_data <- jsonlite::fromJSON(content(resp, 'text'), simplifyVector = FALSE, simplifyDataFrame=T)
    data <- as.data.frame(raw_data)
    message('Return page ',raw_data$page, ' out of ', content(resp)$total_pages, ' pages')
    
    if('results.genre_ids' %in% colnames(data)){
      data$results.genre_ids <- ldply(data$results.genre_ids, paste, collapse=',')$V1
    }
  }
  
  if (http_error(resp)) {
    stop(sprintf("TMDB API request failed [%s]\n%s\n<%s>",
                 status_code(resp),
                 raw_data$status_message,
                 url), 
         call. = FALSE)
  }
  
  structure(
    list(
      data = data,
      response_detail=resp
    ),
    class = "tmdb_api")
}
#'
#' Get result from query that desn't involve pages
#'
#' @description This is a function that takes an assembled URL and query data from it.
#'    Comparing with get_result which maily used for the url with page specified, 
#'    this is a more general version. Instead of messaging the total page, this function 
#'    returns message about the class of the data requested.
#' @usage get_result_general(url)
#' @param url A TMDb url string to pull data from. url can be get using function 
#'     in this package such as get_discover_url and get_search_url
#' @return An object of class \code{tmdb_api}. The object consists of two elements. 
#'     The first element called data is a \code{data frame} if the requested returnings 
#'     can be easily converted to data frame witout confusion. Otherwise it will be 
#'     in\code{list} form.  The second element called response_detail is a \code{list}
#'     that contains metadata of the query such as headers, url and status code. Raw 
#'     data in json format is also available in response_detail.
#'
#' @examples
#' \dontrun{
#' url <- 'https://api.themoviedb.org/3/discover/movie?api_key={Put_API_Key_Here}&year=2011&with_genres=12,18&language=en-US'
#' result <- get_result_general(url)
#' }
#'
#' @export
get_result_general <- function(url){
  
  force(url)
  resp <- GET(url)
  
  raw_data <- jsonlite::fromJSON(content(resp, 'text'), simplifyVector = FALSE, simplifyDataFrame=T)
  
  to_df_if_possible <- function(x){
    
    tryCatch({
      
      final_x <- as.data.frame(x)
      
    }, error=function(e){
      
      final_x <- x
      
    })
  }
  
  rst_data <- to_df_if_possible(raw_data)
  message('Return data as a ', class(rst_data))
  
  if (http_error(resp)) {
    stop(sprintf("TMDB API request failed [%s]\n%s\n<%s>",
                 status_code(resp),
                 raw_data$status_message,
                 url), 
         call. = FALSE)
  }
  
  structure(
    list(
      data=rst_data,
      response_detail=resp
    ),
    class = "tmdb_api")
}
#'
#' 
#' Print object tmdb_api
#' 
#' @description This function controls the output when users print the response object
#' @usage print.tmdb_api(rst) or rst
#' @param rst The returned object of function get_result() or get_result_general().
#'    rst should be of class \code{tmdb_api}
#' @return An object of class \code{tmdb_api}. 
#' @examples
#' \dontrun{
#' url <- 'https://api.themoviedb.org/3/discover/movie?api_key={Put_API_Key_Here}&year=2011&with_genres=12,18&language=en-US'
#' result <- get_result_general(url)
#' result
#' }
#' @export
print.tmdb_api <- function(x, ...) {
  cat("<The Movie Database  path: ", 
      gsub('https://api.themoviedb.org/3','', x$response_detail$url),
      ">\n", sep = "")
  str(x$data)
  invisible(x)
}