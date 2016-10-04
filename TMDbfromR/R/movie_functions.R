
movie_detail_cat_list <- c('account_states', 'alternative_titles', 'changes', 
                           'credits', 'images', 'keywords', 'release_dates',
                           'videos', 'translations', 'recommendations', 'similar',
                           'reviews', 'lists')

#' @description This function takes in a movie_id and a detail category to return.
#'     Noted that this function allow query by movie id or by title. If by title, 
#'     two queries need to be sent to API, the first to get movie_id, the second 
#'     to request data by movie_id
#' @usage get_movie_detail(movie_id, movie_title=NULL, detail_cat=NULL, 
#'                         language='en-US', append_to_response=NULL)
#' @param movie_id The movie_id of the movie of interest
#' @param movie_title If no movie_id is provided, use movie_title to match and 
#'     identify movie
#' @param detail_cat The variable that clarify the query category. For full list
#'     of legit values, print movie_detail_cat_list
#' @param language language of movie. By default is 'en-US'
#' @param append_to_response A series of strings seperated by comma that clearify
#'     objects to append that are related to this quried o movie. Possible values 
#'     are imgages and videos
#' @return An object of class \code{tmdb_api} that consists quried data in a 
#'     data frame or a list and a metadata list 
#'
#' @examples
#' \dontrun{
#' result_data <- get_movie_detail(movie_id=44826, detail_cat='recommendations')
#' }
#' @export
get_movie_detail <- function(movie_id, movie_title=NULL, detail_cat=NULL, language='en-US', append_to_response=NULL){
  
  if(missing(movie_id) & missing(movie_title)){
    stop('Need at least one of movie_id and movie_title')
  } else if(!missing(movie_id)){
    NULL} else {
      query_list <- list(query=movie_title)
      temp_url <- get_search_url(query_list, search_cat='movie')
      temp_rst <- suppressMessages(get_result_general(temp_url))
      movie_id <- temp_rst$data$results.id[1]
    }
  
  url <- URLencode(paste0('https://api.themoviedb.org/3/movie/', movie_id,
                          if(!is.null(detail_cat)) '/', 
                          detail_cat,
                          '?api_key=',api_key,
                          '&language=',language,
                          if(is.null(detail_cat) & !is.null(append_to_response)) '&append_to_response=', 
                          append_to_response))
  
  get_result_general(url)
}

#' @description This function fetch the most newly created movie. This is a 
#'     live response and will continuously change.
#' @usage get_latest_movie(language='en-US')
#' @param language language of movie. By default is 'en-US'
#' @return An object of class \code{tmdb_api} that consists quried data in a 
#'     data frame or a list and a metadata list 
#' @examples
#' \dontrun{
#' result_data <- get_latest_movie(language='en-US')
#' }
#' @export
get_latest_movie <- function(language='en-US'){
  url <- URLencode(paste0('https://api.themoviedb.org/3/movie/latest?api_key=',
                          api_key,'&language=', language))
  rst <- get_result_general(url)
  return(rst)
}

#' @description This function fetch the "now playing" movies. This is a date based 
#'     query looking at the primary_release_date of a movie.
#' @usage get_now_playing_movie(language='en-US', page=1)
#' @param language Language of movie. By default is 'en-US'
#' @param page Which page to return
#' @return An object of class \code{tmdb_api} that consists quried data in a 
#'     data frame or a list and a metadata list 
#' @examples
#' \dontrun{
#' result_data <- get_now_playing_movie(language='en-US', page=2)
#' }
#' @export
get_now_playing_movie <- function(language='en-US', page=1){
  if(length(page) > 1 | !is.numeric(page)){
    stop('Please query only one page using an integer')
  } else {
    url <- URLencode(paste0('https://api.themoviedb.org/3/movie/now_playing?api_key=',
                            api_key, '&language=', language,
                            '&page=', page))
    rst <- get_result(url)
    return(rst)
  }
}

#' @description This function fetch a list of the current popular movies on TMDb. 
#'     This list updates daily.
#' @usage get_popular_movie(language='en-US', page=1)
#' @param language Language of movie. By default is 'en-US'
#' @param page Which page to return
#' @return An object of class \code{tmdb_api} that consists quried data in a 
#'     data frame or a list and a metadata list 
#' @examples
#' \dontrun{
#' result_data <- get_popular_movie(language='en-US', page=2)
#' }
#' @export
get_popular_movie <- function(language='en-US', page=1){
  if(length(page) > 1 | !is.numeric(page)){
    stop('Please query only one page using an integer')
  } else {
    url <- URLencode(paste0('https://api.themoviedb.org/3/movie/popular?api_key=',
                            api_key, '&language=', language,
                            '&page=', page))
    rst <- get_result(url)
    return(rst)
  }
}


#' @description This function returns the top rated movies on TMDb.
#' @usage get_top_rated_movie(language='en-US', page=1)
#' @param language Language of movie. By default is 'en-US'
#' @param page Which page to return
#' @return An object of class \code{tmdb_api} that consists quried data in a 
#'     data frame or a list and a metadata list 
#' @examples
#' \dontrun{
#' result_data <- get_top_rated_movie(language='en-US', page=2)
#' }
#' @export
get_top_rated_movie <- function(language='en-US', page=1){
  if(length(page) > 1 | !is.numeric(page)){
    stop('Please query only one page using an integer')
  } else {
    url <- URLencode(paste0('https://api.themoviedb.org/3/movie/top_rated?api_key=',
                            api_key, '&language=', language,
                            '&page=', page))
    rst <- get_result(url)
    return(rst)
  }
}


#' @description This function fetch the "upcoming" movies. This is a date based 
#'     query looking at the primary_release_date of a movie.
#' @usage get_upcoming_movie(language='en-US', page=1)
#' @param language Language of movie. By default is 'en-US'
#' @param page Which page to return
#' @return An object of class \code{tmdb_api} that consists quried data in a 
#'     data frame or a list and a metadata list 
#' @examples
#' \dontrun{
#' result_data <- get_upcoming_movie(language='en-US', page=2)
#' }
#' @export
get_upcoming_movie <- function(language='en-US', page=1){
  if(length(page) > 1 | !is.numeric(page)){
    stop('Please query only one page using an integer')
  } else {
    url <- URLencode(paste0('https://api.themoviedb.org/3/movie/upcoming?api_key=',
                            api_key, '&language=', language,
                            '&page=', page))
    rst <- get_result(url)
    return(rst)
  }
}
