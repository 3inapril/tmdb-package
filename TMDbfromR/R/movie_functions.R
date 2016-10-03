####################################
# Movie categorry by movie_id
# This function allow query by movie id or by title, If by title, two queries need
# to be sent to API, the first to get movie_id, the second to request data by movie_id
movie_detail_cat_list <- c('account_states', 'alternative_titles', 'changes', 'credits',
                           'images', 'keywords', 'release_dates', 'videos', 'translations',
                           'recommendations', 'similar', 'reviews', 'lists')

get_movie_detail <- function(movie_id, movie_title=NULL, detail_cat=NULL, language='en-US', append_to_response=NULL, api_key){
  
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




get_latest_movie <- function(language='en-US',api_key){
  url <- URLencode(paste0('https://api.themoviedb.org/3/movie/latest?api_key=',
                          api_key,'&language=', language))
  rst <- get_result_general(url)
  return(rst)
}



get_now_playing_movie <- function(language='en-US', page=1, api_key){
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


get_popular_movie <- function(language='en-US', page=1, api_key){
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



get_top_rated_movie <- function(language='en-US', page=1, api_key){
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



get_upcoming_movie <- function(language='en-US', page=1, api_key){
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


