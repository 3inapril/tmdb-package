person_detail_cat_list <- c('movie_credits', 'tv_credits', 'combined_credits', 'external_ids', 'images', 'tagged_images', 'changes')

get_person_detail <- function(person_id, person_name=NULL, detail_cat=NULL, language='en-US', append_to_response=NULL, api_key){
  
  if(missing(person_id) & missing(person_name)){
    stop('Need at least one of person_id and person_name')
  } else if(!missing(person_id)){
    NULL
  } else {
    query_list <- list(query=person_name)
    temp_url <- get_search_url(query_list, search_cat='person')
    temp_rst <- suppressMessages(get_result_general(temp_url))
    person_id <- temp_rst$data$results.id[1]
    message('Matching person ID: ', person_id)
  }
  
  url <- URLencode(paste0('https://api.themoviedb.org/3/person/', person_id, 
                          if(!is.null(detail_cat)) '/', 
                          detail_cat,
                          '?api_key=', api_key,
                          '&language=',language,
                          if(is.null(detail_cat) & !is.null(append_to_response)) '&append_to_response=', 
                          append_to_response))
  get_result_general(url)
  
}


get_latest_person <- function(language='en-US',api_key){
  url <- URLencode(paste0('https://api.themoviedb.org/3/person/latest?api_key=',
                          api_key,'&language=', language))
  rst <- get_result_general(url)
  return(rst)
}


get_popular_person <- function(language='en-US', page=1,api_key){
  if(length(page) > 1 | !is.numeric(page)){
    stop('Please query only one page using an integer')
  } else {
    url <- URLencode(paste0('https://api.themoviedb.org/3/person/popular?api_key=',
                            api_key,'&language=', language,
                            '&page=', page))
    rst <- get_result(url)
    return(rst)
  }
}

