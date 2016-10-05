#' Get detailed information of a certain person
#' 
#' @description This function takes in a person_id and a detail category to return.
#'     Noted that this function allow query by person_id or by person name. If by 
#'     name, two queries need to be sent to API, the first to get person_id, the 
#'     second to request data by person_id
#' @usage get_person_detail(person_id, person_name=NULL, detail_cat=NULL, 
#'                         language='en-US', append_to_response=NULL)
#' @param person_id The person_id of the person of interest
#' @param person_name If no person_id is provided, use person_name to match and 
#'     identify the person
#' @param detail_cat The variable that clarify the query category. For full list
#'     of legit values, print person_detail_cat_list
#' @param language language of movie. By default is 'en-US'
#' @param append_to_response A series of strings seperated by comma that clearify
#'     objects to append that are related to this quried person. Possible values 
#'     are imgages and videos
#' @return An object of class \code{tmdb_api} that consists quried data in a 
#'     data frame or a list and a metadata list 
#'
#' @examples
#' \dontrun{
#' result_data <- get_person_detail(person_id=287, detail_cat='movie_credits')
#' }
#' @export
get_person_detail <- function(person_id, person_name=NULL, detail_cat=NULL, language='en-US', append_to_response=NULL){
  
  api_key <- TMDb_Env$api_key
  if (is.null(api_key)) stop('Need api_key authentication information. \n Please use function auth_key() first.')
  
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

#' Get the most newly created person
#' 
#' @description This function returns the most newly created person. This is a live 
#'     response and will continuously change.
#' @usage get_latest_person(language='en-US')
#' @param language Language of movie. By default is 'en-US'
#' @return An object of class \code{tmdb_api} that consists quried data in a 
#'     data frame or a list and a metadata list 
#' @examples
#' \dontrun{
#' result_data <- get_latest_person(language='en-US')
#' }
#' @export
get_latest_person <- function(language='en-US'){
  
  api_key <- TMDb_Env$api_key
  if (is.null(api_key)) stop('Need api_key authentication information. \n Please use function auth_key() first.')
  
  url <- URLencode(paste0('https://api.themoviedb.org/3/person/latest?api_key=',
                          api_key,'&language=', language))
  rst <- get_result_general(url)
  return(rst)
}

#' Get the list of popular people on TMDb
#' 
#' @description This function fetch the list of popular people on TMDb. This 
#'     list updates daily.
#' @usage get_popular_person(language='en-US', page=1)
#' @param language Language of movie. By default is 'en-US'
#' @param page Which page to return
#' @return An object of class \code{tmdb_api} that consists quried data in a 
#'     data frame or a list and a metadata list 
#' @examples
#' \dontrun{
#' result_data <- get_popular_person(language='en-US', page=2)
#' }
#' @export
get_popular_person <- function(language='en-US', page=1){
  
  api_key <- TMDb_Env$api_key
  if (is.null(api_key)) stop('Need api_key authentication information. \n Please use function auth_key() first.')
  
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

