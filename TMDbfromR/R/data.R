#' Legit query element for the discover functionality
#'
#' @format A list of two elements
#' \describe{
#'   \item{movie}{a group of legit elements to include in query list if search_cat
#'   is movie}
#'   \item{tv}{a group of legit elements to include in query list if search_cat
#'   is tv}
#' }
#' @source \url{https://developers.themoviedb.org/3/discover/movie-discover}
#' 
"discover_query_strings"

discover_query_strings <- list(
  movie=c('language', 'sort_by', 'certification_country', 
          'certification', 'certification.lte', 'include_adult',
          'include_video', 'page', 'primary_release_year',
          'primary_release_date.gte', 'primary_release_date.lte', 
          'release_date.gte', 'release_date.lte', 'vote_count.gte', 
          'vote_count.lte', 'vote_average.gte', 'vote_average.lte', 
          'with_cast','with_crew', 'with_companies', 'with_genres', 
          'with_keywords', 'with_people', 'year'),
  tv=c('language', 'sort_by', 'air_date.gte', 'air_date.lte', 
       'first_air_date.gte', 'first_air_date.lte','first_air_date_year',
       'page', 'timezone', 'vote_average.gte', 'vote_count.gte',
       'with_genres','with_networks'))



#' Legit detail_cat values for the function get_movie_detail()
#' @format A string vector
#' \describe{
#'     A series of values that are legit for the detail_cat argument in the 
#'     function get_movie_detail()
#' }
#' @source \url{https://developers.themoviedb.org/3/movies/get-movie-details}
#' 
"movie_detail_cat_list"
movie_detail_cat_list <- c('account_states', 'alternative_titles', 'changes', 
                           'credits', 'images', 'keywords', 'release_dates',
                           'videos', 'translations', 'recommendations', 'similar',
                           'reviews', 'lists')

#' Legit query element for the function get_person_detail()
#' @format A string vector
#' \describe{
#'     A series of values that are legit for the detail_cat argument in the 
#'     function get_person_detail()
#' }
#' @source \url{https://developers.themoviedb.org/3/movies/get-movie-details}
#' 
"person_detail_cat_list"

person_detail_cat_list <- c('movie_credits', 'tv_credits', 'combined_credits', 'external_ids', 'images', 'tagged_images', 'changes')




#' Legit query element for the function get_search_url() and search_multi_page()
#'
#' @format A list of seven elements
#' \describe{
#'   \item{company}{a group of legit elements to include in query list of function 
#'   get_search_url() and search_multi_page() if search_cat is company}
#'   \item{collection}{a group of legit elements to include in query list of function 
#'   get_search_url() and search_multi_page() if search_cat is collection}
#'   \item{keyword}{a group of legit elements to include in query list of function 
#'   get_search_url() and search_multi_page() if search_cat is keyword}
#'   \item{movie}{a group of legit elements to include in query list of function 
#'   get_search_url() and search_multi_page() if search_cat is movie}
#'   \item{multi}{a group of legit elements to include in query list of function 
#'   get_search_url() and search_multi_page() if search_cat is multi}
#'   \item{person}{a group of legit elements to include in query list of function 
#'   get_search_url() and search_multi_page() if search_cat is person}
#'   \item{tv}{a group of legit elements to include in query list of function 
#'   get_search_url() and search_multi_page() if search_cat is tv}
#' }
#' @source \url{https://developers.themoviedb.org/3/discover/movie-discover}
#' 
"search_query_string"

search_query_string <- list(
  company=c('query', 'page'),
  collection=c('language','query', 'page'),  
  keyword=c('query', 'page'), 
  movie=c('language','query', 'page','include_adult','year','primary_release_year'), 
  multi=c('language','query', 'page','include_adult'), 
  person=c('language','query', 'page','include_adult'), 
  tv=c('language','query', 'page','first_air_date_year')
)
