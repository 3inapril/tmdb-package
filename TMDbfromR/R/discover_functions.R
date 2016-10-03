# take in a list and concatenate the element names and values to build the query part for url

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


get_discover_url <- function(query_list=list(), search_cat='movie'){
  
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
    
    rst <- URLencode(paste0(domain, method, search_cat,
                            '?api_key=', api_key, 
                            if(query_part!='') '&',
                            query_part))
    return(rst)
    
  }, error = function(err) {
    
    rst <- NULL
    return(rst)}, finally = {})
  
  return(url)
}








discover_multi_page <- function(query_list=list(), search_cat='movie', verbose=T){
  
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
      temp_resp <- suppressMessages(get_result(temp_url))}
    
    loop_page <- loop_page[loop_page <= content(temp_resp$response_detail)$total_pages]
    
    if(length(loop_page) == 1){

      temp_resp
      
    } else {
      
      returned_dat <- temp_resp$data_frame
      
      for (i in 2:length(loop_page)){
        
        query_list$page <- loop_page[i]
        temp_url <- get_discover_url(query_list=query_list)
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