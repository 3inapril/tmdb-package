search_query_string <- list(
  company=c('query', 'page'),
  collection=c('language','query', 'page'),  
  keyword=c('query', 'page'), 
  movie=c('language','query', 'page','include_adult','year','primary_release_year'), 
  multi=c('language','query', 'page','include_adult'), 
  person=c('language','query', 'page','include_adult'), 
  tv=c('language','query', 'page','first_air_date_year')
)

get_search_url <- function(query_list, search_cat='movie'){
  
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
    
    rst <- URLencode(paste0(domain, method, search_cat,
                            '?api_key=', api_key, '&', query_part))
    return(rst)
    
  }, error = function(err) {
    
    rst <- NULL
    return(rst)
  }, finally = {})
  
  return(url)
}





search_multi_page <- function(search_cat='movie', query_list, verbose=T){
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