% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/movie_functions.R
\name{get_top_rated_movie}
\alias{get_top_rated_movie}
\title{Get the top rated movies on TMDb}
\usage{
get_top_rated_movie(language='en-US', page=1)
}
\arguments{
\item{language}{Language of movie. By default is 'en-US'}

\item{page}{Which page to return}
}
\value{
An object of class \code{tmdb_api} that consists quried data in a 
    data frame or a list and a metadata list
}
\description{
This function returns the top rated movies on TMDb.
}
\examples{
\dontrun{
result_data <- get_top_rated_movie(language='en-US', page=2)
}
}

