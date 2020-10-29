
#' Clean COVID19 Deaths
#'
#' At the moment, the only thing this does is transform the 
#' date columns into R date vectors from character vectors. 

clean_covid19_deaths <- function(covid19_deaths) {
  
  # convert to dates
  covid19_deaths$start_week %<>% lubridate::mdy()
  covid19_deaths$end_week %<>% lubridate::mdy()
  covid19_deaths$data_as_of %<>% lubridate::mdy()

  return(covid19_deaths)
}
