
#' Load COVID19 Deaths by Race/Ethnicity
#' 
#' @param export An boolean parameter used for exporting to the
#' next parent scope in which covid19_deaths is not defined,
#' typically the global scope. 
#' 
#' @return By default this function exports covid19_deaths
#' covid19_deaths is 
#' returned invisibly for assignment to a variable and to 
#' avoid overwhelming the user with data in their terminal. 
#' 

load_covid19_deaths_by_race_ethnicity <- function(export=T) {

  data_files <- list.files(system.file(
    'deaths-covid-19-by-race-age-state-cdc/',
    package='covid19.deaths.by.race.ethnicity'),
    full.names=T
  )

  covid19_data <- 
    lapply(data_files, readr::read_csv)

  for (iter in 1:length(covid19_data)) {
    colnames(covid19_data[[iter]]) <-
      colnames(covid19_data[[1]]) 
  }

  covid19_deaths <- do.call(rbind.data.frame, covid19_data)

  if (export) 
    covid19_deaths <<- covid19_deaths

  return(invisible(covid19_deaths))
}
