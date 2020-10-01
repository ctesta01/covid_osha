
#' Load the State Population Sizes 
#' 
load_state_popsizes <- function(export = T) {

  # load the census state population size estimates
  # 
  state_popsizes <- openxlsx::read.xlsx(
    system.file(
      "us_state_populations/nst-est2019-01.xlsx",
      package='osha.analysis'),
      startRow=4,
      colNames=T
      )

  # the first column is the state 
  colnames(state_popsizes)[1] <- 'state'

  # remove the . that seems to be prefixed to many of the states
  state_popsizes$state %<>% gsub("\\.", "", .)

  # remove the last 5 rows which contain the following data: 
  # 
  #  "Note: The estimates are based on the 2010 Census and reflect changes to the
  #  April 1, 2010 population due to the Count Question Resolution prog gram
  #  revisions See Geographic Terms and Definitions at
  #  http://wwwcensusgov/programs-surveys/popest/guidance-geographies/terms-and-definitionshtml
  #  fo that are included in each region  All geographic boundaries for the 2019
  #  population estimates series except statistical area delineations are as of
  #  population estimates methodology statements, see
  #  http://wwwcensusgov/programs-surveys/popest/technical-documentation/methodologyhtml"
  # 
  #  "Suggested Citation:"
  # 
  #  "Table 1 Annual Estimates of the Resident Population for the United States,
  #  Regions, States, and Puerto Rico: April 1, 2010 to July 1, 2019 (NS
  # 
  #  "Source: US Census Bureau, Population Division"
  # 
  #  "Release Date: December 2019"
  state_popsizes <- state_popsizes[1:(nrow(state_popsizes)-5),]

  if (export) {
    state_popsizes <<- state_popsizes
  }

  return(invisible(state_popsizes))
}

