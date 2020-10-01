
#' Load Original OSHA Dataset 
#' 
#' Retrieved as of June 25, 2020. 
#' 
#' @export
#' 
load_osha <- function(export=T) {
  osha_closed <- openxlsx::read.xlsx(
    system.file(
      'raw_data/Copy_of_Closed_Federal_and_State_Plan_Valid_Covid-19_Complaints_through_September_20.xlsx',
      package = 'osha.analysis'
    )
  )

  osha_open <- openxlsx::read.xlsx(
    system.file(
      'raw_data/Copy_of_Open_Federal_and_State_Plan_Valid_Covid-19_Complaints_through_September_20.xlsx',
      package = 'osha.analysis'
    )
  )

  osha_closed$status <- 'Closed'
  osha_open$status <- 'Open'

  osha <- merge(osha_open, osha_closed, all=T)

  osha %<>% janitor::clean_names()

  if (export) osha <<- osha
  invisible(osha)
}
