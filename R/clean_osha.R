
#' Provide a Cleaned Copy of the OSHA dataset
#' 
#' In particular, this cleans up the address data by 
#' splitting it out into columns for each major component. 
#' 
clean_osha_addresses <- function(osha) { 
  
  # separate the location column 
  osha %<>% 
    tidyr::separate(col = establishment_name_site_city_state_zip,
      sep = '\r\n', into = c('establishment_name', 'street_address')) %>% 
    tidyr::separate(col = street_address, sep = ', ',
      into = c('street_address1', 'street_address2', 'street_address3', 'street_address4', 'street_address5', 'city', 'state', 'zip'), fill='left')

  return(osha)
}


#' Provide OSHA Counts by County Map with Data Included
#' 
#' @param print_frc_matched A logical argument to indicate whether or not 
#' to print the number of counties which were successfully matched of those 
#' that weren't originally NA.
#' 
make_osha_counts_w_county_data <- function(export = T, print_frc_matched = F, print_unmatched = F) {

  osha <- load_osha(export=F)
  osha %<>% clean_osha_addresses

  territories_counties <- urbnmapr::get_urbn_map(map = "territories_counties", sf = TRUE)

  # convert everything to lowercase to make matching easier
  osha$county_cleaned <- osha$county %>% tolower() 


  # remove words like County, Census Area, Borough, ..., to make matching easier
  territories_counties$county_name_cleaned <- 
    territories_counties$county_name %>% 
    gsub(" County| Census Area| Borough| Municipality| Parish| Municipio| Island", "", .) %>% 
    tolower()

  # standardize references of "saint" to "st."
  territories_counties$county_name_cleaned %<>% gsub("st ", "st. ", .) %>% 
    gsub("saint ", "st. ", .) %>% 
    gsub("'", "", .)


  # manual cleaning for the last few items in the osha addresses
  osha$county_cleaned %<>% 
     gsub("st ", "st. ", .) %>% 
     gsub("saint ", "st. ", .) %>% 
     gsub("la porte", "laporte", .) %>% 
     gsub("de kalb", "dekalb", .) %>% 
     gsub("rock island", "rock", .) %>% 
     gsub("^radford", "radford city", .) %>% 
     gsub("matanuska susitna", "matanuska-susitna", .)


  # if one wants to double check how many counties were matched, the following 
  # calculates the fraction of counties which were matched which were not NA

  if (print_frc_matched) {

    frc_matched <- length(which(! osha$county_cleaned %in% territories_counties$county_name_cleaned & ! is.na(osha$county_cleaned))) / 
      (nrow(osha) - length(which(is.na(osha$county))))

    cat("Fraction of Counties not NA matched: \n")
    print(frc_matched)
  }

  # if one wants to see the counties which were not matched, the following 
  # section prints them: 
  # 
  # we're expecting these to all be "Added for legacy migration"
  #
  if (print_unmatched) {
    cat("Unmatched counties which were not NA: \n")
    print(
      unique(osha$county_cleaned[
        which(! osha$county_cleaned %in% territories_counties$county_name_cleaned & 
        ! is.na(osha$county_cleaned))]))
  }

  # count complaints by county and state
  osha_county_counts <- osha %>% 
    group_by(county_cleaned, state) %>% 
    summarize(count = n())

  # convert both dataset's statenames to lowercase
  osha_county_counts$state %<>% tolower()
  territories_counties$state_name %<>% tolower()


  # merge the data together
  osha_counts_w_county_data <- territories_counties %>% merge(osha_county_counts, 
    by.x = c('county_name_cleaned', 'state_name'), 
    by.y = c('county_cleaned', 'state'),
    all.x = T)
  
  # (optional) export to the highest scope in which "osha_counts_w_county_data is not defined.

  if (export) {
    osha_counts_w_county_data <<- osha_counts_w_county_data
  }

  return(invisible(osha_counts_w_county_data))
}
