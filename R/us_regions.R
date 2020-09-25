
#' Construct a Data Frame of US Census Regions 
#' 
#' 
get_us_census_regions <- function() {

  west <- c('ID', "MT", "WY", "NV", 'UT', 'CO', 'AZ', 'NM',
    'AK', 'HI', 'WA', 'OR', 'CA')

  midwest <- c('ND', 'MN', 'WI', 'MI', 'SD', 'IA', 'IL', 'IN', 'OH',
  'NE', 'KS', 'MO')

  south <- c('OK', 'TX', 'AR', 'LA', 'MS', 'TN', 'KY', 'WV', 'MD',
  'DE', 'DC', 'VA', 'NC', 'AL', 'GA', 'SC', 'FL')

  northeast <- c('ME', 'NH', 'VT', 'MA', 'CT', 'RI', 'NY', 'PA', 'NJ')

  # double check I got them all
  stopifnot(length(c(west, midwest, south, northeast))==51) # should be 51

  regions <- data.frame(
    region = 
      c(rep('west', length(west)),
        rep('midwest', length(midwest)),
        rep('south', length(south)),
        rep('northeast', length(northeast))),
    state_abb = 
      c(west,
      midwest,
      south,
      northeast))

  return(regions)
}
