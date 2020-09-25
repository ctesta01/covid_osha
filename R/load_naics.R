
#' Load NAICS classification data
#' 
#' The NAICS codes were retrieved from 
#' https://www.census.gov/eos/www/naics/
#' 2017 NAICS Manual
#' 

load_naics <- function(export=T) {

  naics <- read_yaml(system.file('naics/naics_formatted.yml', package='osha.analysis'))

  if (export) {
    naics <<- naics
  } 

  invisible(naics)
}

#' Load Detailed NAICS
#' 
#' This file contains detailed descriptions for every 6 digit NAICS code
#' https://www.census.gov/eos/www/naics/downloadables/downloadables.html
#' 2017 NAICS Index File [XLSX, 498KB]
#' 
#' This file has descriptions for 2- or 3-digit NAICS codes
#' 2017 NAICS Descriptions [XLSX, 264KB]
load_detailed_naics <- function(export=T) {
  naics_detailed <- openxlsx::read.xlsx(
    system.file("naics/2017_NAICS_Descriptions.xlsx", package='osha.analysis')
  )

  naics_detailed <- naics_detailed[,c(1,2)]

  colnames(naics_detailed) <- c("naics", "industry_description")

  # remove those silly Ts that appear at the end! 
  # I think they were footnotes in the original file
  naics_detailed$industry_description <- gsub("T[[:space:]]*$", "", naics_detailed$industry_description)
  naics_detailed$industry_description <- gsub("[[:space:]]*$", "", naics_detailed$industry_description)

  naics_detailed[nrow(naics_detailed)+1,] <- c(722110, "Full-Service Restaurants")
  naics_detailed[nrow(naics_detailed)+1,] <- c(722211, "Limited-Service Restaurants")
  naics_detailed[nrow(naics_detailed)+1,] <- c(452111, "Department Stores (except Discount Department Stores")
  naics_detailed[nrow(naics_detailed)+1,] <- c(452112, "Discount Department Stores")
  naics_detailed[nrow(naics_detailed)+1,] <- c(452990, "All Other General Merchandise Stores")
  naics_detailed[nrow(naics_detailed)+1,] <- c(452910, "Warehouse Clubs and Supercenters")
  naics_detailed[nrow(naics_detailed)+1,] <- c(722210, "Limited-Service Eating Places")
  naics_detailed[nrow(naics_detailed)+1,] <- c(722213, "Snack and Nonalcoholic Beverage Bars")
  naics_detailed[nrow(naics_detailed)+1,] <- c(336399, "All Other Motor Vehicle Parts Manufacturing")
  naics_detailed[nrow(naics_detailed)+1,] <- c(999999, "Unclassified Establishments")
  naics_detailed[nrow(naics_detailed)+1,] <- c(454111, "Electronic Shipping and Mail-Order Houses")
  naics_detailed[nrow(naics_detailed)+1,] <- c(517110, "Wired Telecommunications Carriers")
  naics_detailed[nrow(naics_detailed)+1,] <- c(454113, "Mail-Order Houses")
  naics_detailed[nrow(naics_detailed)+1,] <- c(443111, "Household Appliance Stores")
  naics_detailed[nrow(naics_detailed)+1,] <- c(443112, "Electronics Stores")
  naics_detailed[nrow(naics_detailed)+1,] <- c(517212, "Cellular and Other Wireless Telecommunications")
  naics_detailed[nrow(naics_detailed)+1,] <- c(517210, "Wireless Telecommunications Carriers (except Satellite)")
  naics_detailed[nrow(naics_detailed)+1,] <- c(331529, "Other Nonferrous Metal Foundries (except Die-Casting)")
  naics_detailed[nrow(naics_detailed)+1,] <- c(331111, "Iron and Steel Mills and Ferroalloy Manufacturing")
  naics_detailed[nrow(naics_detailed)+1,] <- c(332116, "Metal Stamping")
  naics_detailed[nrow(naics_detailed)+1,] <- c(323110, "Commercial Lithograph Printing")
  naics_detailed[nrow(naics_detailed)+1,] <- c(333295, "Semiconductor Machinery Manufacturing")
  naics_detailed[nrow(naics_detailed)+1,] <- c(333319, "Other Commercial and Service Industry Machinery Manufacturing")
  naics_detailed[nrow(naics_detailed)+1,] <- c(322221, "Coated and Laminated Packaging Paper and Plastics Film Manufacturing")

  if(export) naics_detailed <<- naics_detailed

  return(invisible(naics_detailed))
}
