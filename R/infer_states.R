
#' Infer States for Open Cases
#'
#' 
#' This function matches the open complaints to the state in which the
#' complaint occured. 
#' 
#' The method used is to take the closed complaints (which have address data)
#' and the OSHA office IDs and to create a lookup table from OSHA office ID =>
#' state.
#' 
#' Then using said lookup table, code the open complaints.
#' 
infer_states <- function(osha) {

  # Filter for just closed complaints
  closed_osha <- osha %>% filter(status == "Closed")

  # Select only Office ID, State
  office_states <- closed_osha %>% select(rid, state)

  # Check if any offices match multiple states
  office_states %>% unique() %>% 
    group_by(rid) %>% count(sort=T) 

  # looks like this office serves both North & South Carolina:
  office_states %>% filter(rid == '04-203-00')

  # this office predominantly serves Colorado, but got 2 complaints,
  # 1 from Wyoming and 1 from Utah
  office_states %>% filter(rid == '04-203-00')


  # Count how many times each state is mentioned for each office ID
  office_state_counts <- office_states %>% group_by(rid, state) %>% 
    summarize(count = n()) %>% ungroup() %>% 
    arrange(rid)

  # Get the most common state for each RID (Office ID)
  office_states <- office_state_counts %>% group_by(rid) %>% 
    filter(count == max(count)) %>% 
    select(-count)

  # Confirm that there is 1 most common state for each office id
  unique_matching <- office_states %>% 
    group_by(rid) %>% summarize(count = n()) %>% 
    `[[`('count') %>% max() # should be 1

  stopifnot(unique_matching == 1)

  # Filter for open osha complaints
  open_osha <- osha %>% filter(status == 'Open')

  # Check how many open OSHA complaint office IDs are not in the closed office IDs
  nrow(open_osha) - length(which(open_osha$rid %in% office_states$rid))

  # Look at OSHA Office IDs for Non-Matchables
  open_osha %>% filter(! rid %in% office_states$rid) %>% select(rid)

  # Add the Inferred States to Open OSHA Complaints
  open_osha <- 
    open_osha %>% merge(office_states, by = 'rid') %>% select(-state.x) %>% 
      rename(state = state.y) 

  # Create a new OSHA complaints dataset with inferred states for open cases
  osha_new <- 
    bind_rows(closed_osha, open_osha)

  return(osha_new)
}
