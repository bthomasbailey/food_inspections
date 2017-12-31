library(XML)
library(RCurl)

extract_violation_info <- function(violation, id_inspection) {
  tibble::tibble(id_inspection = id_inspection,
                 code_section = violation[[1]],
                 severity = ifelse(length(violation[[2]]) > 1,
                                   violation[[2]][[1]],
                                   NA),
                 observation = violation[[3]],
                 corrective_action = violation[[4]]$i)
}

extract_inspection_info <- function(inspection, id_inspection) {
  tibble::tibble(id_inspection = id_inspection,
                 rest_name = inspection[[3]]$text,
                 facility_type = inspection[[4]][[3]],
                 inspect_type = inspection[[5]][[2]]$text,
                 inspect_date = inspection[[6]][[3]])
  
}

url <- "https://healthspace.com/Clients/Ohio/Cuyahoga/Cuyahoga_Web_Live.nsf/Food-InspectionDetails?OpenView&RestrictToCategory=197D0E9D17EC2FEA8525811D006C203B"

id_inspection <- "0E72DCF757EADA4D85258113004EF068"

resp <- getURL(url)
doc <- htmlTreeParse(resp, asText = T)

html <- xmlToList(doc$children$html)$body

inspection <- html[[2]]
inspection <- extract_inspection_info(inspection, id_inspection)

violations <- html[[4]]
violations <- violations[violations != "NULL"]
violations <- violations[1:(length(violations) - 6)]
violations <- split(violations, as.numeric(gl(length(violations), 5, length(violations))))
violations <- dplyr::bind_rows(lapply(violations, extract_violation_info, id_inspection = id_inspection))
