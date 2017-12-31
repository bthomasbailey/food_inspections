library(XML)
library(RCurl)
library(stringr)

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

url_base <- "https://healthspace.com/Clients/Ohio/Cuyahoga/Cuyahoga_Web_Live.nsf/"
url <- str_c(url_base, "Food-InspectionDetails?OpenView&RestrictToCategory=624ABC0BDEF620B9852581D8006E6C1D")

url <- str_c(url_base, "Food-InspectionDetails?OpenView&RestrictToCategory=0E72DCF757EADA4D85258113004EF068")

id_inspection <- "0E72DCF757EADA4D85258113004EF068"

resp <- getURL(url)


doc <- htmlTreeParse(resp, useInternalNodes = T)
doc2 <- xmlRoot(doc)
test <- xmlChildren(doc2)$body

header_tbl <- getNodeSet(test, "//table[thead//tr//th='Inspection Information']")
test2 <- unlist(xmlToList(header_tbl[[1]]))
test3 <- getNodeSet(test, "//table[thead//tr//th='Inspection Information']//tr")
header_info <- xpathApply(test, "//table[thead//tr//th='Inspection Information']//tr", xmlValue)
grep("Facility", header_info, value = T)

get_header_info <- function(key) {
  txt <- grep(key, header_info, ignore.case = T, value = T)
  pattern <- str_c("^.*", key)
  val <- gsub(pattern, "", txt, ignore.case = T)
  val <- gsub("\\n", "", val)
  str_trim(val)  
}

get_header_info("facility type:")
get_header_info("inspection type:")
get_header_info("inspection date:")

corrective_actions <- xpathSApply(test, "//table[not(contains(thead//tr//th,'Inspection Information'))]//tr//td//i", xmlValue)
corrective_actions <- grep("a\\s+summary\\s+of\\s+the\\s+violations\\s+found\\s+during\\s+the\\s+inspection", 
                           corrective_actions, ignore.case = T, value = T, invert = T)

severities <- xpathSApply(test, "//table[not(contains(thead//tr//th,'Inspection Information'))]//tr//td[not(i)]//b", 
                        function(x) str_trim(xmlValue(x)))

violations <- xpathSApply(test, "//table[not(contains(thead//tr//th,'Inspection Information'))]//tr//td", xmlValue)
violations <- grep("^\\s*3717-", violations, value = T)

remove_corrective_actions <- function(violation, action) {
  gsub(action, "", violation, fixed = T)
}

violations <- mapply(remove_corrective_actions, violations, corrective_actions, 
                     USE.NAMES = F)

pattern_code <- "3717-1-\\d{2}(\\.\\d{1,2}(\\([A-Z]\\)(\\(\\d{1,2}\\))?)?)?"
violation_codes <- str_extract(violations, pattern_code)

violations <- gsub(pattern_code, "", violations)

remove_severity <- function(violation, severity) {
  pattern <- str_c("^\\s*", severity)
  str_trim(gsub(pattern, "", violation))
}


violations <- mapply(remove_severity, violations, severities, 
                     USE.NAMES = F)








# html <- xmlToList(doc$children$html)$body
# 
# html2 <- unlist(html)
# 
# inspection <- html[[2]]
# inspection <- extract_inspection_info(inspection, id_inspection)
# 
# violations <- html[[4]]
# violations <- violations[violations != "NULL"]
# violations <- violations[1:(length(violations) - 6)]
# violations <- split(violations, as.numeric(gl(length(violations), 5, length(violations))))
# violations <- dplyr::bind_rows(lapply(violations, extract_violation_info, id_inspection = id_inspection))
