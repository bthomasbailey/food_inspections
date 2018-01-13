library(XML)
library(RCurl)
library(stringr)
library(readr)
library(dplyr)


# This function pulls data in from the top table (with the 'Inspection Information' header)
# from the webpage 
get_inspection_info <- function(id_restaurant, id_inspection, body) {

  get_header_info <- function(key) {
    txt <- grep(key, header_info, ignore.case = T, value = T)
    pattern <- str_c("^.*", key)
    val <- gsub(pattern, "", txt, ignore.case = T)
    val <- gsub("\\n", "", val)
    str_trim(val)  
  }
  
  header_info <- xpathApply(body, 
                            "//table[thead//tr//th='Inspection Information']//tr", 
                            xmlValue)
  
  tibble::tibble(id_restaurant = id_restaurant,
                 id_inspection = id_inspection,
                 facility_type = get_header_info("facility type:"),
                 inspect_type = get_header_info("inspection type:"),
                 inspect_date = get_header_info("inspection date:"))
}

# This function pulls data in from the table listing the violations, usually the 3rd down from the top
get_violations <- function(id_inspection, body) {

  remove_corrective_actions <- function(violation, action) {
    if (str_trim(action) != "") {
      gsub(action, "", violation, fixed = T)  
    } else {
      violation
    }
  }
  
  remove_severity <- function(violation, severity) {
    pattern <- str_c("^\\s*", severity)
    str_trim(gsub(pattern, "", violation))
  }
  
  xpath_exclude_hdrs <- "not(contains(thead//tr//th,'Inspection Information'))"
  xpath_exclude_hdrs <- str_c(xpath_exclude_hdrs, " and not(contains(thead//tr//th,'following violation(s)")
  xpath_exclude_hdrs <- str_c(xpath_exclude_hdrs, " have been corrected since the last inspection'))")
  xpath_exclude_hdrs <- str_c(xpath_exclude_hdrs, " and not(contains(thead//th,'Comments'))")
  
  ptrn_corr_in_inspect <- "\\(?corrected\\s+(during|at\\s+(the)?\\s+time\\s+of)\\s+inspection\\)?"
  
  corrective_actions <- xpathSApply(body, 
                                    str_c("//table[", xpath_exclude_hdrs, "]//tr//td//i"), 
                                    xmlValue)
  
  corrective_actions <- grep("a\\s+summary\\s+of\\s+the\\s+violations\\s+found\\s+during\\s+the\\s+inspection", 
                             corrective_actions, ignore.case = T, value = T, invert = T)
  
  severities <- xpathSApply(body, 
                            str_c("//table[", xpath_exclude_hdrs, "]//tr//td[not(i)]//b"), 
                            function(x) str_trim(xmlValue(x)))
  severities <- grep(ptrn_corr_in_inspect, severities, ignore.case = T, value = T, invert = T)
  
  # Use [^(\\d|a-z|A-Z)] to search for dash and allow more flexibility b/c they are not always a regular '-'
  pattern_code1 <- "^\\s*3717[^(\\d|a-z|A-Z)]\\d{1,2}[^(\\d|a-z|A-Z)]\\d{1,2}(\\.\\d{1,2})?\\s*(\\([A-Z]{1,2}\\))?\\s*(\\(\\d{1,2}\\))?\\s*(\\([a-z]{1,2}\\))?"
  pattern_code2 <- "^\\s*901:3[^(\\d|a-z|A-Z)]\\d{1,2}[^(\\d|a-z|A-Z)]\\d{1,2}(\\([A-Z]{1,2}\\))?" 
  pattern_code3 <- "^\\s*3701[^(\\d|a-z|A-Z)]\\d{1,2}[^(\\d|a-z|A-Z)]\\d{1,2}(\\([A-Z]{1,2}\\))?(\\s*OAC)?"
  pattern_code4 <- "^\\s*(V|I|X|R){1,4}\\b(\\s*[^(\\s|\\d|a-z|A-Z)]\\s*P)?"
  pattern_code <- str_c("(", pattern_code1, ")|(", pattern_code2, ")|(", pattern_code3, ")|(", pattern_code4, ")")
  
  
  violations <- xpathSApply(body, 
                            str_c("//table[", xpath_exclude_hdrs, "]//tr//td"), 
                            xmlValue)
  loc_vio_hdr <- grep("a summary of the violations found during the inspection are listed below", violations, ignore.case = T)
  violations <- violations[(loc_vio_hdr + 1):length(violations)]
  violations <- gsub(ptrn_corr_in_inspect, " ", violations, ignore.case = T)
  violations <- grep(pattern_code, violations, value = T, perl = T)
  violations <- mapply(remove_corrective_actions, violations, corrective_actions, 
                       USE.NAMES = F)
  
  violation_codes <- str_extract(violations, pattern_code)
  
  violations <- gsub(pattern_code, "", violations)
  
  violations <- mapply(remove_severity, violations, severities, 
                       USE.NAMES = F)
  
  tibble::tibble(id_inspection = id_inspection,
                 violation_code = violation_codes,
                 severity = severities,
                 observed_violation = violations,
                 corrective_action = corrective_actions)
}


# Read In List of Inspection URLs -----------------------------------------
url_base <- "https://healthspace.com/Clients/Ohio/Cuyahoga/Cuyahoga_Web_Live.nsf/"

inspection_urls <- read_csv("inspection_hrefs.csv") %>% 
                    mutate(href_inspection = str_c(url_base, href_inspection)) %>% 
                    mutate(id_inspection = str_match(href_inspection, 
                                                      "RestrictToCategory=(.+)$")[,2])


inspections <- tibble::tibble(id_restaurant = character(),
                              id_inspection = character(),
                              facility_type = character(),
                              inspect_type = character(),
                              inspect_date = character())

violations <- tibble::tibble(id_inspection = character(),
                             violation_code = character(),
                             severity = character(),
                             observed_violation = character(),
                             corrective_action = character())

for (i in 1:nrow(inspection_urls)) {
  print(i)
  print(inspection_urls$href_inspection[i])
  
  resp <- getURL(inspection_urls$href_inspection[i])
  doc <- htmlTreeParse(resp, useInternalNodes = T)
  doc <- xmlRoot(doc)
  body <- xmlChildren(doc)$body
  
  inspections <- rbind(inspections, 
                       get_inspection_info(inspection_urls$id_restaurant[i],
                                           inspection_urls$id_inspection[i],
                                           body)
                       )

  if (!grepl("no (violations|observations) were documented at the time of inspection", 
             xmlValue(body), 
             ignore.case = T)) {
    
    violations <- rbind(violations, 
                        get_violations(inspection_urls$id_inspection[i],
                                       body)
    )
    
  }
  
}

write_csv(inspections, "inspections.csv")
write_csv(violations, "violations.csv")
