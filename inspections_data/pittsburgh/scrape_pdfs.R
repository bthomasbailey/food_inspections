library(pdftools)
library(lubridate)
library(readr)
library(dplyr)
library(stringr)

scrape_header <- function(pg1, id_inspection) {

  pg1 <- unlist(str_split(pg1, "\\n"))
  
  extract_value <- function(key, trailing_key, perl) {
      nbr_line <- grep(key, pg1, perl = perl)
      line <- pg1[nbr_line]
      pattern <- str_c(key, "\\s*(.+)", trailing_key)
      value <- str_match(line, pattern)[1,2]
      value <- gsub("\\s+", " ", value)
      str_trim(value)
  }
  
  data.frame(id_inspection = id_inspection,
             client_id = extract_value("Client ID:", "Client Name:", F),
             client_name = extract_value("Client Name:", "", F),
             address = extract_value("Address:", "Inspection Date:", F),
             dt_inspection = mdy(extract_value("(?<!Re-)\\sInspection Date:", "", T)),
             city = extract_value("City:", "State:", F),
             state = extract_value("State:", "Zip:", F),
             zip = extract_value("Zip:", "Purpose:", F),
             purpose = extract_value("Purpose:", "", F),
             municipality = extract_value("Municipality:", "Inspector:", F),
             inspector = extract_value("Inspector:", "Permit Exp. Date:", F),
             dt_permit_exp = mdy(extract_value("Permit Exp. Date:", "", F)),
             cat_code = extract_value("Category Code:", "Priority Code:", F),
             re_inspection = extract_value("Re\\s*-\\s*Inspection:", "Re\\s*-\\s*Inspection Date:", F),
             dt_re_inspection = mdy(extract_value("Re\\s*-\\s*Inspection Date:", "", F)),
             stringsAsFactors = F
  )
  
}

scrape_page <- function(pg, id_inspection) {
  
  pg <- unlist(str_split(pg, "\\n"))
  loc_vio_start <- grep("^violation:", pg, ignore.case = T)[1]
  
  #If page doesn't have any violation info, stop scraping
  if (!is.na(loc_vio_start)) {
    line_end <- grep("^(other assess?ment observations and comments:)", pg, ignore.case = T)
    line_end <- ifelse(length(line_end) == 0,
                       length(pg),
                       line_end - 1)
    
    pg <- pg[loc_vio_start:(line_end - 1)]
    
    pattern_skip_lines <- "^((food code section\\(s\\):)"
    pattern_skip_lines <- str_c(pattern_skip_lines, "|(corrective action:)")
    pattern_skip_lines <- str_c(pattern_skip_lines, "|(\\s*client\\s*#))")
    
    pg <- grep(pattern_skip_lines, pg, ignore.case = T, value = T, invert = T)
    
    violations <- grep("^(violation:)", pg, ignore.case = T, value = T)
    violations <- gsub("violation:\\s*", "", violations, ignore.case = T)
    
    comments <- grep("^(violation:)", pg, ignore.case = T, value = T, invert = T)
    
    # Remove the text "Comments:" if it does not start the line, so that if it appears again it 
    # does not mess up the split
    comments <- vapply(comments, 
                       function(x) {
                        if (grepl("^(comments:)", x, ignore.case = T)) {
                          txt <- gsub("^(comments:)", "", x, ignore.case = T)
                          str_c("Comments:", gsub("comments:", "", txt, ignore.case = T))
                        } else {
                          gsub("comments:", "", x, ignore.case = T)
                        }
                      }, FUN.VALUE = character(1), USE.NAMES = F)
    
    comments <- paste(comments, collapse = "")
    comments <- unlist(str_split(comments, "Comments:"))
    comments <- comments[2:length(comments)]
    
    if (length(comments) == 0) {
      comments <- NA
    } else {
      comments <- gsub("\\s+", " ", comments)
    }
    
    data.frame(id_inspection = id_inspection,
               violation = violations,
               comments = comments,
               stringsAsFactors = F)
    
  }
  
}


base_url <- "http://appsrv.achd.net/reports/rwservlet?food_rep_insp&P_ENCOUNTER="

# dt <- ymd(20040415)
dt <- ymd(20160324)
inspections <- data.frame(id = character(),
                          client_name = character(),
                          address = character(),
                          dt_inspection = character(),
                          city = character(),
                          state = character(),
                          zip = character(),
                          purpose = character(),
                          municipality = character(),
                          inspector = character(),
                          dt_permit_exp = character(),
                          cat_code = character(),
                          re_inspection = character(),
                          dt_re_inspection = character())  

violations <- data.frame(id = character(),
                         violation = character(),
                         comments = character())


while (dt <= now()) {
  url <- str_c(base_url, gsub("-", "", dt))

  print(url)  
  print("------------------")
  
  i <- 1
  err <- F
  
  while (!err) {
    id <- str_c(paste(rep(0, 4 - nchar(i)), collapse = ""), i)
    
    url2 <- str_c(url, id)
    print(url2)
    pdf <- suppressMessages(try(pdf_text(url2)))
    
    if (length(attr(pdf, "class")) == 0) {
      
      # Make sure report is not blank
      if (gregexpr("client name:", pdf[1], ignore.case = T)[[1]] != -1) {
        inspection_data <- scrape_header(pdf[1], id = str_c(gsub("-", "", dt), id))
        violation_data <- lapply(pdf[2:length(pdf)], scrape_page, id = str_c(gsub("-", "", dt), id))
        violation_data <- bind_rows(violation_data)
        
        inspections <- rbind(inspections, inspection_data)    
        violations <- rbind(violations, violation_data)    
      }
      
    } else {
      err <- T
    }
    
    i <- i + 1
  }

  dt <- dt + 1
  
}

write_csv(inspections, "inspections.csv")
write_csv(violations, "violations.csv")


