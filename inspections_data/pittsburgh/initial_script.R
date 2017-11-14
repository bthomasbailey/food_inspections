library(pdftools)
library(stringr)
library(lubridate)

doc <- pdf_text("rwservlet.pdf")

pg1 <- doc[[1]]
pg2 <- doc[[2]]
pg3 <- doc[[3]]

pg1_split <- unlist(str_split(pg1, "\\n"))

extract_value <- function(key, trailing_key, perl) {
  nbr_line <- grep(key, pg1_split, perl = perl)
  line <- pg1_split[nbr_line]
  pattern <- str_c(key, "\\s*(.+)", trailing_key)
  value <- str_match(line, pattern)[1,2]
  value <- gsub("\\s+", " ", value)
  str_trim(value)
}

# Header Info
client_name <- extract_value("Client Name:", "\\r", F)
address <- extract_value("Address:", "Inspection Date:", F)
dt_inspection <- mdy(extract_value("(?<!Re-)\\sInspection Date:", "\\r", T))
city <- extract_value("City:", "State:", F)
state <- extract_value("State:", "Zip:", F)
zip <- extract_value("Zip:", "Purpose:", F)
purpose <- extract_value("Purpose:", "\\r", F)
municipality <- extract_value("Municipality:", "Inspector:", F)
inspector <- extract_value("Inspector:", "Permit Exp. Date:", F)
dt_permit_exp <- mdy(extract_value("Permit Exp. Date:", "\\r", F))
cat_code <- extract_value("Category Code:", "Priority Code:", F)
re_inspection <- extract_value("Re\\s*-\\s*Inspection:", "Re\\s*-\\s*Inspection Date:", F)
dt_re_inspection <- mdy(extract_value("Re\\s*-\\s*Inspection Date:", "\\r", F))


# First Chunk
# get_col_position <- function(str_srch, header_line) {
#   regexpr(str_srch, header_line, ignore.case = T)
# }

header1_row <- grep("food safety assessment categories", pg1_split, 
                   ignore.case = T) + 1

header2_row <- grep("general sanitation", pg1_split, 
                    ignore.case = T) + 1

header1_line <- pg1_split[header1_row]

# d_col <- get_col_position("d", header1_line)
# s_col <- get_col_position("s", header1_line)
# no_col <- get_col_position("no", header1_line)
# na_col <- get_col_position("na", header1_line)
# v_col <- get_col_position("v", header1_line)
# high_col <- get_col_position("high", header1_line)
# med_col <- get_col_position("med\\.", header1_line)
# low_col <- get_col_position("low", header1_line)

d_col_orig <- regexpr("d", header1_line, ignore.case = T)
v_col_orig <- regexpr("v", header1_line, ignore.case = T)
assess_status_header <- substr(header1_line, d_col_orig, v_col_orig)
d_col <- 1
s_col <- regexpr("s", assess_status_header, ignore.case = T)
no_col <- regexpr("no", assess_status_header, ignore.case = T)
na_col <- regexpr("na", assess_status_header, ignore.case = T)
v_col <- nchar(assess_status_header)


line <- pg1_split[15]
assess_status <- substr(line, d_col_orig, v_col_orig)
pos <- regexpr("x", assess_status)
