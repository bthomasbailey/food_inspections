library(tidyverse)
library(lubridate)

# TORONTO -----------------------------------------------------------------
to_data <- read_csv("inspection_data_scraping/toronto/dinesafe.csv")

to_restaurants <- to_data %>% 
                    select(establishment_id, establishment_name, 
                           establishmenttype, establishment_address) %>% 
                    distinct()

to_inspections <- to_data %>% 
                    select(establishment_id, inspection_id, establishment_status, inspection_date) %>% 
                    distinct()

to_violations <- to_data %>% 
                    select(inspection_id, infraction_details, severity, action)


# LAS VEGAS ---------------------------------------------------------------
lv_data <- read_csv("inspection_data_scraping/las_vegas/Restaurant_Inspections.csv",
                    col_types = cols(Violations = "c"))
names(lv_data) <- make.names(names(lv_data))

lv_restaurants <- lv_data %>% 
                    select(Permit.Number, Location.Name, Address, City, State, Zip) %>% 
                    distinct() %>% 
                    mutate(Zip = gsub("-\\d*", "", Zip))

# Let Serial.Number function as the inspection ID
lv_inspections <- lv_data %>% 
                    select(Permit.Number, Serial.Number, Inspection.Time, 
                           Inspection.Type, Inspection.Demerits,
                           Inspection.Grade, Permit.Status, Inspection.Result) %>% 
                    mutate(Inspection.Time = parse_date_time(Inspection.Time, "mdYHMS")) %>% 
                    mutate(Inspection.Date = floor_date(Inspection.Time, "day")) %>% 
                    select(-Inspection.Time)

lv_violations <- lv_data %>% 
                    select(Serial.Number, Violations) %>% 
                    mutate(ViolationID = str_split(Violations, ",")) %>% 
                    unnest(ViolationID) %>% 
                    select(-Violations)

# Read in violation code descriptions and join to lv_violations
lv_violation_codes <- read_csv("inspection_data_scraping/las_vegas/Restaurant_Inspection_Violation_Codes.csv",
                               col_types = cols(`Violation ID` = "c"))

names(lv_violation_codes) <- make.names(names(lv_violation_codes))

lv_violations <- lv_violations %>% 
                  left_join(lv_violation_codes, by = c("ViolationID" = "Violation.ID")) %>% 
                  select(-Violation.Code)



# CHAMPAIGN-URBANA --------------------------------------------------------
cu_restaurants <- read_csv("inspection_data_scraping/champaign-urbana/data/restaurants.csv")
cu_inspections <- read_csv("inspection_data_scraping/champaign-urbana/data/inspections.csv")
cu_violations <- read_csv("inspection_data_scraping/champaign-urbana/data/violations.csv")


# CLEVELAND ---------------------------------------------------------------
cle_restaurants <- read_csv("inspection_data_scraping/cleveland/data/restaurants.csv")

re_pat <- ",\\s*(.+)"
cle_restaurants <- cle_restaurants %>% 
                    mutate(city = str_match(address, re_pat)[,2]) %>% 
                    mutate(address = gsub(re_pat, "", address))

cle_inspections <- read_csv("inspection_data_scraping/cleveland/data/inspections.csv")

cle_inspections <- cle_inspections %>% 
                    mutate(inspect_date = dmy(cle_inspections$inspect_date))

cle_violations <- read_csv("inspection_data_scraping/cleveland/data/violations.csv")


# PITTSBURGH --------------------------------------------------------------
pitt_inspections <- read_csv("inspection_data_scraping/pittsburgh/data/inspections.csv")
pitt_violations <- read_csv("inspection_data_scraping/pittsburgh/data/violations.csv")

pitt_restaurants <- pitt_inspections %>% 
                      select(client_id, client_name, address, city, 
                             state, zip) %>% 
                      distinct()

pitt_inspections <- pitt_inspections %>% 
                      select(id_inspection, client_id, dt_inspection, purpose,
                             inspector, re_inspection, dt_re_inspection)

pitt_violations <- pitt_violations %>% 
                    mutate(desc_violation = str_to_lower(str_trim(gsub("\\d", "", violation)))) %>% 
                    mutate(desc_violation = ifelse(desc_violation %in% c("contamination of food and food-contact s", "environmental contamination"), 
                                                   "contamination prevention - food, utensils and equipment", desc_violation)) %>% 
                    mutate(desc_violation = ifelse(desc_violation == "cook/reheat", "cooking temperatures", desc_violation)) %>% 
                    mutate(desc_violation = ifelse(desc_violation == "food cooled safely", "cooling food", desc_violation)) %>% 
                    mutate(desc_violation = ifelse(desc_violation %in% c("food held/ thawed", "food source/condiiton"), "food source/condition", desc_violation)) %>% 
                    mutate(desc_violation = ifelse(desc_violation == "handwashing and personal hygiene", "handwashing facilities", desc_violation)) %>% 
                    mutate(desc_violation = ifelse(desc_violation == "pests, animals and garbage", "pest management", desc_violation)) %>% 
                    mutate(desc_violation = ifelse(desc_violation == "plumbing/sewage", "plumbing", desc_violation)) %>% 
                    mutate(desc_violation = ifelse(desc_violation == "thermometers", "probe-type thermometers", desc_violation)) %>% 
                    mutate(desc_violation = ifelse(desc_violation == "toilet facilities", "toilet room", desc_violation)) %>% 
                    mutate(desc_violation = ifelse(desc_violation == "water source", "water supply", desc_violation)) %>% 
                    mutate(desc_violation = ifelse(desc_violation == "refrigeration and hot holding facilites", "hot holding temperatures", desc_violation)) %>% 
                    mutate(desc_violation = ifelse(desc_violation == "chemicals", "toxic items", desc_violation)) %>% 
                    select(-violation)

pttrn_risk <- "\\*\\s*(low|medium|high)\\s+risk\\s*\\*"

pitt_violations <- pitt_violations %>% 
                    mutate(risk = str_match(comments, regex(pttrn_risk, ignore_case = T))[,2]) %>% 
                    mutate(risk = str_to_lower(risk)) %>% 
                    mutate(comments = gsub(pttrn_risk, "", comments, ignore.case = T)) %>% 
                    select(id_inspection, risk, desc_violation, comments)


# Combine Restaurant DFs --------------------------------------------------
names(to_restaurants) <- c("ID_Rest", "Name", "Type", "Address")
names(lv_restaurants) <- c("ID_Rest", "Name", "Address", "City", "State", "Zip")
names(cu_restaurants) <- c("ID_Rest", "Name", "Address", "City", "State", "Zip")
names(cle_restaurants) <- c("ID_Rest", "Name", "Address", "City")
names(pitt_restaurants) <- c("ID_Rest", "Name", "Address", "City", "State", "Zip")

# Prepend ID with city name in case IDs are same across cities
to_restaurants <- to_restaurants %>% 
                    mutate(City = "Toronto") %>% 
                    mutate(State = "ON") %>% 
                    mutate(ID_Rest = str_c("to", ID_Rest)) %>% 
                    select(-Type)

lv_restaurants <- lv_restaurants %>% 
                    mutate(ID_Rest = str_c("lv", ID_Rest)) %>% 
                    mutate(State = "NV")

cu_restaurants <- cu_restaurants %>% 
                    mutate(ID_Rest = str_c("cu", ID_Rest)) %>% 
                    mutate(Zip = as.character(Zip))

cle_restaurants <- cle_restaurants %>% 
                    mutate(State = "OH") %>% 
                    mutate(ID_Rest = str_c("cle", ID_Rest))

pitt_restaurants <- pitt_restaurants %>% 
                      mutate(ID_Rest = str_c("pitt", ID_Rest)) %>% 
                      mutate(Zip = as.character(Zip))

restaurants <- bind_rows(to_restaurants, lv_restaurants, cu_restaurants, cle_restaurants, pitt_restaurants)

if(!dir.exists("data")) {dir.create("data")}
write_csv(restaurants, "data/restaurants.csv")
