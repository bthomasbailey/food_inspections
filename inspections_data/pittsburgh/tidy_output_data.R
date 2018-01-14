library(tidyverse)

dir.create("data")

inspections <- read_csv("inspections.csv")
violations <- read_csv("violations.csv")

restaurants <- inspections %>% 
                select(client_id, client_name, address, city, 
                       state, zip, municipality, cat_code) %>% 
                distinct()

inspections <- inspections %>% 
                select(id_inspection, client_id, dt_inspection, purpose,
                       inspector, re_inspection, dt_re_inspection)


violations <- violations %>% 
                # mutate(id_violation = str_extract(violation, "(\\d{1,2})")) %>% 
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

violations <- violations %>% 
                mutate(risk = str_match(comments, regex(pttrn_risk, ignore_case = T))[,2]) %>% 
                mutate(risk = str_to_lower(risk)) %>% 
                mutate(comments = gsub(pttrn_risk, "", comments, ignore.case = T)) %>% 
                select(id_inspection, risk, desc_violation, comments)

write_csv(restaurants, "data//restaurants.csv")
write_csv(inspections, "data//inspections.csv")
write_csv(violations, "data//violations.csv")

