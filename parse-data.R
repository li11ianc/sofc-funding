library(tidyverse)
library (lubridate)
library(stringr)

sheet1 <- read.csv("data/Lilly- SOFC programming2.csv")

sheet1_fix <- sheet1[1:4] %>%
  filter(!is.na(totalRequested)) %>%
  mutate(dateOfSenateMeeting = mdy(dateOfSenateMeeting))

sheet2 <- read.csv("data/Nadia- SOFC programming - Sheet1.csv")

sheet2_fix <- sheet2[1:4] %>%
  mutate(dateOfSenateMeeting = mdy(dateOfSenateMeeting)) %>%
  filter(!is.na(totalReceived)) %>%
  rename(totalRequested = totalReceived)

switch_grant <- sheet2_fix[1:3, 3]
switch_req <-sheet2_fix[1:3, 4]

sheet2_fix[1:3, 3] <- switch_req
sheet2_fix[1:3, 4] <- switch_grant

budget2 <- sheet2[7:9] %>%
  rename(orgName = X.2) %>%
  janitor::clean_names()
# need to mutate to str remove all $ in sum of requested/funded and make dbl

sheet3 <- read.csv("data/Natalie- SOFC programming2.csv")

sheet3_fix <- sheet3[1:4] %>%
  mutate(dateOfSenateMeeting = mdy(dateOfSenateMeeting),
         totalRequested = str_remove_all(totalRequested, "\\$"),
         totalRequested = str_remove_all(totalRequested, ","),
         totalRequested = as.numeric(totalRequested),
         totalGranted = str_remove_all(totalGranted, "\\$"),
         totalGranted = str_remove_all(totalGranted, ","),
         totalGranted = as.numeric(totalGranted))

sheet4 <- read.csv("data/Preetha - SOFC2.csv")

sheet4_fix <- sheet4[1:4] %>%
  mutate(dateOfSenateMeeting = mdy(dateOfSenateMeeting),
         totalRequested = str_remove_all(totalRequested, "\\$"),
         totalRequested = str_remove_all(totalRequested, ","),
         totalRequested = as.numeric(totalRequested),
         totalGranted = str_remove_all(totalGranted, "\\$"),
         totalGranted = str_remove_all(totalGranted, ","),
         totalGranted = as.numeric(totalGranted))

programming <- rbind(sheet1_fix, sheet2_fix, sheet3_fix, sheet4_fix) %>%
  rename(date = dateOfSenateMeeting, org = orgName, req = totalRequested, grant = totalGranted) %>%
  mutate(deny = req - grant)

programming_fix <- programming %>%
  mutate(org = as.character(org)) %>% 
  mutate(org = case_when(
    org == "Natuve American Student Organization" 
      ~ "Native American Student Organization",
    org == "Zeta Phi Beta" 
      ~ "Zeta Phi Beta Sorority, Inc.",
    org %in% c("TedXDuke, TEDXDuke") 
      ~ "TEDxDuke",
    org == "SSA (Main account/ organizer), ASA, CSA, KUSA, TASA, Fusion SLG, LangDom SLG, aKDPhi, Lambda Epsilon Phi, Duke Chinese Dance, Duke Temptasians, Duke Asian American Theatre, Duke Department of Asian and Middle Eastern Studies" 
      ~ "Singapore Students Association",
    org == "Phi Beta Sigma" 
      ~ "Phi Beta Sigma Fraternity, Inc.",
    org %in% c("Muslim Students' Association, Muslim Students Association", "Duke MSA") 
      ~ "Duke Muslim Students Association",
    org %in% c("La Unidad Latina, Lambda Upsilon Lambda Fraternity, Inc.", "La Unidad Latina") 
      ~ "La Unidad Latina Lambda Upsilon Lambda Fraternity, Inc.",
    org %in% c("International Association (IA)") 
      ~ "International Association",
    org %in% c("Alpha Kappa Alpha") 
      ~ "Alpha Kappa Alpha Sorority, Inc.",
    org %in% c("Amandala") 
      ~ "Duke Amandla Chorus",
    org %in% c("BDU") 
      ~ "Blue Devils United",
    org == "Catholic Center" 
      ~ "Duke Catholic Center",
    org %in% c("Chinese Student Association", "Chinese Students Association") 
      ~ "Duke Chinese Student Association",
    org %in% c("Delta Sigma Theta Sorority, Incorporated", "Delta Sigma Theta") 
      ~ "Delta Sigma Theta Sorority, Inc.",
    org %in% c("DukeAFRICA") 
      ~ "Duke Africa",
    org %in% c("DukeASL") 
      ~ "Duke American Sign Language",
    org %in% c("alpha Kappa Delta Phi") 
      ~ "	Alpha Kappa Delta Phi",
    org == "CommuniTEA"
      ~ "Duke CommuniTEA",
    org == "Lambda Theta Alpha" 
      ~ "Lambda Theta Alpha Latin Sorority, Inc.",
    org == "Kappa Alpha Psi" 
      ~ "Kappa Alpha Psi Fraternity, Inc.",
    org == "DUSDAC" ~ "Duke University Student Dining Advisory Committee",
    TRUE ~ org
  ))

programming_fix <- programming_fix %>%
  distinct()

write.csv(programming_fix, "data/programming.csv")

