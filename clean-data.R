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
  mutate(deny = req - grant,
         prop_grant = grant / req)

fix_orgnames <- function(x) {
x_new <- x %>%
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
    org %in% c("La Unidad Latina, Lambda Upsilon Lambda Fraternity, Inc.", "La Unidad Latina", "La Unidad Latina, Lambda Upsilon Lambda Fraternity, Inc.", "La Unidad Latina Lambda Upsilon Lambda Fraternity, Inc. ") 
      ~ "La Unidad Latina Lambda Upsilon Lambda Fraternity, Inc.",
    org %in% c("International Association (IA)", "International Association ") 
      ~ "International Association",
    org %in% c("Alpha Kappa Alpha") 
      ~ "Alpha Kappa Alpha Sorority, Inc.",
    org %in% c("Amandala") 
      ~ "Duke Amandla Chorus",
    org %in% c("Duke Hindu Students Association (HSA)")
      ~ "Hindu Students Association",
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
    org %in% c("Lambda Theta Alpha", "Lambda Theta Alpha Latin Sorority Inc.", "Lambda Theta Alpha Latin Sorority, Inc. ")
      ~ "Lambda Theta Alpha Latin Sorority, Inc.",
    org == "Kappa Alpha Psi" 
      ~ "Kappa Alpha Psi Fraternity, Inc.",
    org == "DUSDAC" ~ "Duke University Student Dining Advisory Committee",
    org %in% c("Duke Nepalese Student Association") ~ "Duke Nepali Student Association",
    org %in% c("Duke Association for the Middle East (DAME)") ~ "Duke Association for the Middle East",
    org %in% c("Duke Diya (South Asian Students Association)", "Duke South Asian Students Association (DIYA)")
      ~ "Duke Diya",
    org %in% c("Duke Ethiopian/Eritrean Student Transactional Association")
      ~ "Duke Ethiopian/Eritrean Student Transnational Association",
    org %in% c("Alpha Phi Alpha Fraternity Inc. (Kappa Omicron)")
      ~ "Alpha Phi Alpha Fraternity, Inc.",
    org %in% c("Kappa Alpha Psi Inc", "Kappa Alpha Psi Fraternity, Incorporated")
      ~ "Kappa Alpha Psi Fraternity, Inc.",
    org %in% c("Pakistani Students Association (PSA)")
      ~ "Pakistani Students Association",
    org %in% c("Zeta Phi Beta Sorority Inc. ")
      ~ "Zeta Phi Beta Sorority, Inc.",
    TRUE ~ org
  ))

  return(x_new)
}

programming_fix <- fix_orgnames(programming)

programming_fix <- programming_fix %>%
  distinct()

programming_fix <- programming_fix %>%
  filter(!is.na(date), deny >= 0) %>%
  mutate(prop_grant = grant / req,
         year = year(date),
         month = month(date),
         sem = case_when(
           month %in% c(1, 2, 3, 4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9, 10, 11, 12) ~ "Fall"),
         schoolyr = case_when(
           year == 2016 & sem == "Fall" ~ "2016-2017",
           year == 2017 & sem == "Spring" ~ "2016-2017",
           year == 2017 & sem == "Fall" ~ "2017-2018",
           year == 2018 & sem == "Spring" ~ "2017-2018",
           year == 2018 & sem == "Fall" ~ "2018-2019",
           year == 2019 & sem == "Spring" ~ "2018-2019",
           year == 2019 & sem == "Fall" ~ "2019-2020",
           year == 2020 & sem == "Spring" ~ "2019-2020"
         ))

write.csv(programming_fix, "data/programming.csv")

budg1920 <- read.csv("data/AB 2019-2020.xlsx - Final.csv")

budg1920 <- budg1920 %>%
  janitor::clean_names()

budg1920 <- budg1920 %>%
  select(org = on_behalf_of, req = requested_amount, grant = adjusted_amount) %>%
  mutate(schoolyr = "2019-2020")

budg2021 <- read.csv("data/AB 2020-2021.xlsx - Master AB 2020-2021.csv")

budg2021 <- budg2021[1:126,] %>%
  janitor::clean_names() %>%
  select(org = organization_name, req = amount_requested, grant = amount_funded)

budg2021 <- budg2021 %>%
  mutate(req = str_remove(req, "\\$"),
         req = str_remove(req, ","),
         req = as.numeric(req),
         grant = str_remove(grant, "\\$"),
         grant = str_remove(grant, ","),
         grant = case_when(
           grant == " -   " ~ "0", # or should be as.character(NA)?
           TRUE ~ grant),
         grant = as.numeric(grant),
         schoolyr = "2020-2021")

#budg2021$org <- trimws(budg2021$org, which = c("both"))

budget <- rbind(budg1920, budg2021) %>%
  mutate(deny = req - grant,
         prop_grant = grant / req)

budget <- fix_orgnames(budget)

write.csv(budget, "data/budgets.csv")

sofc1718 <- read.csv("data/2017-2018 sofc - Funded v. Requested.csv")

sofc1718 <- sofc1718 %>%
  janitor::clean_names() %>%
  select(org = x, req = sum_of_requested, grant = sum_of_funded)

sofc1718 <- sofc1718%>%
  mutate(req = str_remove(req, "\\$"),
         req = str_remove(req, ","),
         req = as.numeric(req),
         grant = str_remove(grant, "\\$"),
         grant = str_remove(grant, ","),
         grant = case_when(
           grant == " -   " ~ "0", # or should be as.character(NA)?
           TRUE ~ grant),
         grant = as.numeric(grant),
         schoolyr = "2017-2018")

sofc <- rbind(sofc1718) %>% # can use this if we add more sofc total sheets for other school years
  mutate(deny = req - grant,
         prop_grant = grant / req)

sofc <- fix_orgnames(sofc)

write.csv(sofc, "data/sofctotals.csv")

budget_source <- read.csv("data/Master Budgets from Source.csv")

budget_source <- budget_source %>%
  janitor::clean_names() %>%
  filter(group != "", group != "TOTAL") %>%
  rename(org = group, req = originally_requested, grant = allocated, req_filt = updated_requested)

budget_source <- budget_source %>%
  mutate(
    req = str_remove(req, "\\$"),
    req = str_remove(req, ","),
    req = as.numeric(req),
    req_filt = str_remove(req_filt, "\\$"),
    req_filt = str_remove(req_filt, ","),
    req_filt = as.numeric(req_filt),
    grant = str_remove(grant, "\\$"),
    grant = str_remove(grant, ","),
    grant = case_when(
      grant == " -   " ~ "0", # or should be as.character(NA)?
      TRUE ~ grant),
    grant = as.numeric(grant),
    schoolyr = "2020-2021",
    deny = req - grant,
    prop_grant = grant / req,
    deny_filt = req_filt - grant,
    prop_grant_filt = grant / req_filt,
    update = req_filt - req,
    filt = case_when(
      update != 0 ~ "Y",
      TRUE ~ "N")
    )

budget_source_fix <- fix_orgnames(budget_source)

write.csv(budget_source_fix, "data/filtered-budget-from-source.csv")

a_d <- read_csv("line-item-data/Evelyn - Adjusted Budget Items 2020-2021 (A-D) - Sheet1.csv")
d_e <- read_csv("line-item-data/Natalie - Adjusted Budget Items 2020-2021 (D-E) - Sheet1.csv")
e_n <- read_csv("line-item-data/Shari - Adjusted Budget Items 2020-2021 (E-N) - Sheet1.csv")
n_z <- read_csv("line-item-data/Lilly - Adjusted Budget Items 2020-2021 (N-Z) - Sheet1.csv")

files <- c(a_d, d_e, e_n, n_z)

##for (i in files) {
##  i <- i[-1] %>%

a_d <- a_d[-1,] %>%
    mutate(req = as.numeric(req),
           grant = as.numeric(grant),
           req_filt = as.numeric(req_filt),
           total_removed = as.numeric(total_removed))
d_e <- d_e[-1,] %>%
  mutate(req = as.numeric(req),
         grant = as.numeric(grant),
         req_filt = as.numeric(req_filt),
         total_removed = as.numeric(total_removed))
e_n <- e_n[-1,] %>%
  mutate(req = as.numeric(req),
         grant = as.numeric(grant),
         req_filt = as.numeric(req_filt),
         total_removed = as.numeric(total_removed))
n_z <- n_z[-1,] %>%
  mutate(req = as.numeric(req),
         grant = as.numeric(grant),
         req_filt = as.numeric(req_filt),
         total_removed = as.numeric(total_removed))

budg_adj <- bind_rows(a_d, d_e, e_n, n_z)

budg_adj <- budg_adj %>%
  mutate(deny = req - grant,
         prop_grant = grant / req,
         prop_grant_filt = grant / req_filt)

budg_adj <- fix_orgnames(budg_adj)

write_csv(budg_adj, "line-item-data/budg_adj.csv")
