library(tidyverse)
library (lubridate)
library(stringr)

sheet1 <- read.csv("data/Lilly- SOFC programming - Sheet1.csv")

sheet1_fix <- sheet1[1:4] %>%
  filter(!is.na(totalRequested)) %>%
  mutate(dateOfSenateMeeting = mdy(dateOfSenateMeeting))

sheet2 <- read.csv("data/Nadia- SOFC programming - Sheet1.csv")

sheet2_fix <- sheet2[1:4] %>%
  mutate(dateOfSenateMeeting = mdy(dateOfSenateMeeting)) %>%
  filter(!is.na(totalReceived)) %>%
  rename(totalRequested = totalReceived)

budget2 <- sheet2[7:9] %>%
  rename(orgName = X.2) %>%
  janitor::clean_names()
# need to mutate to str remove all $ in sum of requested/funded and make dbl

sheet3 <- read.csv("data/Natalie- SOFC programming - Sheet1.csv")

sheet3_fix <- sheet3[1:4] %>%
  mutate(dateOfSenateMeeting = mdy(dateOfSenateMeeting),
         totalRequested = str_remove_all(totalRequested, "\\$"),
         totalRequested = str_remove_all(totalRequested, ","),
         totalRequested = as.numeric(totalRequested),
         totalGranted = str_remove_all(totalGranted, "\\$"),
         totalGranted = str_remove_all(totalGranted, ","),
         totalGranted = as.numeric(totalGranted))

sheet4 <- read.csv("data/Preetha - SOFC - Sheet1.csv")

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

write.csv(programming, "data/programming.csv")
