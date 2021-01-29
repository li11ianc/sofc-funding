library(tidyverse)

label_bipoc <- function(x) {
  x_new <- x %>%
    mutate(bipoc = case_when(
      org %in% c("Asian American Alliance", 
                 "Alpha Kappa Alpha Sorority, Inc.",
                 "Alpha Phi Alpha Fraternity, Inc.",
                 "Asian Intervarsity Christian Fellowship", #preetha added
                 "Duke Amandla Chorus",
                 "Duke Africa", #preetha added
                 "Duke Africa ",
                 "Asian Students Association",
                 "Duke Asian American Theater", #preetha added
                 "Black Student Alliance",
                 "Duke Chinese Theater",
                 "Duke Chinese Dance ", #preetha added
                 "Duke Sangeet", #preetha added
                 "Students of the Caribbean Association",
                 "Duke Chinese Student Association", # I added
                 "Singapore Students Association", # I added
                 "Duke Africa", # I added
                 "Black Men's Union", # I added
                 "Delta Sigma Theta Sorority, Inc.",
                 "Duke Dhamaka",
                 "Pureun", #preetha added
                 "Duke East Asia Nexus", #preetha added
                 "Duke East Asian Nexus", #preetha added
                 "Duke Diya", #preetha added
                 "Hindu Students Association",
                 "Kappa Alpha Psi Fraternity, Inc.",
                 "Lambda Theta Alpha Latin Sorority, Inc.",
                 "La Unidad Latina Lambda Upsilon Lambda Fraternity, Inc.",
                 "Mi Gente",
                 "Multicultural Greek Council", #preetha added
                 "Minority Association of Premedical Students", #preetha added
                 "Nakisai African Dance Ensemble",
                 "National Society of Black Engineers",
                 "Native American Student Organization", #preetha added
                 "Duke Rhydhun",
                 "Taiwanese American Students Association",
                 "Duke Southeast Asian Students Association", #preetha added
                 "The Bridge",
                 "United in Praise",
                 "Zeta Phi Beta Sorority, Inc.",
                 "Duke Nepali Student Association",
                 "Duke Ethiopian/Eritrean Student Transnational Association",
                 "Desarrolla",
                 "Sabrosura", #preetha added
                 "Gente Aprendiendo para Nuevas Oportunidades",
                 "Project H.E.A.L. (Health Education and Awareness in Latin America)",
                 "Pakistani Students Association",
                 "Duke CommuniTEA",
                 "Duke Association for the Middle East",
                 "International Association",
                 "Taiwanese American Student Association", # lilly added
                 "Define American", #preetha added
                 "Muslim Students' Association ",
                 "Duke Muslim Students Association",
                 "Muslim Students Association",
                 "Duke Sikh Society",
                 "Students for Justice in Palestine",
                 "Black Women's Union", #lilly added
                 "Duke National Association for the Advancement of Colored People", # lilly added
                 "Duke Lasya", #lilly added
                 "Duke Raas", #lilly added
                 "Duke Students for Justice in Palestine", 
                 "Minority Association of Pre-Medical Students" #lilly added
                 ) ~ "Y",
      TRUE ~ "N"))
  
  x_new <- x_new %>%
    mutate(
      community = case_when(
      org %in% c("Asian American Alliance",
                 "Asian Students Association",
                 "Asian Intervarsity Christian Fellowship",
                 "Duke Chinese Theater",
                 "Duke Chinese Dance ", #preetha added
                 "Duke Dhamaka",
                 "Duke Diya",
                 "Hindu Students Association",
                 "Asian Intervarsity Christian Fellowship", #preetha added
                 "Duke Rhydhun",
                 "Pureun", #preetha added
                 "Duke East Asia Nexus", #preetha added
                 "Duke East Asian Nexus", #preetha added
                 "Taiwanese American Student Association",
                 "Duke CommuniTEA",
                 "Duke Sikh Society",
                 "Duke Nepali Student Association",
                 "Pakistani Students Association",
                 "Duke Southeast Asian Students Association",
                 "Duke Chinese Student Association",
                 "Duke Sangeet",
                 "Japanese Culture Club", #lilly added
                 "Duke Lasya", #lilly added
                 "Duke Raas", #lilly added
                 "Singapore Students Association") ~ "Asian",
      org %in% c("Alpha Kappa Alpha Sorority, Inc.",
                 "Alpha Phi Alpha Fraternity, Inc.",
                 "Duke Amandla Chorus",
                 "Black Student Alliance",
                 "Delta Sigma Theta Sorority, Inc.",
                 "Kappa Alpha Psi Fraternity, Inc.",
                 "Nakisai African Dance Ensemble",
                 "National Society of Black Engineers",
                 "United in Praise",
                 "Zeta Phi Beta Sorority, Inc.",
                 "Duke Ethiopian/Eritrean Student Transnational Association",
                 "Duke Africa",
                 "Black Men's Union",
                 "Black Women's Union", #lilly added
                 "Duke National Association for the Advancement of Colored People" # lilly added
                 ) ~ "Black",
      org %in% c("Lambda Theta Alpha Latin Sorority, Inc.", 
                 "La Unidad Latina Lambda Upsilon Lambda Fraternity, Inc.",
                 "Mi Gente",
                 "Sabrosura",
                 "Gente Aprendiendo para Nuevas Oportunidades",
                 "Project H.E.A.L. (Health Education and Awareness in Latin America)",
                 "Desarrolla") ~ "Latinx",
      org %in% c("Duke Association for the Middle East",
                 "Duke Muslim Students Association",
                 "Duke Students for Justice in Palestine") ~ "Middle-Eastern",
      org %in% c("Native American Student Organization") ~ "Indigenous",
      org %in% c("Multicultural Greek Council", #preetha added
                 "Minority Association of Premedical Students", #preetha added
                 "The Bridge",
                 "United in Praise",
                 "International Association",
                 "Minority Association of Pre-Medical Students" #lilly added
                 ) ~ "Multicultural group",
      org %in% c("Students of the Caribbean Association") ~ "Other BIPOC group",
      TRUE ~ "Non-BIPOC"
    ))
  
  return(x_new)
}

prog <- read.csv("data/programming.csv")
prog <- label_bipoc(prog)
write_csv(prog, "data-labeled/programming.csv")


budget <- read.csv("data/budgets.csv")
budget <- label_bipoc(budget)
write_csv(budget, "data-labeled/budget.csv")

sofc <- read.csv("data/sofctotals.csv")
sofc <- label_bipoc(sofc)
write_csv(sofc, "data-labeled/sofc.csv")

budget_source <- read.csv("data/filtered-budget-from-source.csv")
budget_source <- label_bipoc(budget_source)
write_csv(budget_source, "data-labeled/filtered-budget-from-source.csv")

budg_adj <- read.csv("line-item-data/budg_adj.csv")
budg_adj <- label_bipoc(budg_adj)
write_csv(budg_adj, "data-labeled/filtered-budget-chron.csv")


