########################### PAKKAR
# install.packages(dplyr)          # Mikilvaegur pakki fyrir gagnaumsyslu
# install.packages(stringr)        # Til ad vinna med textagogn
library(dplyr)
library(stringr)



############################ LESA INN SKJOL
# Byrja a ad vista skjolin sem .csv (gert i excel og Save As)
# Tha er haegt ad lesa thau inn og segja ad encoding se ANSI svo ad islensku stafir
# birtist


data_2018 <- read.csv2("Kopi af Burðarsaga 2018.csv", encoding = "ANSI")
data_2019 <- read.csv2("Kopi af Burðarsaga 2019.csv", encoding = "ANSI")
data_2020 <- read.csv2("Kopi af Burðarsaga 2020.csv", encoding = "ANSI")
data_2021 <- read.csv2("Kopi af Burðarsaga 2021.csv", encoding = "ANSI")
data_all <- rbind(data_2018, data_2019, data_2020, data_2021)

colnames(data_all) <-
  c(
    "Farm_numb",
    "ID_dam",
    "Birthdate_dam",
    "Nr_last_insem",
    "Days_insem_calving",
    "Sire_ID",
    "Calving_date",
    "Calving_nr",
    "Sire_dam",
    "Sex",
    "Gangur",
    "Fate",
    "ID_calf"
  )

# Breyta dagsetningum yfir i "Date" format
data_all$Calving_date <- as.Date(data_all$Calving_date, "%d.%m.%Y")
data_all$Birthdate_dam <- as.Date(data_all$Birthdate_dam, "%d.%m.%Y")
################### Endurskilgreiningar á breytum

# Gera aldur vid fyrsta burd
data <- data_all %>% mutate( age_at_1st_calving = case_when ( Calving_nr == 1 ~  Calving_date - Birthdate_dam ))

# Endurkoda fate, virkar svona:
# Fate er sett sem 1 ef ad i Fate stendur "1, Settur á". O.s.frv.

data <- data %>% mutate( Fate = case_when( Fate ==  "1, Settur á" ~ "1",
                                           Fate ==  "2, Alinn til kjötframleiðslu" ~ "2",
                                           Fate ==  "3, Slátrað/í sláturhús" ~ "3",
                                           Fate ==  "4, Fæddist dauður" ~ "4",
                                           Fate ==  "5, Fósturlát" ~ "5",
                                           Fate ==  "6, Seldur" ~ "6",
                                           Fate ==  "7,  Drapst innan sólarhrings" ~ "7",
                                           Fate ==  "8, Drapst innan 20 daga" ~ "8",
                                           Fate ==  "9, Drapst í fæðingu" ~ "9"
                                           ),
                         Gangur = case_when( Gangur ==  "1. Auðveldur burður-bar sjálf" ~ "1",
                                             Gangur ==  "2. Léttur burður-lítil burðarhjálp" ~ "2",
                                             Gangur ==  "3. Erfiður burður-mikil burðarhjálp" ~ "3",
                                             Gangur ==  "4. Mjög erfiður burður-aðstoð dýralæknis" ~ "4",
                                             Gangur ==  "" ~ "NA"

                                               ))
#
# Fjarlaegja milliskref
# rm(data_2018,data_2019, data_2020,data_2021)

############################## Data Quality Filters
# Thessi skipun siar gognin um gestation length og setur þau sem gildi á bilinu 280 - 298 dagar.
# Thid thurfid ad akveda hvad thid viljid gera med thau
# Sem daemi hef eg lika tekid ut allar sem eru ekki fyrsta kalfs kvigur herna ad nedan i gognunum kvigur

data_filtered <- data %>% filter ( Days_insem_calving > 280 & Days_insem_calving < 298 )


kvigur <- data %>% filter (  !is.na(age_at_1st_calving)  ) # fjarlaegir allar NA
kvigur <- data %>% filter ( Calving_nr == 1  )             # fjarlaegir allar sem eru ekki med burdarnumer 1, gefur somu nidurstodu og hitt

# Lika haegt ad sameina mismunandi filtera
data_filtered <- data %>% filter ( Gangur == 1 & Fate == 1 )

# Medalaldur kvigna vid fyrsta burd
mean(data_filtered$age_at_1st_calving, na.rm = T)

# Breyti forminu a aldri kviga ur "difftime" i daga
data_filtered$age_at_1st_calving <- as.integer(data_filtered$age_at_1st_calving)

# Profa aftur ad taka medaltal, en deili med 30 til ad fa manudi
mean(data_filtered$age_at_1st_calving, na.rm = T)/30


############################ Lysandi tolfraedi
# Eingongu gert sem daemi um konnunartolfraedi
# Herna fjoldi athugana eftir aldri
summarised_data <- data %>% group_by( Calving_nr ) %>% tally()

# Hernda fjoldi athugana eftir aldri og Gangur
summarised_data <- data %>% group_by( Calving_nr, Gangur ) %>% tally()

# fjoldi eftir aldri og afdrifum kalfs en skoda bara 1sta og 2nrs kalfs kvigur

summarised_data <- data %>% group_by( Calving_nr, Fate ) %>% filter ( Calving_nr < 3 ) %>% tally()
