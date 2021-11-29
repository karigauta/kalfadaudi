install.packages("tidyr")

load("kalfadaudi.Rdata")
library(dplyr)
library(tidyr)

# Filtera burt auda reiti og setja annad i stadinn en "NA"


data_ekki_heimanaut  <- data[!is.na(data$Sire_ID),]
data_heimanaut <- data[is.na(data$Sire_ID),]

data_heimanaut$Sire_ID <- "Heimanaut"

data_breytt <- rbind(data_heimanaut, data_ekki_heimanaut)

# Tidnitoflur
# Myndir I r eru heil fraedi/listgrein
# sennilega er einfaldast fyrir ykkur ad nota Excel i thad, tha thurfid thid ad
# skrifa ut gognin sem thid aetlid ad nota. T.d. svona


# vista tofluna
tafla_ut <- as.table(ftable(data[c("Calving_nr", "Fate")], row.vars = "Calving_nr", col.vars = "Fate")) # her er thad burdur nr. og afdrif

# thetta skrifar ut tofluna
write.csv2(x= tafla_ut, file="prufa_ut.csv")

# hladid thessu inn i excel og gerid myndina thar, annars er thad alveg heavy youtube vesen ad laera a ad gera thetta ur R :)

# 3 Ny tidnitafla
# thetta er thad sem er sma snuid og aestadan fyrir ad thetta tok lengri tima :)

# bua til lista sem eru fyrri tvikelfingurinn
f1 <- !duplicated(data[c("ID_dam", "Calving_nr", "Calving_date")])
# bua til lista sem er seinni tvikelfingurinn, thad er gert med ad skanna afturabak (fromLast = T)
f2 <- !duplicated(data[c("ID_dam", "Calving_nr", "Calving_date")], fromLast = T)
data_an_tvikelfinga <- data[f1 & f2,]


data_edit <- data_an_tvikelfinga %>% mutate( Afdrif_nytt = case_when(
  Fate ==  "1" ~ 1,
  Fate ==  "2" ~ 1,
  Fate ==  "3" ~ 1,
  Fate ==  "4" ~ 0,
  Fate ==  "5" ~ 0,
  Fate ==  "6" ~ 1,
  Fate ==  "7" ~ 0,
  Fate ==  "8" ~ 1,
  Fate ==  "9" ~ 0))
data_edit <- data_edit %>% filter ( Calving_nr == 1 | Calving_nr ==2 )

# skil ekki hvernig thad koma "NA" i thessa... Allir burdir eru med eitthvad skrad i Fate og Calving_nr
# a ad varpa gagnasettinu thannig ad hver kyr hafi eina linu...
# tharf eitthvad ad skoda thetta betur, thad eru 7700 gripir ca. med NA a fyrsta kalfi... Hugsanlega, tha eru
# thad gripir sem attu kalfa ADUR en ad gognin voru tekin ut!
data_pivot <-
  data_edit %>% pivot_wider(
    id_cols = ID_dam, names_from = Calving_nr,
    values_from = Afdrif_nytt,
    names_prefix = "Calving_nr_"
  )

sum(data_pivot$Calving_nr_2, na.rm = T) # telja hvad thad eru margir einstakir lifandi faeddir kalfar i Calving_nr_2
sum(data_pivot$Calving_nr_1, na.rm = T) # telja hvad thad eru margir einstakir lifandi faeddir kalfar i Calving_nr_1

# thetta virkar en fjoldi staka i talningunni er furdulegur, aetti ad vera sami og summa hernan ad ofan (14808+13435) - tharf ad skoda betur
table(data_pivot$Calving_nr_1, data_pivot$Calving_nr_2)

