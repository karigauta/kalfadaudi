# 1. Taka ut tvikelfinga
library(dplyr)


load("kalfadaudi.Rdata")
# Held thetta eigi ad duga
# tek ut alla sem eru med somu modur, med sama kalving nr og somu faedingardagsetningu
data1 <- data[!duplicated(data[c("ID_dam", "Calving_nr", "Calving_date")]),]

# 2. fylgja eftir kvigu til annars burdar og skoda hvort thaer eru med meiri tidni en medaltalid
# hja 2. kalfs kvigum

# Skoda dreifingu hja 2. kalfs kvigum
data_filtered <- data %>% filter (Calving_nr == 2)
prop.table(ftable(data_filtered[c("Calving_nr", "Gangur")]), margin = 1)

# filtera ut kvigur sem attu dauda kalfa og med mikla burdarerfidleika
data_kvigur_med_burdarerfidleika <-
  data %>% filter (Calving_nr == 1 ,
                   Fate       == 4 |
                   Fate       == 7 |
                   Fate       == 9,
                   Gangur     == 3 |
                   Gangur     == 4   )
# skoda thaer kvigur sem 2. kalfs kvigur
data_kvigur_med_burdarerfidleika <- data %>% filter ( ID_dam %in% data_kvigur_med_burdarerfidleika$ID_dam, Calving_nr ==2)

# skoda dreifinguna
prop.table(ftable(data_kvigur_med_burdarerfidleika[c("Calving_nr", "Gangur")]), margin = 1)
# a thvi ad skoda bara hlutfollinn tha er talsvert fleiri i gang 3 og 4 en hja heildinni
# thad tharf ad gera einhver prof a thessu. Getid spurt hvort ad Chi kvadratprof myndi virka a svona gogn

# 3. Standard deviation

# man ekki hvad thid vildud taka stadalfravik af. Svo eg geri bara af einhverju
# thetta sidasta er mikilvaegt, thad segir fallinu ad sleppa "missing" values
sd(data$Days_insem_calving, na.rm=T)

# lika haegt ad gera svona:
# notid group_by til ad segja nidur a hvada breytur thid viljid flokka, svo thetta
# summarise til ad segja thvi a beita fallinu sd thvert a thessa flokka
stadalfravik_eftir_burdi <- data %>% group_by(Calving_nr) %>% summarise (stad.frv = sd(Nr_last_insem, na.rm = T))

# lika haegt ad gera a marga flokka i einu, en faid tha margar radir
stadalfravik_eftir_burdi_og_gang <- data %>% group_by(Calving_nr, Gangur) %>% summarise (stad.frv = sd(Nr_last_insem, na.rm = T))
