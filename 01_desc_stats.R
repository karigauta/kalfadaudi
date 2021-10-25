library(dplyr)
# her er eg ad nota skjalid sem eg bjo til i fyrri postinum
# vistadi thad med "save(x = data, filename = "kalfadaudu.Rdata")
# thad sparar nokkur skref i greiningunni ad vera ekki ad gera thad uppa nytt
load("kalfadaudi.Rdata")


####################### 1. Fylgja kvigum eftir arum


data <- data %>% filter (  )
# sija ut thaer sem uppfylla skilyrdi um ad hafa att kalf 2018 og thad var 1. kalfur
# set einnig skilyrdi af thad seu oll gogn i afdrif og gang
  listi_kvigur_18 <-  data %>% filter(
    Calving_date < as.Date("01/01/2019", format = "%d/%m/%Y") &
      Calving_nr == 1 & Gangur != "NA" )

  # Her eru oll skilyrdin sem thid toldud upp, thad eru mun faerri gripir sem komast i gegnum thad
  # einnig, skodid hvernig eg set upp skilyrdin.
  # Thetta thydir: takid kvigur sem faeddu a arinu 2018 OG thad var fyrsti kalfur
  # af theim taka afdrif 4 EÐA(|) 7 EÐA 9
  # af theim taka Gang 3 EÐA 4
  # thetta leysir vangaveltu sem thid hofdud i postinum um svona siur. kommur eru til ad setja ny skilyrdi
  listi_kvigur_18_fleiri_siur <- data %>% filter(
    Calving_date < as.Date("01/01/2019", format = "%d/%m/%Y") &
      Calving_nr == 1 ,
        Fate == 4 | Fate == 7 | Fate == 9,
      Gangur == 3 | Gangur == 4)


# Herna vel eg bara thaer kvigur sem sluppu i gegnum filterana adan, til thess ad fa alla burdina a seinni arunum
# thetta %in% thydir ad taka bara thau gildi af ID_dam sem finnast i data_filtered$ID_dam
data_filtered <- data %>% filter ( ID_dam %in% listi_kvigur_18$ID_dam)

# her sjaid thid lysandi tolfraedi fyrir thennan hop
# ftable er snidugt til ad setja upp svona tidnitoflu
ftable(data_filtered[c("Calving_nr", "Fate")]) # her er thad burdur nr. og afdrif

# herna er gangur, en nota prop.table til ad skoda hlutfollinn innan nr. burdar (margin = 1 thydir ad radirnar ganga upp i 1)
prop.table(ftable(data_filtered[c("Calving_nr", "Gangur")]), margin = 1)

################### 2. Skipta burdum eftir seasoni

# her tek eg ut nyja breytu sem er bara numerid a manudinum
# svo by eg til breytu sem segir ad ef burdurinn er mai - sept => sumar
data <- data %>% mutate( Month_calving = format(Calving_date, format = "%m"),
                         Season_calving = case_when(
                          as.integer(Month_calving) %in% c(5,6,7,8,9) ~ "Summer",
                          as.integer(Month_calving) %in% c(1,2,3,4,10,11,12) ~ "Winter",
                         ))
# Herna skoda afdrif kalfa eftir hvort their eru faeddir ad vetri eda sumri
prop.table(ftable(data[c("Season_calving", "Fate")]), margin = 1)

# synist thetta ekki syna mikinn mun.
# profadi ad gera nyja breytu, sem tekur bara saman "lifandi", "daudur"
# fyrir ykkur skiptir liklega ekki miklu mali hvort hann var settur a, seldur eda raektadur til kjotframleidslu

data <- data %>% mutate ( Afdrif_nytt = case_when(
  Fate ==  "1" ~ "Lifandi",
  Fate ==  "2" ~ "Lifandi",
  Fate ==  "3" ~ "Lifandi",
  Fate ==  "4" ~ "Daudur",
  Fate ==  "5" ~ "Fosturlat",
  Fate ==  "6" ~ "Lifandi",
  Fate ==  "7" ~ "Daudur",
  Fate ==  "8" ~ "Lifandi",
  Fate ==  "9" ~ "Daudur"))

# sma munur thegar thad er skodad
prop.table(ftable(data[c("Season_calving", "Afdrif_nytt")]), margin = 1)
# mikill munur thegar gangur er skodadur
prop.table(ftable(data[c("Gangur", "Afdrif_nytt")]), margin = 1)


# profa ad skoda hvernig gognin lita oll ut med thessari nyju breytu
# Tharna syna kvigurnar hrodalegt utslag. 29% af kalfunum drepast innan solarhrings!

prop.table(ftable(data[c("Calving_nr", "Afdrif_nytt")]), margin = 1)
# profum ad skoda lika kyn kalfsins
# hefur mikil ahrif virdist vera, tarfarnir drepast meira allsstadar
prop.table(ftable(data[c("Calving_nr", "Sex", "Afdrif_nytt")]), margin = 1)


###################### 3. Fedur
# flokid ad skoda tha tharsem their eru margir, 1044
length(unique(data$Sire_ID))

# filter til thess ad bua til lista yfir fedurna sem vid viljum skoda, her tek eg tha sem
# eru med meira en 100 daetur og tha sem eru ekki med "missing faderni" (na)
summarised_data <- data %>% group_by( Sire_ID ) %>% filter (  ) %>% tally()

# gera lista yfir tarfa sem eiga fleiri en 100 daetur
bull_list <- subset(summarised_data$Sire_ID, subset = summarised_data$n > 100 & !is.na(summarised_data$Sire_ID))
# gera urtak ur gognunum sem innihalda bara thessa fedur og ekki missing i "gangur"
data_trimmed_bulls <- data %>% filter ( Sire_ID %in% bull_list & Gangur != "NA" )
# skoda, ekki 10000 radir en samt helviti margar. Tharf ad gera einhverja tolfraedi til ad profa
# veit ekki hvad er best, kannski chi square eda eitthvad thannig. Getid raett vid einhverja tolfraedi norda i skolanum
test1 <- as.data.frame.matrix(prop.table(ftable(x = data_trimmed_bulls[c("Sire_ID", "Gangur")]), margin = 1))

######################### 4. ID
# tolurnar birtast bara sem e^14 thvi thannig eru storar tolur syndar i R. Thaer eru "rettar" i gognunum
# getid filterad ut einstaka fedur.
# nokkud viss um ad: 999999999919999 er othekktur fadir en lika thegar reiturinn er tomur (NA)
# thad eru bara 15 kalfar med thennan fodur (nrow er til ad telja linurnar í þessum filter)
 nrow(data %>% filter ( Sire_ID == 999999999919999))
# hinsvegar eru 11544 kálfar með óþekktan föður
 nrow(data %>% filter ( is.na(Sire_ID)))
 #################### 5. margir filterar

 # til ad gera marga filtera tharf bara passa upp a hvernig their eru byggdir upp
 data_filtered_dead <- data %>% filter ( Gangur == 3, 4 & Fate == 4, 7, 9)
# thetta virkar ekki utaf thvi ad thegar thad kemur , byst R vid ad fa nytt skilyrdi

 # thessi virkar, | thydir EDA.
 # Velja, tha sem eru 3 eða 4 OG Fate = 4 eða 7 eða 9
 data_filtered_dead <- data %>% filter ( Gangur == 3 | Gangur == 4 & Fate == 4 | Fate == 7 | Fate == 9)

