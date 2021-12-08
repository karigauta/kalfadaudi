

load("kalfadaudi.Rdata")
library(dplyr)
library(tidyr)


# 04_fishers test - nota
data_edit <- data %>% mutate( Afdrif_nytt = case_when(
  Fate ==  "1" ~ 1,
  Fate ==  "2" ~ 1,
  Fate ==  "3" ~ 1,
  Fate ==  "4" ~ 0,
  Fate ==  "5" ~ 0,
  Fate ==  "6" ~ 1,
  Fate ==  "7" ~ 0,
  Fate ==  "8" ~ 1,
  Fate ==  "9" ~ 0))

prufu_tafla <- (ftable(data_edit[c("Calving_nr", "Afdrif_nytt")], exclude = "NA"))

# thetta drullar a sig - profa chi.square prof
# fishers virkar best fyrir litil gogn held eg - getid spurt tolfraedingin
fisher.test(x=prufu_tafla)

chisq.test(x=prufu_tafla)

# profa ad gera mosaik plot, en thad synir svona myndraent thetta Chi square prof ca.
# raudu litirnir thyda thar sem thad er minna af einhverju en buist var vid m.v. nullthesu
# blau litirnir eru ofugt, meira af einhverju
# thad sem kemur i ljos eru ad kvigur eru med fleiri dauda kalfa
# sja umfjollun her: http://alumni.media.mit.edu/~tpminka/courses/36-350.2001/lectures/day12/
mosaicplot(prufu_tafla, shade = T)

# 05 Logistic regression
# ATH: her er eg ad gera allskonar einfaldanir til thess ad syna bara hvernig thetta er kodad
# eg tek ut "NA" ur gang i fyrsta skrefi
# ATH2: Thid thurfid ad raeda thessar breytur og hvad er continous og hvad ekki vid tolfraedinginn. Herna er eg bara ad syna kodann
# thad er ekki liklegt ad thetta seu rettar breytur. Muna junk in -> junk out regluna i tolfraedi :)
data_edit <-data_edit %>% filter ( Gangur != "NA")
# geri Gang af "Factor" (i stadinn fyrir ad vera kodud sem tala)
data_edit$Gangur <- factor(data_edit$Gangur)
# herna keyri eg likanid, segi thvi med family = "binomial" ad response variable se 0/1 breyta (daudur/lifandi kalfur)
mylogit <- glm(Afdrif_nytt ~ Gangur, data = data_edit, family = "binomial")
# skoda nidurstofuna, sem eru adhvarfsstudlarnir
summary(mylogit)

# muna ad thessir adhvarfsstudlar eru log odds, svo ad minustala thydir ad likur a ad lifa i thessu einfalda likani fyrir grip sem er med gang 4
1/(1+exp(-(-3.64+2.412)))
# svo er haegt ad nota plot til ad fa greiningar. Eg tulka þetta sem svo ad likanid se mjög lelegt :)

plot(mylogit)

# profa ad gera lika fyrir Calving_nr
data_edit <- data %>% mutate( Afdrif_nytt = case_when(
  Fate ==  "1" ~ 1,
  Fate ==  "2" ~ 1,
  Fate ==  "3" ~ 1,
  Fate ==  "4" ~ 0,
  Fate ==  "5" ~ 0,
  Fate ==  "6" ~ 1,
  Fate ==  "7" ~ 0,
  Fate ==  "8" ~ 1,
  Fate ==  "9" ~ 0))

mylogit <- glm(Afdrif_nytt ~ Calving_nr, data = data_edit, family = "binomial")

summary(mylogit)
# likur a ad kalfur 1. kalfs kvigu lifi
1/(1+exp(-(0.272639+0.955476   ))) # er a nedstu myndinni sem blaa linan

# likur a ad kalfur 3. kalfs ku lifi
1/(1+exp(-(0.272639*3+0.955476   ))) # er a nedstu myndinni sem blaa linan

plot(mylogit)

# herna er plot sem synir hvernig likurnar aukast eftir thvi sem kyrnar eru eldri
# thetta er orugglega frekar lengi ad teiknast upp thar sem thad eru mjog margir punktar til ad teikna
# kodinn er hedan: https://daviddalpiaz.github.io/r4sl/logistic-regression.html
plot(Afdrif_nytt ~ Calving_nr, data = data_edit,
     col = "darkorange", pch = "|", ylim = c(-0.2, 1),
     main = "Using Logistic Regression for Classification")
abline(h = 0, lty = 3)
abline(h = 1, lty = 3)
abline(h = 0.5, lty = 2)
curve(predict(mylogit, data.frame(Calving_nr = x), type = "response"),
      add = TRUE, lwd = 3, col = "dodgerblue")
abline(v = -coef(mylogit)[1] / coef(mylogit)[2], lwd = 2)

