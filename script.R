
#Packages
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

#import umsatzdaten
umsatzdaten_gekuerzt <- read_csv("umsatzdaten_gekuerzt.csv")
View(umsa$wochentag <- weekdays(umsatzdaten$Datum)
umsatzdaten_gekuerzt$wochentag_NF <- weekdays(umsatzdaten_gekuerzt$Datum)
# Umwandlung von 'wochentag" in eine Faktor-Variable mit einer vorgegeben Sortierung der Level (Kategorien)
umsatzdaten_gekuerzt$wochentag <- factor(umsatzdaten_gekuerzt$wochentag, levels=c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"))
wochentag <- umsatzdaten_gekuerzt$wochentag_NF
#import kiwo data
kiwo <- read_csv("kiwo.csv")
View(kiwo)

#import schulferien
schulferien <- read_csv("Schulferien.csv")
#View(schulferien)

#import Feiertage
feiertage <- read_csv("Feiertage.csv")
#View(feiertage)

#import Wetter
wetter <- read_csv("wetter.csv")
#View(wetter)

#zusammenführen der Daten
sammlung <- dplyr::full_join(umsatzdaten_gekuerzt,wetter,by="Datum")
sammlung <- dplyr::full_join(sammlung,kiwo,by="Datum")
sammlung <- dplyr::full_join(sammlung,schulferien,by="Datum")
sammlung <- dplyr::full_join(sammlung,feiertage,by="Datum")

####TO DO: Integration Wochentag####

view (sammlung)

#Lineare Regressionen zur Vorhersage des Umsatzes***

#Vorhersage durch Wetterdaten#
mod <- lm (Umsatz ~ Temperatur + Windgeschwindigkeit + as.factor(Wettercode) + as.factor (Bewoelkung),sammlung) 
summary(mod)
#Kein signifikanter Einfluss der Windgeschwindigkeit auf Umsatz (eventuell u-förmigen Effekt prüfen)
#Signifikant positiver Zusammenhang zwischen Temperatur und Umsatz: Bei höheren Temperaturen ist der Umsatz höher# 
#Signifikant geringerer Umsatz bei Wettercode 71 -->  Bei durchgehendem leichten Schneefall weniger gekauft#
#Bewölkung Code 1 sagt Umsatz signifikant vorher: höherer Umsatz bei wolkenlosem Himmel, evtl Bildung dichotomer Variable sonnig vs. bewölkt###

#Vohersage durch Warengruppe#
mod <-lm (Umsatz ~ as.factor (Warengruppe),sammlung) 
summary (mod)
#Einzelne Warengruppen in signifikantem Zusammenhang mit Umsatz (logisch, Aufnahme nur bedingt sinnvoll)
#Warengruppe 2,3 und 5 sagen hohen Umsatz vorher, Warengruppe 4 und 6 (konditorei und Saisonbrot) korrelieren signifikant negativ mit Umsatz

#Vohersage durch Ferien###
mod <- lm (Umsatz ~ Ferien, sammlung)
summary (mod)
###Signifikant höherer Umsatz in Sommerferien###

#Vorhersage durch Feiertag#
mod <- lm (Umsatz ~ Feiertag, sammlung)
summary (mod)
###kein signifikanter Einfluss der Feiertage, 
##eventuell Prüfung in Abhängigkeit von Ferien und langem Wochenende###

mod <-lm (Umsatz ~ KielerWoche, sammlung)
summary (mod)
#Kieler Woche wirkt sich signifikant positiv auf Umsatz aus, starker Effekt**

mod <-lm (Umsatz ~ as.factor(wochentag), umsatzdaten_gekuerzt)
summary(mod)
#Signifikant erhöhter Umsatz am Wochenende###

###Fazit: bedeutsame Haupteffekte###:

##positiv:

##Kieler Woche##
##Wochenende##
##Sommerferien##
##Sonniges Wetter##
#Verkauf Brot, Brötchen und Croissants##
##Hohe Temperaturen##

##negativ
##leichter Schneefall
##Verkauf von Saisonbrot und Konditoreiware (evtl. geringere Marge)

##TO DO: Prüfung von Interaktionen, z.B. Windgeschwindigkeit in Abhängigkeit von Temperatur

  ##Erstellung eines Testdatensatzes##

sammlung_test <- dplyr::full_join(umsatzdaten_gekuerzt,wetter,by="Datum")
sammlung_test <- dplyr::full_join(sammlung,kiwo,by="Datum")
sammlung_test <- dplyr::full_join(sammlung,schulferien,by="Datum")
sammlung_test <- dplyr::full_join(sammlung,feiertage,by="Datum")

# Estimating (Training) Models
#Nur zufällige Kombination von Faktoren
#To Do: Systematisierung der Faktoren und Integration von Wochentages, Feiertagen und Ferien###

mod1 <- lm(Umsatz ~ Temperatur, sammlung)
mod2 <- lm(Umsatz ~ Temperatur + Windgeschwindigkeit, sammlung)
mod3 <- lm(Umsatz ~ Temperatur + Windgeschwindigkeit + as.factor(Bewoelkung), sammlung)
mod4 <- lm(Umsatz ~ Temperatur + Windgeschwindigkeit + as.factor(Bewoelkung) + as.factor(Wettercode), sammlung)
mod5 <- lm(Umsatz ~ Temperatur + Windgeschwindigkeit + as.factor(Bewoelkung) + as.factor(Wettercode) + as.factor(Warengruppe), sammlung)
mod6 <- lm(Umsatz ~ Temperatur + Windgeschwindigkeit + as.factor(Bewoelkung) + as.factor(Wettercode) + as.factor(Warengruppe) + KielerWoche, sammlung)
mod7 <- lm(Umsatz ~ Temperatur + Windgeschwindigkeit + Windgeschwindigkeit*Temperatur + as.factor(Bewoelkung) + as.factor(Wettercode) + as.factor(Warengruppe) + KielerWoche , sammlung)


```{r}
summary(mod1)
summary (mod2)
summary(mod3)
summary(mod3)
summary(mod4)
summary(mod5)
summary(mod6)
summary(mod7)

#Installation notwendiger Packages##

library(dplyr)
library(readr)
library(lubridate)
library(broom)
library(Metrics)

glance(mod1)
glance (mod2)
glance (mod3)
glance (mod4)
glance (mod5)
glance (mod6)
glance (mod7)
```
```{r}
# Preparation of Model Results
rbind(glance(mod1), glance(mod2), glance(mod3), glance(mod4), glance(mod5), glance(mod6), glance(mod7))
```
#Vorhersageverbesserung vor allem durch Aufnahme von Warengruppe und Kieler Woche,
#Aufnahme von Warengruppe jedoch nur bedingt sinnvoll#
#Kieler Woche als sehr starker Umsatztreiber###
##To Do: Unbedingt Wochentag und Ferien intergieren, da sign. Korrelation mit Umsatz in einfacher Regression#

```{r}
#Nur Test: To Do: Prüfung der Fehlermeldung###
# Model Prediction Quality for the Training Data Using the Mean Absolute Error
rbind(mae(sammlung$Umsatz, predict(mod1))
      mae(sammlung$Umsatz, predict(mod2)),
      mae(sammlung$Umsatz, predict(mod3)),
      mae(sammlung$Umsatz, predict(mod4)),
      mae(sammlung$Umsatz, predict(mod5)),
      mae(sammlung$Umsatz, predict(mod6)),
      mae(sammlung$Umsatz, predict(mod7)))
```


#Nur Test: To Do: Prüfung der Fehlermeldung###
# Model Prediction Quality for the Training Data Using the Mean Absolute Percentage Error
rbind(mapre(sammlung$Umsatz, predict(mod1))
       mapre(sammlung$Umsatz, predict(mod2)),
       mape(sammlung$Umsatz, predict(mod3)),
       mape(sammlung$Umsatz, predict(mod4)),
       mape(sammlung$Umsatz, predict(mod5)),
       mape(sammlung$Umsatz, predict(mod6)),
       mape(sammlung$Umsatz, predict(mod7)))
```


#Nur Test: To Do: Prüfung der Fehlermeldung###
# Model Prediction Quality for the (Unknown) Test Data Using the Mean Absolute Percentage Error
rbind(mape(sammlung_test$Umsatz, predict(mod1, newdata=sammlung_test)),
      mape(sammlung_test$Umsatz, predict(mod2, newdata=sammlung_test)),
      mape(sammlung_test$Umsatz, predict(mod3, newdata=sammlung_test)),
      mape(sammlung_test$Umsatz, predict(mod4, newdata=sammlung_test)),
      mape(sammlung_test$Umsatz, predict(mod5, newdata=sammlung_test)),
      mape(sammlung_test$Umsatz, predict(mod6, newdata=sammlung_test)))
      

```




