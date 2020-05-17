#hier geht es bald los
#Packages
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

#import umsatzdaten
umsatzdaten_gekuerzt <- read_csv("umsatzdaten_gekuerzt.csv")
#View(umsa$wochentag <- weekdays(umsatzdaten$Datum)
umsatzdaten_gekuerzt$wochentag <- weekdays(umsatzdaten_gekuerzt$Datum)
# Umwandlung von 'wochentag" in eine Faktor-Variable mit einer vorgegeben Sortierung der Level (Kategorien)
umsatzdaten_gekuerzt$wochentag <- factor(umsatzdaten_gekuerzt$wochentag, levels=c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"))

#import kiwo data
kiwo <- read_csv("kiwo.csv")
#View(kiwo)

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
