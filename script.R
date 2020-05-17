#hier geht es bald los
#Packages
library(readr)

#import kiwo data
kiwo <- read_csv("kiwo.csv")
View(kiwo)

#import umsatzdaten
umsatzdaten_gekuerzt <- read_csv("umsatzdaten_gekuerzt.csv")
View(umsatzdaten_gekuerzt)

#import schulferien
schulferien <- read_csv("Schulferien.csv")
View(schulferien)

#import Feiertage
feiertage <- read_csv("Feiertage.csv")
View(feiertage)

#import Wetter
wetter <- read_csv("wetter.csv")
View(wetter)