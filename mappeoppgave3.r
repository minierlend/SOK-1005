library(tidyverse)
library(ggplot2)
library(rvest)


#leser datasettet
## Denne oppgaven er jobbet i sammarbeid med Jørgen Johansen og Morten Ivarrud.  




bil <- read_html("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132")

filtrert <- html_table(html_nodes(bil, "table")[[1]], header = TRUE)

view(filtrert)

#filtrerer navnene 



filtrert <- filtrert %>% 
  rename(WLTP = `WLTP-tall`) 


filtrert <- filtrert %>% 
  mutate(STOPP = str_remove_all(STOPP, "km"),
         Avvik = str_remove_all(Avvik, "%"),
         WLTP = substr(WLTP, 1, 3)) %>%
  rename(stopp = STOPP,
         model = `Modell (temp. varierte fra 0?? til -10??)`,
         avvik = Avvik) %>% 
  subset(!stopp=="x")

### dat$x = as.numeric(as.character(dat$x))

filtrert$WLTP <- as.numeric(filtrert$WLTP)

filtrert$stopp <- as.numeric(filtrert$stopp)


##lager plotten til oppgave 1



plot <- filtrert %>% 
  ggplot(aes(x= WLTP, y=stopp)) +
  geom_point(size=1.7, alpha= 0.9, color="dark red") +
  scale_x_continuous(breaks = seq(250, 650, 100), limits=c(250, 650)) + 
  scale_y_continuous(breaks = seq(250, 650, 100), limits=c(250, 650)) +
  theme_bw() +
  geom_abline(intercept = 0, slope= 1, size = 0.5, color="dark blue") +
  labs(title= "temp. varierte fra 0?? til -10??") +
  xlab("Faktiske rekkevidde i km") +
  ylab("Leverand??rens indikasjon")

plot

#oppgave 2

plot + geom_smooth(method=lm, se = TRUE,
                   size = 0.3,
                   alpha = 0.4,
                   aes(color="black")) + scale_color_identity(name = "Regresjonslinje",
                                                                                    breaks = c("black"),
                                                                                    labels = c("i fargen svart"),
                                                                                    guide = "legend")

lm(stopp ~ WLTP, data = filtrert)




## Etter og ha sett regresjonslinja ser vi at kjørelengde ligger litt under hva bil produsentene indikerer at den vil kjøre.
## det forbruker ofte opplever er at WLTP ikke tar hensyn til dekkdimensjon og utstyrsmodifisering som forbruker ofte har.
##det vi ser i regresjonslinja er at bilene ligger  ganske likt på linja, litt under den antatte kjørelengden. 
##det vi ser er at stigningstallet er 0.86 og dette betyr at kjøring i kaldt vær har en 13.3% gjennomsnittlig minsket kjørelengde
##iforhold til WLTP tallene. 




##kilder: 
# https://stackoverflow.com/questions/49985671/how-to-remove-specific-words-in-a-column
# https://www.r-bloggers.com/2018/07/creating-legends-when-aesthetics-are-constants-in-ggplot2/
# https://statisticsglobe.com/convert-character-to-numeric-in-r/
#https://stackoverflow.com/questions/14717217/converting-from-a-character-to-a-numeric-data-frame



































                      















