library(tidyverse)
library(ggplot2)
library(rvest)


#leser datasettet

bil <- read_html("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132")

filtrert <- html_table(html_nodes(bil, "table")[[1]], header = TRUE)

view(filtrert)

#filtrerer navnene 

#df %>%
  #mutate(Imp_Office = str_remove_all(Imp_Office, " Country| Office"))

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
  scale_x_continuous(breaks = seq(200, 700, 100), limits=c(250, 650)) + 
  scale_y_continuous(breaks = seq(200, 700, 100), limits=c(250, 650)) +
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







































                      















