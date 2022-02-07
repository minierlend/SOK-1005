#Mappeoppgave 2


#Mappeoppgave 2


install.packages("jsonlite")
install.packages("scales") 
install.packages("cluster")  
install.packages("ggrepel")

library("jsonlite")
library(ggplot2)                                 # Install and load scales, prøvde å laste ned scale prosent pakke
library("scales")
library(tidyverse)
library(cluster)                              #prøver denne for å lage mindre skrift
library(ggrepel)

Covid_death <- 
  fromJSON(readLines('https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json')[1])

##oppgave 1


##gjør om til prosent ved bruk av mutate formelen, trenger ikke gjøre dette siden vi bruker scale_x_continuos
Covid_death <- Covid_death %>% 
  mutate(fully_vaccinated_pct_of_pop_percentage=fully_vaccinated_pct_of_pop*100)



##forkorter navnene 
Covid_death <- Covid_death %>% 
  mutate(forkorta_navn = abbreviate(Covid_death$name, minlength=2))



##ggplot for å prøve å gjenskape bildet vi fikk utdelt.

Covid_death %>% 
  ggplot(aes(x= fully_vaccinated_pct_of_pop, y=deaths_per_100k, label =forkorta_navn, color=forkorta_navn)) +
  geom_text(hjust=0, vjust=0)+
  geom_point() +
  theme(text = element_text(size=10)) +
  labs(title="Korona dødsfall per 100k innbygggere iforhold til vaksjonasjonsgrad")+
  xlab("vaksinerte i populasjonen i %") +
  ylab("Dødsfall per 100k") +
  scale_x_continuous(labels = scales::percent, limits = c(0.45, 0.8), 
                     breaks = c(0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80))+
  theme_bw() +
  theme_grey() +
  theme(legend.position = "none") 
        



###-----


#oppgave 2)

Covid_death %>% 
  ggplot(aes(x= fully_vaccinated_pct_of_pop, y=deaths_per_100k, label = name, color = name)) +
  geom_point() +
  geom_text(hjust=0, vjust=0)+
  scale_x_continuous(labels = scales::percent, limits = c(0.45, 0.8), 
                     breaks = c(0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80)) +
  geom_smooth(method = lm, col = "blue", alpha = 0.2) + 
  theme_bw() +
  theme(legend.position = "none") +
  labs(title="Korona dødsfall per 100k innbygggere iforhold til vaksjonasjonsgrad")+
  xlab("vaksinerte i populasjonen i prosent") +
  ylab("Dødsfall per 100k") 



lm(deaths_per_100k ~ fully_vaccinated_pct_of_pop, data=Covid_death) 


## som vi ser på linja er at det enm korrelasjon mellom vaksineringsgrad og færre dødsfall.
#Jo mindre vaksinerte i populasjonen, jo større sannsynlighet er det og dø av COVID-19 viruset. 
# vi tolker tallene slik:
#det er en negativ linære korrelasjon i dette tilfellet, men har høy spredning. 
#hvis man skal tro på denne modellen, som vi gjør. vil det si at hvis 45% prosent av populasjonen er vaksinerte,
#vil nesten 15 personer dø per 100k. Intercept er bare tallet der dødsfall per 100k treffer y aksen. så det vil si:
#hvis 0 prosent av populasjonen er vaksinert, vil 31 personer død hvis vi følger vår regressjonsmodell. 



  
##----

