# Pakete aktivieren ####
pacman::p_load(tidyverse, lubridate)
# rawdata <- read.csv('Data/covid19data.csv')
#Import ####
rawdata <- read_csv(
  'https://opendata.ecdc.europa.eu/covid19/casedistribution/csv') %>% 
  #Umrechnen Fälle/1 Million Einwohner
  #Formatieren Datum
  #Sortieren datum absteigend
  rename(Country=countriesAndTerritories, #war ein sch..-Name
         Continent=continentExp) %>% 
  mutate(Date=dmy(dateRep),
         `Cases/Million`=cases*10^6/popData2019,
         `Deaths/Million`=deaths*10^6/popData2019) %>% 
  arrange(Country,Date)
ggplot(rawdata, aes(x = Date,y = `Cases/Million`,
                    color=Continent))+
  geom_point()+
  scale_y_log10()
filter(rawdata,Continent=='Europe') %>% 
  ggplot(aes(x = Date,y = `Cases/Million`))+
  geom_point(alpha=.3)+
  scale_y_log10(labels=prettyNum)+
  geom_smooth()

rawdata <- rawdata %>% group_by(Country) %>% 
  mutate(cum_cases=cumsum(cases),
         cum_deaths=cumsum(deaths)) %>%   
  ungroup() %>% 
  mutate(`Cumulative cases/Million`=cum_cases*10^6/popData2019,
         `Cumulative deaths/Million`=cum_deaths*10^6/popData2019)
  

filter(rawdata,Continent=='Europe') %>% 
  ggplot(aes(x = Date,y = cum_cases))+
  geom_point(alpha=.3)+
  scale_y_log10(labels=prettyNum)+
  geom_smooth()

filter(rawdata,Continent=='Europe') %>% 
  ggplot(aes(x = Date,y = `Cumulative cases/Million`))+
  geom_point(alpha=.3, size=.3)+
  scale_y_log10(labels=prettyNum)+
  geom_smooth()

continent_population <- 
  rawdata %>% group_by(Continent,Date) %>% 
  summarize(Population=sum(popData2019, na.rm = T)) %>% 
  group_by(Continent) %>% 
  summarize(Population=max(Population))

rawdata_continent <- rawdata %>% group_by(Continent,Date) %>% 
  summarize(cases=sum(cases,na.rm = T),
            deaths=sum(deaths,na.rm = T)) %>% 
  group_by(Continent) %>% 
  mutate(cum_cases=cumsum(cases),
         cum_deaths=cumsum(deaths)) %>%
  ungroup() %>% 
  left_join(continent_population) %>% 
  mutate( `Cumulative cases/Million`=cum_cases*10^6/Population,
          `Cumulative deaths/Million`=cum_deaths*10^6/Population)
ggplot(rawdata_continent,aes(x = Date,y = `Cumulative cases/Million`,
                             color=Continent))+
  geom_point()+
  geom_line()+
  scale_y_log10(breaks=1*10^(1:10),labels=prettyNum)

ggplot(rawdata_continent,aes(x = cum_cases,y = cum_deaths,
                             color=Continent))+
  geom_line()+
  scale_y_log10(breaks=1*10^(1:10),labels=prettyNum)+
  scale_x_log10(breaks=1*10^(1:10),labels=prettyNum)
