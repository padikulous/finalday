dir.create ("mers")
setwd('~/./mers')
mers <- read.csv('cases.csv')
head(mers)
mers$hospitalized[890] <- ('2015-02-20')
mers <- mers[-471,]

library(lubridate)
mers$onset2 <- ymd(mers$onset)
mers$hospitalized2 <- ymd(mers$hospitalized)
class(mers$onset2)
day0 <- min(na.omit(mers$onset2))
mers$epi.day <- as.numeric(mers$onset2 - day0)
library(ggplot2)
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day)) + 
  labs(x='Epidemic day' , y='Case count' , title='Global count of MERS cases by date of symptom onset' ,
       caption = "Data from : https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day , fill=country)) + 
  labs(x='Epidemic day' , y='Case count' , title='Global count of MERS cases by date of symptom onset' ,
       caption = "Data from : https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")


mers$infectious.period <- mers$hospitalized2-mers$onset2
class(mers$infectious.period)
mers$infectious.period <- as.numeric(mers$infectious.period, units="days")
ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period))+
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

mers$infectious.period2 <- ifelse(mers$infectious.period <0,0,mers$infectious.period)
ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period2))+
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#DENSITY PLOT
ggplot(data=mers) +
  geom_density(mapping=aes(x=infectious.period2))+
  labs(x='Infectious period', y='Frequency', title='Probability density for MERS infectious period (positive values only)',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#AREA PLOT
ggplot(data=mers) +
  geom_area(stat='bin', mapping=aes(x=infectious.period2))+
  labs(x='Infectious period', y='Frequency', title='Area plot for MERS infectious period (positive values only)',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#change in infectious period
ggplot(data=mers) +
  geom_point(aes(x=epi.day, y=infectious.period2, color=country))+
  labs(x='epi.day', y='Infectious period', title='Scatterplot for MERS infectious period (positive values only)',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#SMOOTH LINE
ggplot(data=mers) +
  geom_smooth(aes(x=epi.day, y=infectious.period2, color=country),method="loess")
  labs(x='Infectious period', y='Frequency', title='Area plot for MERS infectious period (positive values only)',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
  
ggplot(data=mers) +
  geom_smooth(aes(x=epi.day, y=infectious.period2),method='loess')
  labs(x='Infectious period', y='Frequency', title='Area plot for MERS infectious period (positive values only)',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#FACETING
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) + 
  geom_point(mapping=aes(color=country)) +
  facet_wrap(~ country) +
  scale_y_continuous(limits = c(0,50)) +
  labs(x='Epidemic Day', y='Infectious Period', title='MERS infectious period (positive values only) over time',
           caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=subset(mers, gender %in% c('M', 'F') & country %in% c('Jordan','Iran','KSA','Oman','Qatar','South Korea','UAE'))) +
  +        geom_point(mapping = aes(x=epi.day, y=infectious.period2, color=country)) +
  +          facet_grid(gender ~ country) +
  +          scale_y_continuous(limits = c(0,50)) +
  +          labs(x='Epidemic Day', y='Infectious Period', title='MERS infectious period by gender and country', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#Case fatality ratio
mers$fatality <- if(mers$outcome = fatality <- 1)
  ggplot(data=mers)+
  geom_histogram(aes(x=fatality)) +
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period')
  