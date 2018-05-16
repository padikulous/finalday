library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)
library(GGally)
library(maptools)
library(ggmap)
library(maps)
dir.create("wrangling")
setwd('~/./wrangling')
prism <- read_csv('climate.csv')
pop <- read_csv('pop.csv')
ld <- read_csv('lyme.csv')
head(pop)

#select columns with fips heading, selects all values under heading
pop %<>% select(fips,starts_with("pop2"))

#transposes years that start with "2", creates columns for "str_year" and "size" under identifiers key and value respectively, special command na.omit removes "NA" data
pop %<>% gather(starts_with ("pop2"),key="str_year",value="size") %>% na.omit 

#takes all values under "str_year" column, finds those that start with "pop" and replaces with nothing
pop  %<>% mutate(year=str_replace_all(str_year,"pop",""))

#editing "str_year" before, while removing "pop", still maintains the <chr> identifier;this changes it into a readable interger
pop %<>% mutate(year=as.integer(year))

#takes all values under "fips" column, takes those that start with a 0, then removes the 0
pop  %<>% mutate(fips=str_replace_all(fips,"^0",""))
pop
#editing "fips" removes the 0, but column is still under <chr> identifier; this changes into integers
pop %<>% mutate(fips=as.integer(fips))
pop
ld <- read_csv('lyme.csv')

#takes all values that start with "cases" and reorganizes into column "str_year" under <chr> identifier
ld  %<>% gather(starts_with("Cases"),key="str_year",value="cases") 

#replaces all values uder "str_year" and removes "cases" under new "year" column
ld  %<>% mutate(year=str_replace_all(str_year,"Cases",""))

#turns year column into interger values
ld  %<>% mutate(year=as.integer(year))

#renames columns for "STNAME" and "CTYNAME"
ld  %<>% rename(state=STNAME,county=CTYNAME)


fips.builder<-function(st,ct){
  if (str_length(ct)==3){
    fips<-paste(as.character(st),as.character(ct),sep="")     %>% as.integer
    }
  else if (str_length(ct)==2){
    fips<-paste(as.character(st),"0",as.character(ct),sep="")  %>% as.integer
    }
  else {fips<-paste(as.character(st),"00",as.character(ct),sep="")  %>% as.integer
  }
  return(fips)
}

#applies fips.builder code to each row to values under "STCODE" and "CTYCODE" columns
ld %<>% rowwise() %>% mutate(fips=fips.builder(STCODE,CTYCODE))

#selects and removes columns for "STCODE", "CTYCODE", and "str_year"
ld   %<>% select(-c(STCODE,CTYCODE,str_year))

#combines transformed data for ld, prism and pop data sets
ld.prism <- inner_join(ld,prism)
prism
ld.prism.pop <- inner_join(ld.prism,pop)
ld.prism.pop


cases_by_year <- ld %>% ungroup %>% group_by(year) %>%
  summarize(total=sum(cases))  %>% arrange(desc(total))

cases_by_state <- ld %>% ungroup %>% group_by(state, county, year) %>% 
  summarize(total=mean(cases)) %>% arrange(desc(total))


cases_by_state

write_csv(ld.prism.pop, "ld_prism_pop.csv")
save(ld.prism.pop, file = "ld_prism_pop.RData")

library(ggplot2)
library(ggmap)
ld.prism.pop
county_map <- map_data("county")
state_map <- map_data("state")
## ANNOTATE FROM HERE
ag.fips <- group_by(ld.prism.pop,fips)
ld.16y<-summarize(ag.fips,all.cases=sum(cases))
ld.16y<-left_join(select(ld.prism.pop,c(state,county,fips)),ld.16y)
ld.16y<-distinct(ld.16y)
ld.16y %<>% rename(region=state,subregion=county)
ld.16y$subregion<-str_replace_all(ld.16y$subregion," County","")
ld.16y$region<-tolower(ld.16y$region)
ld.16y$subregion<-tolower(ld.16y$subregion)
ld.16y %<>% mutate(log10cases=log10(1+all.cases))
map.ld.16y<-left_join(county_map,ld.16y)
ggplot(map.ld.16y)+geom_point(aes(long,lat,color=log10cases),size=0.1) +
  scale_colour_gradientn(colours=rev(rainbow(4)))
