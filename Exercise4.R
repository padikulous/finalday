dir.create('modeling')
setwd('~/./modeling')
ld.prism.pop <- read_csv('ld_prism_pop.csv')
ggpairs(ld.prism.pop,columns=c("prcp","avtemp","size","cases"))
ld.prism.pop

#create log scale data for both size and cases the graph scatterplot through ggpairs
library(dplyr); ld.prism.pop %<>%
  mutate (log10size=log10(size)) %>%
  mutate (log10cases=log10(cases+1)) 
ld.prism.pop %>%
  ggpairs (columns=c("prcp","avtemp","log10size","log10cases"))

#set a fixed sample with set.seed function
set.seed(222) 
small <- ld.prism.pop %>% sample_n(100)

#plot precipitatin vs avg temperature within the defined"small" sample and fit linear model to it
library(ggplot2)
ggplot(data=small) +
  geom_point(aes(prcp,avtemp)) +
  geom_smooth(aes(prcp,avtemp), method = 'lm')

#give exact stats of linear model created
ianModel <- lm(avtemp ~ prcp, data=small)
summary(ianModel)

summary(ianModel)$coefficients[2,1] 

summary(ianModel)$coefficients[2,4]

#gives representaion of population by year
ld.prism.pop %>% group_by(year)  %>% summarize(total=sum(size)) %>%
  plot(.)+geom_point(aes(x=year,y=total))

#creates data frame grouping data by state
by_state <- ld.prism.pop %>% group_by(state) 
by_state

#nests the data according to state, creating a tribble for each
by_state %<>% nest
by_state

#displays the 10th state alphabetically, Georgia
by_state$data[10]

#creates a function that gives a linear model for population size by year
linGrowth_model <- function(df){
  lm(size ~ year, data = df)
}

models <- purrr::map(by_state$data, linGrowth_model)


by_state %<>% mutate(model = map(data, linGrowth_model))
by_state

library(modelr)
by_state  %<>%  mutate (resids= map2(data,  model, add_residuals))
by_state

sum_resids <- function(x){
  sum(abs(x$resid))
  }
by_state %<>% mutate(totalResid  = map(resids,sum_resids))

get_slope <- function(model){
  model$coefficients[2]
  }
by_state %<>% mutate(slope  =  purrr::map(model,  get_slope))
by_state

slopes <- unnest(by_state, slope)
totalResids <- unnest(by_state, totalResid)

slopes %>% ggplot(aes(state,slope))+geom_point()+
  theme(axis.text.x  =  element_text(angle =  90,  hjust =  1))


totalResids %>% ggplot(aes(state,totalResid))+geom_point()+
  theme(axis.text.x  =  element_text(angle =  90,  hjust =  1))

by_state2 <- ld.prism.pop %>% group_by(state) 
by_state2

by_state2 %<>% nest
by_state2

runCor <- function(df){
  suppressWarnings(cor.test(df$cases,df $prcp,method="spearman")$estimate)
  }
by_state2  %<>%  mutate(spCor  =  purrr::map(data,  runCor))

spCors <- unnest(by_state2,spCor) 
spCors  %<>%  arrange(desc(spCor))
spCors$state  <-  factor(spCors$state,  levels=unique(spCors$state))
ggplot(spCors, aes(state,spCor))+geom_point()+
  theme(axis.text.x  =  element_text(angle  =  90,  hjust  =  1))


