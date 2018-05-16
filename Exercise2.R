dir.create("wnv")
setwd('~/./wnv')
wnv <- read.csv('wnv.csv')

#Case Counts of WNV
ggplot(data=wnv) +
  geom_histogram(aes(x=Total, fill=State)) +
  labs(x='State', y='Frequency', title='WNV annual case frequncy in each U.S. state 1999-2007')

wnv$Year <- as.factor(wnv$Year)
ggplot(data=wnv) +
  geom_histogram(aes(x=State, y=Total, fill=Year), stat='identity') +
  labs(x='State', y='Frequency', title='WNV annual case count in each U.S. state 1999-2007')

#log frequency plots
wnv$Year <- as.factor(wnv$Year)
ggplot(data=wnv) +
  geom_histogram(aes(x=State, y=Total, fill=Year), stat='identity') +
  labs(x='Total', y='Case Count', title='Log 10 WNV annual cases in each U.S. state 1999-2007') +
  scale_x_log10()

wnv$Year <- as.factor(wnv$Year)
ggplot(data=wnv) +
  geom_histogram(aes(x=State, y=log10(Total), fill=Year), stat='identity') +
  labs(x='Total', y='Frequency', title='Log 10 WNV annual cases in each U.S. state 1999-2007') 

#yearly faceted frequency plots
wnv$Year <- as.factor(wnv$Year)
ggplot(data=wnv, mapping=aes(x=Total)) +
  geom_histogram(mapping = aes(fill=State)) +
  facet_wrap(~ Year) +
  scale_y_continuous(limits = c(0,50)) +
  labs (x='Year', y='Total', title='WNV frequency in the USA 1999-2007')

#Case fatality ratio
wnv$cfr <- wnv$Fatal/wnv$Total 
ggplot(data=wnv) +
  geom_histogram(aes(x=State, y=cfr, fill=Year), stat='identity') +
  labs(x='State', y='Case Fatality Ratio', title='WNV case fatality per state per year')

wnv$cfr <- wnv$Fatal/wnv$Total 
ggplot(data=wnv, mapping=aes(x=cfr)) +
  geom_histogram(mapping= aes(x=cfr, fill=State)) +
  facet_wrap(~ Year) +
  scale_y_continuous(limits = c(0,25)) +
  labs(x='Case Fatality', y='Frequency', title='WNV case count per state per year')

#Mean and standard error
mean <- function(x){
  s <- sum(x)
  n <- length(x)
  m <- s/n
  return(m)
}

neuro <- c(wnv$EncephMen)
mean(neuro)

standard.error <- function(x) {
  s <- sd(x)
  n <- sqrt(length(x))
  se <- s/n
  return(se)
}

standard.error(neuro)

ColoradoEnceph <- (data=subset(wnv$EncephMen, wnv$State=='Colorado'))
mean(ColoradoEnceph)
standard.error(ColoradoEnceph)

CaliforniaEnceph <- (data=subset(wnv$EncephMen, wnv$State=='California'))
mean(CaliforniaEnceph)
standard.error(CaliforniaEnceph)

NewYorkEnceph <- (data=subset(wnv$EncephMen, wnv$State=='New York'))
mean(NewYorkEnceph)
standard.error(NewYorkEnceph)



