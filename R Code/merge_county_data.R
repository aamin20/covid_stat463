# Compute covid-19 cases per 100,000
# plot versus other variables

#0. Setup
install.packages('fuzzyjoin')
library(tidyverse)
source('hw.R')
source('hwXgrid.R')

# 1. Read the NY Covid and NY State Census Files (Current version = 2019)
NY_Covid <- read_csv('../Data/New_York_State_Statewide_COVID-19_Testing.csv')
NY_Census <- read_csv('../Data/NY_Counties_2019.csv')

# 2. Read US Census data by county (For square area of each county)
Census <- read_csv('../Data/US_County_Areas.csv')

view(NY_Covid)
view(NY_Census)

# 3. Rename column names to set up for joins
# remove blanks in the names
colnames(NY_Covid) <- c('Test_Date','County','New_Positive',
                        'Cumulative_Positive','New_Tests', 'Cumulative_Tests')
colnames(NY_Census) <- c('FIPS', 'St_name', 'County',	'Pop_est',
                         'N_pop_change',	'Births',	'Deaths',	'Natural_inc',	
                          'Internat_mig',	'Domestic_mig',	'Net_mig', 'Residual',	'GQ_estimate',	'R_birth',
                         'R_Death',	'R_natural_inc',	'R_internat_mig',	'R_domestic_mig',	'R_net_mig')

# 4. Join Covid data with NY Census data and NY County area data 
NY_joined <- inner_join(NY_Covid, NY_Census, by = 'County')
#convert census FIPS to numeric for more matching 

Census$FIPS <- as.numeric(Census$FIPS)
NY_joined <- inner_join(NY_joined, Census, by = 'FIPS')
view(NY_joined)


NY_May_14 <- NY_joined[grep("5/14/2020",NY_joined$Test_Date),]
NY_April_14 <- NY_joined[grep("4/14/2020", NY_joined$Test_Date),]

view(NY_May_14)
view(NY_April_14)


attach(NY_April_14)
title_text <- 'NY County Populations (in thousands) and COVID-19 Positive Cases'
subtitle_text <- 'April 14, 2020'

ggplot(NY_April_14,aes(x = Pop_est/1000,y = Cumulative_Positive),label=County)+
  geom_point(shape=21,fill='red',color='black',size=3)+
  geom_text(aes(label=ifelse(Cumulative_Positive>10000,as.character(County),'')),hjust=1,vjust=-1) +
  geom_smooth(method = 'lm',color='blue',size=1.7) +
  labs(x='Population in Thousands',
       y='Number of COVID-19 Cases',
       title= title_text,
       subtitle=subtitle_text) + 
  hwXgrid

NY_April_14.lm <- lm(data = NY_April_14, Cumulative_Positive ~ I(Pop_est/1000))
Summary(NY_April_14.lm)

detach(NY_April_14)

attach(NY_May_14)
title_text <- 'NY County Populations (in thousands) and COVID-19 Positive Cases'
subtitle_text <- 'May 14, 2020'

ggplot(NY_May_14,aes(x = Pop_est/1000,y = Cumulative_Positive),label=County)+
  geom_point(shape=21,fill='green',color='black',size=3)+
  geom_text(aes(label=ifelse(Cumulative_Positive>10000,as.character(County),'')),hjust=1,vjust=-1) +
  geom_smooth(method = 'lm',color='blue',size=1.7) +
  labs(x='Population in Thousands',
       y='Number of COVID-19 Cases',
       title= title_text,
       subtitle=subtitle_text) + 
  hwXgrid

NY_May_14.lm <- lm(data = NY_May_14, Cumulative_Positive ~ I(Pop_est/1000))
summary(NY_May_14.lm)

detach(NY_May_14)

#NEXT SECTION
n_county <- 20
ngrp <- 1
grpSize <- 19 

attach(NY_May_14)
may <- select(NY_May_14, Test_Date, County, Cumulative_Positive, Pop_est)
colnames(may) <- c('May_date', 'County', 'Cumulative_Positive', 'Pop_est')
mayTop <- may %>% arrange(desc(Cumulative_Positive)) %>%
  slice(1:n_county) 

mayTop <- mayTop %>% mutate(mayrate = 100 * Cumulative_Positive/Pop_est)
detach(NY_May_14)

attach(NY_April_14)
april <- select(NY_April_14, Test_Date, County, Cumulative_Positive, Pop_est)
colnames(may) <- c('April_date', 'County', 'Cumulative_Positive', 'Pop_est')

aprilTop <- april %>% arrange(desc(Cumulative_Positive)) %>%
  slice(1:n_county) 

aprilTop <- aprilTop %>% mutate(aprilrate = 100 * Cumulative_Positive/Pop_est)
detach(NY_April_14)

may_april <- inner_join(aprilTop, mayTop, by = 'County')
view(may_april)

may_april_top <-
  may_april %>%
  arrange(desc(may_april$mayrate)) %>%
  slice(1:n_county) %>%
  select(County,aprilrate,mayrate)


grp <- paste0('G',1:ngrp)
grp <- factor(rep(rep(grp,each=grpSize),1),levels=grp)
grpCol= paste0("C",1:grpSize)
grpCol= factor(rep(rep(grpCol,ngrp),1),levels=grpCol)
may_april_top <- may_april_top %>%
  mutate(grp= grp,
         grpCol= grpCol)

datLab <- 'May 15'
txt <- paste("New York's Highest",n_county,"Case Rate Counties on",
             datLab)
subtext <-'April 14 to May 14,2020'

attach(may_april_top)
plt <- ggplot(may_april_top,
              aes(x=aprilrate,
                  xend=mayrate,
                  y=County,
                  yend=County,
                  group=grp,
                  color=grpCol)) +
  geom_segment(
    arrow =arrow(length=unit(.2,'cm'),
                 ends='last',type='open'),size=2.5) +
  geom_segment(size=.6,color='white')+
  hw + theme(legend.position = 'none',
             strip.text.y=element_blank())+
  labs(x='Count Totals Per 100,000',
       y='',
       title=txt,
       subtitle=subtext,
       caption='Data from NYC HD')
plt


