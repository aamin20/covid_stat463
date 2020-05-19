# Compute covid-19 cases per 100,000
# plot versus other variables

#0. Setup
install.packages('fuzzyjoin')
library(tidyverse)
library(fuzzyjoin)
source('hw.R')
source('hwXgrid.R')

# 1. Read the files 

# The file name have the download date.
# A column has report data the is
# normally the previous day

NY_Covid <- read_csv('../Data/New_York_State_Statewide_COVID-19_Testing.csv')
NY_Census <- read_csv('../Data/NY_Counties_2019.csv')
Census <- read_csv('../Data/US_County_Areas.csv')

view(NY_Covid)
view(NY_Census)

# remove blanks in the names
colnames(NY_Covid) <- c('Test_Date','County','New_Positive',
                        'Cumulative_Positive','New_Tests', 'Cumulative_Tests')
colnames(NY_Census) <- c('FIPS', 'St_name', 'County',	'Pop_est',
                         'N_pop_change',	'Births',	'Deaths',	'Natural_inc',	
                          'Internat_mig',	'Domestic_mig',	'Net_mig', 'Residual',	'GQ_estimate',	'R_birth',
                         'R_Death',	'R_natural_inc',	'R_internat_mig',	'R_domestic_mig',	'R_net_mig')

NY_joined <- inner_join(NY_Covid, NY_Census, by = 'County')
view(NY_joined)


NY_May_14 <- NY_joined[grep("5/14/2020",NY_joined$Test_Date),]
NY_April_14 <- NY_joined[grep("4/14/2020", NY_joined$Test_Date),]

view(NY_May_14)
view(NY_April_14)

 
# 3. Select the 2019 census population estimates
#    a two other variables for possible use 
#    in data exploration   
#
#    We  know that infection involves contact with the
#    virus.  
#
#    Still, population statistics may be secondarilly
#    related to human behavior that modifies contact
#    and death probabilities.  For example  
#    birth and deaths can be related to age distributions.
#    
#    In-migration may be related local population clusters
    

census_vars  <- select(VA_cty_stats,
            FIPS,Ctyname,PopEst,Births,Deaths)

# Remove the Virgina State total row 

census_vars <- filter(census_vars,FIPS>0) 

# Turn the 3 digit into the full 5 digit fips code

census_vars <- mutate(census_vars,FIPS=FIPS+51000)
View(census_vars)

# 4. Joining tibbles

#  This is the simplest join situation.
#  There is a 1 to 1 match between the 
#  sorted FIPS codes in the two tibbles.
#
#  We can use the inner_join function.
#  It will address the sorting. 

mar28_cen <- inner_join(mar28,census_vars,by='FIPS')
View(mar28_cen)

colnames(mar28_cen)

# We could remove some columns and reorganize
# the remainder.

# 5. Produce case count plots versus population

# 5.1 Plot cases versus population and a fitted line 

ggplot(mar28_cen,aes(x=PopEst/1000,y=Total_Cases))+
  geom_point(shape=21,fill='red',color='black',size=3)+
  geom_smooth(method = 'lm',color='blue',size=1.7) +
  labs(x='Population in Thousands',
       y='Number of COVID-19 Cases',
       title='Virginia Counties and Independent Cities',
       caption='March 28, 2019') + 
  hwXgrid

# 5.2 Repeat but with a loess smooth 

ggplot(mar28_cen,aes(x=PopEst/1000,y=Total_Cases))+
  geom_point(shape=21,fill='red',color='black',size=3)+
  geom_smooth(color='blue',size=1.7) +
  labs(x='Population in Thousands',
       y='Number of COVID-19 Cases',
       title='Virginia Counties and Independent Cities',
       caption='March 28, 2019') + 
  hwXgrid


# 6. Repeat but with Y as the percent of the population

plt <- ggplot(mar28_cen,aes(x=PopEst/1000,y=100*Total_Cases/PopEst))+
  geom_point(shape=21,fill='red',color='black',size=3) +
  labs(x='Population in Thousands',
       y='Percent COVID-19 Cases',
       title='Virginia Counties and Independent Cities',
       caption='March 28, 2019') + 
  hwXgrid
plt

# 6.1 Find high percent points  and add names

# Compute percents

check <- mutate(mar28_cen,
                percent=100*Total_Cases/PopEst)

# Find high percent rows
nams <- filter(check,percent >.024)
nams

# Add and nudge text

plt + geom_text(aes(x=PopEst/1000,y=percent,
                 label=Locality),data = nams,
                 nudge_y=.0035,
                 nudge_x=0)

# 7. Some Loose ends

#  Some census file edits could be done once saved to a file.
#  The file could save in an .RData file rather the a csv file.  
#  
#  Our perception of what is close for perceptual grouping
#  is based on distance. However the nudging distance
#  is function of plot size. Our nudging values  
#
#  The two word labels are inconsistent: PopEst and Total_Cases
#
#  Variable names could be revised for less typing
#
#  The case count for different day cold kept in one tibble
#  Graphics can show chance over time.  Consider use temporal change 
#  maps


      
