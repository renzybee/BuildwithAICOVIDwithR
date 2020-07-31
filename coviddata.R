#source: https://datahub.io/core/covid-19#python
library(ggtext)
#install.packages("ggthemes")
library(ggthemes)
library(ggplot)
library(tidyverse)
library(lubridate)

my_theme <- theme(
  text = element_text(color = "grey35"),
  plot.title = element_text(size = 20, face = "bold"),
  axis.title = element_text(size = 15),
  axis.text = element_text(size = 8),
  axis.line = element_line(size = 1.2, color = "grey35"),
  legend.title = element_blank())

#extracting COVID CASES DIRECTLY FROM WEBSITE
date.end.month <- seq(as.Date("2020-01-01"),length=12,by="months")-1
date.end.month <- c(date.end.month, Sys.Date()-3)
del <- c("Diamond Princess","Puerto Rico", "American Samoa", "Grand Princess", "Guam", "Northern Mariana Islands", "Virgin Islands", "United_States")

us_case <- read.csv("https://datahub.io/core/covid-19/r/us_confirmed.csv") %>% 
  select("Date", "Case", "Province.State")
us_deaths <- read.csv("https://datahub.io/core/covid-19/r/us_deaths.csv")%>% 
  select("Date", "Case", "Province.State")

#GETTING END OF THE MONTH DATA FOR CASES
us_case2 <-us_case %>% 
  filter(!Province.State %in% del) %>%
  mutate(Date = as.Date(Date,format = '%m/%d/%Y')) %>%
  filter(Date %in% date.end.month) %>%
  group_by(Province.State, Date) %>%
  summarise(Case = sum(Case)) %>%
  mutate(Month = month(Date)) %>%
  select(Month, Province.State, Case)

#GETTING MONTHLY DATA FOR DEATHS
us_deaths2 <-us_deaths %>% 
  mutate(Month = month(Date)) %>%
  group_by(Province.State, Month) %>%
  summarise(Death = sum(Case)) 

covid<- us_deaths2 %>% full_join(us_case2, c("Month","Province.State")) %>%
  mutate(Month = as.numeric(Month))

#write.csv(covid,'perstatemonthlycasedeath.csv')

#source: https://oui.doleta.gov/unemploy/claimssum.asp
#ASHWAMEGHA GAVE A SCRAPED SHEET FOR THIS
#CONVERTED THE SHEET TO NUMERIC BECAUSE IT GIVES ERROR UPON WRANGLING

claims <- read.csv("C:/Users/Renzy/Documents/Monthly_States_data.csv") %>% 
  rename("Province.State" = "STATE")%>%
  select("Province.State", "Initial", "First", "Benefits", "Month")
claims <- claims[-which(claims$Province.State==""),] 
claims$Month[claims$Month=="January"] <- 1
claims$Month[claims$Month=="february"] <- 2
claims$Month[claims$Month=="March"] <- 3
claims$Month[claims$Month=="April"] <- 4
claims$Month[claims$Month=="May"] <- 5
claims$Month <- as.numeric(claims$Month)
claims$Year <- 2020

ic20 <- claims %>%
  select(Month, Year, Province.State, Initial)

#EXTRACTED MANUALLY
ic19<- read.csv("C:/Users/Renzy/Documents/claims2019.csv") 

initialclaims <- rbind(ic19,ic20)

#DATA FOR UNEMPLOYMENT PER SECTOR PER STATE
#DATA EXTRACTED BY VAISHALI FROM https://www.bls.gov/webapps/legacy/cesbtab6.htm
state <- read.csv("C:/Users/Renzy/Documents/state_industry.csv") %>% 
  select(-X)

statereshape<- gather(state, "Date", "Val",3:134)
statereshape <- spread(statereshape, Key_word, Val) 
state2 <- statereshape %>% 
  separate(Date, c("Month", "Year")) %>% 
  mutate(Year = as.numeric(Year)+2000) %>%
  mutate(Month = match(Month,month.abb)) %>%
  arrange(State, Year, Month) %>%
  rename("Province.State" = "State")%>%
  filter(Year != 2001:2006) %>% 
  filter(!Province.State %in% del)

unemployment <- state2 %>%
  select(Month, Year, Province.State, unemployment, `unemployment rate`) %>%
  filter(Year == 2019 & Month %in% 7:12     |
           Year == 2020) %>%
  left_join(initialclaims) %>%
  mutate(Month = month.abb[Month]) %>%
  mutate(Date = (paste(Month,Year,sep = "-"))) %>%
  mutate(Date = (paste("01",Date,sep="-"))) %>%
  mutate(Date = as.Date(Date, "%d-%b-%Y"))


#PANEL VIEW OF UNEMPLOYMENT VS INITIAL CLAIMS
unemployment %>%
  ggplot(aes(Date))+
  geom_line(aes(y = Initial ), color = "darkred") + 
  geom_line(aes(y = unemployment/1), color="steelblue", linetype="twodash")+
  scale_y_continuous(name = "Claims", sec.axis = sec_axis(~.*1, name="UE")) + 
  facet_wrap(~Province.State)

#FILTERING FOR FOCUSED ANALYSIS
drops <- c("employment","employment-population ratio", "labor force", "labor force participation rate", "Total nonfarm", "Total private","unemployment rate")
industry <- state2 %>%
  mutate(Month = month.abb[Month]) %>%
  mutate(Date = (paste(Month,Year,sep = "-"))) %>%
  mutate(Date = (paste("01",Date,sep="-"))) %>%
  mutate(Date = as.Date(Date, "%d-%b-%Y")) %>% 
  select(-one_of(drops)) %>%
  select(Date, everything()) 


industry$Year = integer(industry$Year)
industry$Month = factor(industry$Month, levels = month.abb)
industry %>%
  filter(Year %in% 2019:2020) %>%
  filter(Month %in% c("Jan","Feb", "Mar","Apr","May","Jun")) %>%
  select(Province.State, Month, Year, unemployment) %>%
  ggplot(aes(reorder(Province.State, -unemployment), unemployment/1000000, fill = factor(Year)))+
  geom_bar(stat = "identity", position = "dodge")+
  #scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values = c("seagreen3","midnightblue"))+
  labs(x = NULL, y ="", title = "Unemployment per State 2019 vs 2020", subtitle = "In millions")+
  theme_test() + 
  my_theme +
  coord_flip()+
  facet_grid(~Month)

#GETTING THE AMOUNT PAID
insurance <- claims %>%
  filter(!Province.State %in% del) %>%
  filter(Year == 2020) %>%
  select(Month, Province.State, Benefits)

#GETTING  STATES WHO RECD MOST
top<- insurance %>% 
  group_by(Province.State) %>%
  summarise(Total = sum(Benefits)) %>%
  arrange(desc(Total)) %>%
  head(n = 10) %>%
  ggplot(aes(reorder(Province.State, -Total), Total/1000000000))+
  geom_bar(stat = "identity", fill = "plum4")+
  geom_text(aes(label=Total/1000000000), vjust = "center", hjust="center", color="white", size=3.5)+
  #scale_y_continuous(labels = scales::comma)+
  labs(x = NULL, y ="", title = "States that Received the Most Unemployment Insurance Pay for 2020", subtitle = "In Billions")+
  theme_test() + 
  my_theme +
  coord_flip()

#================ VIZ
#PICKING SUBSECTOR
subsect<- c("Manufacturing","Mining, Logging, and Construction","Trade, transportation, and utilities","Professional and business services", "Education and health services","Leisure and hospitality","Financial activities","Information","Other services","Government")
state2$farm <- state2$employment-(state2$`Total nonfarm` *1000)

subsector <- state2 %>%
  select(Province.State, Month, Year, subsect)%>%
  mutate(Month = month.abb[Month]) %>%
  mutate(Date = (paste(Month,Year,sep = "-"))) %>%
  mutate(Date = (paste("01",Date,sep="-"))) %>%
  mutate(Date = as.Date(Date, "%d-%b-%Y")) %>% 
  select(Date, everything()) 

subshape <- subsector %>%
  filter(Year == 2020) %>%
  gather("Industry", "Employed", 5:14)
  
subshape$Month = factor(subshape$Month, levels = month.abb)

#Proportion of Employed per Industry in the US 2020
ggplot(subshape, aes(fill = Industry, y=Employed, x=Month)) + 
  geom_bar(position = "fill", stat = "identity") +
  labs(x = NULL, y ="", title = "Proportion of Employed per Industry in the US 2020")+
  scale_fill_tableau()+
  my_theme +
  coord_flip()

#GETTING PROP FOR STATES WITH MOST CASES
topcov <-us_case %>% 
  filter(!Province.State %in% del) %>%
  mutate(Date = as.Date(Date,format = '%m/%d/%Y')) %>%
  filter(Date %in% date.end.month) %>%
  group_by(Province.State) %>%
  summarise(Case = sum(Case)) %>%
  arrange(desc(Case)) %>%
  head(10)
topcov<- topcov$Province.State

subshape2<- subshape
subshape2$Province.State = factor(subshape2$Province.State, levels = topcov)
subshape2 %>%
  filter(Province.State %in% topcov) %>%
  ggplot(aes(fill = Industry, y=Employed, x=Month)) + 
  geom_bar(position = "fill", stat = "identity") +
  labs(x = NULL, y ="", title = "Proportion of Employed per Industry in the US 2020")+
  scale_fill_tableau()+
  my_theme +
  #theme(legend.position="bottom")+
  coord_flip()+
  facet_wrap(Province.State~., ncol = 2)

subshape %>%
  group_by(Industry, Province.State, Month) %>%
  summarise(Total= sum(Employed))

  