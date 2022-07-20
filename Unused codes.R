# Jan2018 <- read.csv("Jan 2018.csv", header = TRUE) 
dataall <- list.files(path = "E:/Projects/R Projects/Uber Data Project", 
                      pattern = "*.csv", full.names = TRUE)%>%
  lapply(read_csv)%>%
  bind_rows()

# 
# temp <-  list.files(pattern="*.csv") %>% last(2)
# myfiles <-  lapply(temp, read.delim) %>% bind_rows()
#     
# 
# Jan2018 <- read.csv("April.csv", header = TRUE) 
# aug2018 <- read.csv("Aug 2018.csv", header = TRUE) 
# str(aug2018)
# rbind(Jan2018,aug2018)
# cbind.data.frame(colnames(Jan2018),colnames(aug2018))

##.csv
## .xlsx
## .xls
## .txt 
# json 
MonthlyData %>%
  ggplot( aes(x=MonthlyTotals, color=Year, fill=Year)) +
  geom_density(alpha=0.6) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  geom_text( data=MonthlyData, aes(x=MonthlyTotals, y=Months, label=Year, color=Year), hjust=0, size=4.5) +
  theme_ipsum() +
  theme(
    legend.position="none"
  ) +
  ylab("") +
  xlab("Assigned Probability (%)")
ggplot(data = MonthlyData, aes(x = Months, y = MonthlyTotals, 
                               fill = MonthlyTotals, group = Year))+
  geom_bar(position = "stack", stat = "identity")+
  legend(legend = MonthlyData$Year, col = c("Red","Blue"))


##employee
New.UberData%>%
  mutate(Year = as.numeric(format(`Date Requested`, "%Y")),
         +            Months = factor(months(as.Date(`Date Requested`)),
                                      +                            levels = month.name))%>%
  select(`Last Name`,`Charges`, Months, Year)%>%
  group_by(`Last Name`,Months)%>%
  filter(Year == "2018")%>%
  na.omit("")%>%
  summarise(monthlyCharges = sum(Charges))



##renaming column one and removing the last three columns
# #Nuber.2017 <- 
#   uber.2017%>%
#   rename(Request.Date = (1))%>%
#   select(-c(26,27,28))

##combining the two datasets and removing some columns
# UberData <- 
#   rbind.data.frame(uber.2018,Nuber.2017)%>%
#   select(-c(1,2,5,6,7,10,17,18))

##consuming in data
UberData = read.csv("UberData.csv", header = TRUE,stringsAsFactors = FALSE)
colnames(UberData)

##Checking my data
head(UberData)
str(UberData)


##consuming in the data
##2017 Data
#uberJan_July <- read.csv("2017.csv", header = TRUE,stringsAsFactors = FALSE)
#Aug.Dec <- read.csv("AUG.DEC 2017.csv",header=TRUE, stringsAsFactors = FALSE)

#removing the last two columns from the August to Dec Datta
#uberAug.Dec <- 
#Aug.Dec%>%
#select(-c(28,29))

##combining the 2017 Data
# uber2017 <- 
#   rbind(uberJan_July,uberAug.Dec)%>%
#   select(-c(1,27))


##combining 2017 and 2018 Data
# uberData <- rbind(uber2017,Data.2018)
# 
# head(uberData)
# str(uberData)


#quarterly analysis
New.UberData%>%
  mutate(Year = as.numeric(format(`Date Requested`, "%Y")),
         Months = factor(months(as.Date(`Date Requested`)),
                         levels = month.name),
         Quarters = as.yearqtr(format(Year),"Q%q %Y"))


New.UberData%>%
  mutate(Year = as.numeric(format(`Date Requested`, "%Y")),
         Months = factor(months(as.Date(`Date Requested`)),
                         levels = month.name),
         quarter(Year, type = "year.quarter"))


New.UberData%>%
  mutate(Year = as.numeric(format(`Date Requested`, "%Y")),
         Months = factor(months(as.Date(`Date Requested`)),
                         levels = month.name),
         FullNames = paste(`First Name`,`Last Name`))%>%
  select(9,12,17,19,20,21,22)%>%
  group_by(FullNames, Year)%>%
  summarise(Individual_Charges = sum(Charges))%>%
  filter(Year == "2018")%>%
  arrange(desc(Individual_Charges))