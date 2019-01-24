
  
  # Load all of the packages used in the analysis
  library(gender)
library(reshape2)
library(magrittr)
library(ggplot2)
library(scales)
library(dplyr)
#install.pacakages("genderdata", repos = "http://packages.ropensci.org",
#type = "source")
library(gender)
library(stringr)
library(lubridate)
library(tidyr)
library(zipcode)
library(maps)
library(gridExtra)
library(choroplethr)
library(choroplethrMaps)
#library(devtools)
#install_github('arilamstein/choroplethrZip@v1.4.0')
library(choroplethrZip)

##Data Loading and Cleaning

#The data set to be loaded contains all the financial contributions to the united states presedential campaign of 2016 by the State of New Jersey. 


# Load the Data
NJContributions <- read.csv('NewJerseyData.csv', stringsAsFactors = F,
                            row.names = NULL)[,-19]

colnames(NJContributions)<-c('cmte_id','CandidateID','CandidateName', 
                             'ContributorName','ContributorCity',
                             'ContributorState','ContributorZip',
                             'ContributorEmployer','ContributorOcupation',
                             'ContributorReceiptAmount','ContributorReceiptDate',
                             'ReceiptDescription','memo_cd','memo_text',
                             'form_tp','file_num','tran_id','election_tp')
dim(NJContributions)



#This dataset contains 203,883 observations and 18 variables. 

#Struture of the Data: 

str(NJContributions)





NewNJContributions <- subset(NJContributions, 
                             NJContributions$ContributorReceiptAmount > 0 &
                               NJContributions$ContributorReceiptAmount <= 2700)

dim(NewNJContributions)


#Add party variable based on the Candidates Name

Democrats <- c("Clinton, Hillary Rodham", "Sanders, Bernard",
               "O'Malley, Martin Joseph", "Lessig, Lawrence",
               "Webb, James Henry Jr.")

Libertarian <- c("Johnson, Gary")

Green <- ("Stein, Jill")

Independent <- c("McMullin, Evan")

NewNJContributions$Party<-ifelse(NewNJContributions$CandidateName %in% Democrats,
                                 "Democrat",
                                 ifelse(NewNJContributions$CandidateName %in% Libertarian,
                                        "Libertarian",
                                        ifelse(NewNJContributions$CandidateName %in% Green,
                                               "Green",
                                               ifelse(NewNJContributions$CandidateName %in% Independent,
                                                      "Independent", "Republican"))))


print("Structure of Party Variable")
str(NewNJContributions$Party)

# Add gender variable using the gender library 

NameSplit<-colsplit(NewNJContributions$ContributorName, ",",c("FirstName","LastName"))
NewNJContributions <- cbind(NewNJContributions, NameSplit)
NewNJContributions$FirstName <- gsub(" ", "", NewNJContributions$FirstName, 
                                     fixed = TRUE)
NewNJContributions$FirstName <- str_replace_all(NewNJContributions$FirstName,
                                                "[[:punct:]]", "")
gender <- gender(NewNJContributions$FirstName, c(1932,2012), method = "ssa", 
                 countries = "United States")
names(gender)[1] = "FirstName"
gender <- unique(gender)
NewNJContributions <- merge(NewNJContributions,gender[,c("FirstName","gender")],all.x = TRUE)

print("Structure of Gender Variable")
str(NewNJContributions$gender)

#Adding the month, year variable 

NewNJContributions <- NewNJContributions %>%
  mutate(date = as.Date(ContributorReceiptDate,"%d-%b-%y"), 
         year = year(date),
         month = month(date),
         year_month = paste(month.abb[month], ",", year))

print("Structure of Month,Year Variable")
str(NewNJContributions$year_month)

#Adding Latitude, longitude and other variables used to plot in map 


data(zipcode)
NewNJContributions$ContributorZip<-substr(NewNJContributions$ContributorZip,1,5)
NewNJContributions$ContributorZip<-as.character(NewNJContributions$ContributorZip)
NewNJContributions$ContributorZip<-clean.zipcodes(NewNJContributions$ContributorZip)
NJZipcode <- subset(zipcode, state =="NJ")[,-c(2,3)]
colnames(NJZipcode) <- c("ContributorZip", "Latitude" , "Longitude")
NewNJContributions <- merge(NewNJContributions, NJZipcode, all.x = TRUE)

#Adding Map of NJ 

NJmap = map_data('county','New Jersey')

#Name of Counties on map 

CountyNames <-aggregate(cbind(long,lat) ~ subregion, data = NJmap,
                        function(x) mean(range(x)))


# Univariate Plots Section


NumberofContributions<-as.data.frame(sort(table
(NewNJContributions$CandidateName,dnn = 'Candidate'),decreasing = T),
responseName = 'NumberofContributions')

ggplot(aes(x= reorder(Candidate,NumberofContributions), 
y = NumberofContributions), data = NumberofContributions[1:15,]) +
geom_bar(stat = 'identity') +
coord_flip() + 
geom_text(stat='identity', aes(label = NumberofContributions),
data = NumberofContributions[1:15,], hjust = 0) +
theme(axis.text = element_text(size = 12,face = "bold")) +
#theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4)) +
xlab("Candidate") +
ylab("Count") + 
ggtitle("TOP 15 CANDIDATES BASED ON MOST NUMBER OF CONTRIBUTIONS") 



ggplot(aes(x=reorder(Candidate,NumberofContributions), 
y = NumberofContributions/sum(NumberofContributions)), 
data = NumberofContributions[1:7,]) +
geom_bar(stat = 'identity') +
coord_flip() +
geom_text(stat='identity', 
aes(label = percent(NumberofContributions/sum(NumberofContributions))), 
data = NumberofContributions[1:7,], hjust = 0 ) +
theme(axis.text = element_text(size = 12,face = "bold")) +
#theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4)) +
scale_y_continuous(labels = percent_format()) +
xlab("Candidate") +
ylab("Percent of total contributions") + 
ggtitle("TOP 7 CANDIDATES BASED ON HIGHEST PERCENT OF CONTRIBUTIONS") 




ContributionsByCity<-as.data.frame(sort(table
(NewNJContributions$ContributorCity,dnn = 'City'),decreasing = T),
responseName = 'NumberofContributions')


ggplot(aes(x= reorder(City, NumberofContributions), y = NumberofContributions), 
data = ContributionsByCity[1:15,]) +
geom_bar(stat = 'identity') +
coord_flip() +
#theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4)) +
geom_text(stat='identity', aes(label = NumberofContributions),
data = ContributionsByCity[1:15,], hjust = 0) +
theme(axis.text = element_text(size = 12,face = "bold")) +
xlab("City") +
ylab("Count") + 
ggtitle("TOP 15 CITIES WITH THE MOST NUMBER OF CONTRIBUTORS") 



ContributionsAmounts <- as.data.frame(sort(table
(NewNJContributions$ContributorReceiptAmount,dnn = 'ContributionAmount'),
decreasing = T),responseName = 'NumberofContributions')

options(scipen = 5)
ggplot(aes(x=ContributorReceiptAmount), data = NewNJContributions) +
geom_histogram(binwidth = 100) +
scale_x_continuous(breaks = seq(0, 2700, 200), labels = dollar_format(prefix = "$")) +
xlab("Conribution Amount") +
ylab("Contibution Count") + 
ggtitle("HISTOGRAM OF CONTRIBUTION AMOUNTS") 

}

ggplot(aes(x=ContributionAmount, y = NumberofContributions), 
data = ContributionsAmounts[1:15,]) +
geom_bar(stat = 'identity') +
#theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4)) +
geom_text(stat='identity', aes(label = NumberofContributions), 
data = ContributionsAmounts[1:15,], vjust = -0.4) +
xlab("Contribution Amount (In Dollars)") +
ylab("Count") + 
ggtitle("TOP 15 MOST CONTRIBUTED AMOUNTS") 




Occupations <- as.data.frame(sort(table
(NewNJContributions$ContributorOcupation,dnn = 'Occupation'),decreasing = T),
responseName = 'NumberofContributions')

#Exclue the Information requested 

NewOccupations <- subset(Occupations,Occupation != "INFORMATION REQUESTED" )

ggplot(aes(x= reorder(Occupation, NumberofContributions),
y = NumberofContributions), 
data = NewOccupations[1:15,]) +
geom_bar(stat = 'identity') +
coord_flip() +
theme(axis.text = element_text(size = 12,face = "bold")) +
#theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4)) +
geom_text(stat='identity', aes(label = NumberofContributions), 
data = NewOccupations[1:15,], hjust = 0) +
xlab("Occupation") +
ylab("Count") + 
ggtitle("TOP 15 MOST CONTRIBUTED OCCUPATIONS") 

CountByParty <- as.data.frame(sort(table
(NewNJContributions$Party,dnn = 'Party'),decreasing = T),
responseName = 'NumberofContributions')

ggplot(aes(x=Party, y = NumberofContributions), data = CountByParty) +
geom_bar(stat = 'identity') +
geom_text(stat='identity', aes(label = NumberofContributions),
data = CountByParty, vjust = -0.4) +
xlab("Party") +
ylab("Count") + 
ggtitle("NUMBER OF CONTRIBUTIONS RECEIVED BY EACH PARTY") 



Democratic party has got the most number of contributions followed by the Republican party although the difference is huge.

CountByGender <- as.data.frame(sort(table
(NewNJContributions$gender,dnn = 'Gender'),decreasing = T),
responseName = 'NumberofContributions')

NewCountByGender<-subset(CountByGender, (is.na(gender) = TRUE))

GenderbyCount <- ggplot(aes(x=Gender, y = NumberofContributions),
data = NewCountByGender) +
geom_bar(stat = 'identity') +
geom_text(stat='identity', aes(label = NumberofContributions),
data = NewCountByGender, vjust = -0.4) +
xlab("Gender") +
ylab("Count") + 
ggtitle("CONTRIBUTION COUNT BY GENDER") 

GenderbyPercent <- ggplot(aes(x=Gender, 
y=NumberofContributions/sum(NumberofContributions)), 
data = NewCountByGender) +
geom_bar(stat = 'identity') +
geom_text(stat='identity', 
aes(label =percent((NumberofContributions/sum(NumberofContributions)))),
data = NewCountByGender, vjust = -0.4) +
scale_y_continuous(labels = percent_format()) +
xlab("Gender") +
ylab("Percent") + 
ggtitle("CONTRIBUTION PERCENT BY GENDER") 

grid.arrange(GenderbyCount,GenderbyPercent, ncol = 2)



CountByYearMonth <- as.data.frame(sort(table
(NewNJContributions$year_month,dnn = 'Month-Year'),decreasing = T),
responseName = 'NumberofContributions')


ggplot(aes(x=reorder( Month.Year, NumberofContributions), y = NumberofContributions), 
data = CountByYearMonth[1:15,]) +
geom_bar(stat = 'identity') +
coord_flip() +
theme(axis.text = element_text(size = 12,face = "bold")) +
#theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4)) +
geom_text(stat='identity', aes(label = NumberofContributions), 
data = CountByYearMonth[1:15,], hjust = 0) +
xlab("Month-Year") +
ylab("Count") + 
ggtitle("Top 15 most Contributions by Month and Year") 



##SUMMARY OF UNIVARIATE ANALYSIS

####Structure of the data set



# Bivariate Plots Section

#contibutions by party 

ggplot(aes(x=Party, y = ContributorReceiptAmount, fill = Party), 
data = NewNJContributions) +
geom_boxplot() +
geom_jitter(position = position_jitter(alpha = 1/4))
scale_y_continuous(labels = dollar_format(prefix = "$")) +
xlab("Party") +
ylab("Contibution Amount") + 
ggtitle("Contribution Amount BoxPlot by Party") +
scale_fill_manual(values = c("#0000ff","#00e600","#c61aff","#ffd633",
"#ff471a"))


ContributionsByParty <- NewNJContributions %>%
group_by(Party) %>%
summarise(Total = sum(ContributorReceiptAmount))

#by( NewNJContributions$ContributorReceiptAmount,NewNJContributions$Party,
#     summary)

ggplot(aes(x=Party, y=Total/1000, fill = Party), 
data = ContributionsByParty) +
geom_bar(stat='identity') +
geom_text(stat='identity', aes(label = round(Total/1000)),
data = ContributionsByParty, vjust = -0.4) +
ylab("Total Contribution (In Thousands)") +
xlab("Party") + 
ggtitle("TOTAL CONTRIBUTIONS RECEIVED BY PARTIES (IN THOUSANDS)") +
scale_y_continuous(labels = dollar_format(prefix = "$")) +
scale_fill_manual(values = c("#0000ff","#00e600","#c61aff","#ffd633",
"#ff471a"))

#Contribuyions by candidates 

Candidates <- NewNJContributions %>%
group_by(CandidateName) %>%
summarise(TotalContributions = sum(ContributorReceiptAmount),
contributor = n()) %>%
arrange(TotalContributions)

Candidates$CandidateName <- factor(Candidates$CandidateName)


#by(NewNJContributions$ContributorReceiptAmount,NewNJContributions$CandidateName,
#       sum)

ggplot(aes(x= reorder(CandidateName, TotalContributions),
y= TotalContributions), data = Candidates) +
geom_bar(stat = 'identity') + 
coord_flip() +
theme(axis.text = element_text(size = 12,face = "bold")) +
scale_y_continuous(labels = dollar_format(prefix = "$")) +
geom_text(stat = 'identity', aes(label = dollar(round(TotalContributions))),
data = Candidates, hjust = 0) +
ylab("Contibution Amount (In DOllars)") +
xlab("Candidate") +
ggtitle("CONTRIBUTIONS RECEIVED BY CANDIDATES") 

# Contribution by occupation 


Occupations <- subset(NewNJContributions,  
ContributorOcupation != "INFORMATION REQUESTED") %>%
group_by(ContributorOcupation) %>%
summarise(TotalContributions = sum(ContributorReceiptAmount), n = n()) %>%
top_n(10,n)

ggplot(aes(x= reorder(ContributorOcupation,TotalContributions), 
y= TotalContributions), data = Occupations) +
geom_bar(stat = 'identity') + 
coord_flip() +
scale_y_continuous(labels = dollar_format(prefix = "$")) +
theme(axis.text = element_text(size = 12,face = "bold")) +
#theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4)) +
geom_text(stat = 'identity', aes(label = dollar(TotalContributions)),
data = Occupations, hjust = 0) +
ylab("Contribution Amount (In Dollars)") +
xlab("Occupation") +
ggtitle("CONTRIBUTIONS BY OCCUPATIONS") 


# Contribution by date

ContributionAmountbyDate <- NewNJContributions  %>%
group_by(year_month) %>%
summarise(TotalContributions = sum(ContributorReceiptAmount), n = n()) %>%
top_n(10,n)

ggplot(aes(x= reorder(year_month,TotalContributions), 
y= TotalContributions), data = ContributionAmountbyDate) +
coord_flip() +
geom_bar(stat = 'identity') +
scale_y_continuous(labels = dollar_format(prefix = "$")) +
theme(axis.text = element_text(size = 12,face = "bold")) +
geom_text(stat = 'identity', aes(label = dollar(TotalContributions)), 
data = ContributionAmountbyDate, hjust = 0) +
ylab("Contribution Amount (In Dollars)") +
xlab("Month-Year") +
ggtitle("CONTRIBUTIONS RECEIVED BY MONTH-YEAR") 



# Contribution by Cities

ContributionAmountbyCity <- NewNJContributions  %>%
group_by(ContributorCity) %>%
summarise(TotalContributions = sum(ContributorReceiptAmount), n = n()) %>%
top_n(10,n)

ggplot(aes(x= reorder(ContributorCity,TotalContributions), 
y= TotalContributions), data = ContributionAmountbyCity) +
geom_bar(stat = 'identity') + 
scale_y_continuous(labels = dollar_format(prefix = "$")) +
theme(axis.text = element_text(size = 12,face = "bold")) +
coord_flip() +
geom_text(stat = 'identity', aes(label = dollar(TotalContributions)), 
data = ContributionAmountbyCity, hjust = 0) +
#theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4)) +
ylab("Contribution Amount") +
xlab("City") +
ggtitle("CONTRIBUTION AMOUNT BY CITIES") 



# Multivariate Plots Section




ggplot(aes(x=ContributorReceiptAmount, fill = Party), 
data = NewNJContributions) +
geom_histogram(binwidth = 100,position = position_dodge()) +
scale_x_continuous(breaks = seq(0, 2700, 200)) +
ylim(0,1000) +
xlab("Conribution Amount (In Dollars)") +
ylab("Contibution Count") + 
ggtitle("HISTOGRAM OF CONTRIBUTION AMOUNTS TO PARTIES") +
scale_fill_manual(values = c("#0000ff","#00e600","#c61aff","#ffd633",
"#ff471a"))




CandidatesbyParty <- NewNJContributions %>%
group_by(Party,CandidateName) %>%
summarise(TotalContributions = sum(ContributorReceiptAmount),
contributor = n()) %>%
arrange(TotalContributions)

options(scipen = 5)
ggplot(aes(x=reorder(CandidateName, TotalContributions), 
y= TotalContributions), data = CandidatesbyParty) +
geom_bar(stat = 'identity', aes(fill = Party)) + coord_flip() +
geom_text(stat = 'identity', aes(label = dollar(TotalContributions))
, data = CandidatesbyParty, hjust = 0) +
scale_y_continuous(labels = dollar_format(prefix = "$")) +
theme(axis.text = element_text(size = 12,face = "bold")) +
ylab("Contribution Amounts(In Dollars)") +
#ylim(0,13000000) +
xlab("Candidate") +
ggtitle("CONTRIBUTIONS RECEIVED BY CANDIDATES") +
scale_fill_manual(values = c("#0000ff","#00e600","#c61aff","#ffd633",
"#ff471a"))



TopOccupations = c("RETIRED", "ATTORNEY", "HOMEMAKER", "NOT EMPLOYED",
"PHYSICIAN", "CONSULTANT", "PROFESSOR", "ENGINEER", "SALES")

MajorParty = c("Democrat", "Republican")


OccupationsContributionsByParty<-subset(NewNJContributions, 
ContributorOcupation %in% TopOccupations & Party %in% MajorParty) %>%
group_by(ContributorOcupation, Party) %>% 
summarise(n=n(), Total = sum(ContributorReceiptAmount)) 

ggplot(aes(x= ContributorOcupation, y= Total), 
data = OccupationsContributionsByParty) +
geom_histogram(stat = 'identity',position = position_dodge(),
aes(fill = Party)) + 
coord_flip() +
#theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4)) +
ylab("Contibution Amount (In Dollars)") +
xlab("Candidate") +
ggtitle("CONTRIBUTIONS BY OCCUPATIONS TO THE LEADING PARTIES") +
scale_fill_manual(values = c("#0000ff","#ff471a"))



ggplot() + 
geom_polygon(data = NJmap, aes(x=long, y=lat, group = group), 
colour = "black", fill = "#00b3b3") +
geom_point(data = NewNJContributions, aes(x=Longitude , y= Latitude, 
color = Party)) +
geom_text(data = CountyNames, aes(long,lat,label=subregion))+
xlab("Longitude") +
ylab("Latitude") +
ggtitle("CONTRIBUTION SPREAD ACROSS COUNTIES") +
scale_color_manual(values = c("#0000ff","#00e600","#c61aff","#ffd633",
"#ff471a"))



TotalZip <- NewNJContributions %>%
group_by(ContributorZip) %>%
summarise(value = sum(ContributorReceiptAmount)) 

TotalZip$region <- TotalZip$ContributorZip

TotalZip <- na.omit(TotalZip)

zip_choropleth(TotalZip, num_color = 6, state_zoom = "new jersey", 
title="TOTAL CONTRIBUTIONS BY COUNTY") +
coord_map() +
scale_fill_brewer(name="Contribution", palette=2, drop=FALSE)




# Multivariate Analysis


# Final Plots and Summary

### Box Plot of Contribution Amounts for each Party 


ggplot(aes(x=Party, y = ContributorReceiptAmount, fill = Party),
data = NewNJContributions) +
geom_boxplot() +
scale_y_continuous(labels = dollar_format(prefix = "$")) +
xlab("Party") +
ylab("Contibution Amount (In Dollars") + 
ggtitle("BOXPLOT OF CONTRIBUTION AMOUNTS RECEIVED BY PARTIES") +
scale_fill_manual(values = c("#0000ff","#00e600","#c61aff","#ffd633",
"#ff471a"))



### Description 

The Democratic party has a median contribution of $25.00 which is lesser than the median contribution of the Republican Party which is 53.00 dollars.The Democartic Party has more number of outliers which implies that the party has received more number of large contribution amounts from the contributors than small amount.Next to this is the republican party which has a better spread than the democratic party with more number of small contributors. The republican party also has outliers, the large contributors but it is lesser compared to the Democratic Party.

### Contribution Amount Received by the Candidates

ggplot(aes(x=reorder(CandidateName, TotalContributions), 
y= TotalContributions), data = CandidatesbyParty) +
geom_bar(stat = 'identity', aes(fill = Party)) + coord_flip() +
geom_text(stat = 'identity', aes(label = dollar(TotalContributions))
, data = CandidatesbyParty, hjust = 0) +
scale_y_continuous(labels = dollar_format(prefix = "$")) +
ylab("Contribution Amounts(In Dollars)") +
#ylim(0,13000000) +
xlab("Candidate") +
theme(axis.text = element_text(size = 12,face = "bold")) +
ggtitle("CONTRIBUTIONS RECEIVED BY CANDIDATES") +
scale_fill_manual(values = c("#0000ff","#00e600","#c61aff","#ffd633",
"#ff471a"))



### Description 



zip_choropleth(TotalZip, num_color = 6, state_zoom = "new jersey",  
title="TOTAL CONTRIBUTIONS BY COUNTY") +
coord_map() + 
scale_fill_brewer(name="Contribution", palette=2, drop=FALSE)


