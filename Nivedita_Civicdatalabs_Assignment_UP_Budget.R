#loading packages 
library(dplyr)
library(tidyr)
library(readr)

#importing dataset 
load(district_level_mapping_2017 <- read_csv("C:/Users/HP/Downloads/district_level_mapping_2017.csv"))
View(district_level_mapping_2017)



#viewing the entries in column Head Description 
table(district_level_mapping_2017$`Grant Head Description`)



#changing column name for R to read it 
colnames(district_level_mapping_2017)c[5]<-  "Grant.Number"


#filtering out data for school education, Elementary, Secondary level education and Higher Education will be treated as school level education 
#71 is the code foe elementary level education, 72 for Secondary level and 73 for Higher education 
School_Education_2017<-district_level_mapping_2017 %>% filter(Grant.Number%in%c("71", "72", "73")) 



#saving the file 
save(School_Education_2017, file = "school.2017.RData")


#changing column name for R to read the name 
colnames(School_Education_2017)[7]<- "Major.Head.Code"


#The filtered data contains education at level of elementary, secondary and higher education. The Major heads which have spend on education when data for school education 
#has been filtered out included- Pension and Other Retirement Benefits;General Education; Sports and Youth Services ;Art and Culture; Capital outlay
#on education- sports, arts and culture. The Major head description column does not have entry of the name of Capital Outlay but we find it out from the code
#given in Readme File, the code for Capital Outlay in Major Head Code is 4202. The four Major Head Description will be called Departments/ Ministeries according to the 
#ReadMe file


#To clear confusion about Progressive Actual Progressive expenditure and total expenditure we will check if the two columns are actually same 
identical(School_Education_2017$`Total Expenditure Upto Month (November)`, School_Education_2017$`Actual Progressive Expenditure upto month (October)`)

#####################################################################################################################################################################################################################################

#Filtering out data for only school education and ministry level (it includes four Ministries-  Pension and Other Retirement Benefits;General Education; 
#Sports and Youth Services ;Art and Culture)
#The data on capital outlay has been removed for the analysis here. This is done to lay more focus on expenditure of recurring nature, capital expenditure 
#will be shown separetly. 
#The inclusion of capital expenditure would inflate the value of expenditure therefore it was removed for Ministery level analysis 
Ministry_School_2017<-School_Education_2017 %>% select(Grant.Head.Description, Grant.Number, `Major.Head.Code`, 
                                                       `Major Head Description`, `Total Expenditure Upto Month (November)`, `Division Description`, `Division Code`) %>%
                                                   filter(Major.Head.Code%in% c("2071", "2202", "2204", "2205"))

#here 2071 shows department budget expenditure of Pension and Other Retirement Benefits, 2202 shows expenditure by General Education, 2204 shows expenditure by 
#Sports and Youth Services and finally 2205 shows expenditure by Arts and Culture   


#easy to keep figures in crores 
Ministry_School_2017<-Ministry_School_2017 %>% mutate(Total.expd.Crores= `Total Expenditure Upto Month (November)`/10000000) %>%   mutate_if(is.numeric, round, 1)


#saving the file 
save(Ministry_School_2017, file = "Ministry.Expenditure.School.subset.RData")

###################################################################################################################################################################################################################################################

#creating dataset that shows expenditure done by different ministeries on school education 
Answer1<-Ministry_School_2017 %>% group_by(Major.Head.Code) %>% summarise(Total.Expenditure.in.Crores= sum(Total.expd.Crores))
Answer1<-as.data.frame(Answer1)
Answer1<-Answer1 %>% mutate(Ministry.Name= c("Pension and Other Retirement Benefits", "Genral Education", "Sports and Youth Services", "Art and Culture"), 
                                              Share.Percentage= (100*Total.Expenditure.in.Crores/sum(Total.Expenditure.in.Crores))) %>% 
                     mutate_if(is.numeric, round, 2)
                  
#########################################################################################################################################################################################################################################################
#Here 4202 shows the department of Capital Outlay on Education- sports, Art and Culture 
                  
Capital_School_Exp<-School_Education_2017 %>% select(Grant.Head.Description, Grant.Number, Major.Head.Code,`Major Head Description`,`Total Expenditure Upto Month (November)`) %>% 
                                              filter(Major.Head.Code%in%"4202") %>% 
                                              mutate(Total.expenditure.in.Crores= `Total Expenditure Upto Month (November)`/10000000) %>% 
                                              mutate_if(is.numeric, round, 1)
Answer2<-Capital_School_Exp %>% group_by(Major.Head.Code) %>% summarise(Total.Expenditure.in.crores= sum(Total.expenditure.in.Crores))
Answer2
Answer2<-as.data.frame(Answer2)

#To find the share of Capital Expenditure, will find out total expenditure on school education (Total expenditure is found by summing up the expenditure over all Ministery 
#level expenditure calculated in Answer 1 and adding capital expenditure calculated in Answer2 above)
a<-2436.1
b<-sum(Answer1$Total.Expenditure.in.Crores)
b
Total.Expenditure<-a+b
Total.Expenditure   #(114598.6)


#mutating a cloumn of proportion/share of capital expenditure 
Answer2<-Answer2 %>% mutate(Share.Percentage.of.total.Exp= Total.Expenditure.in.crores*100/Total.Expenditure, Head.Description= "Capital Outlay on Education") %>%
                     mutate_if(is.numeric, round, 2)


##################################################################################################################################################################################################################################################################

#To find the expenditure on school education by the state of UP in 2017, will sum up the expenditure done by different ministeries to get total expenditure by state 
#and divide it by population of UP according to 2011 Census which states that UP population is 19.98 crores. Capital Outlay department has been ignored in calculating total 
#expenditure here since expenditure on education is mostly revenue in nature and capital expenditure includes buying funiture or new buildings of school which odes not 
#happen every year hence to get an overview of how much UP government spends on running schools, the four Ministery segregated for Answer 1 are used  

##To find the per capita expenditure on education in the state of UP I have divided the total expenditure by population of UP according to 2011 census 

Answer3.1<-Answer1 %>% summarise(Total.School.Expenditure.per.capita.in.UP= sum(Total.Expenditure.in.Crores))
Answer3.1<- as.data.frame(Answer3.1/19.98)

#To find the per capita expenditure in each district of UP a new dataset is created which contains expenditure on school education for all 5 Major heads, 

district_pop_exp<-School_Education_2017 %>% select(Grant.Head.Description, `Division Description`,  Major.Head.Code, `Major Head Description`, `Total Expenditure Upto Month (November)`, `Progressive Allotment`) %>% 
                                            mutate(Total.Expenditure.in.Crores= `Total Expenditure Upto Month (November)`/10000000, Prog.Allotment.in.crores= `Progressive Allotment`/10000000) %>% 
 
                                             mutate_if(is.numeric,round,1)

#Loading dataset with districts against population, population data has been taken from census 2011 and a new dataset was created in excel 
library(readxl)
X18_district_pop <- read_excel("18_district_pop.xlsx")


#Grouping by the data of total expenditure according to districts 
Draft3.2<-district_pop_exp %>% group_by(`Division Description`) %>% summarise(District.wise.expenditure= sum(Total.Expenditure.in.Crores))
Draft3.2<-as.data.frame(Draft3.2)


#Mutating the column of population against every district from the dataset created in excel containing information of population of each district 
Draft3.2<-Draft3.2 %>% mutate(population= X18_district_pop$Population, Population.in.crores= population/ 10000000) 


#Two districts don't have population data, therefore removing them from final study. Azamgarh and Lucknow Coll. will be removed 
Answer3.2<-na.omit(Draft3.2)


#Finally per capita expenditure will be found 
Answer3.2<-Answer3.2 %>% mutate(Expenditure.per.capita= District.wise.expenditure/Population.in.crores) %>% 
                         mutate_if(is.numeric, round, 1)

########################################################################################################################################################################################################################################################

#To find out utilization of funds, we will see how proportion of expenditure is done out of total allocated fund amount to the district for both revenue 
#and capital expenditure on school education and therefore rank the districts 
#The read me file shows that revenue expenditure on any area in general can be found in General Education, thus the major head code 2202 will be used to filter out data 

Answer.Revenue.4.1<- School_Education_2017 %>% select(`Division Description`, `Major Head Description`, Major.Head.Code, `Progressive Allotment`, `Total Expenditure Upto Month (November)`) %>% 
                                      filter(Major.Head.Code%in%"2202") %>% 
                                      mutate(Prog.Allot.in.crores = `Progressive Allotment`/10000000, Total.Expd.in.crores= `Total Expenditure Upto Month (November)`/10000000) %>% 
                                      mutate_if(is.numeric, round,1) %>% 
                                      group_by(`Division Description`) %>% 
                                      summarise(Prog.Allotment.in.crores= sum(Prog.Allot.in.crores),Total.Expenditure.in.crores= sum(Total.Expd.in.crores))

Answer.Revenue.4.1<-Answer.Revenue.4.1 %>% mutate(Percent.utilization.funds= Total.Expenditure.in.crores*100/Prog.Allotment.in.crores) %>% 
                                           mutate_if(is.numeric, round, 1) %>% 
                                           arrange(desc(Percent.utilization.funds))
Answer.Revenue.4.1$Rank <- 1:nrow(Answer.Revenue.4.1)
                 
###########################################################################################################################################################################################################################################################
#To find the expenditure of capital nature, the 4202 code whose description reads as Capital Outlay on Education- sports, arts and culture is used 

Answer.Capital.4.2<- School_Education_2017 %>% select(`Division Description`, `Major Head Description`, Major.Head.Code, `Progressive Allotment`, `Total Expenditure Upto Month (November)`) %>% 
                                       filter(Major.Head.Code%in%"4202") %>% 
                                       mutate(Prog.Allot.in.crores = `Progressive Allotment`/10000000, Total.Expd.in.crores= `Total Expenditure Upto Month (November)`/10000000) %>% 
                                       mutate_if(is.numeric, round,1) %>% 
                                       group_by(`Division Description`) %>% 
                                       summarise(Prog.Allotment.in.crores= sum(Prog.Allot.in.crores),Total.Expenditure.in.crores= sum(Total.Expd.in.crores))
Answer.Capital.4.2<-Answer.Capital.4.2 %>% mutate(Percent.utilization.funds= Total.Expenditure.in.crores*100/Prog.Allotment.in.crores) %>% 
                                           mutate_if(is.numeric, round, 1) %>% 
                                           arrange(desc(Percent.utilization.funds))
Answer.Capital.4.2$Rank <- 1:nrow(Answer.Capital.4.2)

#######################################################################################################################################################################################################################################################
                                      














