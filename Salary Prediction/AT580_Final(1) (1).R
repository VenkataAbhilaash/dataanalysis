#importing libraries
## for drawing graphs

library(ggplot2)
##for splitting  the dataset
library(rpart)
library(rpart.plot)
library(caTools)
##for playing with data

library(dplyr)
library(tidyr)

##for missing data
library('Amelia')
##Coding part starts
##import dataset

adult<-read.csv("C:\\Users\\ABHILAASH ANNAMREDDI\\OneDrive\\Desktop\\adult_salary.csv",stringsAsFactors=FALSE, na.strings = ".")
## exploring the dataset to modify if any needed
## to get rid of mandaotry first column count 

head(select(adult,-X))
print(str(adult))
## changing the datatype to factors if necessary
adult$type_employer<-factor(adult$type_employer)
adult$occupation<-factor(adult$occupation)
adult$country<-factor(adult$country)
adult$education<-factor(adult$education)
adult$marital<-factor(adult$marital)
adult$relationship<-factor(adult$relationship)
adult$race<-factor(adult$race)
adult$sex<-factor(adult$sex)
adult$income<-factor(adult$income)

summary(adult)
## check for na values
adult<-na.omit(adult)
## featured engineering 
## combining extra terms into one 
### DATA CLEANING
table(adult$type_employer)
umemp<-function(job){
  job<-as.character(job)
  if(job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}
adult$type_employer<-sapply(adult$type_employer,umemp)

group_emp <- function(job){
  if (job=='Local-gov' | job=='State-gov'){
    return('SL-gov')
  }else if (job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    return('self-emp')
  }else{
    return(job)
  }
}

adult$type_employer<-sapply(adult$type_employer,group_emp)
## changed table structure
table(adult$type_employer)

## do the same for marital 
table(adult$marital)

group_mar<- function(mar){
  marital<-as.character(mar)
  if(mar=='Divorced'| mar=='Widowed' |mar=='Seperated'){
    return('Not-Married')
  }else if(mar=='Never-married'){
    return(mar)
  }else{
    return('Married')
  }
}
adult$marital<-sapply(adult$marital,group_mar)

## As you see in the str(adult) there are somany levels using continents as groups is my idea to approach this
table(adult$country)

Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

count<-function(cou){
  cou<-as.character(cou)
  if(cou%in%Asia){
    return('Asia')
  }else if(cou%in%North.America){
    return('North-America')
  }else if(cou%in%Europe){
    return('Europe')
  }else if(cou%in%Latin.and.South.America){
    return('Latin and SouthAmerica')
  }else{
    return('Other')
  }
}
adult$country<-sapply(adult$country,count)

str(adult)

## Handling Missing Data
adult[adult=='?']=NA
table(adult$type_employer)

missmap(adult)
missmap(adult,y.at=c(1),y.labels = c(),col=c('Yellow','Black'))

adult<-na.omit(adult)

missmap(adult,y.at=c(1),y.labels=c(),col=c('Yellow','Black'))

## Exploring the data before applying prediction algoritherm
# age 
ggplot(adult,aes(age))+geom_histogram(aes(fill=income),binwidth=1,color='black')+theme_bw()

names(adult)
# hr
ggplot(adult,aes(hr_per_week))+geom_histogram(bins=30)
# education vs age
ggplot(adult,aes(education,age))+geom_boxplot(aes(fill=income))

# renaming the country to regions for ideal levels identification 
names(adult)[names(adult)=="country"] <- "region"
str(adult)

# region 
ggplot(adult,aes(region))+geom_bar(aes(fill=income))

### Starting the model building using logistic regression

# Set a random seed
set.seed(101) 

# splitting the sample data
sample <- sample.split(adult$income, SplitRatio = 0.70) # SplitRatio = percent of sample==TRUE

# Training Data
train = subset(adult, sample == TRUE)

# Testing Data
test = subset(adult, sample == FALSE)

## MODEL CREATION
model.glm<-glm(income~.,family=binomial(link='logit'),data=train)
summary(model.glm)
### More starts menas more significant to predicting the model
new_model<-step(model.glm)
summary(new_model)
test$predicted.income = predict(model.glm, newdata=test, type="response")
test$predicted.income
table(test$income, test$predicted.income > 0.5)

## calculating accuracy
(6375+1422)/(6375+1422+545+873)

## calculating precision
6735/(6375+873)

## calculating recall
6735/(6375+545)

tree <- rpart(income~.,method='class',data =train)
prp(tree)
##Since we are just inferencing it is good a good model because it has ideal recall Value.