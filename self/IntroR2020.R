# useful links
# http://www.r-project.org/
# http://www.rstudio.com/
# http://en.wikibooks.org/wiki/R_Programming
# http://www.statmethods.net/index.html
# http://cran.r-project.org/web/packages/#available-packages-R
# http://www.bioconductor.org/
# https://www.rstudio.com/resources/cheatsheets/
pacman::p_load(tidyverse)
### simple calculations -----
2+5
3*5
15/3 #not 15:3!!
3^2
9^0.5
10%%3 #modulo

### simple data types -----
numeric()
character()
factor(levels=c('m','f'),
       labels=c('male','female'))
logical()
str(1) #str shows structure
str('1')
str(1L) 

### variables ####
result<-9
result
result<-9^(1/2)
result
numbers1<-c(5,3,6,8,2,1)
numbers1
Numbers1 #case sensitivity
(numbers2 <- 1:3) #outer() creates immediate output
numbers1+numbers2 #not stored anywhere
numbers1[2] #[] square brackets indicate an index
numbers1[c(1,2,3)]
numbers1[numbers2] #index can be a variable or calculation
numbers1[1:3]
numbers1[c(1,3)]
numbers1[numbers2[-2]] # index can be nested
numbers1[length(numbers1)]
tail(x=numbers1,n=1)
summ<-numbers1+numbers2
summ
(patients<-c('Pat1','Pat2'))
(patients = paste('Pat',1:6,sep='#'))
(patientslist<-paste('Pat',1:6,sep=' ',collapse=', '))
length(patients)
nchar(patientslist)
length(patientslist)
test<-c(1,2,3,'a','b','c')
print(test)
(test_n<-as.numeric(test))
as.character(numbers2)
truth<-as.logical(c(T,F,TRUE,FALSE,1,0,2,-5e3))
print(truth)
test_na<-is.na(test_n)
str(test_na)   #Structure of a variable
(as.numeric(test_na))
cat(a <- 6)
(c(2,4,6,8)+1) # R (usually) recycles  arguments!
(c(2,4,6,8)+c(1,2))
(c(2,4,6,8)+c(1,2,3))

### functions -----
# FunctionName(parameter1=x1,parameter2=x2,x3,...)
c('my','name')
mean(x=c(3,5,7,NA),na.rm=T) 
mean(na.rm=T,x=c(3,5,7,NA)) 
?mean
mean(c(3,5,7,NA),T)
sd(x = c(3,5,7,NA),na.rm = T)
median(x = 1:100,na.rm = T)
# will become more and more complex in future lessons
floor(as.numeric(as.Date(Sys.Date())-
                    as.Date('1985/12/10'))/
         365.25)

AnimalID<-c('1.1','1.2','1.3','2.1','2.2','1.11')

(StrainID<-gsub(pattern='(\\d+)\\..+',
                replacement='\\1',x=AnimalID))

(AnimalNo<-factor(
   as.numeric(
      gsub(pattern='(\\d+)\\..+',
           replacement='\\1',x=AnimalID))*100+
      as.numeric(
         gsub(pattern='\\d+\\.(\\d+)',
              replacement='\\1',x=AnimalID))))

gsub('(.+)_(.+)','\\2_\\1','Erstes_Zweites')

str_replace(string = 'Erstes_Zweites',
            pattern = '(.+)_(.+)',
            replacement = '\\2_\\1')

'Erstes_Zweites' %>% str_replace('(.+)_(.+)',
                                 '\\2_\\1')


### More complex data types, created by functions -----
# Matrix: 2 dimensions, 1 data type 
my1.Matrix<-
   matrix(data=1:12,
          nrow=4,byrow=T,
          dimnames=list(paste0('row',1:4),
                        paste0('col',1:3)))
print(my1.Matrix)
data <- 1:100
nrow <- 20
matrix(data=data,
       nrow=nrow,
       byrow=T,
       dimnames=list(paste0('row',1:nrow),
                     paste0('col',1:(length(data)/nrow))))

my1.Matrix[2,3]   #Index:[row,column]
my1.Matrix[2,]
my1.Matrix[,2]
my1.Matrix[c(1,3),-2]
my1.Matrix[1,1]<-NA
mdat <- matrix(c(1,2,3, 11,12,13),
               nrow = 2, ncol=3) #byrow=TRUE,
mdat

# Data frame: 2 dimensions, various data types (1 per columns)
patientN<-15
options(stringsAsFactors =FALSE)
(myTable<-data.frame(
   patientCode=paste0('pat',1:patientN),
   Var1=1,
   Var2=NA))

myTable<-data.frame(
   patientCode=paste0('pat',1:patientN),
   Age=round(runif(n=patientN,min=18,max=65)),
   Sex=factor(rep(x=NA,times=patientN),
              levels=c('m','f')),
   sysRR=round(rnorm(n=patientN,mean=140,sd=10)))

head(myTable)



myTable[,1]
myTable$patientCode
myTable[,'patientCode']
spalten<-c('Sex','Age')
myTable[,spalten]
myTable[,c('Sex','Age')]
# Wenn die Zahl und die Namen der Spalten eines Datenframes
# erst zur Laufzeit des Scripts bekannt sind, kann als
#Zwischenschritt eine Matrix erzeugt werden. 
# Diese Matrix wird in einen Datenframe umgewandelt, die
# Namen der Spalten werden nachträglich durch das Script
# vergeben, z.B. aus den Faktorstufen einer 
# Gruppierungsvariable
# test<-as.data.frame(
#    matrix(ncol=nlevels(myTable$Sex)+1,
#           nrow=patientN))
# colnames(test)<-c('Var',
#                   paste('Mean',
#                         levels(myTable$Sex)))

myTable$Sex[]<-sample(x=c('m','f',NA),
                    size=patientN,
                    prob = c(.4,.4,.2),
                    replace=T)
str(myTable)


myTable[,1]<-paste0('Code',1:patientN)

str(myTable)
myTable[c(1,3,4),c(1,4)]
myTable$patientCode
myTable[,'patientCode']
myTable[,1]

#lists
shopping<-list(beverages=c('beer','water',
                           'gin(not Gordons!!)','tonic'),
               snacks=c('chips','pretzels'),
               nonfood=c('DVDs','Akku'),
               mengen=1:10)
shopping
shopping$snacks
shopping[1]    #returns a list
shopping[[1]]  #returns a vector
str(shopping[1])
str(shopping[[1]])
str(shopping$beverages)
shopping[1][2]
shopping[[1]][2]  
shopping$beverages[2]

# tibble ####
pacman::p_load(wrappedtools, randomNames)
patientN <- 200
rawdata <- tibble(
   PatID=paste('P',1:patientN), #wie bei data.frame
   Sex=sample(x = c('male','female'),
              size = patientN,replace = T,
              prob = c(.7,.3)),
   Ethnicity=sample(1:6,patientN,T,c(.01,.01,.05,.03,.75,.15)),
   `Given name`=randomNames(n = patientN,
                            gender = Sex,
                            ethnicity = Ethnicity,
                            which.names = 'first'),
   `Family name`=randomNames(n = patientN,
                             ethnicity = Ethnicity,
                             which.names = 'last'),
   Treatment=sample(c('Placebo','Verum'),patientN,T),
   sysRR=round(rnorm(n=patientN,mean=140,sd=10))-
      (Treatment=='Verum')*15,
   diaRR=round(rnorm(n=patientN,mean=80,sd=10))-
      (Treatment=='Verum')*10,
   HR=round(rnorm(n=patientN,mean=90,sd=7)))
rawdata
cn()
rawdata %<>% 
   mutate(Ethnicity=factor(
      Ethnicity,levels = 1:6,
      labels=	c(
         'American Indian or Native Alaskan',
         'Asian or Pacific Islander',
         'Black (not Hispanic)',
         'Hispanic',
         'White (not Hispanic)',
         'Middle-Eastern, Arabic')))
ggplot(rawdata,aes(x = Treatment,y = sysRR))+
   geom_boxplot()
rawdata[1:5,1:2]
rawdata[,6]
rawdata[6]
rawdata[[6]]
rawdata$`Family name`

#tibble and [ always returns tibble
#tibble and [[ always returns vector
#data.frame and [ may return data.frame or vector
#data.frame and [[ always returns vector
rawdata_df <- as.data.frame(rawdata)
rawdata[2] #returns Tibble with 1 column
rawdata[[2]] #returns vector
rawdata[,2] #returns Tibble with 1 column
rawdata[,2:3] #returns tibble with 2 columns
rawdata_df[2] #returns DF with 1 column
rawdata_df[[2]] #returns vector
rawdata_df[,2] #returns vector
rawdata_df[,2:3] #returns DF with 2 columns

rawdata %>% select(PatID:Ethnicity,sysRR:HR)
rawdata %>% select(PatID:Ethnicity,sysRR:HR) %>% slice(1:5)
rawdata %>% select(contains('RR'))
rawdata %>% select(ends_with('R'))
rawdata %>% select(-contains('name'))
rawdata %>% select(sysRR)
rawdata %>% pull(sysRR)

testvars <- FindVars(varnames = c('Eth','Sex','R'))
normvars <- FindVars(varnames = c('R'))
for(var_i in 3:5){
print(ggplot(rawdata,aes_string(x = 'Treatment',
                          y = testvars$bticked[var_i]))+
   geom_boxplot())
}
for(var_i in testvars$bticked[3:5]){
   print(ggplot(rawdata,aes_string(x = 'Treatment',
                                   y = var_i))+
            geom_boxplot())
}
#compare2numvars()
### functions ####
#FunctionName<-function(parameters...){definition}
markSign<-function(SignIn)
{
   SignIn <- as.numeric(SignIn)
   SignOut<-' '
   if (!is.na(SignIn))
   {
      SignOut<-'n.s.'
      if (SignIn<=0.1) {SignOut<-'+'}
      if (SignIn<=0.05) {SignOut<-'*'}
      if (SignIn<=0.01) {SignOut<-'**'}
      if (SignIn<=0.001) {SignOut<-'***'}
   }
   return(SignOut)
}

markSign(SignIn=0.035)
markSign('p=3,5%')   #wrong parameter

Mymean<-function(werte)
{
   return(base::mean(werte,na.rm=T))
}

### loops ####
print('### Game of Loops ###')
for(season_i in 1:3) {
   print(paste('GoL Season',season_i))
   for(episode_i in 1:5)
   {
      print(paste0('   GoL S.',season_i,
                   ' Episode ',episode_i))
   }
   cat('\n')
}
test <- 0
while(test<100){
   print(test)
   test  %<>% +1
}

### conditions -----
sex<-'male'
if (sex=='male') {
   print('männlich')
} else {
   print('weiblich')
}

if (sex!='male') {
   print('nicht männlich')
} else {
   print('männlich')
}
if (!sex=='male') {
   print('nicht männlich')
} else {
   print('männlich')
}

print(ifelse(test = sex=='male',
             yes = 'männlich',
             no = 'weiblich'))

p <- .0012
paste('Das ist',
      ifelse(p<.05,'','nicht'),
      'signifikant')


### useful tools -----
?strtrim
?substr
?grep
?grepl
?paste
?any
?'%in%'
2%in%1:5
2%in%c(1,3,4,5)

