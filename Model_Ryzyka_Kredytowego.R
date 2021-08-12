#Poczatkowe ustawienia
packages_vector <- c("lmtest", "zoo", "lattice", "pROC", "forcats", "RColorBrewer", "devtools", "smbinning", 
                     "sqldf", "ggplot2", "scales", "Formula", "partykit", "plyr", "dplyr", "caTools", "tidyr", "gridExtra", "pcaPP", "ggrepel","readr", "woeBinning", "caTools", "magrittr","vcd","forcats","pROC","pcaPP","corrplot","InformationValue"
                     ,"gtools","LogisticDx","randomForest","caret")

package.check <- lapply(packages_vector, FUN = function(x) {
  if(!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})
path<-getwd()
# Ustawienia obszaru wykresu
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}

par(resetPar()) 
par(xpd = T, mfrow=c(1,1))

#Wczytanie danych
dane<-read.csv(paste(path,"/data/application_data.csv",sep = ""),sep = ",",header=T)
dane %>% as_tibble()
str(dane)
colSums(is.na(dane)) 

#Analiza poszczegolnych zmiennych 

colnames(dane)[1]<-'ID'

colnames(dane)[2]<-'DEF'
prop.table(table(dane$DEF))
table(dane$DEF)
#Typ_kredytu
colnames(dane)[3]<-"LOAN_TYPE"
dane$LOAN_TYPEF<-factor(dane$LOAN_TYPE,levels = c("Cash loans","Revolving loans"),labels=c("CASH","REVOLVING"))

#Plec
colnames(dane)[4]<-"SEX"
sum(is.na(dane$SEX)) #nie ma brakow danych
table(dane$SEX)
dane<-dane[!(dane$SEX=="XNA"),]
table(dane$SEX)
dane$SEXF <- factor(dane$SEX, levels = c("M","F"), labels = c("male", "female"),)
prop.table(table(dane$SEXF,dane$DEF),1)

#czy posiada samochod
colnames(dane)[5]<- "CAR"
sum(is.na(dane$CAR)) #nie ma brakow danych
table(dane$CAR)
dane$CARF <- factor(dane$CAR, levels = c("N","Y"), labels = c("NO", "YES"))
prop.table(table(dane$CARF,dane$DEF),1)

#Czy posiada nieruchomosc
colnames(dane)[6]<- "PROPERTY"
sum(is.na(dane$PROPERTY)) #nie ma brakow danych
table(dane$PROPERTY)
dane$PROPERTYF <- factor(dane$PROPERTY, levels = c("N","Y"), labels = c("NO", "YES"))
prop.table(table(dane$PROPERTYF,dane$DEF),1)

#LICZBA DZIECI
colnames(dane)[7]<- "NUM_CHILDREN"
table(dane$NUM_CHILDREN)
#Polaczmy 3+ dzieci w jedna grupe
dane$NUM_CHILDREN<-ifelse(dane$NUM_CHILDREN>=3,3,dane$NUM_CHILDREN)
dane$NUM_CHILDRENF <- factor(dane$NUM_CHILDREN, levels = c(0,1,2,3 ), labels = c("NO_CHILD","ONE_CHILD","TWO_CHILD","THREE_CHILD+"),)
#DOCHOD
colnames(dane)[8]<- "INCOME"
#cena_dobra
colnames(dane)[11]<- "GOOD_PRICE"

#czy kredyt wspolny
colnames(dane)[12]<- "JOINT_LOAN"
table(dane$JOINT_LOAN)
#Zamienmy na odpowiedz ze wspolny badz nie
dane$JOINT_LOAN<-ifelse(dane$JOINT_LOAN %in% c("","Unaccompanied"),0,1)
dane$JOINT_LOANF<-factor(dane$JOINT_LOAN, levels = c(0,1 ), labels = c("NO","YES"))

prop.table(table(dane$JOINT_LOANF,dane$DEF),1)
#ZRODLO DOCHODU
colnames(dane)[13]<- "SOURCE_INCOME"
sum(is.na(dane$SOURCE_INCOME)) #nie ma brakow danych

table(dane$SOURCE_INCOME)
#POLACZMY NIEKTORE GRUPY ZE WZGLEDU NA MALE LICZNOSCI
dane$SOURCE_INCOME<-ifelse(dane$SOURCE_INCOME %in% c("Unemployed","Student","Maternity leave","Pensioner"),"Unemployed",dane$SOURCE_INCOME)
dane$SOURCE_INCOME<-ifelse(dane$SOURCE_INCOME %in% c("Businessman","Commercial associate"),"Commercial associate",dane$SOURCE_INCOME)
dane$SOURCE_INCOMEF<-as.factor(dane$SOURCE_INCOME)
prop.table(table(dane$SOURCE_INCOMEF,dane$DEF),1)
#Poziom wyksztalcenia
colnames(dane)[14]<- "EDUCATION"
sum(is.na(dane$EDUCATION)) #nie ma brakow
table(dane$EDUCATION)
#Polaczmy academic degree z higher education
dane$EDUCATION<-ifelse(dane$EDUCATION %in% c("Academic degree","Higher education"),"Higher education",dane$EDUCATION)
dane$EDUCATIONF<-factor(dane$EDUCATION, levels =c("Secondary / secondary special", "Higher education", "Incomplete higher","Lower secondary" ),labels = c("Secondary","Higher","Incomplete higher","Lower secondary"))
prop.table(table(dane$EDUCATIONF,dane$DEF),1)
#Stan cywilny
colnames(dane)[15]<- "CIVIL_STATE"
sum(is.na(dane$CIVIL_STATE))# nie ma brakow
table(dane$CIVIL_STATE)
#Usunmy 2 unknown obserwacje
dane<-dane[!(dane$CIVIL_STATE=="Unknown"),]
dane$CIVIL_STATEF<-as.factor(dane$CIVIL_STATE)
prop.table(table(dane$CIVIL_STATEF,dane$DEF),1)

#Jak mieszka
colnames(dane)[16]<- "HOUSING_TYPE"
sum(is.na(dane$HOUSING_TYPE)) # nie ma brakow
table(dane$HOUSING_TYPE)
unique(dane$HOUSING_TYPE)
dane$HOUSING_TYPEF<-factor(dane$HOUSING_TYPE,levels=c("House / apartment","Rented apartment","With parents", "Municipal apartment","Office apartment","Co-op apartment" ),labels = c("House","Rented","With parents", "Municipal apart.","Office apart.","Co-op apart."))
prop.table(table(dane$HOUSING_TYPEF,dane$DEF),1)

#Region
colnames(dane)[17]<-"REGION"
summary(dane$REGION)

#Data urodzenia w dniach od zlozenia aplikacji
summary(dane$DAYS_BIRTH)

#okres zatrudnienia  od zlozenia aplikacji
summary(dane$DAYS_EMPLOYED)
dane$DAYS_EMPLOYED<-ifelse(dane$DAYS_EMPLOYED >=0,0,dane$DAYS_EMPLOYED)

#Days_registration
summary(dane$DAYS_REGISTRATION)

#DAYS_ID_PUBLISH - LICZBA DNI OD OSTATNIEJ ZMIANY DOKUMENTU TOZSAMOSCI
colnames(dane)[21]<-"DAYS_ID_CHANGE"
summary(dane$DAYS_ID_CHANGE)

#Wiek samochodu
colnames(dane)[22]<-"CAR_AGE"

#Czy aplikujacy ma telefon komorkowy
colnames(dane)[23]<-"MOBILE_PHONE"
table(dane$MOBILE_PHONE)
#Usuwamy zmienna poniewaz tylko jedna osoba nie posiada telefonu i w tym wypadku zmienna ta nie jestes w stanie roznicować aplikujacych
dane<-dane[,-c(23)]

#czy aplikujacy ma telefon sluzbowy
colnames(dane)[23]<-"EMP_MOBILE_PHONE"
table(dane$EMP_MOBILE_PHONE)
dane$EMP_MOBILE_PHONEF<-factor(dane$EMP_MOBILE_PHONE,levels=c(0,1),labels = c("NO",'YES'))
prop.table(table(dane$EMP_MOBILE_PHONEF,dane$DEF),1)

#czy telefon domowy
colnames(dane)[24]<-"HOME_PHONE"
sum(is.na(dane$HOME_PHONE))
table(dane$HOME_PHONE)
dane$HOME_PHONEF<-factor(dane$HOME_PHONE,levels=c(0,1),labels = c("NO",'YES'))
prop.table(table(dane$HOME_PHONEF,dane$DEF),1)

#Flag_cont_mobile
table(dane$FLAG_CONT_MOBILE)
# tak jak w przypadku mobile_phone zmienna bedzie w niewielkim stopniu roznicować dlatego w celu redukcji wymiaru pozbywamy sie jej
dane<-dane[,-c(25)]

#Flag_phone
table(dane$FLAG_PHONE)
sum(is.na(dane$FLAG_PHONE)) #NIE MA BRAKOW
dane$FLAG_PHONEF<-factor(dane$FLAG_PHONE,levels=c(0,1),labels = c("NO",'YES'))
prop.table(table(dane$FLAG_PHONEF,dane$DEF),1)

#cZY KLIENT POSIADA MAIL
table(dane$FLAG_EMAIL)
colnames(dane)[26]<-"EMAIL"
dane$EMAILF<-factor(dane$EMAIL,levels=c(0,1),labels = c("NO",'YES'))
prop.table(table(dane$EMAILF,dane$DEF),1)

#STANOWISKO
colnames(dane)[27]<-"JOB"
sum(is.na(dane$JOB)) # nie ma brakow
table(dane$JOB)
#polaczmy it z high skill tech staff
dane$JOB<-ifelse(dane$JOB =="IT staff","High skill tech staff",dane$JOB)
#a puste miejsca oznaczmy jako brak danych
dane$JOB<-ifelse(dane$JOB =="","Missing",dane$JOB)
dane$JOBF<-as.factor(dane$JOB)
prop.table(table(dane$JOBF,dane$DEF),1)

#liczba czlonkow rodziny
sum(is.na(dane$CNT_FAM_MEMBERS)) # nie ma brakow
table(dane$CNT_FAM_MEMBERS)
#polaczmy dane 5+ czlonkow w jedna grupe
dane$CNT_FAM_MEMBERS<-ifelse(dane$CNT_FAM_MEMBERS>=5,5,dane$CNT_FAM_MEMBERS)
dane$CNT_FAM_MEMBERSF <- factor(dane$CNT_FAM_MEMBERS, levels = c(1,2,3,4,5 ), labels = c("ONE_PERSON","TWO_PEOPLE","THREE_PEOPLE","FOUR_PEOPLE","FIVE_PEOPLE+"))
prop.table(table(dane$CNT_FAM_MEMBERSF,dane$DEF),1)

#ocena regionu wg. klienta
colnames(dane)[29]<-"REGION_RATE"
sum(is.na(dane$REGION_RATE)) # nie ma brakow
table(dane$REGION_RATE)
dane$REGION_RATEF<-as.factor(dane$REGION_RATE)
prop.table(table(dane$REGION_RATEF,dane$DEF),1)

#ocena regionu z uwzgledniem miasta
#sprawdzmy czy ta zmienna nie jest silnie skorelowana z poprzednia i nie mowi tego samego
assocstats(table(dane$REGION_RATE,dane$REGION_RATING_CLIENT_W_CITY))
#Statystyka pokazuje ze korelacja wynosi 0.96 czyli zmienne posiadaja prawie identyczne informacje roznicujace
#Usuwamy zmienna
dane<-dane[,-c(30)]

#W KTORYM DNIU ZLOZONY WNIOSEK
colnames(dane)[30]<-"WEEKDAY_START"
sum(is.na(dane$WEEKDAY_START)) # nie ma brakow
dane$WEEKDAY_STARTF<-as.factor(dane$WEEKDAY_START)
prop.table(table(dane$WEEKDAY_STARTF,dane$DEF),1)

#O KTOREJ GODZINIE ZLOZONO WNIOSEK
colnames(dane)[31]<-"HOUR_START"
sum(is.na(dane$HOUR_START)) # nie ma brakow
prop.table(table(dane$HOUR_START,dane$DEF),1)

#Zmienna mowiaca o typie organizacji kredytobiorcy
sum(is.na(dane$ORGANIZATION_TYPE)) # nie ma brakow danych
table(dane$ORGANIZATION_TYPE)

#Pogrupujmy obserwacje
for(i in 1:13){
  dane$ORGANIZATION_TYPE<-gsub(paste("Industry: type ",i,sep=""),"Industry",dane$ORGANIZATION_TYPE)
  
}
for(i in 0:3){
  dane$ORGANIZATION_TYPE<-gsub(paste("Industry",i,sep=""),"Industry",dane$ORGANIZATION_TYPE)
  
}
for(i in 1:3){
  dane$ORGANIZATION_TYPE<-gsub(paste("Business Entity Type ",i,sep=""),"Business Entity",dane$ORGANIZATION_TYPE)
  
}
for(i in 1:7){
  dane$ORGANIZATION_TYPE<-gsub(paste("Trade: type ",i,sep=""),"Trade",dane$ORGANIZATION_TYPE)
  
}
for(i in 1:4){
  dane$ORGANIZATION_TYPE<-gsub(paste("Transport: type ",i,sep=""),"Transport",dane$ORGANIZATION_TYPE)
  
}
dane$ORGANIZATION_TYPE<-gsub("XNA","Other",dane$ORGANIZATION_TYPE)

table(dane$ORGANIZATION_TYPE)
dane$ORGANIZATION_TYPEF<-as.factor(dane$ORGANIZATION_TYPE)
as.numeric(prop.table(table(dane$ORGANIZATION_TYPEF,dane$DEF),1))

#Zmienne od 39 do 92
colSums(is.na(dane[,39:92]))
#Nalezy sie zastanowic czy zmienne dotyczace mieszkania kredytobiorcy sa istotne,
#poniewaz wystepuja powazne braki danych w tej czesci
#Zmienna mowiaca o liczbie dni od zmiany telefonu
colnames(dane)[93]<-"DAYS_PHONE_CHANGE"
summary(dane$DAYS_PHONE_CHANGE)
dane$DAYS_PHONE_CHANGE[is.na(dane$DAYS_PHONE_CHANGE)]<-mean(dane$DAYS_PHONE_CHANGE,na.rm = TRUE)

#Zmienne dotyczace czy kredytobiorca dostarczy dokument
colSums(is.na(dane[,94:113]))
#nie ma brakow danych

for(i in 2:21){
  dane[,paste("DOCUMENT",i,"F",sep="")]<-factor(dane[,paste("FLAG_DOCUMENT_",i,sep="")],levels = c(0,1),labels=c("NO","YES"))
}

#ZMIENNE DOTYCZACE LICZBY ZAPYTAN NA TEMAT KREDYTOBIORCY W BIURZE KREDYTOWYM
colSums(is.na(dane[,114:119]))
#NARAZIE ZOSTAWMY TE ZMIENNE TAKIE JAKIE SA


#PODZIELMY ZMIENNE NA FAKTOROWE I NUMERYCZNE
str(dane)
dane<-dane %>% mutate_if(is.character,as.factor)
dane<-dane %>% mutate_if(is.integer,as.numeric)
str(dane)
columns<-colnames(dane)

baza<-dane[,columns]

nums <- sapply(baza, is.numeric ) 
baza.n<-baza[,nums]

fact <- sapply(baza, is.factor)
baza.f<-baza[,fact]

#Pozbadzmy sie powtorzonych kolumn
baza.f<-baza.f[,-c(1:11)]
baza.n<-baza.n[,-c(1:3,8,15:20,79:98)]


#Dyskretyzacja
percentile<-apply(X=baza.n, MARGIN=2, FUN=function(x) round(quantile(x, seq(0.1,1,0.1), na.rm=TRUE),2))
unique<-apply(baza.n, MARGIN=2, function(x) length(unique(x)))
numeric<-colnames(baza.n[unique>=10])
num_as_fact<-colnames(baza.n[which(unique<10 & unique>1 )])

baza.f <- bind_cols(baza.f, baza.n[num_as_fact])
baza.n <- baza.n[,-which(unique<10 & unique>1 )]

#Do bazy faktorowej zostaly dodane zmienne dotyczace adresu zamieszkania klienta i ich powiazan z praca, miejscem podpisania kredytu itp
#oraz jedna zmienna dotyczaca ilosci zapytań o kredytobiorce w biurze kredytowym w ciagu ostatniej godziny
#Przeanalizujmy zmienne faktorowe, które zostaly pominiete w poczatkowej analizie

#Zmienne dotyczace budynku w ktorym mieszka kredytobiorca
colSums(is.na(baza.f[,1:3])) #brak problemu z brakami danych

table(baza.f[,1])
baza.f$FONDKAPREMONT_MODEF<-as.factor(baza.f$FONDKAPREMONT_MODE)
baza.f$FONDKAPREMONT_MODEF<-ifelse(baza.f$FONDKAPREMONT_MODEF=="","Missing",baza.f$FONDKAPREMONT_MODEF)
baza.f$FONDKAPREMONT_MODEF<-factor(baza.f$FONDKAPREMONT_MODEF,levels=c(2,3,4,5,"Missing"),labels=c("not specified","org spec account", "reg oper account","reg oper spec account","Missing"))

prop.table(table(baza.f$FONDKAPREMONT_MODEF,baza$DEF),1)
baza.f<-baza.f[,-c(1)]


table(baza.f$HOUSETYPE_MODE)
baza.f$HOUSETYPE_MODEF<-as.factor(baza.f$HOUSETYPE_MODE)
baza.f$HOUSETYPE_MODEF<-ifelse(baza.f$HOUSETYPE_MODEF=="","Missing",baza.f$HOUSETYPE_MODEF)
baza.f$HOUSETYPE_MODEF<-factor(baza.f$HOUSETYPE_MODEF,levels=c(2,3,4,"Missing"),labels=c("block of flats","specific housing", "terraced house","Missing"))

prop.table(table(baza.f$HOUSETYPE_MODEF,baza$DEF),1)
baza.f<-baza.f[,-c(1)]

table(baza.f$WALLSMATERIAL_MODE)
baza.f$WALLSMATERIAL_MODEF<-as.factor(baza.f$WALLSMATERIAL_MODE)
baza.f$WALLSMATERIAL_MODEF<-ifelse(baza.f$WALLSMATERIAL_MODEF=="","Missing",baza.f$WALLSMATERIAL_MODEF)
baza.f$WALLSMATERIAL_MODEF<-factor(baza.f$WALLSMATERIAL_MODEF,levels=c(2,3,4,5,6,7,8,"Missing"),labels=c("Block","Mixed", "Monolithic","Others","Panel","Stone,brick","Wooden","Missing"))
prop.table(table(baza.f$WALLSMATERIAL_MODEF,baza$DEF),1)
baza.f<-baza.f[,-c(1)]

table(baza.f$EMERGENCYSTATE_MODE)
baza.f$EMERGENCYSTATE_MODEF<-as.factor(baza.f$EMERGENCYSTATE_MODE)
baza.f$EMERGENCYSTATE_MODEF<-ifelse(baza.f$EMERGENCYSTATE_MODEF=="","Missing",baza.f$EMERGENCYSTATE_MODEF)
baza.f$EMERGENCYSTATE_MODEF<-factor(baza.f$EMERGENCYSTATE_MODEF,levels=c(2,3,"Missing"),labels=c("NO","YES","Missing"))
prop.table(table(baza.f$EMERGENCYSTATE_MODEF,baza$DEF),1)
baza.f<-baza.f[,-c(1)]

#Zmienne 0-1 mowiace o adresie zamieszkania kredytobiorcy
colSums(is.na(baza.f[,40:45])) # nie ma problemow z brakami danych

for(i in 40:45){
  baza.f[,paste(colnames(baza.f)[i],"F",sep="")]<-factor(baza.f[,i],levels=c(0,1),labels=c("NO","YES"))
}
baza.f<-baza.f[,-c(40:45)]

#Zmienna mowiaca o liczbie zapytan na temat kredytobiorcy w ciagu ostatniej godziny (polciagla)
table(baza.f$AMT_REQ_CREDIT_BUREAU_HOUR)
table(baza$AMT_REQ_CREDIT_BUREAU_QRT)
#Po przYjrzeniu sie blizej tym zmiennym uznalismy ze warto je pogrupowac i potraktowac jak faktory
sum(is.na(baza.f$AMT_REQ_CREDIT_BUREAU_HOUR))

baza.f$AMT_REQ_CREDIT_BUREAU_HOUR<-ifelse(is.na(baza.f$AMT_REQ_CREDIT_BUREAU_HOUR),"Missing",baza.f$AMT_REQ_CREDIT_BUREAU_HOUR)
baza.f$AMT_REQ_CREDIT_BUREAU_HOUR<-ifelse(baza.f$AMT_REQ_CREDIT_BUREAU_HOUR %in% c(1:4),1,baza.f$AMT_REQ_CREDIT_BUREAU_HOUR)
baza.f$AMT_REQ_CREDIT_BUREAU_HOURF<-factor(baza.f$AMT_REQ_CREDIT_BUREAU_HOUR,levels=c(0,1,"Missing"),labels=c("No_enquiries","One+","Missing"))
table(baza.f$AMT_REQ_CREDIT_BUREAU_HOURF)
prop.table(table(baza.f$AMT_REQ_CREDIT_BUREAU_HOURF,baza$DEF),1)
baza.f<-baza.f[,-c(40)]

for(i in 63:67){
  baza.f[,paste(colnames(baza.n[i]),"F",sep="")]<-baza.n[,i]
}
colSums(is.na(baza.f[,51:55]))
#Sa braki danych, wypelnijmy je
for(i in 51:55){
  baza.f[,i]<-ifelse(is.na(baza.f[,i]),"Missing",baza.f[,i])
}
colSums(is.na(baza.f[,51:55]))

table(baza.f[,51])
baza.f[,51]<-ifelse(baza.f[,51] %in% c(1:9),1,baza.f[,51])
baza.f[,51]<-factor(baza.f[,51],levels =c(0,1,"Missing"),labels=c("No_enquiries","One+","Missing") )

table(baza.f[,52])
baza.f[,52]<-ifelse(baza.f[,52] %in% c(1:8),1,baza.f[,52])
baza.f[,52]<-factor(baza.f[,52],c(0,1,"Missing"),labels=c("No_enquiries","One+","Missing"))

table(baza.f[,53])
baza.f[,53]<-ifelse(baza.f[,53] %in% c(3:27),3,baza.f[,53])
baza.f[,53]<-factor(baza.f[,53],c(0,1,2,3,"Missing"),labels=c("No_enquiries","One","Two","Three+","Missing"))

table(baza.f[,54])
baza.f[,54]<-ifelse(baza.f[,54] %in% c(3:261),3,baza.f[,54])
baza.f[,54]<-factor(baza.f[,54],c(0,1,2,3,"Missing"),labels=c("No_enquiries","One","Two","Three+","Missing"))

table(baza.f[,55])
baza.f[,55]<-ifelse(baza.f[,55] %in% c(6:261),6,baza.f[,55])
baza.f[,55]<-factor(baza.f[,55],c(0,1,2,3,4,5,6,"Missing"),labels=c("No_enquiries","One","Two","Three","Four","Five","Six+","Missing"))

#sprawdzmy czy wszystkie zmienne w bazie baza.f sa factorami
str(baza.f)

#Usunmy powyzsze zmienne z bazy numeric
baza.n<-baza.n[,-c(63:67)]
numeric<-colnames(baza.n)

#Sprawdzmy baze numeric
colSums(is.na(baza.n))




# ----------------------- BINARYZACJA -----------------------------
options(scipen=999)

for (m in numeric)
{
  baza.n[,paste(m,"_fine", sep="")]<-cut(
    x=as.matrix(baza.n[m]), 
    breaks=c(-Inf,unique(percentile[,m])), 
    labels =c(paste("<=",unique(percentile[,m])))
  ) 
}
#Tworzymy zmienna def_woe w ktorej odwracamy znaczenie defaultu
baza.n$def_woe<-(1- dane$DEF)
baza.n$def<-dane$DEF

# #Zbadajmy baza.n
colSums(is.na(baza.n[,grepl(pattern="_fine" , x=names(baza.n))]))

#Zarzadzmy brakami danych
summary(baza.n$AMT_ANNUITY_fine)
#Ze wzgledu, ze braki stanowia niewielki procent danych nie powinno to wplynac na wyniki wiec dodajmy ja przykladowo do ostatniej grupy
baza.n$AMT_ANNUITY_fine <- as.character(baza.n$AMT_ANNUITY_fine)
baza.n$AMT_ANNUITY_fine[is.na(baza.n$AMT_ANNUITY_fine)] <- "<= 258025.5"
baza.n$AMT_ANNUITY_fine <- as.factor(baza.n$AMT_ANNUITY_fine)

#cena dobra
summary(baza.n$GOOD_PRICE_fine)
#Dodajmy puste obserwacje do srodkowej obserwacji
baza.n$GOOD_PRICE_fine <- as.character(baza.n$GOOD_PRICE_fine)
baza.n$GOOD_PRICE_fine[is.na(baza.n$GOOD_PRICE_fine)] <- "<= 522000"
baza.n$GOOD_PRICE_fine <- as.factor(baza.n$GOOD_PRICE_fine)

#Region_fine
summary(baza.n$REGION_fine)
#W tym przypadku stworzmy oddzielna grupe danych Missing
baza.n$REGION_fine<- as.character(baza.n$REGION_fine)
baza.n$REGION_fine[is.na(baza.n$REGION_fine)] <- "<= 0.07"
baza.n$REGION_fine <- as.factor(baza.n$REGION_fine)

#wiek samochodu
summary(baza.n$CAR_AGE_fine)
baza.n$CAR_AGE_fine<- as.character(baza.n$CAR_AGE_fine)
baza.n$CAR_AGE_fine[is.na(baza.n$CAR_AGE_fine)] <- "<= 91"
baza.n$CAR_AGE_fine <- as.factor(baza.n$CAR_AGE_fine)

#ZEWNETRZNE INFORMACJE NA TEMAT KLIENTA
summary(baza.n$EXT_SOURCE_1_fine)
baza.n$EXT_SOURCE_1_fine<- as.character(baza.n$EXT_SOURCE_1_fine)
baza.n$EXT_SOURCE_1_fine[is.na(baza.n$EXT_SOURCE_1_fine)] <- "<= 0.21"
baza.n$EXT_SOURCE_1_fine <- as.factor(baza.n$EXT_SOURCE_1_fine)

summary(baza.n$EXT_SOURCE_2_fine)
baza.n$EXT_SOURCE_2_fine<- as.character(baza.n$EXT_SOURCE_2_fine)
baza.n$EXT_SOURCE_2_fine[is.na(baza.n$EXT_SOURCE_2_fine)] <- "<= 0.22"
baza.n$EXT_SOURCE_2_fine <- as.factor(baza.n$EXT_SOURCE_2_fine)

summary(baza.n$EXT_SOURCE_3_fine)
baza.n$EXT_SOURCE_3_fine<- as.character(baza.n$EXT_SOURCE_3_fine)
baza.n$EXT_SOURCE_3_fine[is.na(baza.n$EXT_SOURCE_3_fine)] <- "<= 0.23"
baza.n$EXT_SOURCE_3_fine <- as.factor(baza.n$EXT_SOURCE_3_fine)

#Sprawdzmy czy zmienne te nie sa ze soba scisle skorelowane
assocstats(table(baza.n$EXT_SOURCE_2_fine,baza.n$EXT_SOURCE_3_fine))
assocstats(table(baza.n$EXT_SOURCE_1_fine,baza.n$EXT_SOURCE_3_fine))
assocstats(table(baza.n$EXT_SOURCE_2_fine,baza.n$EXT_SOURCE_1_fine))

#Pomiedzy zmiennymi nie wystepuja korelacje

#Kolejne zmienne dotycza informacji na temat budynku i okolicy w ktorym mieszka kredytobiorca
#ze wzgledu na liczne braki danych oraz nieintuicyjna interpretacje oraz w celu redukcji wymiaru i ograniczenie zlozonosci analizy
#usuwamy ta partie zmiennych
baza.n<-baza.n[-c(15:57,77:119)]


#Ostatnia czesc zmiennych mowi o spolecznosci kredytobiorcy ktorzy mieli default w okreslony dpd
#Sprwadzmy czy zmienne te nie sa ze soba skorelowane
assocstats(table(baza.n$OBS_30_CNT_SOCIAL_CIRCLE_fine,baza.n$OBS_60_CNT_SOCIAL_CIRCLE_fine))
assocstats(table(baza.n$DEF_30_CNT_SOCIAL_CIRCLE_fine,baza.n$DEF_60_CNT_SOCIAL_CIRCLE_fine))
assocstats(table(baza.n$OBS_30_CNT_SOCIAL_CIRCLE_fine,baza.n$DEF_30_CNT_SOCIAL_CIRCLE_fine))
assocstats(table(baza.n$OBS_30_CNT_SOCIAL_CIRCLE_fine,baza.n$DEF_60_CNT_SOCIAL_CIRCLE_fine))
assocstats(table(baza.n$OBS_60_CNT_SOCIAL_CIRCLE_fine,baza.n$DEF_30_CNT_SOCIAL_CIRCLE_fine))
assocstats(table(baza.n$OBS_60_CNT_SOCIAL_CIRCLE_fine,baza.n$DEF_60_CNT_SOCIAL_CIRCLE_fine))

#Mocno skorelowane sa ze soba zmienne obs_30 i obs_60 oraz def_30 i def_60, usunmy zmienne obs_30 oraz def_30
baza.n<-baza.n[-c(15:16,34:35)]

#Zajmijmy sie brakami w pozostalych 2 zmiennych
summary(baza.n$OBS_60_CNT_SOCIAL_CIRCLE_fine)
baza.n$OBS_60_CNT_SOCIAL_CIRCLE_fine<- as.character(baza.n$OBS_60_CNT_SOCIAL_CIRCLE_fine)
baza.n$OBS_60_CNT_SOCIAL_CIRCLE_fine[is.na(baza.n$OBS_60_CNT_SOCIAL_CIRCLE_fine)] <- "<= 344"
baza.n$OBS_60_CNT_SOCIAL_CIRCLE_fine <- as.factor(baza.n$OBS_60_CNT_SOCIAL_CIRCLE_fine)

summary(baza.n$DEF_60_CNT_SOCIAL_CIRCLE_fine)
baza.n$DEF_60_CNT_SOCIAL_CIRCLE_fine<- as.character(baza.n$DEF_60_CNT_SOCIAL_CIRCLE_fine)
baza.n$DEF_60_CNT_SOCIAL_CIRCLE_fine[is.na(baza.n$DEF_60_CNT_SOCIAL_CIRCLE_fine)] <- "<= 24"
baza.n$DEF_60_CNT_SOCIAL_CIRCLE_fine <- as.factor(baza.n$DEF_60_CNT_SOCIAL_CIRCLE_fine)


# lista dla informacji WOE
WOE<-list()

#baza danych dla information value
IV<-data.frame(VAR=character(), IV=integer())

#Wybranie zmiennych  _fine 
baza.n_fine<-baza.n[ ,grepl(pattern="_fine" , x=names(baza.n))]

smbinning.eda(baza.n_fine, rounding = 3, pbar = 1)


pdf(file=paste(path,"/report/WoE_numeric.pdf",sep=""),paper="a4")
names.n<-colnames(baza.n[,!names(baza.n) %in% c(colnames(baza.n_fine),"def","def_woe")])
total_pb <- length(names.n)
pb<- txtProgressBar(min = 0, max = total_pb, style = 3)
i<-names.n[1]


for (i in names.n){
  
  # ustawienia wykresów
  par(mfrow=c(2,2))
  
  
  
  results<- smbinning.custom(df=baza.n, y="def_woe", x=i, cuts=unique(percentile[,i]))
  #Wyrzucenie obserwacji NaN oraz infinite
  results1<-results[["ivtable"]]
  results1<-results1[!is.nan(results1$WoE),]
  results1<-results1[!is.infinite(results1$WoE),]
  results<-list(results1,results[["iv"]],results[["bands"]],results[["x"]],results[["col_id"]],results[["cuts"]])
  names(results)<-c("ivtable","iv","bands","x","col_id","cuts")
  #Wykresy
  
  # BOXPLOT
  boxplot(baza.n[,i]~baza.n$def, 
          horizontal=T, frame=F, col="lightgray",main="Distribution") 
  mtext(i,3) 
  # wykres czestosci 
  smbinning.plot(results,option="dist",sub=i)
  # Bad rate 
  smbinning.plot(results,option="badrate",sub=i) 
  # WoE
  try(smbinning.plot(results,option="WoE",sub=i))
  
  
  
  # information value
  IV<-rbind(IV, as.data.frame(cbind("VAR"=i, "IV"=results$ivtable[results$ivtable$Cutpoint=="Total", "IV"])))
  
  # zapis danych
  d<-results$ivtable[,c("Cutpoint","WoE","PctRec")]
  # usuniecie zmiennej total
  d<-d[d$Cutpoint!="Total",]
  # Posortowanie tablicy po wartosciach woe
  d<-d[with(d, order(d$WoE)),]
  # Id 
  d$numer<-11:(nrow(d)+10)
  # zapis wartosci woe do listy
  WOE[[i]]<-d
  
  #Update progess bar  
  setTxtProgressBar(pb,  min(grep(i, names.n)))
} 
close(pb)
dev.off()
#Zmienne faktorowe
str(baza.f)
smbinning.eda(baza.f, rounding = 3, pbar = 1)
#Usunmy zmienne dotyczace budynku i okolicy kredytorbiorcy. Bez tego prccedura smbining wyrzuca blad atomic vetor
baza.f<-baza.f[,-c(40:43)]
names.f<-colnames(baza.f[,!names(baza.f) %in% c("def","def_woe")])
baza.f$def_woe<-(1- dane$DEF)
baza.f$def<-dane$DEF

pdf(file=paste(path,"/report/WoE_factor.pdf",sep=""),paper="a4")

# progress bar
total_pb <- length(names.f)
pb<- txtProgressBar(min = 0, max = total_pb, style = 3)
for (i in names.f){
  baza.f[,paste(i,"_fine", sep="")]<-baza.f[,i]
  baza.f[,i] <- as.factor(baza.f[,i])
  par(mfrow=c(2,2))
  results<- smbinning.factor(df=baza.f, y="def_woe", x=i, maxcat=length(unique(baza.f[,i])))
  
  #Wyrzucenie obserwacji NaN oraz infinite
  results1<-results[["ivtable"]]
  results1<-results1[!is.nan(results1$WoE),]
  results1<-results1[!is.infinite(results1$WoE),]
  results<-list(results1,results[["iv"]],results[["x"]],results[["col_id"]],results[["cuts"]])
  names(results)<-c("ivtable","iv","x","col_id","cuts")
  
  #Wykresy
  try(smbinning.plot(results,option="dist",sub=i), silent = T)
  try(smbinning.plot(results,option="badrate",sub=i), silent = T) 
  try(smbinning.plot(results,option="WoE",sub=i),silent = T)
  IV<-rbind(IV, as.data.frame(cbind("VAR"=i, "IV"=results$ivtable[results$ivtable$Cutpoint=="Total", "IV"])))
  d<-results$ivtable[,c("Cutpoint","WoE","PctRec")]
  d<-d[d$Cutpoint!="Total",]
  d<-d[with(d, order(d$WoE)),]
  d$numer<-11:(nrow(d)+10)
  WOE[[i]]<-d
  setTxtProgressBar(pb,  min(grep(i, names.f)))
} 
close(pb)
dev.off()

baza<-cbind(baza.n,baza.f)
def<-baza[,c("def_woe","def")]

baza <- baza[,-grep("def",colnames(baza))]
baza<-cbind(baza,def)

total_pb <- length(names(WOE))
pb<- txtProgressBar(min = 0, max = total_pb, style = 3)
stats<-cbind(IV, Gini=NA, miss=NA)
for (l in names(WOE)){
  #baza[,l] <- as.factor(baza[,l])
  #baza[,paste(l,"_fine",sep="")] <- as.factor(baza[,paste(l,"_fine",sep="")])
  baza[,paste(l,"_fine", sep="")]<-fct_explicit_na(baza[,paste(l,"_fine", sep="")], na_level="Missing")
  zmienna<-baza[,c("def_woe", paste(l,"_fine", sep=""))]
  woe<- WOE[[l]][c("Cutpoint", "WoE")]
  if (is.character(woe$Cutpoint)==TRUE) 
  { 
    woe$Cutpoint<-as.factor(gsub("= '|'", "", woe$Cutpoint))
    woe$Cutpoint<-as.factor(woe$Cutpoint)
  }
  # laczenie poziomow zmiennej z wartosciami WoE
  
  zbior_temp<-merge(zmienna, woe, by.x=paste(l,"_fine", sep=""), by.y="Cutpoint", all.x=T)
  colnames(zbior_temp)[which(names(zbior_temp) == "WoE")] <- paste(l, "_woe", sep="")  
  baza<-merge(baza, woe, by.x=paste(l,"_fine", sep=""), by.y="Cutpoint", all.x=T)
  colnames(baza)[which(names(baza) == "WoE")] <- paste(l, "_woe", sep="")
  print(c(any(is.na(zbior_temp[,paste(l, "_woe", sep="")])), l))
  
  # wyliczenie wartosci giniego
  gini<- c(2*auc(zbior_temp$def_woe,zbior_temp[,paste(l, "_woe", sep="") ])-1)
  stats[stats$VAR==l, "Gini"]<-gini
  miss<-1-c(nrow(zbior_temp[zbior_temp[,paste(l,"_fine", sep="")]!='Missing', ])/nrow(zbior_temp))
  stats[stats$VAR==l, "miss"]<-miss
  setTxtProgressBar(pb,  min(grep(l, names(WOE))))
}

close(pb)
write.csv(stats, paste(path,"/data/stats.csv",sep=""))



# ------------------------ ANALIZA KORELACJI ------------------------------
zm_do_analizy<-colnames(baza)[grep("_woe", colnames(baza))]
zm_do_analizy<-zm_do_analizy[-1]
baza_kor<-baza[,zm_do_analizy]

colSums(is.na(baza_kor))
#Trzeba zastapic czyms braki danych bo korelacje sie nie wylicza
#Ze wzgledu ze jest to WoE zastapmy to najmniejszymi wartosciami
baza$REGION_woe<-ifelse(is.na(baza$REGION_woe),min(baza$REGION_woe[!is.na(baza$REGION_woe)]),baza$REGION_woe)

baza$EXT_SOURCE_2_woe<-ifelse(is.na(baza$EXT_SOURCE_2_woe),min(baza$EXT_SOURCE_2_woe[!is.na(baza$EXT_SOURCE_2_woe)]),baza$EXT_SOURCE_2_woe)



zm_do_analizy<-colnames(baza)[grep("_woe", colnames(baza))]
zm_do_analizy<-zm_do_analizy[-1]
baza_kor<-baza[,zm_do_analizy]
colSums(is.na(baza_kor))


#Widac ze mamy problem z wartosciami zerowymi w zmiennych dotyczacych dostarczenia dokumentów.
#Sprawdzmy te zmienne
prop.table(table(baza$DOCUMENT4F,baza$def),1)
prop.table(table(baza$DOCUMENT10F,baza$def),1)
prop.table(table(baza$DOCUMENT12F,baza$def),1)
#Tak jak widać wszyscy ktorzy przyniesli dokumenty byli oznaczeni jako brak defaultu dlatego woe=ln(1/0) wychodzi infinity i nie ma ty mozliwosci roznicowania,
#dlatego pozbadzmy sie tych zmiennych 
baza<-baza[,-c(22,24,30,107,113,115,177,183,185)]


#Liczenie korelacji
zm_do_analizy<-colnames(baza)[grep("_woe", colnames(baza))]
zm_do_analizy<-zm_do_analizy[-1]
baza_kor<-baza[,zm_do_analizy]
tim <- proc.time ()[1]	
kendall2 <-cor.fk (baza_kor)
cat ("cor runtime [s]:", proc.time ()[1] - tim, "(n =", nrow (baza_kor), ")\n")

save(kendall2,file=paste(path,"/data/kendall2.RData",sep=""))

corrplot(kendall2)

kor_list<-data.frame()


#Laczenie statystyk i korelacji
stats$VARW<-paste(stats$VAR,"_woe",sep="")
stats<-unique(stats)
stat<-merge(x = stats, y = kendall2, by.x = "VARW",by.y="row.names",all.y=T)
#posortowanie zbioru po Gini od najwiekszych wartosci
stat<-stat[ order(-stat[,"Gini"]), ]
#nadanie wierszom nazw jak zmienne
row.names(stat)<-stat[,1]
#wybor tylko zmiennych do macierzy korelacji
stat<-stat[6:length(stat[1,])]
#wektor nazw zmiennych z wierszy
cols<-row.names(stat[,])
#uporzadkowanie kolumn wedlug Gini
stat<-stat[cols]

#w sytuacji, gdy brakuje wartosci korelacji przypisanie minimalnej wartosci, 
#ktora nie wyelimuje zadnej zmiennej z analizy
temp_k<-stat
temp_k<-replace(temp_k, is.na(temp_k), 0.000001000)

threshold<-0.5

repeat{
  
  if (length(temp_k)<1) {
    break
  }
  
  wiersz_k<-abs(temp_k[1,]) > threshold
  wiersz_k[1]<-TRUE
  
  if(length(wiersz_k)>1){
    wiersz_k2<-wiersz_k[,wiersz_k]
  }else{wiersz_k2<-wiersz_k}
  
  temp_k<-as.data.frame(temp_k[!t(wiersz_k),!wiersz_k])
  
  if(length(temp_k)==1){ 
    colnames(temp_k)<- dimnames(wiersz_k)[[2]][-which(wiersz_k)]
    rownames(temp_k)<- dimnames(wiersz_k)[[2]][-which(wiersz_k)]
  }
  if(length(wiersz_k2)==1) {
    zmienna<-row.names(wiersz_k)
    nazwy<-as.data.frame(zmienna)
  }
  
  if(length(wiersz_k2)>1) {
    wiersz_k2<-wiersz_k2[2:length(wiersz_k2)]
    wiersz_k2<-as.data.frame(t(wiersz_k2))
    nazwy<-colnames(wiersz_k2)
    nazwy<-as.data.frame(t(nazwy))
    row.names(nazwy) <- row.names(wiersz_k)
    zmienna<-row.names(wiersz_k)
    zmienna<-as.data.frame(zmienna)
    nazwy<-merge(zmienna,nazwy)
  }
  kor_list<-rbind.fill(kor_list,nazwy)
  if(length(temp_k)==1){ 
    zmienna<-dimnames(wiersz_k)[[2]][-which(wiersz_k)]
    nazwy<-as.data.frame(zmienna)
    kor_list<-rbind.fill(kor_list,nazwy)
    break}
  
}
all<-merge(x = stats, y = kor_list, by.x = "VARW",by.y="zmienna",all.y=T)
all<-all[ order(-all[,"Gini"]), ]
all<-all[all$Gini>0.08,]
write.csv(all, paste(path,"/data/wyniki_fine_classing.csv",sep = ""))
baza_coarse<-baza[-grep("_fine",colnames(baza))]
kols<-c(as.character(all$VAR),"def","def_woe")
baza_coarse<-baza_coarse[kols]

#Zarzadzmy brakami danych w baza_coarse przed podzialem na test i train
colSums(is.na(baza_coarse))

#Dla ext_source zastosujmy mediane w przypadku braków
baza_coarse$EXT_SOURCE_1[is.na(baza_coarse$EXT_SOURCE_1)] <- median(baza_coarse$EXT_SOURCE_1[!is.na(baza_coarse$EXT_SOURCE_1)])
baza_coarse$EXT_SOURCE_2[is.na(baza_coarse$EXT_SOURCE_2)] <- median(baza_coarse$EXT_SOURCE_2[!is.na(baza_coarse$EXT_SOURCE_2)])
baza_coarse$EXT_SOURCE_3[is.na(baza_coarse$EXT_SOURCE_3)] <- median(baza_coarse$EXT_SOURCE_3[!is.na(baza_coarse$EXT_SOURCE_3)])
#ze wzgledu na niewielka ilosc brakow danych nie ma wielkiego znaczenia jaki rodzaj imputacji wezmiemy

baza_coarse$GOOD_PRICE[is.na(baza_coarse$GOOD_PRICE)] <- mean(baza_coarse$GOOD_PRICE[!is.na(baza_coarse$GOOD_PRICE)])
baza_coarse$AMT_ANNUITY[is.na(baza_coarse$AMT_ANNUITY)] <- mean(baza_coarse$AMT_ANNUITY[!is.na(baza_coarse$AMT_ANNUITY)])

colSums(is.na(baza_coarse))

save(baza_coarse,file=paste(path,"/data/baza_coarse",sep = ""))

#Podzial danych na test i train
set.seed(1916)
size<-0.7
smp_split2<-sample.split(baza_coarse$def, SplitRatio = size)

train<-baza_coarse[smp_split2==T,]
test<-baza_coarse[smp_split2==F,]

mean(train$def)
mean(test$def)

save(train,file=paste(path,"/data/train",sep = ""))
save(test,file=paste(path,"/data/test",sep = ""))


# ---------------------- COARSE CLASSING --------------------------
#Wczytanie danych
load(paste(path,"/data/train",sep = ""))
load(paste(path,"/data/test",sep = ""))

#podzial na numeric i factor
columns<-colnames(train)[-which(names(train) %in% c("def","def_woe"))]

nums <- sapply(train[,columns], is.numeric) 
nums
columns.n<-columns[nums==T]
columns.f<-columns[nums==F]

# pasek postepu
total <- length(columns.n)
pasek<- txtProgressBar(min = 0, max = total, style = 3)

temp<-data.frame(VAR=character(), VALUE=integer())
stats<-data.frame(VAR=character(), IV=integer(),Gini=integer(), MISS=integer(),IV_val=integer(),Gini_val=integer(), MISS_val=integer())


#cut_off dla testowanych danych
cut_offs<-data.frame(VAR=character(), cuts=integer())


#Binaryzacja dla zmiennych ciaglych (columns.n)
k<-0

zmienna_c<-columns.n[2]

pdf(paste(path,"/report/WoE_coarse_numeric.pdf",sep=""))
for (zmienna_c in columns.n){
  
  
  k<-k+1
  par(xpd = T, mar = par()$mar, mfrow=c(1,1))
  
  
  #jesli jakas zmienna jest typu character zrob z niej factor
  if (class(train[,c(zmienna_c)])[1]=="character"){train[,c(zmienna_c)]<-as.factor(train[,c(zmienna_c)])}
  
  #glowna funkcja
  result <- woe.tree.binning(train, "def", zmienna_c, min.perc.total=0.1,min.perc.class=0.05, event.class=1)
  
  
  #warunek, żeby dla braków nie wyrzucalo bledow i dawalo wyniki tylko dla prawidlowych wynikow
  if(length(result)>1&result[[3]]>0){
    
    IV<-result[[3]]
    # dane train
    train<-woe.binning.deploy(train, result, add.woe.or.dum.var="woe")
    train[,paste0("woe.",zmienna_c,".binned")]<-train[,paste0("woe.",zmienna_c,".binned")]/100
    
    train[,paste0("woe.",zmienna_c,".binned")]<-ifelse(train[,paste0("woe.",zmienna_c,".binned")]==-Inf,-2,train[,paste0("woe.",zmienna_c,".binned")])
    train[,paste0("woe.",zmienna_c,".binned")]<-ifelse(train[,paste0("woe.",zmienna_c,".binned")]==Inf,2,train[,paste0("woe.",zmienna_c,".binned")])
    gini <- 2*pROC::auc(train[,"def"], train[,paste0("woe.",zmienna_c,".binned")],direction=">")-1
    # % brakow
    miss <- 1-nrow(train[!(is.na(train[,zmienna_c])),])/nrow(train)
    
    #Dane test
    test<-woe.binning.deploy(test, result, add.woe.or.dum.var="woe")
    test[,paste0("woe.",zmienna_c,".binned")]<-test[,paste0("woe.",zmienna_c,".binned")]/100
    
    test[,paste0("woe.",zmienna_c,".binned")]<-ifelse(test[,paste0("woe.",zmienna_c,".binned")]==-Inf,-2,test[,paste0("woe.",zmienna_c,".binned")])
    test[,paste0("woe.",zmienna_c,".binned")]<-ifelse(test[,paste0("woe.",zmienna_c,".binned")]==Inf,2,test[,paste0("woe.",zmienna_c,".binned")])
    giniw <- 2*auc(test[,"def"], test[,paste0("woe.",zmienna_c,".binned")],direction=">")-1
    
    IVw<-IV(X=test[,paste0(zmienna_c,".binned")], Y=test$def)[1]
    # % braków
    missw <- 1-nrow(test[!(is.na(test[,zmienna_c])),])/nrow(test)
    
    stats <- rbind(stats, 
                   as.data.frame(cbind("VAR"=zmienna_c, "IV"=result[[3]],"Gini"=gini, "MISS"=miss,"IV_val"=IVw,"Gini_val"=giniw, "MISS_val"=missw)))
    
    #Tworzenie wykresow dla woe i default rate
    mixed <- c("#cccccc","#e6e6e6","#cccccc","#e6e6e6","#cccccc","#e6e6e6","#cccccc","#e6e6e6","#cccccc","#e6e6e6")
    
    plot<-data.frame(result[[2]])
    plot$bins <- rownames(plot)
    plot$woe<-plot$woe/100
    plot$woe<-ifelse(plot$woe==Inf, -2, ifelse(plot$woe==-Inf, -2,plot$woe))
    plot$woe<-ifelse(rownames(plot)=="Missing"&plot$woe>3,0,plot$woe)
    plot$woe<-ifelse(rownames(plot)=="Missing"&plot$woe<(-3),0,plot$woe)
    plot <- na.omit(plot)
    plot <- plot %>% filter(X1+X0>0)
    
    plot$woe_plot <- paste0('WoE=', round(plot$woe,2))
    plot$fill_plot <- paste0('Fill=', round(((plot$X1+plot$X0)/sum(plot$X1+plot$X0)),2))
    plot$GoodRate<-plot$X1/plot$X0
    
    
    plot$bins <- factor(plot$bins)
    plot$bins <- fct_reorder(plot$bins, plot$cutpoints.final, min)
    
    plot_1 <- ggplot(plot, aes(x=bins, y=woe)) +
      geom_bar(stat = "identity", fill=mixed[1:length(unique(plot$cutpoints.final..1.))]) +
      geom_line(aes(x = bins, y = GoodRate), group=1, color="#333333") +
      geom_text_repel(aes(x = bins, y = GoodRate, label=scales::percent(GoodRate)), vjust = -0.6, size=2) +
      geom_text_repel(aes(label=woe_plot), vjust = 1, size=2) +
      geom_text_repel(aes(label=fill_plot), vjust= -0.5, size=2) +
      scale_y_continuous(sec.axis = sec_axis(~., labels = function(b) {paste0(round(b * 100, 0), "%")})) +
      theme_minimal() +
      labs(title ="WoE and default rate per bucket", subtitle=zmienna_c) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 9),
        plot.subtitle = element_text(hjust = 0.5, size = 9),
        axis.text = element_text(size = 7)) 
    
    print(plot_1)
  }
  
  
  temp<-rbind(temp, as.data.frame(cbind(VAR=zmienna_c,VALUE=length(result)>1 )))
  names(temp)<-c("VAR","VALUE")
  setTxtProgressBar(pasek,k)
}
dev.off()


#Binaryzacja dla zmiennych faktorowych(columns.f)
k<-0

zmienna_c<-columns.f[2]

# pasek postepu
total <- length(columns.f)
pasek<- txtProgressBar(min = 0, max = total, style = 3)

pdf(paste(path,"/report/WoE_coarse_factor.pdf",sep=""))
for (zmienna_c in columns.f){
  
  
  k<-k+1
  par(xpd = T, mar = par()$mar, mfrow=c(1,1))
  
  
  
  #jesli jakas zmienna jest typu character zrob z niej factor
  if (class(train[,c(zmienna_c)])[1]=="character"){train[,c(zmienna_c)]<-as.factor(train[,c(zmienna_c)])}
  
  result <- woe.tree.binning(train, "def", zmienna_c, min.perc.total=0.1,min.perc.class=0.05, event.class=1)
  
  #warunek, żeby dla braków nie wyrzucalo bledow i dawalo wyniki tylko dla prawidlowych wynikow
  if(length(result)>1&result[[3]]>0){
    
    
    IV<-result[[3]]
    
    #Dane treningowe
    train<-woe.binning.deploy(train, result, add.woe.or.dum.var="woe")
    train[,paste0("woe.",zmienna_c,".binned")]<-train[,paste0("woe.",zmienna_c,".binned")]/100
    
    train[,paste0("woe.",zmienna_c,".binned")]<-ifelse(train[,paste0("woe.",zmienna_c,".binned")]==-Inf,-2,train[,paste0("woe.",zmienna_c,".binned")])
    train[,paste0("woe.",zmienna_c,".binned")]<-ifelse(train[,paste0("woe.",zmienna_c,".binned")]==Inf,2,train[,paste0("woe.",zmienna_c,".binned")])
    
    gini <- 2*pROC::auc(train[,"def"], train[,paste0("woe.",zmienna_c,".binned")],direction=">")-1
    # % brakow
    miss <- 1-nrow(train[!(is.na(train[,zmienna_c])),])/nrow(train)
    
    
    #Dane testowe
    test<-woe.binning.deploy(test, result, add.woe.or.dum.var="woe")
    test[,paste0("woe.",zmienna_c,".binned")]<-test[,paste0("woe.",zmienna_c,".binned")]/100
    
    test[,paste0("woe.",zmienna_c,".binned")]<-ifelse(test[,paste0("woe.",zmienna_c,".binned")]==-Inf,-2,test[,paste0("woe.",zmienna_c,".binned")])
    test[,paste0("woe.",zmienna_c,".binned")]<-ifelse(test[,paste0("woe.",zmienna_c,".binned")]==Inf,2,test[,paste0("woe.",zmienna_c,".binned")])
    giniw <- 2*auc(test[,"def"], test[,paste0("woe.",zmienna_c,".binned")],direction=">")-1
    
    IVw<-IV(X=test[,paste0(zmienna_c,".binned")], Y=test$def)[1]
    
    # % brakow
    missw <- 1-nrow(test[!(is.na(test[,zmienna_c])),])/nrow(test)
    
    stats <- rbind(stats, as.data.frame(cbind("VAR"=zmienna_c, "IV"=result[[3]],"Gini"=gini, "MISS"=miss,"IV_val"=IVw,"Gini_val"=giniw, "MISS_val"=missw)))
    
    #Tworzenie wykresow
    mixed <- c("#cccccc","#e6e6e6","#cccccc","#e6e6e6","#cccccc","#e6e6e6","#cccccc","#e6e6e6","#cccccc","#e6e6e6")
    
    plot<-data.frame(result[[2]])
    plot$woe<-ifelse(plot$woe==Inf, -2, ifelse(plot$woe==-Inf, -2,plot$woe))
    plot$woe<-plot$woe/100
    plot <- na.omit(plot)
    plot <- plot %>% dplyr::select(-Group.1) %>% distinct()
    
    plot$woe_plot <- paste0('WoE=', round(plot$woe,2))
    plot$fill_plot <- paste0('Fill=', round(((plot$X1+plot$X0)/sum(plot$X1+plot$X0)),2))
    plot$GoodRate <- ifelse(is.na(plot$X0),1,(plot$X1)/(plot$X1+plot$X0))
    
    plot$Group.2 <- fct_reorder(plot$Group.2, plot$woe, min)
    
    plot_1 <- ggplot(plot, aes(x=Group.2, y=woe)) +
      geom_bar(stat = "identity", fill=mixed[1:length(unique(plot$woe))]) +
      geom_line(aes(x = Group.2, y = GoodRate), group=1, color="#333333") +
      geom_text_repel(aes(x = Group.2, y = GoodRate, label=scales::percent(GoodRate)), vjust = -0.6, size=2) +
      geom_text_repel(aes(label=woe_plot), vjust = 1, size=2) +
      geom_text_repel(aes(label=fill_plot), vjust= -0.5, size=2) +
      scale_y_continuous(sec.axis = sec_axis(~., labels = function(b) {paste0(round(b * 100, 0), "%")})) +
      theme_minimal() +
      labs(title ="WoE and default rate per bucket", subtitle=zmienna_c) +
      theme(axis.title.x = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 9),
            plot.subtitle = element_text(hjust = 0.5, size = 9),
            axis.text = element_text(size = 7)) 
    print(plot_1)
  }
  
  
  temp<-rbind(temp, as.data.frame(cbind(VAR=zmienna_c,VALUE=length(result)>1 )))
  names(temp)<-c("VAR","VALUE")
  setTxtProgressBar(pasek,k)
}
dev.off()

warnings()

#Zapis wyników

write.csv(stats, file=paste(path,"/data/stats.csv",sep = ""))
save(train,file=paste(path,"/data/trainm",sep = ""))
save(test,file=paste(path,"/data/testm",sep = ""))



# ------------------- Regresja logistyczna --------------------------

load(file=paste(path,"/data/trainm",sep = ""))
load(file=paste(path,"/data/testm",sep = ""))

#Funkcje potrzebne do oceny jakosci

#Funkcja mierzaca dopasowanie modelu do danych
hosmerlem = function(y, yhat, g=20) {
  cutyhat = cut(yhat,breaks = quantile(yhat, probs=seq(0,1, 1/g)), include.lowest=TRUE)  
  obs = xtabs(cbind(1 - y, y) ~ cutyhat)  
  expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)  
  chisq = sum((obs - expect)^2/expect)  
  P = 1 - pchisq(chisq, g - 2)  
  return(list(chisq=chisq,p.value=P))
  hr=P
}

#Funkcja mierzaca stabilnosc rozkladu oceny
cal_psi <- function(data1,data2, bench, target, bin)
{
  ben<-sort(data1[,bench]);
  tar<-sort(data2[,target]);
  
  ttl_bench<-length(ben);
  ttl_target<-length(tar);
  
  n<-ttl_bench%/%bin; 
  psi_bin<-rep(0,times=bin) 
  
  for (i in 1:bin) 
  {
    
    lower_cut<-ben[(i-1)*n+1];
    if(i!=bin){upper_cut<-ben[(i-1)*n+n]; pct_ben<-n/ttl_bench;} else
    {upper_cut<-ben[ttl_bench];
    pct_ben<(ttl_bench-n*(bin-1))/ttl_bench;}
    
    
    pct_tar<-length(tar[tar>lower_cut&tar<=upper_cut])/ttl_target;
    psi_bin[i]<-(pct_tar-pct_ben)*log(pct_tar/pct_ben);
  }
  psi<-sum(psi_bin);
  return(psi);
}

#Funkcja mierzaca jakosc zmiennych
cal_psi_zm <- function(data1,data2, bench, target)
{
  ben<-sort(data1[,bench]);
  tar<-sort(data2[,target]);
  bin<-length(unique(ben))
  bin_tar<-length(unique(tar))
  
  ttl_bench<-length(ben);
  ttl_target<-length(tar);
  
  tab_ben<-table(ben)
  pct_ben<-tab_ben/ttl_bench
  names<-names(tab_ben)
  tab_tar<- as.data.frame(table(tar))
  
  
  tab_tar<-merge(as.data.frame(tab_ben),tab_tar,by.x="ben",by.y="tar")
  tab_tar[,is.na(tab_tar)]<-0
  
  pct_tar<-tab_tar[,3]/ttl_target
  pct_ben<-tab_tar[,2]/ttl_bench
  psi_bin<-rep(0,times=bin) 
  
  psi_bin<-(pct_tar-pct_ben)*log(pct_tar/pct_ben);
  psi<-sum(psi_bin);
  return(psi);
}

# Estymacja modelu
cols<-colnames(train)[grep("woe", colnames(train))]
cols<-cols[-grep("def_woe", cols)]
data<-train[,c("def",cols)]

#model ze stala
baza<-glm(def ~ 1,data=data, family=binomial("logit"))

mean(train$def)

max<-glm(def ~ .,data=data, family=binomial("logit"))
rownames(summary(max)$coeff)
summary(max)

#Wyrzucmy zmienna amt_credit
model<-glm(def ~ woe.EXT_SOURCE_2.binned+woe.EXT_SOURCE_3.binned+woe.DAYS_EMPLOYED.binned+
             woe.GOOD_PRICE.binned+woe.DAYS_BIRTH.binned+woe.EXT_SOURCE_1.binned+woe.DAYS_PHONE_CHANGE.binned+woe.DAYS_ID_CHANGE.binned+
             woe.AMT_ANNUITY.binned+woe.DAYS_REGISTRATION.binned+woe.JOBF.binned+woe.ORGANIZATION_TYPEF.binned+woe.SOURCE_INCOMEF.binned+
             woe.REGION_RATEF.binned+woe.SEXF.binned+woe.EDUCATIONF.binned ,data=data, family=binomial("logit"))
summary(model)

#Metoda stepwise

model_stepwise_both<-step(baza, scope = list(upper=max, lower=baza ), direction = "both", trace=T,steps=30,k=4)
summary(model_stepwise_both)
#Metoda Stepwise nie znalazla modelu lepszego niz klasyczne od ogolu do szczegolu


### ----------------- Analiza jakosci modelu -------------------


#Podstawowy test, w ktorem porównujemy otrzymany model z modelami "idealnymi" i
# sprawdzamy, czy otrzymana wartość MLV jest statystycznie bliska 0
# H0: model jest dobrze dopasowany do danych
(gf<-pchisq(model$deviance, model$df.residual,lower.tail = F))
# wniosek: Model jest dobrze dopasowany do danych

# Test LR na laczna istotność zmiennych
# sprawdzamy, czy statystyka dla modelu jest istotnie większe niż dla modelu tylko ze stałą - test na całkowitą istotność modelu
# H0: zmienne są statystycznie nieistotne
(ist<-pchisq(model$null.deviance-model$deviance, model$df.null-model$df.residual,lower.tail = F))
# wniosek: Odrzucamy hipoteze zerowa ze zmienne lacznie sa statystycznie nieistotne



# Test Hosmera - Lemeshowa - podstawowy test jakoscidopasowania dla modelu z binarną zmienną zależną
# H0: model jest dobrze dopasowany do danych
# ma wiele wad - przede wszystkim jest bardzo wrażliwy na liczbę grup
hr<-hosmerlem(y=data$def, yhat=fitted(model),g=10)
hosmerlem(y=data$def, yhat=fitted(model),g=7)
hosmerlem(y=data$def, yhat=fitted(model),g=8)
hosmerlem(y=data$def, yhat=fitted(model),g=9)
#wnioski : dla roznych wartosci g mozna otrzymac rozne wnioski

gof<-gof(model, g=10)

#przypisanie wartosci dopasowanych do zbioru data

data$baza<-baza$fitted.values
data$model<-model$fitted.values
data$max<-max$fitted.values

# skalowanie PD do założonej skali
# 660 punktów oznacza ODDS = 72, a ODDS podwaja się dla 40 punktów

data$score<-(660-40/log(1/2)*log(1/72))+40/log(1/2)*model$linear.predictors

#Przypisanie PD do danych
test$model<-predict(model, newdata=test, type="response") 
test$score<-(660-40/log(1/2)*log(1/72))+40/log(1/2)*predict(model, newdata=test, type="link") 

train$model<-predict(model, newdata=train, type="response") 
train$score<-(660-40/log(1/2)*log(1/72))+40/log(1/2)*predict(model, newdata=train, type="link") 

#test roc - sprawdzenie czy dwa modele posiadaja podobne krzywe ROC
#H0:  Krzywe ROC sa sobie rowne dla obu modeli
roc_test_baza<-roc.test(data$def, data$model, data$baza,method="d")$p.value
roc_test_og<-roc.test(data$def, data$max, data$model,method="d")$p.value
#wnioski
#Krzywa Roc mowi ze model ze zmiennymi jest lepszy niz model jedynie ze stala
#Za to nie istnieje roznica miedzy modelem bez zmiennej AMT_CREDIT, a ze wszystkimi zmiennymi



hist(data[data$def==0,c("score")])
hist(data[data$def==1,c("score")])

# gini index - miare opisujaca zdolnosc dyskryminacyjna modelu
#Im wieksza wartosc tym lepiej
gini_t<-2*auc(data$def,data$model,direction="<")-1 
gini_w<-2*auc(test$def,test$model,direction="<")-1

2*auc(data$def,data$max,direction="<")-1


#Przedzialy ufnosci dla indeksu giniego. Metoda Delonga z wykorzystaniem 
#analitycznego wzoru na wariancje
(ci_delong_t<-2*ci.auc(data$def, data$model,method="d",direction="<")-1)
(ci_delong_w<-2*ci.auc(test$def, test$model,method="d",direction="<")-1)

# Statystyka testu Kołmogorowa - Smirnowa porównującego dwa rozkłady.
# Pozwala ocenic jakosc dyskryminacyjna modelu
# im bardziej się od siebie różnią tym lepiej
(ks_score_t<-ks.test(data[data$def==0,c("score")],data[data$def==1,c("score")])$statistic)
(ks_score_w<-ks.test(test[test$def==0,c("score")],test[test$def==1,c("score")])$statistic)


#Stabilnosc modelu za pomoca testu PSI, ktory sprawdza roznice pomiedzy dwowa
#rozkladami
psi<-cal_psi(data1=data, data2=test, bench="score",target="score",bin=20)


ks<-ks.test(data$score,test$score)$p.value

#Koncentracja wynikow
t<-as.data.frame(sort(table(data$score)/length(data$score),decreasing=T))[1:3,1:2]
w<-as.data.frame(sort(table(test$score)/length(test$score),decreasing=T))[1:3,1:2]



#------------------- Podsumowanie wyników w tabeli ----------------------
mdl<-"model_og"
zmienne<-names(model$coefficients)[2:length(model$coefficients)]

var_qual<-NULL
models_qual<-NULL
zmienne_tab<-NULL


for (i in 1:length(zmienne)) {
  tab<-NULL 
  tab$model<-mdl
  tab$v<-zmienne[i]
  tab$gini_t<-2*ci.auc(data[!is.na(data[,zmienne[i]]),c("def")], data[!is.na(data[,zmienne[i]]),zmienne[i]],direction=">",method="d")[2]-1
  tab$gini_w<-2*ci.auc(test[!is.na(test[,zmienne[i]]),c("def")], test[!is.na(test[,zmienne[i]]),zmienne[i]],direction=">",method="d")[2]-1
  
  tab$psi<-cal_psi_zm(data1=data[!is.na(data[,zmienne[i]]),zmienne], data2=test[!is.na(test[,zmienne[i]]),zmienne], bench=zmienne[i],target=zmienne[i])
  tab<-as.data.frame(tab)
  zmienne_tab<-rbind(zmienne_tab, tab)
}





temp_tab<-as.data.frame(cbind("Model"="model_og",
                              
                              'ist_param'=ist,
                              "roc_test_baza"=roc_test_baza,
                              "gof"=gof$gof$pVal[3],
                              "hosmer"=hr$p.value,
                              "gf"=gf,
                              "ist_ogr"=ist,
                              "roc_test_og"=roc_test_og,
                              "gini_t_cil"=ci_delong_t[1],
                              "gini_t"=gini_t,
                              "gini_t_ciu"=ci_delong_t[3],
                              "gini_w_cil"=ci_delong_w[1],
                              "gini_w"=gini_w,
                              "gini_w_ciu"=ci_delong_w[3],
                              "ks_score_t"=ks_score_t,
                              "ks_score_w"=ks_score_w,
                              "psi"=psi,
                              "ks_test"=ks,
                              
                              
                              "t_1_n"=t[1,1],
                              "t_1"=t[1,2],
                              "t_2_n"=t[2,1],
                              "t_2"=t[2,2],
                              "t_3_n"=t[3,1],
                              "t_3"=t[3,2],
                              "w_1_n"=w[1,1],
                              "w_1"=w[1,2],
                              "w_2_n"=w[2,1],
                              "w_2"=w[2,2],
                              "w_3_n"=w[3,1],
                              "w_3"=w[3,2]
))


models_qual<-rbind(models_qual,temp_tab)
var_qual<-rbind(var_qual,zmienne_tab)


save(models_qual,file=paste(path,"/data/models_qual.rdata",sep=""))
save(var_qual,file=paste(path,"/data/var_qual.rdata",sep=""))
save(train,file=paste(path,"/data/trainm",sep = ""))
save(test,file=paste(path,"/data/testm",sep = ""))


#------------------------ RANDOM FOREST ---------------------------

#Podzial danych na test i train
load(paste(path,"/data/baza_coarse",sep=""))
set.seed(1916)
size<-0.7
smp_split2<-sample.split(baza_coarse$def, SplitRatio = size)

train<-baza_coarse[smp_split2==T,]
test<-baza_coarse[smp_split2==F,]


x_train<-train[,1:17]
y_train<-factor(train[,18],levels=c(0,1),labels=c(0,1))

x_test<-test[,1:17]
y_test<-factor(test$def,levels=c(0,1),labels=c(0,1))
#Estymacja lasu losowego
las_zwykly <- randomForest(x =x_train,y = y_train, ntree = 300)
las_zwykly
#Wykres istotnosci zmiennych
importance(las_zwykly)
varImpPlot(las_zwykly,main="Istotnosc zmiennych zmierzona z pomoca lasu losowego",cex=1.5,pt.cex=3)

#statystyki oceniajace jakosc modelu dla zbioru treningowego
2*auc(y_train,las_zwykly$votes[,2],direction="<")-1

rf.roc<-roc(y_train,las_zwykly$votes[,2])
plot(rf.roc)
auc(rf.roc)

#Ewaluacja modelu
pred_y_probs <- predict(las_zwykly, newdata=x_test,type="vote")
pred_y<-predict(las_zwykly, newdata=x_test,type="response")

#Statystyki oceniajce jakosc modelu dla zbioru testowego
2*auc(y_test,pred_y_probs[,2],direction="<")-1
rf.roc<-roc(y_test,pred_y_probs[,2])
auc(rf.roc)

# Macierz pomylek
caret::confusionMatrix(pred_y, y_test)



#Zobaczmy jak model sie zachowa dla zmiennych woe
load(file="trainm")
load(file="testm")
cols<-colnames(train)[grep("woe", colnames(train))]
cols<-cols[-grep("def_woe", cols)]
x_train<-train[,c(cols)]
y_train<-as.factor(train[,"def"])
x_test<-test[,c(cols)]
y_test<-as.factor(test[,"def"])

las_zwykly <- randomForest(x =x_train,y = y_train, ntree = 300)
importance(las_zwykly)
varImpPlot(las_zwykly)

2*auc(y_train,las_zwykly$votes[,2],direction="<")-1

rf.roc<-roc(y_train,las_zwykly$votes[,2])
plot(rf.roc)
auc(rf.roc)

#Ewaluacja modelu
pred_y_probs <- predict(las_zwykly, newdata=x_test,type="vote")
pred_y<-predict(las_zwykly, newdata=x_test,type="response")

#Statystyki oceniajce jakosc modelu dla zbioru testowego
2*auc(y_test,pred_y_probs[,2],direction="<")-1
rf.roc<-roc(y_test,pred_y_probs[,2])
auc(rf.roc)

# Macierz pomylek
confusionMatrix(pred_y, y_test)

#Ostatecznie widać, że model na zmiennych nieprzeksztalconych daje lepsze wyniki.
