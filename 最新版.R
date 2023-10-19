rm(list = ls())
library(dplyr)
library(survey)

# Download & Read SAS Transport Files
# Demographic (DEMO2013-2018)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/DEMO_H.XPT", tf <- tempfile(), mode="wb")
DEMO_H <- foreign::read.xport(tf)[,c("SEQN","DMDEDUC3","DMDEDUC2","RIDAGEYR","RIAGENDR","RIDRETH1","INDFMPIR")]
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DEMO_I.XPT", tf <- tempfile(), mode="wb")
DEMO_I <- foreign::read.xport(tf)[,c("SEQN","DMDEDUC3","DMDEDUC2","RIDAGEYR","RIAGENDR","RIDRETH1","INDFMPIR")]
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/DEMO_J.XPT", tf <- tempfile(), mode="wb")
DEMO_J <- foreign::read.xport(tf)[,c("SEQN","DMDEDUC3","DMDEDUC2","RIDAGEYR","RIAGENDR","RIDRETH1","INDFMPIR")]


# DATA FOR 2013-2014
download.file("http://wwwn.cdc.gov/nchs/nhanes/2013-2014/SMQ_H.XPT", tf <- tempfile(), mode="wb")
SMQ_H <- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2013-2014/OHXREF_H.XPT", tf <- tempfile(), mode="wb")
OHXREF_H<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2013-2014/OHXDEN_H.XPT", tf <- tempfile(), mode="wb")
OHXDEN_H<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2013-2014/PBCD_H.XPT", tf <- tempfile(), mode="wb")
PBCD_H<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2013-2014/COT_H.XPT", tf <- tempfile(), mode="wb")
COT_H<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2013-2014/OHQ_H.XPT", tf <- tempfile(), mode="wb")
OHQ_H<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2013-2014/SMQRTU_H.XPT", tf <- tempfile(), mode="wb")
SMQRTU_H<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2013-2014/BMX_H.XPT", tf <- tempfile(), mode="wb")
BMX_H<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2013-2014/ALQ_H.XPT", tf <- tempfile(), mode="wb")
ALQ_H<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2013-2014/HIQ_H.XPT", tf <- tempfile(), mode="wb")
HIQ_H<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2013-2014/DIQ_H.XPT", tf <- tempfile(), mode="wb")
DIQ_H<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2013-2014/HSQ_H.XPT", tf <- tempfile(), mode="wb")
HSQ_H <- foreign::read.xport(tf)

#DATA FOR 2015-2016
# DATA FOR 2013-2014
download.file("http://wwwn.cdc.gov/nchs/nhanes/2015-2016/SMQ_I.XPT", tf <- tempfile(), mode="wb")
SMQ_I <- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2015-2016/OHXREF_I.XPT", tf <- tempfile(), mode="wb")
OHXREF_I<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2015-2016/OHXDEN_I.XPT", tf <- tempfile(), mode="wb")
OHXDEN_I<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2015-2016/PBCD_I.XPT", tf <- tempfile(), mode="wb")
PBCD_I<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2015-2016/COT_I.XPT", tf <- tempfile(), mode="wb")
COT_I<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2015-2016/OHQ_I.XPT", tf <- tempfile(), mode="wb")
OHQ_I<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2015-2016/SMQRTU_I.XPT", tf <- tempfile(), mode="wb")
SMQRTU_I<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2015-2016/BMX_I.XPT", tf <- tempfile(), mode="wb")
BMX_I<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2015-2016/ALQ_I.XPT", tf <- tempfile(), mode="wb")
ALQ_I<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2015-2016/HIQ_I.XPT", tf <- tempfile(), mode="wb")
HIQ_I<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2015-2016/DIQ_I.XPT", tf <- tempfile(), mode="wb")
DIQ_I<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2015-2016/HSQ_I.XPT", tf <- tempfile(), mode="wb")
HSQ_I <- foreign::read.xport(tf)


#DATA FOR 2017-2018
download.file("http://wwwn.cdc.gov/nchs/nhanes/2017-2018/SMQ_J.XPT", tf <- tempfile(), mode="wb")
SMQ_J <- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2017-2018/OHXREF_J.XPT", tf <- tempfile(), mode="wb")
OHXREF_J<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2017-2018/OHXDEN_J.XPT", tf <- tempfile(), mode="wb")
OHXDEN_J<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2017-2018/PBCD_J.XPT", tf <- tempfile(), mode="wb")
PBCD_J<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2017-2018/COT_J.XPT", tf <- tempfile(), mode="wb")
COT_J<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2017-2018/OHQ_J.XPT", tf <- tempfile(), mode="wb")
OHQ_J<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2017-2018/SMQRTU_J.XPT", tf <- tempfile(), mode="wb")
SMQRTU_J<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2017-2018/BMX_J.XPT", tf <- tempfile(), mode="wb")
BMX_J <- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2017-2018/ALQ_J.XPT", tf <- tempfile(), mode="wb")
ALQ_J<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2017-2018/HIQ_J.XPT", tf <- tempfile(), mode="wb")
HIQ_J<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2017-2018/DIQ_J.XPT", tf <- tempfile(), mode="wb")
DIQ_J<- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2017-2018/HSQ_J.XPT", tf <- tempfile(), mode="wb")
HSQ_J <- foreign::read.xport(tf)


#Binding
DEMO <- bind_rows(DEMO_H, DEMO_I, DEMO_J)
SMQ<- bind_rows(SMQ_H,SMQ_I,SMQ_J)
OHXREF<- bind_rows(OHXREF_H,OHXREF_I,OHXREF_J)
OHXDEN<- bind_rows(OHXDEN_H, OHXDEN_I, OHXDEN_J)
PBCD<- bind_rows(PBCD_H,PBCD_I,PBCD_J)
COT<- bind_rows(COT_H,COT_I,COT_J)
OHQ<- bind_rows(OHQ_H,OHQ_I,OHQ_J)
SMQRTU<- bind_rows(SMQRTU_H, SMQRTU_I, SMQRTU_J)
BMX<- bind_rows(BMX_H, BMX_I, BMX_J)
ALQ<- bind_rows(ALQ_H, ALQ_I, ALQ_J)
HIQ<- bind_rows(HIQ_H, HIQ_I, HIQ_J)
DIQ<- bind_rows(DIQ_H, DIQ_I, DIQ_J)
HSQ<- bind_rows(HSQ_H,HSQ_I,HSQ_J)



# select adult only
DEMO18TO50<-DEMO[DEMO$RIDAGEYR>=18 & DEMO$RIDAGEYR <= 50,]



# Keeping merging
DEMO2<- left_join(DEMO18TO50, SMQ, by="SEQN")
DEMO2<- left_join(DEMO2, OHXREF, by="SEQN")
DEMO2<- left_join(DEMO2, OHXDEN, by="SEQN")
DEMO2<- left_join(DEMO2, PBCD, by="SEQN")
DEMO2<- left_join(DEMO2, COT, by="SEQN")
DEMO2<- left_join(DEMO2, OHQ, by="SEQN")
DEMO2<- left_join(DEMO2, SMQRTU, by="SEQN")
DEMO2<- left_join(DEMO2, BMX, by="SEQN")
DEMO2<- left_join(DEMO2, ALQ, by="SEQN")
DEMO2<- left_join(DEMO2, HIQ, by="SEQN")
DEMO2<- left_join(DEMO2, DIQ, by="SEQN")
DEMO2<- left_join(DEMO2, HSQ, by="SEQN")


# Select varibles
DEMO5<- DEMO2 %>% select(SEQN, RIAGENDR, RIDAGEYR, DMDEDUC3,DMDEDUC2 , RIDRETH1, SMQ050Q, SMQ050U,SMD055 ,INDFMPIR, BMXBMI,OHAROCGP, OHAROCDT,OHQ030,OHQ680 ,SMQ040,HSD010 ,SMQ020, SMD650, SMD030, OHAREC, OHXRCAR, OHXRCARO, LBXBPB, LBXCOT, LBDCOTLC, OHQ850, ALQ130, HIQ011, DIQ010)  


# remove the NA value for SMQ040
DEMO5<- subset(DEMO5, !is.na(SMQ020))
DEMO5[c("OHXRCAR", "OHXRCARO")][is.na(DEMO5[c("OHXRCAR", "OHXRCARO")])] <- 3
DEMO5 <- DEMO5 %>%mutate(OHXCART = pmin(OHXRCAR,OHXRCARO))# 如果值为3则代表是missing value。把NA值变成3然后取一行的最小值
DEMO5 <- DEMO5 %>%
  mutate(.,OHXCART = na_if(OHXCART, 3))
DEMO5 <- subset(DEMO5, select = -c(OHXRCAR, OHXRCARO) )#删除用来合并的变量
DEMO5$OHXCART[DEMO5$OHXCART==1]<-"Yes"
DEMO5$OHXCART[DEMO5$OHXCART==2]<-"No"
DEMO5$OHXCART[DEMO5$OHXCART==9]<-NA

# OHQ680gai:
DEMO5$UNCOMFFREQ[DEMO5$OHQ680==1 | DEMO5$OHQ680==2]<-"HIGH"
DEMO5$UNCOMFFREQ[DEMO5$OHQ680==3 ]<-"Occasionally"
DEMO5$UNCOMFFREQ[DEMO5$OHQ680==4 | DEMO5$OHQ680==5]<-"Hardly or never"
DEMO5$UNCOMFFREQ[DEMO5$OHQ680==7 | DEMO5$OHQ680==9]<-NA


#年龄分类
DEMO5$AGE[DEMO5$RIDAGEYR<=34]<-"18-34"
#DEMO5$AGE[DEMO5$RIDAGEYR>25 & DEMO5$RIDAGEYR<=40 ]<-"25-40"
DEMO5$AGE[DEMO5$RIDAGEYR>34]<-"34-50"

# BMI 分类
DEMO5$BMXBMI[DEMO5$BMXBMI<25]<-"< 25"
DEMO5$BMXBMI[DEMO5$BMXBMI>=25&DEMO5$BMXBMI<30]<-"25-30"
DEMO5$BMXBMI[DEMO5$BMXBMI>=30]<-"≥ 30"

#INDFMPIR 分类：
DEMO5$INDFMPIR[DEMO5$INDFMPIR< 1.85]<-"< 1.85"
DEMO5$INDFMPIR[DEMO5$INDFMPIR>= 1.85]<-"≥ 1.85"

#Alcohol 分类：
DEMO5$ALQ130[!is.na(DEMO5$ALQ130)]<-"Yes"
DEMO5$ALQ130[is.na(DEMO5$ALQ130)]<-"No"

#保险分类：
DEMO5$HIQ011[DEMO5$HIQ011==1]<-"Yes"
DEMO5$HIQ011[DEMO5$HIQ011==2 | DEMO5$HIQ011==7]<-"No"
DEMO5$HIQ011[DEMO5$HIQ011==9]<-"No"

#糖尿病分类：
DEMO5$DIQ010[DEMO5$DIQ010==1]<-"Yes"
DEMO5$DIQ010[DEMO5$DIQ010==2]<-"No"
DEMO5$DIQ010[DEMO5$DIQ010==3]<-"Yes"
DEMO5$DIQ010[DEMO5$DIQ010==7]<-"No"
DEMO5$DIQ010[DEMO5$DIQ010==9]<-"No"

# 总体健康
DEMO5$HEALTH[DEMO5$HSD010 == 1 | DEMO5$HSD010 == 2]<-"GOOD"
DEMO5$HEALTH[DEMO5$HSD010 == 3]<-"GOOD"
DEMO5$HEALTH[DEMO5$HSD010 == 4 ]<-"FAIR"
DEMO5$HEALTH[DEMO5$HSD010 == 5 ]<-"POOR"
DEMO5$HEALTH[DEMO5$HSD010 == 7 | DEMO5$HSD010 == 9]<-"POOR"



# For those who answered 3 for SMD040 should smoke zero cigs (SMD020=1 OR 2) in past 30 days
# 逻辑应该是现在不抽烟的人SMQ040==3中SMQ=1OR2的人过去抽烟30天平均数为0
DEMO5$SMD650[DEMO5$SMQ040 == 3 & DEMO5$SMQ020 == 1]<- 0
DEMO5$SMD650[DEMO5$SMQ040 == 3 & DEMO5$SMQ020 == 2]<- 0



# Set nonsmoker current smoker and former smoker as SMQTYPE
DEMO5$SMQTYPE[DEMO5$SMQ040 == 1 & DEMO5$SMQ020 == 1]<-"Current Smoker"
DEMO5$SMQTYPE[DEMO5$SMQ040 == 2 & DEMO5$SMQ020 == 1]<-"Current Smoker"
DEMO5$SMQTYPE[DEMO5$SMQ040 == 3 & DEMO5$SMQ020 == 1]<-"Former Smoker"
DEMO5$SMQTYPE[DEMO5$SMQ020 == 2]<-"Never Smoker"
DEMO5$SMQYR[!is.na(DEMO5$SMD030)]<-DEMO5$RIDAGEYR-DEMO5$SMD030# 可以去掉SMD030了
DEMO5<-DEMO5 %>% filter(!is.na(SMQTYPE))



# Combine education level
DEMO5$EDULEVEL[DEMO5$DMDEDUC3<=12]<-"Below High school"
DEMO5$EDULEVEL[DEMO5$DMDEDUC3==14]<-"High school graduate"
DEMO5$EDULEVEL[DEMO5$DMDEDUC3==13]<-"High school graduate"
DEMO5$EDULEVEL[DEMO5$DMDEDUC3==55&DEMO5$DMDEDUC3==66]<-"Below High school"
DEMO5$EDULEVEL[DEMO5$DMDEDUC3==77|DEMO5$DMDEDUC3==99]<-NA
DEMO5$EDULEVEL[DEMO5$DMDEDUC3==15]<-"College or above"
DEMO5$EDULEVEL[DEMO5$DMDEDUC2<=2]<-"Below High school"
DEMO5$EDULEVEL[DEMO5$DMDEDUC2==3]<-"High school graduate"
DEMO5$EDULEVEL[DEMO5$DMDEDUC2>=4]<-"College or above"
DEMO5 <- subset(DEMO5, select = -c(DMDEDUC2, DMDEDUC3) )



# 合并gum disease and decay teeth 为gum_decayteeth:
DEMO5[c("OHAROCDT", "OHAROCGP")][is.na(DEMO5[c("OHAROCDT", "OHAROCGP")])] <- 3
DEMO5 <- DEMO5 %>%mutate(GUM_DECAYTEETH = pmin(OHAROCDT,OHAROCGP))# 如果值为3则代表是missing value。把NA值变成3然后取一行的最小值
DEMO5 <- DEMO5 %>%
  mutate(.,GUM_DECAYTEETH = na_if(GUM_DECAYTEETH, 3))
DEMO5 <- subset(DEMO5, select = -c(OHAROCDT,OHAROCGP) )#删除用来合并的变量
DEMO5$GUM_DECAYTEETH[DEMO5$GUM_DECAYTEETH==1]<-"Yes"
DEMO5$GUM_DECAYTEETH[DEMO5$GUM_DECAYTEETH==2]<-"No"

# 改名SEX:
DEMO5$RIAGENDR[DEMO5$RIAGENDR==1]<-"Male"
DEMO5$RIAGENDR[DEMO5$RIAGENDR==2]<-"Female"

# 改名race：
DEMO5$RIDRETH1[DEMO5$RIDRETH1==1]<-"Mexican American"
DEMO5$RIDRETH1[DEMO5$RIDRETH1==2]<-"Other Hispanic"
DEMO5$RIDRETH1[DEMO5$RIDRETH1==3]<-"Non-Hispanic White"
DEMO5$RIDRETH1[DEMO5$RIDRETH1==4]<-"Non-Hispanic Black"
DEMO5$RIDRETH1[DEMO5$RIDRETH1==5]<-"Other Race - Including Multi-Racial"


# 多久前看的牙医分类
DEMO5$LAST_DENTAL_SESSION[DEMO5$OHQ030==1]<-"Within 6 months"
DEMO5$LAST_DENTAL_SESSION[DEMO5$OHQ030==2]<-"Within 2 years"
DEMO5$LAST_DENTAL_SESSION[DEMO5$OHQ030==3]<-"Within 2 years"
DEMO5$LAST_DENTAL_SESSION[DEMO5$OHQ030>3]<-"More than 2 years even never"

#DEMO5 <- subset(DEMO5, select = -c(OHQ030) )

# 可丁宁分层：
DEMO5$COTLEVEL[DEMO5$LBXCOT>10.34]<-"HIGH"
DEMO5$COTLEVEL[DEMO5$LBXCOT<=10.34]<-"NORMAL"

# 血铅分层：
DEMO5$BLLEVEL[DEMO5$LBXBPB>=3.5]<-"Elevated"
DEMO5$BLLEVEL[DEMO5$LBXBPB<3.5]<-"Control"



# 按照OH分层,设置OH（overal health）二项值1是不好，0为好
DEMO5$OH[DEMO5$OHAREC == 1]<-1
DEMO5$OH[DEMO5$OHAREC == 2]<-1
DEMO5$OH[DEMO5$OHAREC == 3]<-1
DEMO5$OH[DEMO5$OHAREC == 4]<-0  #good
DEMO5<-DEMO5 %>% filter(!is.na(OH))


# 查看不同吸烟者口腔状况对比
library('ggplot2')
ggplot(DEMO5,aes(x = SMQTYPE,fill=factor(OH))) +
  geom_bar() +
  ggtitle("Smoker type v/s Overall Oral Health Recommendation")+
  xlab("Smoker Type") +
  ylab("Total Count") +
  labs(fill = "Oral Health") 

#OHQ850 改数据：
DEMO5$OHQ850[DEMO5$OHQ850==9|DEMO5$OHQ850==7]<-NA
DEMO5$OHQ850[DEMO5$OHQ850==1]<-"Yes"
DEMO5$OHQ850[DEMO5$OHQ850==2]<-"No"



# Table one
library(tableone)
showvar<-c("RIAGENDR", "RIDRETH1","RIDAGEYR","INDFMPIR", "BMXBMI","LBXBPB","LBXCOT","ALQ130","HIQ011","DIQ010","SMQTYPE","LAST_DENTAL_SESSION", "EDULEVEL","HEALTH")
numvar<- c("RIDAGEYR", "LBXBPB", "LBXCOT")
catvar<-c("RIAGENDR", "RIDRETH1", "OH", "LBDCOTLC","GUM_DECAYTEETH", "OHXCART", "SMQ040", "SMQ020", "LAST_DENTAL_SESSION", "EDULEVEL", "OHQ850", "BMXBMI", "INDFMPIR", "ALQ130", "HIQ011", "DIQ010")
catvar2<-c("RIAGENDR", "RIDRETH1", "SMQTYPE", "LAST_DENTAL_SESSION", "EDULEVEL","BMXBMI", "INDFMPIR", "ALQ130", "HIQ011", "DIQ010", "HEALTH")

#按照SMQTYPE分层(需要注意那些需要展示和变量分类确认)
tab <- CreateTableOne( strata = "SMQTYPE" , data = DEMO5, factorVars = catvar)
print(tab, formatOptions = list(big.mark = ","), nonnormal =numvar)

# 按照OH分层(需要注意那些需要展示和变量分类确认)
tab21 <- CreateTableOne( vars = showvar, strata = "OH" , data = DEMO5, factorVars = catvar2)
TABLE<-print(tab21, formatOptions = list(big.mark = ","), nonnormal =numvar, showAllLevels=TRUE)
write.csv(TABLE,file = 'table分OH.csv')

tab21 <- CreateTableOne(vars = showvar, data = DEMO5, factorVars = catvar2)
TABLE<-print(tab21, formatOptions = list(big.mark = ","), nonnormal =numvar, showAllLevels=TRUE)
write.csv(TABLE,file = 'table总.csv')



#univariable analysis selection
p2<-DEMO5$OH
Oral_Health_Condition<-DEMO5$OH
#AGE
f0<-glm(p2~RIDAGEYR,family=binomial() ,data = DEMO5)
summary(f0)
exp(confint(f0))#CI
exp(f0$coef)#OR#YES

#SEX
SEX<-as.factor(DEMO5$RIAGENDR)
f0<-glm(p2~SEX,family=binomial() ,data = DEMO5)
summary(f0)
exp(confint(f0))#CI
exp(f0$coef)#OR#YES

#SMQTYPE!!!!!!!!!!!!!!!
SMOKER<-as.factor(DEMO5$SMQTYPE)
f0<-glm(p2~SMOKER, family=binomial(), data = DEMO5)
summary(f0)
exp(confint(f0))#CI
exp(f0$coef)#OR#YES

#AVG CIG PER DAY
f0<-glm(p2~SMD650, family=binomial(), data = DEMO5)
summary(f0)
exp(confint(f0))#CI
exp(f0$coef)#OR#YES


#smoking years
f0<-glm(p2~SMQYR, family=binomial(), data = DEMO5)
summary(f0)
exp(confint(f0))#CI!!!!!
exp(f0$coef)#OR#NO

# RACE
RACE<-as.factor(DEMO5$RIDRETH1)
f0<-glm(p2~RACE, family=binomial(), data = DEMO5)
summary(f0)
exp(confint(f0))#CI!!!!!
exp(f0$coef)#OR#NO

#INDFMPIR
PIR<-as.factor(DEMO5$INDFMPIR)
f0<-glm(p2~PIR, family=binomial(), data = DEMO5)
summary(f0)
exp(confint(f0))#CI!!!!!
exp(f0$coef)#OR YES

#BMI
BMI<-as.factor(DEMO5$BMXBMI)
f0<-glm(p2~BMI, family=binomial(), data = DEMO5)
summary(f0)
exp(confint(f0))#CI!!!!!
exp(f0$coef)#OR YES

#Age starting regularly smoking
f0<-glm(p2~SMD030, family=binomial(), data = DEMO5)
summary(f0)
exp(confint(f0))#CI!!!!!
exp(f0$coef)#OR YES

#blood lead level
f0<-glm(p2~LBXBPB, family=binomial(), data = DEMO5)
summary(f0)
exp(confint(f0))#CI!!!!!
exp(f0$coef)#OR YES

#Cotinine

f0<-glm(p2~LBXCOT, family=binomial(), data = DEMO5)
summary(f0)
exp(confint(f0))#CI!!!!!
exp(f0$coef)#OR YES

#oral health history
OHHIST<-as.factor(DEMO5$OHQ850)
f0<-glm(p2~OHHIST, family=binomial(), data = DEMO5)
summary(f0)
exp(confint(f0))#CI!!!!!
exp(f0$coef)#OR YES

#牙病状况Non/Root caries(删除！！！！！！！！！！！！！！！！！！)
ROOT<-as.factor(DEMO5$OHXCART)
f0<-glm(p2~ROOT, family=binomial(), data = DEMO5)
summary(f0)
exp(confint(f0))#CI!!!!!
exp(f0$coef)#OR NO

#EDUCATION LEVEL
EDU<-as.factor(DEMO5$EDULEVEL)
f0<-glm(p2~EDU, family=binomial(), data = DEMO5)
summary(f0)
exp(confint(f0))#CI!!!!!
exp(f0$coef)#OR YES

# GUM/DECAY TEETH:
GUM_DECAY<-as.factor(DEMO5$GUM_DECAYTEETH)
f0<-glm(p2~GUM_DECAY, family=binomial(), data = DEMO5)
summary(f0)
exp(confint(f0))#CI!!!!!
exp(f0$coef)#OR YES# TOO HIGH!!!!!!!!!!!!!!!

#Last dental session
SESSION<-as.factor(DEMO5$LAST_DENTAL_SESSION)
f0<-glm(p2~SESSION, family=binomial(), data = DEMO5)
summary(f0)
exp(confint(f0))#CI!!!!!
exp(f0$coef)#OR YES

# DETECTABLE Cotinine:
DETECT_COT<-as.factor(DEMO5$LBDCOTLC)
f0<-glm(p2~DETECT_COT, family=binomial(), data = DEMO5)
summary(f0)
exp(confint(f0))#CI!!!!!
exp(f0$coef)#OR YES

# 保险
INSURANCE<-as.factor(DEMO5$HIQ011)
f0<-glm(p2~INSURANCE, family=binomial(), data = DEMO5)
summary(f0)
exp(confint(f0))#CI!!!!!
exp(f0$coef)#OR YES

#酒
DRINK<-as.factor(DEMO5$ALQ130)
f0<-glm(p2~DRINK, family=binomial(), data = DEMO5)
summary(f0)
exp(confint(f0))#CI!!!!!
exp(f0$coef)#OR YES

#糖尿：
DIA<-as.factor(DEMO5$DIQ010)
f0<-glm(p2~DIA, family=binomial(), data = DEMO5)
summary(f0)
exp(confint(f0))#CI!!!!!
exp(f0$coef)#OR NO

#健康：
H<-as.factor(DEMO5$HEALTH)
DEMO5$HEALTH1<-relevel(H,ref="POOR")
f0<-glm(p2~HEALTH1, family=binomial(), data = DEMO5)
summary(f0)
exp(confint(f0))#CI!!!!!
exp(f0$coef)#OR NO

#口腔不适频率：
I<-as.factor(DEMO5$UNCOMFFREQ)
DEMO5$I2<-relevel(I,ref="Hardly or never")
f0<-glm(p2~I2, family=binomial(), data = DEMO5)
summary(f0)
exp(confint(f0))#CI!!!!!
exp(f0$coef)#OR NO



# Multivariables analysis:p2~EDU+RIDAGEYR+RACE+SESSION+SEX+SMOKER+INDFMPIR+BMXBMI+LBXBPB+LBXCOT+OHHIST//(+OHHIST+DRINK)
#f2<-glm(p2~SESSION+RIDAGEYR+RACE+SEX+SMOKER+{DRINK}+INSURANCE+PIR+BMI+{LBXBPB+LBXCOT+OHHIST}+EDU+(DIA1), family=binomial(), data = DEMO5)
f2<-glm(p2~SESSION+RIDAGEYR+RACE+SEX+SMOKER+INSURANCE+PIR+BMI+EDU+HEALTH1, family=binomial(), data = DEMO5)
summary(f2)
A<-data.frame(exp(confint(f2)))#CI!!!!!
B<-data.frame(exp(f2$coef))#OR
with(summary(f2), 1 - deviance/null.deviance)



#forest plot:
library(readxl)
library(forestplot)
dt <- read_excel("forest3.xlsx")
attach(dt)
forestplot(labeltext = as.matrix(dt[,1:5]),##研究下刻度
           #设置用于文本展示的列，此处我们用数据的前六列作为文本，在图中展示
           mean = dt$OR, #设置均值
           lower = dt$LowerCI, #设置均值的下限
           upper = dt$UpperCI, #设置均值的上限
           is.summary = c(F,T,T,F,T,F,F,F,F,T,F,F,T,F,F,T,F,T,F,F,T,F,F,T,F,F,T,F,T,F,T,F),
           #该参数接受一个逻辑向量，用于定义数据中的每一行是否是汇总值，若是，则在对应位置设置为TRUE，若否，则设置为FALSE；设置为TRUE的行则以粗体出现
           zero = 1, #设置参照值，此处我们展示的是HR值，故参照值是1，而不是0
           boxsize = 0.2, #设置点估计的方形大小
           lineheight = unit(7,'mm'),#设置图形中的行距
           xlab = "<---Good Oral Health--     --Bad Oral Health--->    ",
           colgap = unit(10,'mm'),#设置图形中的列间距
           lwd.zero = 2,#设置参考线的粗细
           lwd.ci = 2,#设置区间估计线的粗细
           lwd.xaxis=2,#设置X轴线的粗细
           xticks = c(0,0.5,1, 1.5,2, 2.5,3),
           xlog=FALSE,
           graphwidth = unit(0.4,"npc"),
           ci.vertices.height = 0.1,
           #clip = c(0.2,1.3), # 设置森林图展示的可信区间范围，超过的部分用箭头展示
           grid = FALSE,
           lty.ci = 1,
           col=fpColors(box='#458B00',  summary= "#8B008B",lines = 'black',zero = '#7AC5CD'),
           #使用fpColors()函数定义图形元素的颜色，从左至右分别对应点估计方形，汇总值，区间估计线，参考线
           graph.pos = 2)#设置森林图的位置，此处设置为5，则出现在第五列






#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# 分层细化：前吸烟者中不同程度 （月为单位）戒烟者的口腔(需要考虑上次抽烟到现在什么时候SMD055) 
TEST1<- DEMO5 %>% filter(SMQTYPE=="Former Smoker")
TEST1[ , 'QUITTIME'] <- NA
for(i in 1:nrow(TEST1)) { 
  if (!is.na(TEST1[i,"SMQ050U"]) && TEST1[i,"SMQ050U"]==1) {
    TEST1[i,"QUITTIME"] <- TEST1[i, "SMQ050Q"] * (1/30)
  }
  else if (!is.na(TEST1[i,"SMQ050U"]) &&TEST1[i,"SMQ050U"]==2)
  {
    TEST1[i,"QUITTIME"] <- TEST1[i, "SMQ050Q"] * (1/4)
  }
  else if (!is.na(TEST1[i,"SMQ050U"]) &&TEST1[i,"SMQ050U"]==4){
    TEST1[i,"QUITTIME"] <- TEST1[i, "SMQ050Q"]*12
  }
  else if (!is.na(TEST1[i,"SMQ050U"]) && TEST1[i,"SMQ050U"]==3) {
    TEST1[i,"QUITTIME"] <- TEST1[i, "SMQ050Q"]
  }
  else if (!is.na(TEST1[i,"SMQ050U"]) && TEST1[i,"SMQ050U"]==66666) {
    TEST1[i,"QUITTIME"] <- 25
  }else{
    TEST1[i,"QUITTIME"] <- NA
  }
}
TEST1<- TEST1 %>% filter(!is.na(QUITTIME))

#TEST
TEST1$SMQTYPE2[TEST1$QUITTIME<=24]<-"Quit smoking less than 24 months"
TEST1$SMQTYPE2[TEST1$QUITTIME>24 & TEST1$QUITTIME<=60]<-"Quit smoking 25 to 60 months"
TEST1$SMQTYPE2[TEST1$QUITTIME>60]<-"Quit smoking more than 60 months"


# Boxplot#忽略NA！！！！！！！！！！
Oral_health<-as.factor(TEST1$OH)
ggplot(data=TEST1, mapping = aes(x = Oral_health, y = QUITTIME))+
  geom_boxplot() +
  theme_bw()

# barplot!!
# TEST1$X<-relevel(TEST1$SMQTYPE2, ref="Quit smoking less than 24 months")
# ggplot(TEST1,aes(x = X,fill=factor(OH))) +
#   geom_bar() +
#   ggtitle("Smoker type v/s Overall Oral Health Recommendation")+
#   xlab("Years quit smoking") +
#   ylab("Total Count") +
#   labs(fill = "Oral Health")

# barplot 百分比！
SMOKER_OH <- table(TEST1$OH,TEST1$SMQTYPE2)
# Plot the conditional distributions
barplot( prop.table(SMOKER_OH, 2),
         legend.text = TRUE,
         ylab = "Proportion SMOKER",
         xlab = "OH"
)



#TEST1$SMQTYPE2[TEST1$OH==0 & TEST1$SMQ040==3]<-"Healthy former smoker"

# table one
showvar1<-c("SMQTYPE2","RIAGENDR", "RIDRETH1","RIDAGEYR","INDFMPIR", "BMXBMI","LBXBPB","LBXCOT","ALQ130","HIQ011","DIQ010","SMQTYPE","LAST_DENTAL_SESSION", "EDULEVEL","HEALTH")
numvar<- c("RIDAGEYR", "BMXBMI", "SMD030", "LBXBPB", "LBXCOT")
catvar<-c("RIAGENDR", "RIDRETH1", "OH", "LBDCOTLC","GUM_DECAYTEETH", "OHXCART", "SMQ040", "SMQ020", "LAST_DENTAL_SESSION", "EDULEVEL", "OHQ850")
catvar2<-c("RIAGENDR", "RIDRETH1","INDFMPIR", "SMQTYPE2", "LBDCOTLC","GUM_DECAYTEETH", "OHXCART", "SMQ040","SMQ020" ,"LAST_DENTAL_SESSION", "EDULEVEL", "OHQ850")


#按照SMQTYPE分层(需要注意那些需要展示和变量分类确认)

tab <- CreateTableOne( strata = "SMQTYPE2" , data = TEST1, factorVars = catvar)
print(tab, formatOptions = list(big.mark = ","), nonnormal =numvar)

# 按照OH分层(需要注意那些需要展示和变量分类确认)
tab2 <- CreateTableOne(vars = showvar1, strata = "OH" , data = TEST1, factorVars = catvar2)
TABLE<-print(tab2, formatOptions = list(big.mark = ","), nonnormal =numvar, showAllLevels=TRUE )
write.csv(TABLE,file = 'table1.1.csv')
tab21 <- CreateTableOne(vars = showvar1, data = TEST1, factorVars = catvar2)
TABLE<-print(tab21, formatOptions = list(big.mark = ","), nonnormal =numvar, showAllLevels=TRUE)
write.csv(TABLE,file = 'table总1.csv')
#table1.1



# glm:
p3<-TEST1$OH
Oral_health_condition<-p3
#SMQTYPE2
SMQTYPE3<-as.factor(TEST1$SMQTYPE2)
f0<-glm(p3~SMQTYPE3, family=binomial(), data = TEST1)
summary(f0)
exp(confint(f0))#CI!!!!!
exp(f0$coef)#OR YES



# multi:
OHHIST1<-as.factor(TEST1$OHQ850)
SESSION1<-as.factor(TEST1$LAST_DENTAL_SESSION)
RACE1<- as.factor(TEST1$RIDRETH1)
SEX1<- as.factor(TEST1$RIAGENDR)
EDU1<-as.factor(TEST1$EDULEVEL)
SMOKER<-as.factor(TEST1$SMQTYPE2)
ROOT1<-as.factor(TEST1$OHXCART)
INSU<-as.factor(TEST1$HIQ011)
DRINK1<-as.factor(TEST1$ALQ130)
TEST1$X<-relevel(SMOKER, ref="Quit smoking less than 24 months")
H1<-as.factor(TEST1$HEALTH)
TEST1$HEALTH2<-relevel(H1,ref="POOR")
f2<-glm(p3~SESSION1+RIDAGEYR+RACE1+SEX1+X+INDFMPIR+BMXBMI+EDU1+INSU+HEALTH2, family=binomial(), data = TEST1)
summary(f2)
A1<-data.frame(exp(confint(f2)))#CI!!!!!
B1<-data.frame(exp(f2$coef))#OR
with(summary(f2), 1 - deviance/null.deviance)

#forestplot:
dt1 <- read_excel("forest3.1.xlsx")
attach(dt1)
forestplot(labeltext = as.matrix(dt1[,1:5]),##研究下刻度
           #设置用于文本展示的列，此处我们用数据的前六列作为文本，在图中展示
           mean = dt1$OR, #设置均值
           lower = dt1$LowerCI, #设置均值的下限
           upper = dt1$UpperCI, #设置均值的上限
           is.summary = c(F,T,T,F,T,F,F,F,F,T,F,F,T,F,F,T,F,T,F,F,T,F,F,T,F,F,T,F,T,F,T,F),
           #该参数接受一个逻辑向量，用于定义数据中的每一行是否是汇总值，若是，则在对应位置设置为TRUE，若否，则设置为FALSE；设置为TRUE的行则以粗体出现
           zero = 1, #设置参照值，此处我们展示的是HR值，故参照值是1，而不是0
           boxsize = 0.2, #设置点估计的方形大小
           lineheight = unit(7,'mm'),#设置图形中的行距
           xlab = "<---Good Oral Health--  --Bad Oral Health--->",
           colgap = unit(7,'mm'),#设置图形中的列间距
           lwd.zero = 2,#设置参考线的粗细
           lwd.ci = 2,#设置区间估计线的粗细
           lwd.xaxis=2,#设置X轴线的粗细
           xticks = c(0,0.5,1, 1.5,2, 2.5,3),
           xlog=FALSE,
           graphwidth = unit(0.4,"npc"),
           ci.vertices.height = 0.1,
           #clip = c(1,1), # 设置森林图展示的可信区间范围，超过的部分用箭头展示
           grid = FALSE,
           lty.ci = 1,
           col=fpColors(box='#458B00',  summary= "#8B008B",lines = 'black',zero = '#7AC5CD'),
           #使用fpColors()函数定义图形元素的颜色，从左至右分别对应点估计方形，汇总值，区间估计线，参考线
           graph.pos = 2)#设置森林图的位置，此处设置为5，则出现在第五列