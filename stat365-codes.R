library(ggplot2)
library(dplyr)
library(ggmosaic)
library(xlsx)
library(survey)
library(readxlsx)
library(plotrix)
getwd()
setwd("C:/Users/90507/Desktop")
mydata <- read.xlsx("STAT365.xlsx", sheetName = "sayfa1")
names(mydata)<-c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11EK","Q11","Q12","Q13","Q14","Q15","Q16","Q17","Q18","Q19","Q20")
mydata$Q14F <- as.factor(mydata$Q14)
#two stages cluster
fpc <- data.frame(Q4=c("Faculty of Arts and Sciences","Faculty of Engineering",
                       "Faculty of Economic and Administrative Sciences", "Faculty of Architecture"),
                  fpc1=c(6,6,6,6),fpc2=c(43,72,14,6), weight=c((6/4)*4423/43,
                                                               (6/4)*9294/72,
                                                               (6/4)*2162/14,
                                                               (6/4)*1412/6))
table(mydata$Q1)
tbl <- c(64,70,1) 
pielbl <- c("female","male","other")
pie3D(tbl,labels=pielbl,explode=0.1, main="Pie Chart of Gender")
hist(mydata$Q2,xlab = "Birth Year of Students",main="Birth Year Of Students")
summary(mydata$Q2)
summary(mydata$Q18)
summary(mydata)
summary(mydata$Q14)
mydata <- left_join(mydata, fpc)
mydata$ID <- 1:nrow(mydata)
mydata$Q11 <- as.numeric(mydata$Q11)
design <- svydesign(ids = ~Q4+ID, fpc = ~fpc1+fpc2, data = mydata)
svymean(~Q11, design)
#Mean CGPA among METU students is estimated. two stage cluster is applied, and 4 faculties are choosen from 5 faculties.
svymean(~Q14, design)
#Mean anxiety level is estimated.
svymean(~Q13, design)
#Mean values of whether students have job anxiety or not.

#RESEARCH QUESTION 1.(COUNTRY VS JOB ANXYETY LEVEL)
svyboxplot(Q14~Q3, design)
#There is no difference in median values and distributions of anxiety level based on countries of students' families.
ggplot(mydata, aes(x=Q3, y=Q14))+
  geom_boxplot()
ggplot(mydata, aes(x=Q14, color=Q3))+
  geom_density()
#RESEARCH QUESTION 2.
cgpa<-as.factor(ifelse(mydata$Q11>median(mydata$Q11),"Above","Below"))
anxiety<-as.factor(mydata$Q14)
ggplot(mydata) + 
  geom_mosaic(aes(weight = weight, x = product(anxiety, cgpa), fill=anxiety, color = "black"))+
  labs(title="Mosaic Plot of the Relationship between CGPA and Level of Anxiety", x = "CGPA",
       y = "Anxiety", fill = "Anxiety Level")
model <- svyglm(Q14~Q11, design)
summary(model)
svyboxplot(Q11~Q14F, design)
#There is no significant relationship between CGPA and job anxiety level.


#RESEARCH 3.(Does ANXIETY LEVEL CHANGE ACCORDING TO FACULTY)
Q11 <- as.numeric(mydata$Q11)
Q14 <- mydata$Q14
svychisq(~Q14F+Q4, design)
svyby(~Q14,~Q4, svymean, design = design)
#RESEARCH QUESTION 4.
svychisq(~Q14F+Q15, design)
svychisq(~Q14F+Q16, design)
svyranktest(Q14~Q15, design,test = "KruskalWallis")
#Mean values of answers for question "Have you ever looked for an internship?" is different.
