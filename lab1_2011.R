library(ggplot2)
library(scales)

a=read.csv(file = "G:\\Required\\6th Sem\\DA\\Lab\\Lab1\\2011.csv",head=TRUE, sep = ",", nrows = 41)
a
summary(a)
var(a)
mean(a$Live_births)
median(a$Still_births)
mode(a$Vital_Births)
var(a$Deaths)
sd(a$Percentage_Births)
IQR(a$Vital_Births)

plot(a)

hist(a$Percentage_Births)
hist(a$Percentage_Deaths)

ggplot(a,aes(x=a$Year,y=a$Deaths))+  geom_line() + xlim(1970, 2011) + scale_y_continuous(limits=c(87000,390000))
ggplot(a,aes(x=a$Percentage_Births,y=a$Percentage_Deaths))+  geom_line()
ggplot(a, aes(x=a$Year, y=a$Live_births)) + geom_line()
ggplot(a, aes(x=a$Year, y=a$Still_births)) + geom_col()
ggplot(a, aes(x=a$Year,y=a$Deaths)) + geom_boxplot()
IQR(a$Deaths)

outlier=boxplot.stats(a$Still_births)$out
boxplot(a$Still_births, main="Deaths", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier, collapse = ", ")),cex=0.6)

pie(a$Vital_Births)
pie(a$Vital_Deaths)

##############################################################################################


b=read.csv(file = "G:\\Required\\6th Sem\\DA\\Lab\\Lab1\\2011.csv",head=TRUE, sep = ",", skip=46, nrows=30)
b
summary(b)
mean(b$Births_Registered)
median(b$Birth_Rate)
mode(b$Death_Registered)
var(b$Rate)
sd(b$Registered_Infant_Death)
IQR(b$Still.Birth_Registered)
var(b$Still.Birth_Rate)

plot(b)

hist(b$Births_Registered)
ggplot(b, aes(x=b$Births_Registered,y=b$Death_Registered)) + geom_line()
ggplot(b, aes(x=b$Birth_Rate, y=b$Rate)) + geom_line()
ggplot(b, aes(x=b$Still.Birth_Rate, y=b$Still.Birth_Registered)) + geom_col()

pie(b$Registered_Infant_Death)

barplot(b$Registered_Infant_Death, names.arg = b$District)
barplot(b$Births_Registered, names.arg = b$District)
barplot(b$Death_Registered, names.arg = b$District)
barplot(b$Registered_Infant_Death, names.arg = b$District)


outlier1=boxplot.stats(b$Births_Registered)$out
boxplot(b$Births_Registered, main="Births Registered", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier1, collapse = ", ")),cex=0.6)

outlier2=boxplot.stats(b$Death_Registered)$out
boxplot(b$Death_Registered, main="Death Registered", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier2, collapse = ", ")),cex=0.6)

outlier3=boxplot.stats(b$Rate)$out
boxplot(b$Rate, main="Death Rate", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier3, collapse = ", ")),cex=0.6)

outlier4=boxplot.stats(b$Registered_Infant_Death)$out
boxplot(b$Registered_Infant_Death, main="Registered Infant Death", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier4, collapse = ", ")),cex=0.6)

outlier5=boxplot.stats(b$Still.Birth_Registered)$out
boxplot(b$Still.Birth_Registered, main="Still Birth Registered", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier5, collapse = ", ")),cex=0.6)

##########################################################################################


c=read.csv(file = "G:\\Required\\6th Sem\\DA\\Lab\\Lab1\\2011.csv",head=TRUE, sep = ",", skip=83, nrows=30)
c
summary(c)

var(c$Birth_Registered)
var(c$Birth_Rate)
var(c$Death_Registered)
var(c$Death_Rate)
var(c$Registered.Infant.Death)
var(c$Still.Birth_Registered)
var(c$Still.Birth_Rate)

sd(c$Birth_Registered)
sd(c$Birth_Rate)
sd(c$Death_Registered)
sd(c$Death_Rate)
sd(c$Registered.Infant.Death)
sd(c$Still.Birth_Registered)
sd(c$Still.Birth_Rate)


plot(c)

ggplot(c, aes(x=c$Birth_Registered,y=c$Death_Registered)) + geom_line()
ggplot(c, aes(x=c$Birth_Rate, y=c$Death_Rate)) + geom_line()
ggplot(c, aes(x=c$Still.Birth_Rate, y=c$Still.Birth_Registered)) + geom_col()

pie(c$Registered.Infant.Death)

barplot(c$Registered.Infant.Death, names.arg = c$District)
barplot(c$Birth_Registered, names.arg = c$District)
barplot(c$Death_Registered, names.arg = c$District)
barplot(c$Registered.Infant.Death, names.arg = c$District)


outlier6=boxplot.stats(c$Birth_Registered)$out
boxplot(c$Birth_Registered, main="Birth Registered", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier6, collapse = ", ")),cex=0.6)

outlier7=boxplot.stats(c$Death_Registered)$out
boxplot(c$Death_Registered, main="Death Registered", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier7, collapse = ", ")),cex=0.6)

outlier8=boxplot.stats(c$Death_Rate)$out
boxplot(c$Death_Rate, main="Death Rate", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier8, collapse = ", ")),cex=0.6)

outlier9=boxplot.stats(c$Registered.Infant.Death)$out
boxplot(c$Registered.Infant.Death, main="Deaths", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier9, collapse = ", ")),cex=0.6)

outlier10=boxplot.stats(c$Still.Birth_Registered)$out
boxplot(c$Still.Birth_Registered, main="Still Birth Registered", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier10, collapse = ", ")),cex=0.6)

outlier11=boxplot.stats(c$Still.Birth_Rate)$out
boxplot(c$Still.Birth_Rate, main="Still Birth Rate", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier11, collapse = ", ")),cex=0.6)


#################################################################################################################



d=read.csv(file = "G:\\Required\\6th Sem\\DA\\Lab\\Lab1\\2011.csv",head=TRUE, sep = ",", skip=119, nrows=30)
d
summary(d)

var(d$Birth_Registered)
var(d$Birth_Rate)
var(d$Death_Registered)
var(d$Death_Rate)
var(d$Registered.Infant.Death)
var(d$Still_Birth_Registered)
var(d$Still_Birth_Rate)

sd(d$Birth_Registered)
sd(d$Birth_Rate)
sd(d$Death_Registered)
sd(d$Death_Rate)
sd(d$Registered.Infant.Death)
sd(d$Still_Birth_Registered)
sd(d$Still_Birth_Rate)


plot(d)

ggplot(d, aes(x=d$Birth_Registered,y=d$Death_Registered)) + geom_line()
ggplot(d, aes(x=d$Birth_Rate, y=d$Death_Rate)) + geom_line()
ggplot(d, aes(x=d$Still_Birth_Rate, y=d$Still_Birth_Registered)) + geom_col()

pie(d$Registered.Infant.Death)

barplot(d$Registered.Infant.Death, names.arg = d$District)
barplot(d$Birth_Registered, names.arg = d$District)
barplot(d$Death_Registered, names.arg = d$District)
barplot(d$Registered.Infant.Death, names.arg = d$District)


outlier12=boxplot.stats(d$Birth_Registered)$out
boxplot(d$Birth_Registered, main="Birth Registered", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier12, collapse = ", ")),cex=0.6)

outlier13=boxplot.stats(d$Death_Registered)$out
boxplot(d$Death_Registered, main="Death Registered", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier13, collapse = ", ")),cex=0.6)

outlier14=boxplot.stats(d$Registered.Infant.Death)$out
boxplot(d$Registered.Infant.Death, main="Registered Infant Death", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier14, collapse = ", ")),cex=0.6)

outlier15=boxplot.stats(d$Still_Birth_Registered)$out
boxplot(d$Still_Birth_Registered, main="Still Birth Registered", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier15, collapse = ", ")),cex=0.6)


###################################################################################################################


e=read.csv(file = "G:\\Required\\6th Sem\\DA\\Lab\\Lab1\\2011.csv",head=TRUE, sep = ",", skip=165, nrows=30)
e
summary(e)

var(e$Actual)
var(e$Adjusted.for.incomplete.returns)
var(e$No.of.Registration.Units)
var(e$No..of.monthly.Returnd.Due)
var(e$No.of.Monthly.Returns.not.received)
var(e$Total)
var(e$Adjusted.for.incomplete.returns.1)

sd(e$Actual)
sd(e$Adjusted.for.incomplete.returns)
sd(e$No.of.Registration.Units)
sd(e$No..of.monthly.Returnd.Due)
sd(e$No.of.Monthly.Returns.not.received)
sd(e$Total)
sd(e$Adjusted.for.incomplete.returns.1)

q1=min(e$Actual)
q2=min(e$Total)
q3=max(e$Actual)
q4=max(e$Total)
plot(e)
hist(e$Actual)
ggplot(e, aes(x=e$No.of.Registration.Units,y=e$No..of.monthly.Returnd.Due)) + geom_line()
ggplot(d, aes(x=e$No.of.Registration.Units, y=e$No.of.Monthly.Returns.not.received)) + geom_line()
ggplot(d, aes(x=e$Actual, y=e$Total)) + geom_area()

pie(e$Adjusted.for.incomplete.returns.1)

barplot(e$Actual, names.arg = e$Districts)
barplot(e$No.of.Registration.Units, names.arg = e$Districts)
barplot(e$No.of.Monthly.Returns.not.received, names.arg = e$Districts)
barplot(e$Total, names.arg = e$Districts)


outlier16=boxplot.stats(e$Actual)$out
boxplot(e$Actual, main="Actual Population", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier16, collapse = ", ")),cex=0.6)

outlier17=boxplot.stats(e$Adjusted.for.incomplete.returns)$out
boxplot(e$Adjusted.for.incomplete.returns, main="Adjusted for incomplete returns", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier17, collapse = ", ")),cex=0.6)

outlier18=boxplot.stats(e$No.of.Registration.Units)$out
boxplot(e$No.of.Registration.Units, main="No of Registration Units", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier18, collapse = ", ")),cex=0.6)

outlier19=boxplot.stats(e$No..of.monthly.Returnd.Due)$out
boxplot(e$No..of.monthly.Returnd.Due, main="No of monthly Returnd Due", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier19, collapse = ", ")),cex=0.6)

outlier20=boxplot.stats(e$No.of.Monthly.Returns.not.received)$out
boxplot(e$No.of.Monthly.Returns.not.received, main="No of Monthly Returns not received", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier20, collapse = ", ")),cex=0.6)

outlier21=boxplot.stats(e$Total)$out
boxplot(e$Total, main="Total Population", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier21, collapse = ", ")),cex=0.6)

outlier22=boxplot.stats(e$Adjusted.for.incomplete.returns.1)$out
boxplot(e$Adjusted.for.incomplete.returns.1, main="Adjusted for.incomplete returns 1", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier22, collapse = ", ")),cex=0.6)


##############################################################################################################


f=read.csv(file = "G:\\Required\\6th Sem\\DA\\Lab\\Lab1\\2011.csv",head=TRUE, sep = ",", skip=212, nrows=30)
f
summary(f)

var(f$Actual)
var(f$Adjusted.for.incomplete.returns)
var(f$No.of.Registration.Units)
var(f$No.of.Monthly.Returns.due)
var(f$No.of.Monthly.Returns.not.received)
var(f$Total)
var(f$Adjusted.for.incomplete.returns.1)

sd(f$Actual)
sd(f$Adjusted.for.incomplete.returns)
sd(f$No.of.Registration.Units)
sd(f$No.of.Monthly.Returns.due)
sd(f$No.of.Monthly.Returns.not.received)
sd(f$Total)
sd(f$Adjusted.for.incomplete.returns.1)

plot(f)
hist(f$Actual)
ggplot(f, aes(x=f$No.of.Registration.Units,y=f$No.of.Monthly.Returns.due)) + geom_line()
ggplot(f, aes(x=f$No.of.Registration.Units, y=f$No.of.Monthly.Returns.not.received)) + geom_line()
ggplot(f, aes(x=f$Actual, y=f$Total)) + geom_area()

pie(f$Adjusted.for.incomplete.returns.1)

barplot(f$Actual, names.arg = f$Districts)
barplot(f$No.of.Registration.Units, names.arg = f$Districts)
barplot(f$No.of.Monthly.Returns.not.received, names.arg = f$Districts)
barplot(f$Total, names.arg = f$Districts)


outlier23=boxplot.stats(f$Actual)$out
boxplot(f$Actual, main="Actual Population", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier23, collapse = ", ")),cex=0.6)

outlier24=boxplot.stats(f$Adjusted.for.incomplete.returns)$out
boxplot(f$Adjusted.for.incomplete.returns, main="Adjusted for incomplete returns", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier24, collapse = ", ")),cex=0.6)

outlier25=boxplot.stats(f$No.of.Registration.Units)$out
boxplot(f$No.of.Registration.Units, main="No of Registration Units", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier25, collapse = ", ")),cex=0.6)


outlier26=boxplot.stats(f$No.of.Monthly.Returns.not.received)$out
boxplot(f$No.of.Monthly.Returns.not.received, main="No of Monthly Returns not received", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier26, collapse = ", ")),cex=0.6)

outlier27=boxplot.stats(f$Total)$out
boxplot(f$Total, main="Total Population", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier27, collapse = ", ")),cex=0.6)

outlier28=boxplot.stats(f$Adjusted.for.incomplete.returns.1)$out
boxplot(f$Adjusted.for.incomplete.returns.1, main="Adjusted for incomplete returns 1", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier28, collapse = ", ")),cex=0.6)



