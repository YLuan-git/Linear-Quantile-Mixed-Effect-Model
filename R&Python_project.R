

#############################################################################################################
################################################# lqmm ######################################################
#############################################################################################################

library(lqmm)

### load data #### 
setwd("/Users/yuanyuanluan/Library/Mobile Documents/com~apple~CloudDocs/TAMU/job & funding/DR.TEKWE/quantile regression/data")
data_w = read.csv("flr_iv_data_gilson.csv",header=TRUE)
head(data_w)
dim(data_w)
names(data_w)

####### data checking ########
## check missing values ####
sum(is.na(data_w))

### drop missing values ###
data_clean <- na.omit(data_w)
str(data_clean$Gender)

### convert gender from numeric to characteriscitc ###
data_clean$Gender <-as.character(data_clean$Gender)
str(data_clean$Gender)



#fit.lqmm <- lqmm(fixed = raw_bmi_springyr2 ~ mean_ee, random = ~ 1, group = Subject, data = data_clean, tau = c(0.25,0.5,0.75), nK = 7, type = "normal")


#fit.lqmm <- lqmm(fixed = raw_bmi_springyr2 ~ mean_ee, random = ~ 1, group = Subject, data = data_clean, tau = c(0.25,0.5,0.75), nK =7, type = "robust")



#fit.lqmm <- lqmm(fixed = raw_bmi_springyr2 ~ mean_ee*Gender, random = ~ 1, group = Subject, data = data_clean, tau = c(0.25,0.5,0.75), nK =7, type = "robust")


###### variance covariance matrix: pdDiag diagonal #####
fit.lqmm <- lqmm(fixed = delta_yr1 ~ mean_ee*Gender, random = ~ 1, group = Subject, data = data_clean, tau = c(0.25,0.5,0.75), nK =7, type = "robust")
fit.lqmm

summary(fit.lqmm)

###### variance covariance matrix: pdIdent multiple of an identity #####
fit.pdIdent <- lqmm(fixed = delta_yr1 ~ mean_ee*Gender, random = ~ 1, group = Subject, data = data_clean, tau = c(0.25,0.5,0.75), nK =7, type = "robust", 
                    covariance = "pdIdent")
fit.pdIdent 

summary(fit.pdIdent )


###### variance covariance matrix: pdSymm general positive–definite matrix #####
fit.pdSymm <- lqmm(fixed = delta_yr1 ~ mean_ee*Gender, random = ~ 1, group = Subject, data = data_clean, tau = c(0.25,0.5,0.75), nK =7, type = "normal", 
                   covariance = "pdSymm")
fit.pdSymm 

summary(fit.pdSymm)
########################################################################################################################
######################## row bmi second semester ######################################################################################
######################## ########################################################################################################################

fit.lqmm_raw<- lqmm(fixed = raw_bmi_springyr1 ~ mean_ee*Gender, random = ~ 1, group = Subject, data = data_clean, tau = c(0.25,0.5,0.75), nK =7, type = "robust")
fit.lqmm_raw
summary(fit.lqmm_raw)

##########################################################################################
########################################## plots ##########################################
##########################################################################################

library(plyr)
library(ggplot2)


#multiplot() for plot multiple plots with ggplot()
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
setwd("/Users/yuanyuanluan/Library/Mobile Documents/com~apple~CloudDocs/TAMU/job & funding/DR.TEKWE/quantile regression/data")
data = read.csv("all_subjects_ee.csv",header=TRUE)
head(data)
dim(data)

#mean EE of day of each student at each time point(count) excluding the NA values. 

EE.studentmean_allday = ddply(data, .(Subject, count), summarize,  EE.studentmean_allday=mean(Energy_expenditure,na.rm=TRUE))

#EE.daymean = aggregate(data[,2], list(data$Subject), mean)

#convert subject to numeric variable since ggplot() require group to be numeric. 
test = as.data.frame(cbind( as.numeric(EE.studentmean_allday$Subject), as.numeric(EE.studentmean_allday$count),EE.studentmean_allday$EE.studentmean_allday))
names(test) = c("Subject","count", "EE.studentmean_allday" )

ggplot(test, aes(x=count, y=EE.studentmean_allday, group=Subject))+geom_line()+geom_point() #+geom_smooth(alpha=.2, size=1)

# mean EE of all students of 5 days by each count
EE.allmean = ddply(data, .(count), summarize,  EE.daymean=mean(Energy_expenditure,na.rm=TRUE))
head(EE.allmean)
ggplot(EE.allmean, aes(x=count, y=EE.daymean))+geom_line()+geom_point() #+geom_smooth(alpha=.2, size=1)

# mean EE of all students of each day by each count
EE.allmean_day = ddply(data, .(day,count), summarize,  EE.daymean=mean(Energy_expenditure,na.rm=TRUE))
head(EE.allmean_day)

#par(mfrow=c(2, 3))
p1 = ggplot(EE.allmean_day[which(EE.allmean_day$day==1),], aes(x=count, y=EE.daymean))+geom_line()+geom_point()+ggtitle("day 1")
p2 = ggplot(EE.allmean_day[which(EE.allmean_day$day==2),], aes(x=count, y=EE.daymean))+geom_line()+geom_point()+ggtitle("day 2")
p3 = ggplot(EE.allmean_day[which(EE.allmean_day$day==3),], aes(x=count, y=EE.daymean))+geom_line()+geom_point()+ggtitle("day 3")
p4 = ggplot(EE.allmean_day[which(EE.allmean_day$day==4),], aes(x=count, y=EE.daymean))+geom_line()+geom_point()+ggtitle("day 4")
p5 = ggplot(EE.allmean_day[which(EE.allmean_day$day==5),], aes(x=count, y=EE.daymean))+geom_line()+geom_point()+ggtitle("day 5")


multiplot(p1, p2, p3, p4,p5, cols=2)

#> `geom_smooth()` using method = 'loess'



#mean EE of each day of each student at each time point(count) excluding the NA values. 

EE.studentmean_eachday = ddply(data, .(Subject, day,count), summarize,  EE.daymean=mean(Energy_expenditure,na.rm=TRUE))
head(EE.studentmean_eachday)

p1 = ggplot(EE.studentmean_eachday[which(EE.studentmean_eachday$day==1),], aes(x=count, y=EE.daymean, group=Subject))+geom_line()+ggtitle("day 1") #+geom_point()
p2 = ggplot(EE.studentmean_eachday[which(EE.studentmean_eachday$day==2),], aes(x=count, y=EE.daymean, group=Subject))+geom_line()+ggtitle("day 2")
p3 = ggplot(EE.studentmean_eachday[which(EE.studentmean_eachday$day==3),], aes(x=count, y=EE.daymean, group=Subject))+geom_line()+ggtitle("day 3")
p4 = ggplot(EE.studentmean_eachday[which(EE.studentmean_eachday$day==4),], aes(x=count, y=EE.daymean, group=Subject))+geom_line()+ggtitle("day 4")
p5 = ggplot(EE.studentmean_eachday[which(EE.studentmean_eachday$day==5),], aes(x=count, y=EE.daymean, group=Subject))+geom_line()+ggtitle("day 5")
multiplot(p1, p2, p3, p4,p5, cols=2)


# add mean of all students to the plots above
p1 = ggplot(data=EE.studentmean_eachday[which(EE.studentmean_eachday$day==1),], aes(x=count, y=EE.daymean, group=Subject))+geom_line()+ggtitle("day 1")+
  geom_line(data=EE.allmean, aes(x=count, y=EE.daymean,group=NULL, colour="blue"))
#NOTE: since the first ggplot has "group" in it, the second one will look for "group" too. Set it to be NULL, since we dont have a group factor for the second plot
p2 = ggplot(data=EE.studentmean_eachday[which(EE.studentmean_eachday$day==2),], aes(x=count, y=EE.daymean, group=Subject))+geom_line()+ggtitle("day 2")+
  geom_line(data=EE.allmean, aes(x=count, y=EE.daymean,group=NULL, colour="blue"))
p3 = ggplot(data=EE.studentmean_eachday[which(EE.studentmean_eachday$day==3),], aes(x=count, y=EE.daymean, group=Subject))+geom_line()+ggtitle("day 3")+
  geom_line(data=EE.allmean, aes(x=count, y=EE.daymean,group=NULL, colour="blue"))
p4 = ggplot(data=EE.studentmean_eachday[which(EE.studentmean_eachday$day==4),], aes(x=count, y=EE.daymean, group=Subject))+geom_line()+ggtitle("day 4")+
  geom_line(data=EE.allmean, aes(x=count, y=EE.daymean,group=NULL, colour="blue"))
p5 = ggplot(data=EE.studentmean_eachday[which(EE.studentmean_eachday$day==5),], aes(x=count, y=EE.daymean, group=Subject))+geom_line()+ggtitle("day 5")+
  geom_line(data=EE.allmean, aes(x=count, y=EE.daymean,group=NULL, colour="blue"))
multiplot(p1, p2, p3, p4,p5, cols=2)






################ BMI vs mean EE stratified by gender #############

qplot( delta_yr1, mean_ee, data=data_clean, geom= "smooth",  color=Gender, 
       main="Distribution of BMI Difference against Mean Energy Expeniture", 
       xlab="Mean Engergy Expeniture", ylab="BMI difference")

################ general BMI vs mean EE #############
qplot( delta_yr1, mean_ee, data=data_clean, geom= "smooth",
       main="Distribution of BMI Difference against Mean Energy Expeniture", 
       xlab="Mean Engergy Expeniture", ylab="BMI difference")

############ density plot of BMI for fall year 1 and spring year1 #########
qplot(raw_bmi_fallyr1, data=data_clean, geom= "density",alpha=I(.5), 
      main="Distribution of BMI at Fisrt Fall", 
      xlab="BMI", ylab="density")

qplot(raw_bmi_springyr1, data=data_clean, geom= "density",alpha=I(.5), 
      main="Distribution of BMI at Fisrt Spring", 
      xlab="BMI", ylab="density")

############## boxplot ##################
boxplot(quantile(data_clean$mean_ee,c(.25,.5,.75)),whisklty = 0, staplelty = 0)
#whisklty=0 gets rid of the lines or whiskers
#staplelty=0 gets rid of the ends or staples

### creat a character variable of mean EE for box plot ####
head(data_clean)
summary(data_clean$mean_ee)
### define below minimum as leve1, between min and 1st quantile as level2, 
#between 1st and median as level 3, between median and 3rd as level4, more than 3rd as level5.
attach(mydata)
mydata$agecat[age > 75] <- "Elder"
mydata$agecat[age > 45 & age <= 75] <- "Middle Aged"
mydata$agecat[age <= 45] <- "Young"
detach(mydata)

attach(data_clean)
data_clean$meanEELevel[mean_ee< 0.3291] <-"level 1"
data_clean$meanEELevel[mean_ee=0.3291] <-"level 1"
data_clean$meanEELevel[mean_ee< 0.9809 & 0.3291<mean_ee] <-"level 2"
data_clean$meanEELevel[mean_ee =0.9809] <-"level 2"
data_clean$meanEELevel[0.9809 < mean_ee & mean_ee < 1.2207] <-"level 3"
data_clean$meanEELevel[mean_ee = 1.2207] <-"level 3"
data_clean$meanEELevel[1.2207<mean_ee & mean_ee<1.3133] <-"level 4"
data_clean$meanEELevel[mean_ee = 1.3133] <-"level 4"
data_clean$meanEELevel[1.3133 < mean_ee & mean_ee< 1.5428] <-"level 5"
data_clean$meanEELevel[mean_ee = 1.5428] <-"level 5"
data_clean$meanEELevel[mean_ee > 1.5428] <-"level 6"
detach(data_clean)
head(data_clean)


## Data and plot settings
dev.off()

p <- ggplot(data_clean, aes(factor(meanEELevel), delta_yr1)) +
  labs(list(title = "Chicken weights after six weeks", x = "Energy expeniture", y = "BMI Difference")) +
  theme(axis.title.x = element_text(face="bold"), axis.text.x = element_text(face="bold")) +
  theme(axis.title.y = element_text(face="bold"), axis.text.y = element_text(face="bold"))

p2 <- p + geom_boxplot(outlier.size = 0, coef = 0)
print(p2)


p2 <- p + geom_boxplot(aes(ymin=..lower.., ymax=..upper..), outlier.size=0) 
print(p2)




###########################################
################ practice #################
###########################################

library("nlme") # get the data
library("lqmm")

data("Orthodont", package="nlme")
Orthodont$Subject <- as.character(Orthodont$Subject)
Orthodont <- update(Orthodont, units = list(x = "(years)", y = "(mm)"),order.groups = FALSE)
summary(Orthodont)
head(Orthodont)

Orthodont$age.c <- Orthodont$age - 11
Orthodont.sub <- subset(Orthodont, Orthodont$Sex == "Female")
head(Orthodont.sub)
str(Orthodont.sub)



set.seed(123)
M <- 50
n <- 10
test <- data.frame(x = runif(n*M,0,1), group = rep(1:M,each=n))
test$y <- 10*test$x + rep(rnorm(M, 0, 2), each = n) + rchisq(n*M, 3)
head(test)
fit.lqmm <- lqmm(fixed = y ~ x, random = ~ 1, group = group,
                 data = test, tau = 0.5, nK = 11, type = "normal")
fit.lqmm






p1= ggplot(data=data_clean[which(data_clean$Gender==1),], aes(x=mean_ee, y=delta_yr1))+ 
  labs( x = "Energy expeniture", y = "BMI Difference")

# linear relationship 
reg <- lm(data=data_clean[which(data_clean$Gender==1),], delta_yr1 ~mean_ee)
reg
p2 = p1+ geom_abline(slope=0.3737, intercept = -0.2407, colour="red", size=1.5)+
  geom_point()+ggtitle("Plot of mean SDEE against BMI difference")
p2

#25 quantile 
p3=p2 + geom_abline(slope= 0.09027, intercept = -0.18684, colour="light blue", size=1.5, show.legend=TRUE)
p3

#50 quantile 
p4=p3 + geom_abline(slope= 0.129981, intercept = -0.002989, colour="blue", size=1.5, show.legend=TRUE)
p4

#75 quantile 
p5=p4 + geom_abline(slope= 0.2592706, intercept = -0.0417498, colour="dark blue", size=1.5, show.legend=TRUE)
p5
