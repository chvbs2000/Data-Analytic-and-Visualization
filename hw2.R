##########################
#   GT account: kchen360 #
#   name: Kai-yu Chen    #
#   GT ID: 903233101     #
##########################


#### Problem 1 ####

#load data
library(ggplot2)
library(GGally)
data(midwest)

#get percentage of profesional employment in each state
il <- sum(midwest$popadults[midwest$state =="IL"] * midwest$percprof[midwest$state=="IL"])/sum(midwest$popadults[midwest$state=="IL"]) 
In <- sum(midwest$popadults[midwest$state =="IN"] * midwest$percprof[midwest$state=="IN"])/sum(midwest$popadults[midwest$state=="IN"]) 
mi <- sum(midwest$popadults[midwest$state =="MI"] * midwest$percprof[midwest$state=="MI"])/sum(midwest$popadults[midwest$state=="MI"]) 
oh <- sum(midwest$popadults[midwest$state =="OH"] * midwest$percprof[midwest$state=="OH"])/sum(midwest$popadults[midwest$state=="OH"]) 
wi <- sum(midwest$popadults[midwest$state =="WI"] * midwest$percprof[midwest$state=="WI"])/sum(midwest$popadults[midwest$state=="WI"]) 

#gather five states into one vector
prof_vector <- c(il,In,mi,oh,wi)
# percentage of total population with a professional emplyment in each state
prof_vector

#plot state vs percentage of professional employment
ggplot(midwest,                             #data
       #ordered percentage of professional employment from lowest to highest 
       aes(reorder(state,-percprof,FUN = 'median'),percprof)) + 
       geom_boxplot() +                     # boxplot
       coord_flip()+                        #flip coordinates
       scale_x_discrete("state") +          # x axis
       ylab("Professional Employment") +    # y label
       ggtitle("Professional Employment in States")

#### Problem 2 ####

#percentage of poeople with high school deploma
#Illinois
hsd_per_il <-sum((midwest$popadults[midwest$state=="IL"])*(midwest$perchsd[midwest$state=="IL"]))/sum(midwest$popadults[midwest$state=="IL"])
#indian
hsd_per_in <-sum((midwest$popadults[midwest$state=="IN"])*(midwest$perchsd[midwest$state=="IN"]))/sum(midwest$popadults[midwest$state=="IN"])
#Michigan
hsd_per_mi <-sum((midwest$popadults[midwest$state=="MI"])*(midwest$perchsd[midwest$state=="MI"]))/sum(midwest$popadults[midwest$state=="MI"])
#Ohio
hsd_per_oh <-sum((midwest$popadults[midwest$state=="OH"])*(midwest$perchsd[midwest$state=="OH"]))/sum(midwest$popadults[midwest$state=="OH"])
#Wisconsin
hsd_per_wi <-sum((midwest$popadults[midwest$state=="WI"])*(midwest$perchsd[midwest$state=="WI"]))/sum(midwest$popadults[midwest$state=="WI"])
#vector includes percentage of high school degree in all midwest stats 
hsd_vector <-c(hsd_per_il,hsd_per_in,hsd_per_mi,hsd_per_oh,hsd_per_wi)
hsd_vector

#percentage of people college educated
#Illinois
ce_per_il <-sum((midwest$popadults[midwest$state=="IL"])*(midwest$percollege[midwest$state=="IL"]))/sum(midwest$popadults[midwest$state=="IL"])
#indian
ce_per_in <-sum((midwest$popadults[midwest$state=="IN"])*(midwest$percollege[midwest$state=="IN"]))/sum(midwest$popadults[midwest$state=="IN"])
#Michigan
ce_per_mi <-sum((midwest$popadults[midwest$state=="MI"])*(midwest$percollege[midwest$state=="MI"]))/sum(midwest$popadults[midwest$state=="MI"])
#Ohio
ce_per_oh <-sum((midwest$popadults[midwest$state=="OH"])*(midwest$percollege[midwest$state=="OH"]))/sum(midwest$popadults[midwest$state=="OH"])
#Wisconsin
ce_per_wi <-sum((midwest$popadults[midwest$state=="WI"])*(midwest$percollege[midwest$state=="WI"]))/sum(midwest$popadults[midwest$state=="WI"])
#vector includes percentage of college educated in all midwest stats, Wisconsin has highest percentage of people with college educated
ce_vector <-c(ce_per_il,ce_per_in,ce_per_mi,ce_per_oh,ce_per_wi)
ce_vector

#facets plot for three way
qplot(x=perchsd, y=percollege,         # x, y  
      facets=state~.,                  #percentage vary by states 
      xlab=("High School Degree"),     # x label
      ylab=("College Educated"),       # y label
      data=midwest, main="High School degree with College Education by States")

#ggpairs for three way 
education <- midwest[,c('state','perchsd','percollege')]
ggpairs(education)+theme(axis.text.x = element_text(angle = -90, vjust = 1, hjust = 0))

#plot state vs high school deploma
ggplot(midwest,                        #data
       #ordered percentage of high school diploma from lowest to highest 
       aes(reorder(state,-perchsd,FUN = 'median'),perchsd)) + 
  geom_boxplot() +                     # boxplot
  coord_flip()+                        #flip coordinates
  scale_x_discrete("state") +          # x axis
  ylab("High School Deploma") +    # y label
  ggtitle("High School Deploma in States")

#plot state vs college educated
ggplot(midwest,                        #data
       #ordered percentage of college educated from lowest to highest 
       aes(reorder(state,-percollege,FUN = 'median'),percollege)) + 
  geom_boxplot() +                     # boxplot
  coord_flip()+                        #flip coordinates
  scale_x_discrete("state") +          # x axis
  ylab("College Educated") +    # y label
  ggtitle("College Educated in States")

#plot high school diploma vs log-transformed college educated
ggplot(midwest,                        #data
       aes(perchsd,log(percollege))) + #transformation on percollege
  geom_point() +                       #scatter plot
  xlab("high school educated")+
  ylab("College Educated")        




#### Problem 3 ####

#load data
data(diamonds)

#sampling 
#sampling redroducible
set.seed(29) 
#sample size = 10
id.small <- sample(1:nrow(diamonds), 10)
sample.small <-diamonds[id.small,]
#see min, 1st Q, mean, median, 3rd Q, max information of carat
summary(sample.small$carat)
#standard deviation
sd(sample.small$carat)

set.seed(1)
#sample size = 100
id.medium <- sample(1:nrow(diamonds),100)
sample.medium <-diamonds[id.medium,]
summary(sample.medium$carat)
sd(sample.medium$carat)

set.seed(2245)
#sample size = 1000
id.large <-sample(1:nrow(diamonds),1000)
sample.large <-diamonds[id.large,]
summary(sample.large$carat)
sd(sample.large$carat)


#box plot
#small sample size = 50
ggplot(sample.small,aes("",carat)) +
  geom_boxplot()
#medium sample size = 500
ggplot(sample.medium,aes("",carat)) +
  geom_boxplot() 
#large sample size = 5000
ggplot(sample.large,aes("",carat)) +
  geom_boxplot() 

#histogram
#n = 10, number of cells = 4
hist(log(sample.small$carat),breaks = 4, xlab = "log carat", main="N = 10")
#n=100, number of cells = 10
hist(log(sample.medium$carat),breaks = 10, xlab ="log carat", main ="N = 100" )
#n = 1000, number of cells = 15
hist(log(sample.large$carat),breaks = 15, xlab ="log carat", main = "N = 1000")


#### Problem 4 ####
#load graphic device package
library(jpeg)
library(png)

#write a function to store a vector of file memory
filememo <- function(a,c){

  set.seed(43)
  x<- round(runif(a,0,1),digits = 3)
  y<- round(runif(a,0,1),digits = 3)
  
  if (c == "jpg"){
    jpeg('rplot.jpg')
    plot(x,y)
    dev.off()
    s <-"rplot.jpg"
    file_size <- file.info(s)$size
    
  } else if (c == "pdf"){
    pdf('rplot.pdf')
    plot(x,y)
    dev.off()
    s <-"rplot.pdf"
    file_size <- file.info(s)$size

    
  } else if (c == "png"){
    png('rplot.png')
    plot(x,y)
    dev.off()
    s <-"rplot.png"
    file_size <- file.info(s)$size

  } else if(c == "ps"){
    postscript('rplot.ps')
    plot(x,y)
    dev.off()
    s <-"rplot.ps"
    file_size <- file.info(s)$size
    
  } else{
    return (FALSE)
  }
  
  return(file_size)

}

#sample size = 10~100000
n <- seq(10,100000,length.out = 500)

#create file size vector in each file format
#JPEG
file_size_jpeg<-mapply(filememo,n,"jpg")
#PDF
file_size_pdf<-mapply(filememo,n,"pdf")
#PNG
file_size_png<-mapply(filememo,n,"png")
#PS
file_size_ps<-mapply(filememo,n,"ps")

#create a graph grid 2x2
par(mfrow=c(2,2))
# both jpg and png file formats show instant boost to peak
# and floowed by exponential decay in the relationship of N and file size, 
# while pdf and ps file format show positive linear correlation in N and file size 
plot(n,file_size_jpeg, main="N vs File Size in JPG format")
plot(n,file_size_pdf, main="N vs File Size in PDF format")
plot(n,file_size_png, main="N vs File Size in PNG format")
plot(n,file_size_ps, main="N vs File Size in PS format")

#### Problem 5 ####
#load data
data(diamonds)
#subset data size to 10000
set.seed(609)
id <- sample(1:nrow(diamonds),10000)
subset <- diamonds[id,] 

#histogram
#color
ggplot(subset,aes(x=color))+geom_bar()
#carat
qplot(x= carat, data=subset, binwidth = 0.2, main="Carat")
#price
qplot(x= price, data=subset, binwidth = 1000, main="Price")

#both carat and price were right-skewed, so log transformation was performed 
#carat
qplot(x= log2(carat), data=subset, binwidth = 0.4, main="Carat")
#price
qplot(x= log2(price), data=subset, binwidth = 0.3, main="Price")

#log-log plot in carat and price
ggplot(subset, aes(log(carat),log(price))) + geom_point() +                       
  xlab("log carat")+
  ylab("log price")        


#three-way relations between color, carat, price (Facets)
qplot(x = carat, y= price, facets=color~.,data=subset,main="Carat vs. Price by Color")
#three-way relations between color, carat, price (gg-paired)
df <- subset[,c('color','carat','price')]
ggpairs(df)+theme(axis.text.x = element_text(angle = -90, vjust = 1, hjust = 0))


