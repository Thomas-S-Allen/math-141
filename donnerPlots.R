install.packages("alr3")
library(alr3)
data(donner)
?donner


head(donner)
str(donner)


data("Titanic")

Titanic <- as.data.frame(Titanic)
str(Titanic)

library(tidyverse)

donner$Outcome<-factor(donner$Outcome)

## Bar Chart 

barplot <- ggplot(donner, aes(Outcome)) +
  geom_bar()

barplot



## Stacked Bar Chart 

barplot2 <- ggplot(donner, aes(x=Outcome,fill=Sex)) +
  geom_bar()

barplot2 

head(Titanic)

titanicTab<-Titanic%>%
  group_by(Class)%>%
  summarise(Count=sum(Freq))

barplotT <- ggplot(Titanic, aes(x=Survived,y=Freq)) +
  geom_bar(stat="identity")+
  theme_bw()+
  ggtitle("Bar Chart of Titanic Survival Status")

barplotT

barplotT2 <- ggplot(Titanic, aes(x=Survived,y=Freq,fill=Sex)) +
  geom_bar(stat="identity")+
  theme_bw()+
  ggtitle("Stacked Bar Chart of Titanic Survival Status by Sex")

barplotT2


barplotT3<- ggplot(titanicTab, aes(x="",y=Count,fill=Class)) +
  geom_bar(stat="identity")+
  theme_bw()+
  ggtitle("Stacked Bar Chart of Titanic Passenger Distribution by Class")

barplotT3

pie <- barplotT3 + coord_polar("y", start=0)
pie

#install.packages("scales")
library(scales)

pie +geom_text(aes(y = 2000-c(0, cumsum(Count)[-length(Count)]), 
                label = percent(Count/2201)), size=5)+
  theme_bw()+
  ggtitle("Pie Chart of Titanic Passenger Distribution by Class")

barplotT4 <- ggplot(Titanic, aes(x=Sex,y=Freq,fill=Survived)) +
  geom_bar(stat="identity",position=position_dodge())+
  theme_bw()+
  ggtitle("Side-by-side Bar Chart of Titanic Passenger Survival by Sex")


barplotT4


barplotT5 <- ggplot(Titanic, aes(x=Survived,y=Freq,fill=Sex)) +
  geom_bar(stat="identity") +
  facet_wrap( ~ Class )+
  theme_bw()+
  ggtitle("Bar Chart of Titanic Passenger Survival by Sex", 
          subtitle = "Faceted by Class")

barplotT5


## Side-by-side Bar Chart 


barplot3 <- ggplot(donner, aes(x=Sex,fill=Outcome)) +
  geom_bar(position=position_dodge())

barplot3 



## Faceted Bar Chart 

barplot4 <- ggplot(donner, aes(x=Outcome,fill=Sex)) +
  geom_bar() +
  facet_wrap( ~ Status)

barplot4





## Dot Plot

dotplot1 <- ggplot(donner, aes(x=Age)) +
  geom_dotplot()

dotplot1

## Side-by-Side Dot Plot


dotplot2 <- ggplot(donner, aes(x=Sex,y=Age)) +
  geom_dotplot(binaxis='y')

dotplot2



## Histogram


hist1 <- ggplot(donner, aes(x=Age)) +
  geom_histogram(breaks=c(0,10,20,30,40,50,60, 70))

hist1

## More Histograms



hist2 <- ggplot(donner, aes(x=Age,fill=Sex)) +
  geom_histogram(bins=5)

hist2 

## Density Plot


densplot1 <- ggplot(donner, aes(x=Age)) +
  geom_density()

densplot1

## Box Plot


boxplot1 <- ggplot(donner, aes(x=Sex,y=Age)) +
  geom_boxplot()

boxplot1 

boxplot1 <- ggplot(donner, aes(x=Outcome,y=Age)) +
  geom_boxplot()

boxplot1 


### 
data(mpg)
str(mpg)

unique(mpg$class)


##### MPG

stem(mtcars$mpg, 
     scale=.5)

histCar <- ggplot(mtcars, aes(x=mpg)) +
  geom_histogram(bins=5)+
  theme_bw()+
  ggtitle("Histogram of MPG")

histCar

histCar2 <- ggplot(mtcars, aes(x=mpg,fill=factor(cyl))) +
  geom_histogram(bins=5)+
  theme_bw()+
  ggtitle("Histogram of MPG by Number of Cylinders")

histCar2


dotplot1 <- ggplot(mtcars, aes(x=mpg)) +
  geom_dotplot(binaxis = 'y')+
  theme_bw()+
  ggtitle("Dot Plot of MPG")

dotplot1 

dotplot2 <- ggplot(mtcars, aes(x=factor(cyl),y=mpg)) +
  geom_dotplot(binaxis='y')+
  theme_bw()+
  ggtitle("Side-by-Side Dot Plot of MPG")

dotplot2

densplot1 <- ggplot(mtcars, aes(x=mpg)) +
  geom_density()+
  theme_bw()+
  ggtitle("Density Plot of MPG")

densplot1 
