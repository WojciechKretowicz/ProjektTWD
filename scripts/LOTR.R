library(dplyr)
library(ggplot2)


dane1 <- read.csv("./WordsByCharacter.csv", sep = ",")

Fellowship <- dane1 %>% filter(Film == "The Fellowship Of The Ring") %>% filter(Race == "Dwarf")

FellowshipData1 <- Fellowship %>% group_by(Character) %>% summarise(Words = sum(Words),Race=first(Race))
FellowshipData2 <- FellowshipData1 %>% group_by(Race) %>% summarise(n=n(), sum = sum(Words))

Towers <- dane1 %>% filter(Film == "The Two Towers")

TowersData1 <- Towers %>% group_by(Character) %>% summarise(Words = sum(Words),Race=first(Race))
TowersData2 <- TowersData1 %>% group_by(Race) %>% summarise(n=n(), sum = sum(Words))

King <- dane1 %>% filter(Film == "The Return Of The King")

KingData1 <- King %>% group_by(Character) %>% summarise(Words = sum(Words),Race=first(Race))
KingData2 <- KingData1 %>% group_by(Race) %>% summarise(n=n(), sum = sum(Words))

LOTR <- bind_rows(FellowshipData2, TowersData2, KingData2) %>% group_by(Race) %>% summarise_all(sum) %>% mutate(mean = sum/n)

ggplot(data=LOTR,aes(x=reorder(Race, -sum),y=sum)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_line(data=LOTR,aes(x=Race,y=mean, group=1,color='Mean')) +
  geom_point(data=LOTR,aes(x=Race,y=mean,color='Mean'))+
  geom_text(data=LOTR,aes(y=mean,label = round(mean)), stat = "identity", vjust = -0.5,color='Pink',size=5,fontface=2)+
  scale_y_continuous(breaks=seq(0,7000,500))

##
dane2 <- read.csv("./Movies.csv", sep = ",")
Movies <- dane2[2:4,c(1,2,6,7)]

Movies[1,5] <- sum(FellowshipData2$sum)
Movies[2,5] <- sum(TowersData2$sum)
Movies[3,5] <- sum(KingData2$sum)
names(Movies)[5] <- "Words"

Movies <- Movies %>% mutate("Words/min" = Words/RuntimeInMinutes)
