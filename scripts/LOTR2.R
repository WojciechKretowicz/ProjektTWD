library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(jpeg)
library(grid)
library(extrafont)
library(extrafontdb)
library(ggpubr)
library(ggimage)



dane1 <- read.csv("./WordsByCharacter.csv", sep = ",")

Fellowship <- dane1 %>% filter(Film == "The Fellowship Of The Ring")

FellowshipData1 <- Fellowship %>% group_by(Character) %>% summarise(Words = sum(Words),Race=first(Race))
FellowshipData2 <- FellowshipData1 %>% group_by(Race) %>% summarise(n=n(), sum = sum(Words))

Towers <- dane1 %>% filter(Film == "The Two Towers")

TowersData1 <- Towers %>% group_by(Character) %>% summarise(Words = sum(Words),Race=first(Race))
TowersData2 <- TowersData1 %>% group_by(Race) %>% summarise(n=n(), sum = sum(Words))

King <- dane1 %>% filter(Film == "The Return Of The King")

KingData1 <- King %>% group_by(Character) %>% summarise(Words = sum(Words),Race=first(Race))
KingData2 <- KingData1 %>% group_by(Race) %>% summarise(n=n(), sum = sum(Words))

LOTR <- bind_rows(FellowshipData2, TowersData2, KingData2) %>% group_by(Race) %>% summarise_all(sum) %>% mutate(mean = sum/n)

g <- ggplot(data=LOTR,aes(x=reorder(Race, -sum),y=sum)) +
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

##
LOTR1 <- bind_rows(FellowshipData1, TowersData1, KingData1) %>% group_by(Character)
LOTR1 %>% filter(Race != "Orc") %>% filter(Race != "Nazgul") %>% filter(Race != "Dead") 
LOTR1_NAzgul <- LOTR1 %>% filter(Race == "Nazgul")
LOTR1MAX <- LOTR1 %>% group_by(Race) %>% top_n(n = 1, wt = Words)
LOTR1MAX
font_import()
font_import(pattern="[R/r]ing")
loadfonts(device="win")
LOTR1 
LOTR2M <- filter(LOTR1, Character %in% c("Theoden","Gollum","Merry","Bilbo","Pippin","Arwen","Bilbo","Denethor","Gimli","Boromir","Faramir","Grima","Frodo","Treebeard","Aragorn","Legolas","Sam","Gandalf","Legolas","Saruman","Fomer","Strider","Elrond", "Galadriel"))
LOTR2M <- aggregate(LOTR2M, by=LOTR2M[,1],FUN=function(x) paste0(unique(x)))
LOTR2M$Words <- lapply(LOTR2M$Words, function(x) x[which.max(x)])
LOTR2M
colnames(LOTR2M) <- c("-","Character","Words","Race")
LOTR2M$Words <- as.numeric(LOTR2M$Words)

LOTR1 <- LOTR1[order(LOTR1$Words, decreasing=TRUE),]
unique(LOTR1$Race)
LOTR1 %>% filter(Race!="NA")
LOTR1 <- LOTR1[is.na(LOTR1$Race)==FALSE,]
LOTR1$Race <- factor(LOTR1$Race,levels=c("Ainur","Hobbit","Ent","Gollum","Dwarf","Men","Elf"))

img <- readJPEG('wallpaperlotr.jpg')


p <- ggplot(data = LOTR1, aes(x = Race, y = Words)) +
  geom_boxplot(lwd=1.2,outlier.colour="pink", outlier.shape=16,outlier.size=2, notch=FALSE, fill= "gold1",colour = "goldenrod2") +
  geom_point(data=LOTR2M, aes(x = Race, y = Words,colour = "Red"))+
  geom_text_repel(data=LOTR2M, aes(label=Character),vjust = -0.5,colour = "White",size=7, family="Ringbearer") +
  ggtitle("The biggest prattlers in Lord Of The Rings")+
  scale_y_continuous(breaks=seq(0,3000,250))+
  theme(plot.title=element_text(size = 20, family="Ringbearer", colour ="gold1"),
        text=element_text(size = 16, family="Ringbearer", colour="gold1"),
        axis.text.x=element_text(size = 12,colour="gold1"),
        axis.text.y=element_text(size = 12,colour="gold1"),
        legend.position="none",
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        #panel.grid.minor.y = element_line(colour = "white",size=0.05, linetype = "dashed"),
        panel.grid.minor.x = element_blank())


url <- "https://stmed.net/sites/default/files/the-lord-of-the-rings%3A-the-fellowship-of-the-ring-wallpapers-30068-4392087.jpg"
ggbackground(p,url)
p


