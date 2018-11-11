source('./data.R')
source('./how_often.R')

library(data.table)
library(ggplot2)
library(stringi)
library(jpeg)
library(grid)
library(ggrepel)
library(extrafont)
library(extrafontdb)

df <- merge(how_often_per_word(scripts, regex = "( ring| precious)"), how_often_per_word(scripts, regex = "( mine| my| want)"), by = 'char', all.x = FALSE, all.y = FALSE)

df <- df[df$char != 'GALADRIEL VOICE OVER',]

colnames(df) <- c("char", "ring", "mine")

df <- df[,.(char,ring,mine,
            race = c('man', 'hobbit', 'man', 'man', 'elf', 'man', 'hobbit', 'wizard', 'dwarf', 'hobbit', 'man', 'elf', 'hobbit', 'hobbit', 'hobbit', 'wizard'))]

med.x <- median(df$ring)
med.y <- median(df$mine)

labels.x <- paste(seq(0.00, 0.021, 0.002),"%",sep = "")
labels.y <- paste(seq(0.00, 0.015, 0.001),"%",sep = "")

#link do pobrania czcionki https://dl.1001fonts.com/ringbearer.zip
img <- readJPEG('./../data/ring.jpg')

#font_import()

loadfonts(device="win")


ggplot(data = df, aes(x = ring, y = mine, label = char, color = race)) +
  annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), -Inf, Inf, -Inf, Inf) + 
  geom_point(color = 'white', size = 3) + geom_label_repel(size = 5, force = 15, segment.color = 'white', family="Ringbearer", label.padding = .6,
                                                 label.size = 3) + geom_hline(yintercept = med.y, color = 'white') + geom_vline(xintercept = med.x, color = 'white') +
  annotate("label", x = 0.015, y = 0.016, label = 'Anxious', size = 12, color = 'red', family="Ringbearer") + 
  annotate('label', x = 0.015, y = 0.001, label = 'Interested', size = 12, color = 'orange', family="Ringbearer") +
  annotate('label', x = 0.0016, y = 0.001, label = 'Neutral', size = 12, color = 'green', family="Ringbearer") +
  annotate('label', x = 0.0016, y = 0.016, label = 'Greedy', size = 12, color = 'orange', family="Ringbearer") + 
  xlab('Frequency of mentioning the Ring') + ylab('Frequency of utter \"my/mine\"') +
  scale_x_continuous(breaks = seq(0.00, 0.021, 0.002), labels = labels.x) +
  scale_y_continuous(breaks = seq(0.00, 0.015, 0.001), labels = labels.y) +
  ggtitle("How hard did Gollum covet the ring?") +
  theme(title = element_text(color = 'gold', size = 20),axis.title = element_text(size = 20, color = 'gold'),
        plot.background = element_rect('black'), axis.text = element_text(color = 'gold'),
        text=element_text(size=16, family="Ringbearer"))

#link do obrazku w tle
#https://www.hdwallpaper.nu/wp-content/uploads/2015/04/rings_the_lord_of_the_rings_one_ring_hd_wallpaper.jpg