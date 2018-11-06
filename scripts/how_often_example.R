source('./data.R')
source('./how_often.R')

library(data.table)
library(ggplot2)
library(stringi)
library(jpeg)
library(grid)
library(ggrepel)

df <- merge(how_often_per_word(scripts, regex = "(ring|precios)"), how_often_per_word(scripts, regex = "(mine|my|want)"), by = 'char', all.x = FALSE, all.y = FALSE)

df <- df[df$char != 'GALADRIEL VOICE OVER',]

colnames(df) <- c("char", "ring", "mine")

med.x <- median(df$ring)
med.y <- median(df$mine)

labels.x <- paste(seq(0.00, 0.015, 0.001),"%",sep = "")
labels.y <- paste(seq(0.00, 0.025, 0.003),"%",sep = "")

img <- readJPEG('./../data/ring.jpg')

ggplot(data = df, aes(x = ring, y = mine, label = char)) +
  annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), -Inf, Inf, -Inf, Inf) + 
  geom_point(color = 'white') + geom_label_repel(size = 5, force = 10, segment.color = 'white') + geom_hline(yintercept = med.y, color = 'white') + geom_vline(xintercept = med.x, color = 'white') +
  annotate("label", x = 0.011, y = 0.018, label = '¯¹dni pierœcienia', size = 10, color = 'red') + 
  annotate('label', x = 0.01, y = 0.005, label = 'Zainteresowani pierœcieniem', size = 10, color = 'orange') +
  annotate('label', x = 0.0015, y = 0.005, label = 'Obojêtni', size = 10, color = 'green') +
  annotate('label', x = 0.001, y = 0.019, label = 'Chciwi', size = 10, color = 'orange') + 
  xlab('Czêstotliwoœæ odnoszenia siê do pierœcienia') + ylab('Czêstotliwoœæ wymawiania \"my/mine\"') +
  scale_x_continuous(breaks = seq(0.00, 0.015, 0.001), labels = labels.x) +
  scale_y_continuous(breaks = seq(0.00, 0.025, 0.003), labels = labels.y) +
  ggtitle("Czy Gollum po¿¹da³ pierœcienia?") +
  theme(title = element_text(color = 'gold', size = 20),axis.title = element_text(size = 20, color = 'gold'), plot.background = element_rect('black'), axis.text = element_text(color = 'gold'))




#https://www.hdwallpaper.nu/wp-content/uploads/2015/04/rings_the_lord_of_the_rings_one_ring_hd_wallpaper.jpg