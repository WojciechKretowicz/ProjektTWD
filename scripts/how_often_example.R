source('./data.R')
source('./how_often.R')

library(data.table)
library(ggplot2)
library(stringi)
library(jpeg)
library(grid)

df <- merge(how_often_per_word(scripts, regex = "(ring|precios)"), how_often_per_word(scripts, regex = "(mine|my)"), by = 'char', all.x = FALSE, all.y = FALSE)

colnames(df) <- c("char", "ring", "mine")

med.x = median(df$ring)
med.y = median(df$mine)

img = readJPEG('./../data/ring.jpg')

ggplot(data = df, aes(x = ring, y = mine, label = char)) +
  annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), -Inf, Inf, -Inf, Inf) + 
  geom_point() + geom_label() + geom_hline(yintercept = med.y) + geom_vline(xintercept = med.x) +
  annotate("label", x = 0.025, y = 0.015, label = '¯¹dni pierœcienia', size = 10, color = 'red') + 
  annotate('label', x = 0.025, y = 0.005, label = 'Zainteresowani pierœcieniem', size = 10, color = 'orange') +
  annotate('label', x = 0.0015, y = 0.005, label = 'Obojêtni', size = 10, color = 'green') +
  annotate('label', x = 0.001, y = 0.018, label = 'Chciwi', size = 10, color = 'darkgoldenrod1') + 
  ggtitle("Czy Gollum po¿¹da³ pierœcienia?")




#https://www.hdwallpaper.nu/wp-content/uploads/2015/04/rings_the_lord_of_the_rings_one_ring_hd_wallpaper.jpg