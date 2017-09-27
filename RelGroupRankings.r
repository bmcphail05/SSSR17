rm(list=ls())

library(tidyverse)
library(directlabels)

# Load Data
dat <- read.csv("~/Dropbox/Projects/ReligionInequalityChina/data/Pyle2006data.csv")


p <- ggplot(dat, aes(factor(Year), Score, group = Group, color = Group, label = Group)) + 
  geom_line(size = 2) +
  geom_point(shape = 21, stroke = 2, size=2, fill = "white", aes(color=Group)) +
  geom_dl(aes(label = Group, color = "black"), 
          method = list(dl.trans(x = x + .4), "last.bumpup", fontsize = 8)) +
  geom_dl(aes(label = Group, color = "black"), 
          method = list(dl.trans(x = x - .4), "first.bumpup", fontsize = 8)) + 
  scale_color_manual(values = c("black", "#1f78b4", "#1f78b4", "#33a02c", 
                                "#1f78b4", "#33a02c","#33a02c","#1f78b4", "#33a02c")) +
  labs(title = "U.S. Religious Group Rankings on the Socioeconomic Index", 
       x = "", y = "Socioeconomic Index (St. Dev.)", caption = "Source: Pyle 2006") +
  theme_minimal() +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20, face = "bold", 
                                  margin = margin(20,0,30,0), hjust = 0.3),
        axis.title = element_text(size = 16, face = "bold", 
                                  margin = margin(0,10,0,0)),
        axis.text.x = element_text(size = 16, face = "bold", 
                                   margin = margin(10,0,0,0)),
        axis.text.y = element_text(size = 16, face = "bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border = element_blank(),
        plot.margin=unit(c(1,1,1,1),"cm"))

png('USrank.png', units="in", width=10, height=7, res=300)
p
dev.off()

