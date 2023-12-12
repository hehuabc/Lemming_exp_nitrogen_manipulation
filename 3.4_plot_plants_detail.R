# set work place
setwd("C:/Users/hehu/Documents/data/2023_Lemming_exp") 

# Loading packages
library(dplyr)
library(plyr)
library(tidyr)
library(Rmisc)
library(ggplot2)
library(ggpubr)
library(ggeffects)
library(data.table)

# loading data

data <- readRDS("./data/plant_trend2.Rds")

data$date <- as.Date(data$date)

data <- filter(data,date > as.Date("2016-06-14"),date < as.Date("2021-06-14"))

data$warming <- factor(data$warming,levels = c("A","W","EW"),
                       labels = c("Ambient",
                                  "Warming",
                                  expression(Extreme~warming)))

data$nutrient <- factor(data$nutrient,levels = c("Low","High"))



# plot figure 1 ----
data_b <- summarySE(data,measurevar = "pvi",
                    groupvars = c("date",'nutrient',
                                  'warming',"index"),
                    na.rm = T)

data_b$index <- factor(data_b$index,levels = c("totpvi","filapvi"),
                       labels = c(expression(Total),
                                  expression(Filamentous~algae)))





# main figure
plot_3 <- ggplot(data_b,aes(date,pvi,color= nutrient),na.rm = T) + 
  theme_bw() +
  geom_point(size = 1) +
  facet_grid(index~warming,scales = "free_y",labeller = label_parsed)  +
  geom_line(size = 0.2) +
  geom_errorbar(aes(ymin = pvi - se,ymax = pvi + se),size = 0.3,
                width = 0) +
  labs(x = 'Year',y = expression(PVI~('%')))+
  theme(legend.position = c(0.55,0.9),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        aspect.ratio = 1/2,
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 14)) +
  scale_color_manual(values = c("orange3","darkgreen"))+
  annotate("rect", fill = "blue", alpha = 0.2, 
           xmin = as.Date('2018-06-14'), 
           xmax = as.Date("2019-06-14"),
           ymin = -Inf, ymax = Inf) 
plot_3

pdf("./figures/plot_plants_details.pdf",height = 5,width = 12)
plot_3
dev.off()

