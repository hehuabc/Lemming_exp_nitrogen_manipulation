# set work place
setwd("C:/Users/hehu/Documents/data/Lemming_exp") 

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
data <- readRDS("./data/temp_production_trend.rds")

data$time <- as.Date(data$time)

data <- filter(data,time > as.Date("2016-06-14"),time < as.Date("2021-06-14"))

data$warming <- factor(data$warming,levels = c("A","W","EW"),
                       labels = c("Ambient",
                                  "Warming",
                                  expression(Extreme~warming)))

data$nutrient <- factor(data$nutrient,levels = c("Low","High"))

# plot figure 1 ----
data_b <- summarySE(data,measurevar = "conc",
                    groupvars = c("time",'nutrient',
                                  'warming',"group"),
                    na.rm = T) %>%filter(group != "temp")

data_b$group <- factor(data_b$group,levels = c(
                                               "production",
                                               "respiration",
                                               "netproduction"),
                       
                       labels = c(expression(Production),
                                  
                                  expression(Respiration),
                                  
                                  expression(Net~production)
                                  
                       ))





# main figure
plot_3 <- ggplot(data_b,aes(time,conc,color= nutrient),na.rm = T) + 
  theme_bw() +
  geom_point(size = 1) +
  facet_grid(group~warming,scales = "free_y",labeller = label_parsed)  +
  geom_line(size = 0.2) +
  geom_errorbar(aes(ymin = conc - se,ymax = conc + se),size = 0.3,
                width = 0) +
  labs(x = 'Year',y = expression(Metabolism~rate~(g~O[2]~m^{-2}~d^{-1})))+
  theme(legend.position = "none",aspect.ratio = 1/2,
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 14)) +
  scale_color_manual(values = c("orange3","darkgreen"))+
  annotate("rect", fill = "blue", alpha = 0.2, 
           xmin = as.Date('2018-06-14'), 
           xmax = as.Date("2019-06-14"),
           ymin = -Inf, ymax = Inf) 
plot_3

pdf("./figures/plot_prod_details.pdf",height = 7,width = 12)
plot_3
dev.off()

## temperature
data_temp <- filter(data,group == "temp") %>%
  summarySE(measurevar = "conc",
            groupvars = c("time",'warming'),
            na.rm = T)
data_temp$warming <- factor(data_temp$warming,labels = c("Ambient",
                                                         "Warming",
                                                         "Extreme warming"))


pdf("./figures/plot_temp_details.pdf",width = 7,height = 4)
plot_temp <- ggplot(data_temp,aes(time,conc,color= warming)) + 
  theme_bw() +
  geom_point(size = 1) +
  geom_line(size = 0.2) +
  geom_errorbar(aes(ymin = conc - se,ymax = conc + se),size = 0.3,
                width = 0.5) +
  labs(x = "Year",y = "Temperature (¡ãC)") +
  theme(legend.position = "top",aspect.ratio = 1/2,
        legend.title = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 14)) +
  scale_color_manual(values = c("blue","darkcyan","red"))+
  annotate("rect", fill = "blue", alpha = 0.2, 
           xmin = as.Date('2018-06-14'), 
           xmax = as.Date("2019-06-14"),
           ymin = -Inf, ymax = Inf) 
plot_temp

dev.off()
