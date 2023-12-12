
# set work place
setwd("C:/Users/hehu/Documents/data/2023_Lemming_exp")

# Loading packages
library(dplyr) ## tidy data packages
library(plyr)
library(tidyr)
library(Rmisc)
library(ggplot2) ## plot figures
library(data.table)
library(ggeffects)


# loading data
data <- readRDS("./data/nutrient_trend.rds")

data$time <- as.Date(data$time)

data$warming <- factor(data$warming,levels = c("A","W","EW"),
                        labels = c("Ambient",
                                   "Warming",
                                   expression(Extreme~warming)))

data$nutrient <- factor(data$nutrient,levels = c("Low","High"))
data$days <- julian(data$time,origin = as.Date("2016-06-02"))

# before
p1 <- filter(data,time > "2016-06-14",time <"2018-06-14") %>% cbind(BA = 0)

# during
p2 <- filter(data,time > "2018-06-14",time <"2019-06-14") %>% cbind(BA = 1)

# after
p3 <- filter(data,time > "2019-06-14",time <"2021-06-13") %>% cbind(BA = 2)

# combined three periods
data0 <- rbind(p1,p2,p3) %>% setDT() # all

data0$BA <- as.factor(data0$BA)

data0$conc <- log(data0$conc + 1)

### plot 1 ------------
plot_data <- summarySE(data0,measurevar = c("conc"),
                       groupvars =  c("nutrient","warming","BA","time","group"),
                       na.rm = T)

plot_data1 <- filter(plot_data,group != "dnp" & group != "np")

plot_data1$group <- factor(plot_data1$group,levels = c('chla',
                                                 "tn",
                                                 "nox",
                                                 "tp",
                                                 "po4"),
                        labels = c(expression(Chl~italic(a)~(¦Ìg~L^{-1})),
                                   
                                   expression(TN~(mg~L^{-1})),
                                   
                                   expression(NO[x]~(mg~L^{-1})),
                                   
                                   expression(TP~(mg~L^{-1})),
                                   
                                   expression(PO[4]~(mg~L^{-1}))))


pdf("./figures/plot_nutrient_details.pdf",height = 10,width = 12)

plot1 <- ggplot(plot_data1,aes(time,conc,color= nutrient),na.rm = T) + 
  theme_bw() +
  geom_point(size = 1) +
  facet_grid(group~warming,scales = "free_y",labeller = label_parsed)  +
  geom_line(size = 0.2) +
  geom_errorbar(aes(ymin = conc - se,ymax = conc + se),size = 0.3,
                width = 0) +
  labs(x = 'Year',y = "") +
  theme(legend.position = "none",aspect.ratio = 1/2,
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 14)) +
  scale_color_manual(values = c("orange3","darkgreen"))+
  annotate("rect", fill = "blue", alpha = 0.2, 
           xmin = as.Date('2018-06-14'), 
           xmax = as.Date("2019-06-14"),
           ymin = -Inf, ymax = Inf) 
plot1

dev.off()

# plot 2----------

plot_data2 <- filter(plot_data,group == "dnp" | group == "np") %>% setDT()

plot_data2$group <- factor(plot_data2$group,levels = c('dnp',
                                                       "np"),
                           labels = c(expression(NO[x]~':'~PO[4]),
                                      
                                      expression(TN~':'~TP)))



pdf("./figures/plot_npratios_details.pdf",height = 5,width = 12)

ggplot(plot_data2,aes(time,conc,color= nutrient),na.rm = T) + 
  theme_bw() +
  facet_grid(group~warming,scales = "free_y",labeller = label_parsed)  +
  geom_point(size = 1) + 
  geom_line(size = 0.2) +
  geom_errorbar(aes(ymin = conc - se,ymax = conc + se),size = 0.1,
                width = 0)  +
  scale_color_manual(values = c("orange3","darkgreen"))+
  labs(x = 'Year',y = "") +
  theme(legend.position = "none",
        legend.title = element_blank(),
        aspect.ratio = 1/2,
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 14)) +
  geom_hline(aes(yintercept = 3.0445), color = "red",
             size = 0.5,linetype = "dashed") +
  geom_hline(aes(yintercept = 2.3979), color = "blue",
             size = 0.5,linetype = "dashed") +
  annotate("rect", fill = "blue", alpha = 0.2, 
           xmin = as.Date('2018-06-14'), 
           xmax = as.Date("2019-06-14"),
           ymin = -Inf, ymax = Inf) 

dev.off()









