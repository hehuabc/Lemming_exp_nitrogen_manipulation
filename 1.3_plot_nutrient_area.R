
# set the work space
setwd("C:/Users/hehu/Documents/data/2023_Lemming_exp")

# Loading packages and data ------
library(tidyverse)
library(Rmisc)
# loading data
data <- readRDS("./data/area_nutrient.rds") 

# change the unit of chla to mg/l
data1 <- filter(data,group == "chla") %>% mutate(area = area/1000)

data2 <- filter(data,group != "chla")

data <- rbind(data1,data2)

# plot 6-----
data$year <- factor(data$year,levels = c("y1","y2","y3"),
                    labels = c("1","2","3")) %>% as.numeric()

# save the averages
# data_x <- summarySE(data,groupvars = c("nutrient","warming","year","group"),measurevar = "area")
# write.csv(data_x,"./data/nutrient_averages.csv",row.names = F)

data$warming <- factor(data$warming,levels = c("A","W","EW"),
                       labels = c("Ambient",
                                  "Warming",
                                  expression(Extreme~warming)))

data$nutrient <- factor(data$nutrient,levels = c("High","Low"))

data3 <- filter(data,group != "np",group!= "dnp")

data3$group <- factor(data3$group,levels = c("tn", 
                                           "nox", 
                                           "tp", 
                                           "po4",
                                           'chla'),
                     labels = c(expression(TN),
                                
                                expression(NO[x]),
                                
                                expression(TP),
                                
                                expression(PO[4]),
                                
                                expression(Chl~italic(a))))

data4 <- summarySE(data3,groupvars = c("nutrient","warming","year","group"),
                   measurevar = "area")

# Plot chla, tn,tp,nox and po4-----------------------
# here we plot a plain figure, the significance letter were added manually
# pdf('./figures/plot_nutrient_area.pdf',width = 6,height = 9)

ggplot(data4,aes(year,area,color = nutrient)) + 
  geom_point(stat = "identity",size = 2, 
             position = position_dodge(width = 0.6)) +
  geom_line(position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = area - se, ymax = area + se),
                position = position_dodge(width = 0.6),
                width = 0.3) +
  facet_grid(group~warming,scales = "free",labeller = label_parsed) +
  theme_bw() +
  theme(aspect.ratio = 2/2,
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = c(0.8,0.97),
        legend.background = element_blank(),
        legend.direction = "horizontal") + 
  scale_color_manual(values = c("darkgreen","orange")) +
  labs(x = "",y = expression(Daily~concentration~(mg~L^{-1}))) +
  scale_x_continuous(breaks = c(1,2,3),
                     labels = c("Before","During","After")) 
# dev.off()


# plot NP and DNP--------------------------------------------------------------
data5 <- filter(data,group == "np"|group == "dnp")

data5$group <- factor(data5$group,levels = c('np',
                                             "dnp"),
                      labels = c(expression(TN~':'~TP),
                                 
                                 expression(NO[x]~':'~PO[4])))

data6 <- summarySE(data5,groupvars = c("nutrient","warming","year","group"),
                   measurevar = "area",na.rm = T)

# pdf('./figures/plot_npratio_area.pdf',width = 6,height = 4)

ggplot(data6,aes(year,area,color = nutrient)) + 
  geom_point(stat = "identity",size = 1, 
             position = position_dodge(width = 0.6)) +
  geom_line(position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = area - se, ymax = area + se),
                position = position_dodge(width = 0.6),
                width = 0.3) +
  facet_grid(group~warming,scales = "free",labeller = label_parsed) +
  theme_bw() +
  theme(aspect.ratio = 2/2,
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = c(0.5,0.95),
        legend.background = element_blank(),
        legend.direction = "horizontal") + 
  scale_color_manual(values = c("darkgreen","orange")) +
  labs(x = "",y = expression(Average~ratio)) +
  scale_x_continuous(breaks = c(1,2,3),
                     labels = c("Before","During","After")) +
  geom_hline(aes(yintercept = 20),linetype = 2)
# dev.off()
