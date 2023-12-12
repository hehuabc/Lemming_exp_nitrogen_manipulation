
setwd("C:/Users/hehu/Documents/data/2023_Lemming_exp")

# Loading packages and data ------
library(dplyr) ## tidy data packages
library(plyr)
library(tidyr)
library(Rmisc)
library(ggplot2) ## plot figures
library(data.table)
library(ggeffects)
library(DescTools)
library(glmmTMB)

data <- readRDS("./data/area_temp_prod.rds")

# plot -----
data$year <- factor(data$year,levels = c("y1","y2","y3"),
                    labels = c("1","2","3")) %>% as.numeric()
# save the averages
# data_x <- summarySE(data,groupvars = c("nutrient","warming","year","group"),measurevar = "area")

# write.csv(data_x,"./data/temp_prod_averages.csv",row.names = F)


data$warming <- factor(data$warming,levels = c("A","W","EW"),
                       labels = c("Ambient",
                                  "Warming",
                                  expression(Extreme~warming)))

data$nutrient <- factor(data$nutrient,levels = c("High","Low"))


data$group <- factor(data$group,levels = c('temp',
                                           "production", 
                                           "respiration", 
                                           "netproduction"),
                     labels = c(expression(Temperature),
                                
                                expression(Production),
                                
                                expression(Respiration),
                                
                                expression(Net~production)))

data1 <- summarySE(data,groupvars = c("nutrient","warming","year","group"),
                   measurevar = "area") %>%
  filter(group != "Temperature")



# pdf('./figures/plot_prod_area.pdf',width = 6,height = 5.9)
ggplot(data1,aes(year,area,color = nutrient)) + 
  geom_point(stat = "identity",size = 2,
             position = position_dodge(width = 0.4)) +
  geom_line(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = area - se, ymax = area + se),
                position = position_dodge(width = 0.4),
                width = 0.2,linewidth = 0.3) +
  facet_grid(group~warming,scales = "free",labeller = label_parsed) +
  theme_bw() +
  theme(aspect.ratio = 1,
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = c(0.14,0.96),
        legend.background = element_blank(),
        legend.direction = "horizontal") + 
  scale_color_manual(values = c("orange","darkgreen")) +
  labs(x = "",y = expression(Daily~rate~(g~O[2]~m^{-2}~d^{-1}))) +
  scale_x_continuous(breaks = c(1,2,3),labels = c("Before","During","After")) 
# dev.off()

#Temp ###################################################

data2 <- summarySE(data,groupvars = c("warming","year","group","nutrient"),
                   measurevar = "area") %>%
  filter(group == "Temperature")


# pdf('./figures/plot_temp_area.pdf',width = 6,height = 2.6)

plot_temp_auc <- ggplot(data2,aes(year,area,color = nutrient)) + 
  geom_point(stat = "identity",size = 2, 
             position = position_dodge(width = 0.0)) +
  geom_line(position = position_dodge(width = 0.0)) +
  geom_errorbar(aes(ymin = area - se, ymax = area + se),
                position = position_dodge(width = 0.0),
                width = 0.2,linewidth = 0.3) +
  theme_bw() +
  facet_grid(.~warming,labeller = label_parsed) +
  theme(aspect.ratio = 1,
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.1,0.85)) + 
  labs(x = "",y = "Daily temperature (¡ãC)") +
  scale_color_manual(values = c("darkgreen","orange")) +
  scale_x_continuous(limits = c(0.5,3.5),breaks = c(1,2,3),labels = c("Before","During","After")) +
  scale_y_continuous(limits = c(8,17))
plot_temp_auc

#dev.off()


