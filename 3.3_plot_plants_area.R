
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

data <- readRDS("./data/area_plants.rds") 

# plot 6-----
data$year <- factor(data$year,levels = c("y2","y3"),
                    labels = c("2","3")) %>% as.numeric()

# save the averages
# data_x <- summarySE(data,groupvars = c("nutrient","warming","year","group"),measurevar = "area")

# write.csv(data_x,"./data/plants_averages.csv",row.names = F)

data$warming <- factor(data$warming,levels = c("A","W","EW"),
                       labels = c("Ambient",
                                  "Warming",
                                  expression(Extreme~warming)))

data$nutrient <- factor(data$nutrient,levels = c("High","Low"))


data$group <- factor(data$group,levels = c('totpvi',
                                           "filapvi"),
                     labels = c(expression(Total),
                                
                                expression(Filamentous~algae)))

data1 <- summarySE(data,groupvars = c("nutrient","warming","year","group"),
                   measurevar = "area")



# pdf('./figures/plot_plants_area.pdf',width = 6,height = 4)

ggplot(data1,aes(year,area,color = nutrient)) + 
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
        legend.position = c(0.1,0.4),
        legend.background = element_blank()) + 
  scale_color_manual(values = c("darkgreen","orange")) +
  labs(x = "",y = expression(PVI~('%'))) +
  scale_x_continuous(breaks = c(1,2),labels = c("During","After")) 

# dev.off()

