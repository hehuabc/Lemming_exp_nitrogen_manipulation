setwd("C:/Users/hehu/Documents/data/2023_Lemming_exp")

# Loading packages
library(dplyr) ## tidy data packages
library(plyr)
library(tidyr)
library(Rmisc)
library(ggplot2) ## plot figures
library(data.table)
library(ggeffects)
library(DescTools)


#################
data <- readRDS("./data/plant_trend.Rds")
  
data$date <- as.Date(data$date)

data$warming <- factor(data$warming,levels = c("A","W","EW"))

data$nutrient <- factor(data$nutrient,levels = c("Low","High"))


index <- count(data$group)$x
tankno <- count(data$tank)$x
data_a <- list()
data_b <- list()

for (i in 1:length(index)) {
  
  data1 <- filter(data,group == index[i])
  
  for (j in 1:length(tankno)) {
    
    data2 <- filter(data1,tank == tankno[j])
    
    y1 <- AUC(data2$date,data2$conc,from = as.Date("2016-06-14"), 
              to = as.Date("2018-06-14"),
              method = "spline")/730
    
    y2 <- AUC(data2$date,data2$conc,from = as.Date("2018-06-14"), 
              to = as.Date("2019-06-14"),
              method = "spline")/365
    
    y3 <- AUC(data2$date,data2$conc,from = as.Date("2019-06-14"), 
              to = as.Date("2021-06-14"),
              method = "spline")/731
    
    area <- rbind(y1,y2,y3) %>% 
      as.data.frame() %>%
      cbind(tank = tankno[j]) %>%
      cbind(year = c("y1","y2","y3"))
    
    colnames(area) <- c("area","tank",'year')
    
    data_b[[j]] <- area
    
  }
  
  data_a[[i]] <- bind_rows(data_b) %>%  
    cbind(group = index[i])
}


data_c <- bind_rows(data_a)

index <- read.csv("./data/index.csv",header = T,fileEncoding = "UTF-8-BOM")

data_d <- full_join(data_c,index,by = 'tank') %>% select(!stnr) %>%
  filter(year != "y1")


# write

saveRDS(data_d,"./data/area_plants.rds")
