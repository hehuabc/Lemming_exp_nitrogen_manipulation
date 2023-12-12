

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

# loading data
data <- readRDS("./data/area_nutrient.rds") 

filter(data,warming == "A",nutrient == "High")
# change the unit of chla to mg/l
data1 <- filter(data,group == "chla") %>% mutate(area = area/1000)

data2 <- filter(data,group != "chla")

data <- rbind(data1,data2)

data$warming <- factor(data$warming,levels = c("A","W","EW"))

data$nutrient <- factor(data$nutrient,levels = c("High","Low"))

# define y2 (N-stopping period) as a control year
data$year <- factor(data$year,levels = c("y2","y1","y3"))

# GLMM analysis ################################################################

# seven variables
index <- count(data$group)$x

# three temp scenarios
temp <- count(data$warming)$x  

for (i in 1:length(index)) {
  
  data1 <- dplyr::filter(data,group == index[i])
  
  for (j in 1:length(temp)){
    
    data2 <- dplyr::filter(data1,warming == temp[j])
    
    # model
    fit0 <- glmmTMB(log(area) ~ nutrient * year + (1|tank),
                    family = gaussian(),data2)
    
    summary(fit0)
    
    filename <- paste0('fit_',index[i],"_",temp[j]) 
    
    #save data
    save(fit0,file = paste("./analysis/",filename,".RData"))
    
    # residual diagnostics
    res <- simulateResiduals(fit0)

    pdf(file = paste("./analysis/",filename,".pdf"))
    
    plot(res)
    
    dev.off()
    
  }
  
}


