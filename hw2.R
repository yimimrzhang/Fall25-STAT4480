##############################
####### Assignment Two #######
##############################

# import organics data #
library(tidyverse)
organics<-read_csv("organics.csv",na=c(".", "NA", "", "?"))

# fix measurement levels
organics <- organics %>% 
  mutate(across(where(is.character) | TargetBuy, as.factor)) 

# part 1a #
# generate mosaic or bar plot 
library(ggmosaic)
ggplot(organics) +
  geom_mosaic(aes(x=product(DemGender), fill=TargetBuy))

# part 1b #
ggplot(organics, aes(x=TargetBuy, y=PromTime, fill=TargetBuy)) +
  geom_boxplot(notch=TRUE)

library(skimr) 
organics %>% 
  group_by(TargetBuy) %>% 
  select(PromTime) %>%
  skim

# part 1c #
organics %>% 
  group_by(TargetBuy) %>% 
  select(PromClass) %>%
  skim



# part 2a #
organics %>% 
  summarise(across(where(is.factor), ~ chisq.test(.,TargetBuy)$p.value)) %>% 
  unlist %>% 
  sort
chisq.test(organics$DemGender, organics$TargetBuy)$statistic


# part 2b #
organics %>% 
  summarise(across(where(is.numeric) & !TargetAmt, ~ t.test(.~TargetBuy, var.equal=FALSE)$p.value)) %>%
  unlist %>% 
  sort

library(caret)
organics %>% 
  select(TargetBuy, where(is.numeric)) %>%  
  filterVarImp(.$TargetBuy) %>%  
  arrange(desc(X1)) %>% 
  slice(-(1:2)) 

# part 2c #
organics %>% 
  summarise(across(where(is.numeric) & !TargetAmt, ~ abs(cor(.,TargetAmt, use = "complete.obs")))) %>% 
  unlist %>% 
  sort(decreasing = TRUE)

organics %>% 
  summarise(across(where(is.numeric) & !TargetAmt, ~ cor(.,TargetAmt, use = "complete.obs"))) 


# part 3a #
library(caret)
TransformParams <- organics %>% 
  as.data.frame %>%   
  select(PromSpend) %>% 
  preProcess(method=c("BoxCox"))
TransformParams$bc

organics.xf<-organics %>%  
  as.data.frame %>%  
  predict(TransformParams,.) %>% 
  as_tibble


# part 3b #
par(mfrow=c(1,2))
hist(organics$PromSpend)
hist(organics.xf$PromSpend)
par(mfrow=c(1,1))


# part 3c #
library(fBasics)
basicStats(organics$PromSpend)
basicStats(organics.xf$PromSpend)

# part d #
missprop <- organics %>% 
  summarise(across(everything() & !DemCluster, ~ sum(is.na(.))/length(.))) %>%  # remove DemCluster as its model role is Rejected
  unlist() %>%            
  sort(decreasing = TRUE)  

missprop[missprop > 0] # print variables with missing values


var.cat <- organics %>% 
  select(names(missprop[missprop > 0]) & where(is.factor)) %>%
  names

var.cat  # categorical variables with missing values

var.num <- organics %>% 
  select(names(missprop[missprop > 0]) & where(is.numeric)) %>%
  names

var.num # numerical variables with missing values

mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

organics %>% 
  summarise(across(all_of(var.cat), ~ mode(.))) # all_of(): select variables by names in a character vector

MedianParams <- organics %>% 
  select(all_of(var.num)) %>% 
  preProcess(method=c("medianImpute"))

MedianParams$median






