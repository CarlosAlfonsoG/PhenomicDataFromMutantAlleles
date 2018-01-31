library(dplyr)
library(tidyverse)
library(readxl)
#Define the path of the file and make a list of it
path <- file.path("A/test_file_prg.xls")
Epitools_Data <- path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = path)

#For Loop to separe each dataframe and obtain frecuencys of polygonclasses##
for(i in 1:20){
  assign(paste0("T", i), Epitools_Data[[i]] %>%
           group_by(polygonNo) %>%
           summarise (n = n()) %>%
           mutate(freq = n / sum(n)))
  }
#create data frame for analysis
#establish number of frames on a vector
df_names <- paste("T", 100:120, sep = "")
#add identifyer for each frame for time line analysis
df2 <- do.call(rbind, lapply(df_names, function(x){data.frame(id=x, eval(parse(text=x)))}))
rm(list = df_names)
#set polygon classes as factors
df2$polygonNo <- as.factor(df2$polygonNo)
#set time as numeric in order to get a more defined plot
df2$id <- as.numeric(df2$id)
#ggplot
ggplot(df2, aes(x=id, y=freq, group=polygonNo)) +
  geom_line(aes(color=polygonNo))+
  geom_point(aes(color=polygonNo))+
  scale_x_continuous(breaks = seq(0, 20, by = 5))
  theme(legend.position="top")
###for more complex analsysis check: https://garthtarr.github.io/meatR/ggplot2.html
## FOR SUBSETING TO JUST SOME OF THE POLYGONCLASS 
## Create a vector with the numbers of polygons of interest
p <- c("4", "5", "6", "7", "8", "9") 
#then plot based on a subset array and define all exactly the same  
ggplot(subset(df2,polygonNo == p), aes(x=id, y=freq, group=polygonNo)) +
    geom_line(aes(color=polygonNo))+
    geom_point(aes(color=polygonNo))
  scale_x_continuous(breaks = seq(0, 20, by = 5))
  theme(legend.position="top")
