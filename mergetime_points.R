##substing polygon frecuencies into time##
library(tidyverse)
library(readxl)
#Define the path of the file and make a list of it
path <- file.path("A/test_file_prg.xls")
Epitools_Data <- path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = path)

#For Loop to separe each dataframe and obtain frecuencys of polygonclasses##
for(i in 1:120){
  assign(paste0("T", i), Epitools_Data[[i]] %>%
           group_by(polygonNo) %>%
           summarise (n = n()) %>%
           mutate(freq = n / sum(n)))
}
#create data frame for analysis
#establish number of frames on a vector
df_names <- paste("T", 1:120, sep="")
df_names1_40 <- paste("T", 1:40, sep = "")
names40 <- 1:418
 
df_names40_80  <- paste("T", 40:80, sep = "")
df_names80_120  <- paste("T", 40:80, sep = "")
#add identifyer for each frame for time line analysis
df20min <- do.call(rbind, lapply(df_names1_40, function(x){data.frame(id=x, eval(parse(text=x)))}))
df20min$Time <-20
df40min <- do.call(rbind, lapply(df_names40_80, function(x){data.frame(id=x, eval(parse(text=x)))}))
df40min$Time <-40
df60min <- do.call(rbind, lapply(df_names80_120, function(x){data.frame(id=x, eval(parse(text=x)))}))
df60min$Time <-60
rm(list = df_names)
#set polygon classes as factors
time_frec <- rbind(df20min, df40min)
time_frec <- rbind(time_frec, df60min)
time_frec$polygonNo <- as.factor(time_frec$polygonNo)
#set time as numeric in order to get a more defined plot
time_frec$id <- as.numeric(time_frec$polygonNo)
