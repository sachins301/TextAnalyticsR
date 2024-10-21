
library(tidyverse)
df<-tibble(
Docs =(c("D1","D2","D3","D4")),
Service = c(0,1,2,1),
Room = c(1,2,2,0))


library(gt)
gt_tbl <- gt(data = df) 
gt_tbl


tdf<-as_tibble(cbind(nms = names(df), t(df)))
colnames(tdf) <- NULL
library(janitor)
tdf<-tdf %>%
    row_to_names(row_number = 1)

library(gt)
gt_tbl <- gt(data = tdf) 
gt_tbl


tdf<-as_tibble(cbind(nms = names(df), t(df)))
colnames(tdf) <- NULL
library(janitor)
tdf<-tdf %>%
    row_to_names(row_number = 1)

library(gt)
gt_tbl <- gt(data = tdf) 
gt_tbl

