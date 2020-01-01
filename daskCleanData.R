rm(list = ls())
library(fst)
library(data.table)
library(dplyr)
library(tidyverse)
library(readxl)
library(janitor)
library(pryr)

########################################################################
### =====   Import large csv file (30 million row)             ===== ### 
########################################################################
# **    This file contains cost and revenue data from five shop     ** #
pathCost = 'C:/GitHub/sho/costShop.fst'
pathCostCod = 'C:/GitHub/sho/costCodShop.fst'
pathRevenue = 'C:/GitHub/sho/revenueShop.fst'
pathRevenueCod = 'C:/GitHub/sho/revenueCodShop.fst'
pathShop = 'C:/GitHub/sho/shop.xlsx'

# ** import data from binary format ** #
start = Sys.time()
cost = read.fst(pathCost)
costCod = read.fst(pathCostCod)
revenue = read.fst(pathRevenue)
revenueCod = read.fst(pathRevenueCod)
shop = pd.read_excel(pathShop, sheet_name = 'Sheet1')
finish = time.time()
finish - start
gc()

# ** Changing the variable type ** #
var_revenue = c('YearMonth','CodGroup','CodManager', 'CodShop', 'CodType', 'Id')
var_cost = c('Jc', 'date', 'shop')

for(i in var_revenue){
  revenue[[i]] <- as.character(revenue[[i]])
}
rm(i)

for(i in var_cost){
  cost[[i]] <- as.character(cost[[i]])
}
rm(i)

for(i in 1:dim(revenueCod)[2]){
  revenueCod[[i]] <- as.character(revenueCod[[i]])
}
rm(i)

# ** Join the files ** #
start = Sys.time()
shop_revenue = left_join(revenue,revenueCod, by = 'IdService')
shop_cost = left_join(cost,costCod, by = 'Eaccount')
finish - start
finish = time.time()
finish - start
gc()

# ** First cleaning data ** #
# Firts I exclude all data before 2018. The 'FirstO' column contains the cost category that I need 
# to analyze the dataset, then I will turn this column to head (new labels), next exclude all 
# missing.
start = time.time()
dtCost <- shop_cost %>% filter(date>201712) %>% 
  group_by(shop, date, FirstO) %>% 
  summarise(Total = sum(Value, na.rm = T)) %>% 
  gather(key, value, -shop, -date, -FirstO) %>% 
  spread(FirstO, value) %>% 
  select(-key, -ShopOther, -FreeShop, -RevenueOther) %>% 
  clean_names() %>% ##remove blanks
  mutate(TotalCost = sum(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15,
                         v16, v17, v18, na.rm = T)) %>% as.data.frame()
finish = time.time()
finish - start
gc()


# ** Second cleaning data ** #
start = time.time()
dtRev <- shop_revenue %>% clean_names() %>% 
  group_by('CodShop', 'YearMonth', 'Type') %>% 
  summarise(revenue = sum(Revenue, na.rm = T)) %>% 
  gather(key, value, -CodShop, -YearMonth, -Type) %>% 
  spread(Type, value) %>% rename(typeRevenue = key) %>% clean_names(.) %>% 
  mutate(TotalRev = sum(v1, v2, v3, v4, v5, v6, na.rm = T)) %>% as.data.frame()
finish = time.time()
finish - start
rm(shop_revenue); rm(shop_cost)
gc()


# ** Join dtCost and dtRev dataset ** #
db = left_join(dtCost,dtRev, by = 'shopDate')
#View(anti_join(dtCost, dtRev, by = 'shopDate') #Note: The 'dtRev' dataset should have the same amount row of the 'dtCost', 
#but don't have
rm(dtCost); rm(dtRev)
gc()

# ** Calculate a productivity index ** #
db <- db %>% mutate(PI = TotalCost/TotalRev)

# ** Third cleaning data ** #
db <- db %>% filter(!PI %in% c(NA)) %>% filter(PI >= 0 & PI < 1.5)
db <- db %>% mutate(PI_cat = ifelse(PI > 0.56, 'Inadequado','Adequado'))
# Filter just cost and total revenue variable #
db <- db %>% select(1:19, 30)

# ** Forth cleaning data ** #
shop <- shop %>% select(1, 7, 13)

db <- left_join(db, shop, by = 'shop')
db <- db %>% mutate(region = ifelse(UF %in% c('AC', 'AP', 'AM', 'PA', 'RO', 'RR', 'TO'), 'Norte',
                                    ifelse(UF %in% c('CE', 'AL', 'BA', 'MA', 'PB', 'PE', 'PI', 'RN', 'SE'), 'Nordeste',
                                           ifelse(UF %in% c('GO', 'MT', 'MS', 'DF'), 'Centro-Oeste',
                                                  ifelse(UF %in% c('ES', 'MG', 'RJ', 'SP'), 'Sudeste',
                                                         ifelse(UF %in% c('PR', 'SC', 'RS'), 'Sul', 'Other'))))))
gc()

# The dataframe is now clean and I can normalize the data, create new variables...., to application machine leraning algoritm #


