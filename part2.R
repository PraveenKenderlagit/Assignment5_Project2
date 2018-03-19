####################################################################################################################################
#Assignment 5
#Part2
#Author: Anthea Li
#        Tianying Zhang
#        Senhao Li
####################################################################################################################################

library(tidyverse)
library(readxl)
library(stringr)
library(ggplot2)
veg_1 <- read_xlsx("veg1.xlsx")

colnames_1 <- colnames(veg_1)

## try
n_distinct(veg_1[,1])
#1
n_distinct(veg_1[,2])
#13
unique(veg_1[,2])
# # A tibble: 13 x 1
# Year
# <dbl>
#   1  2016
# 2  2015
# 3  2014
# 4  2010
# 5  2006
# 6  2004
# 7  2002
# 8  2000
# 9  1998
# 10  1996
# 11  1994
# 12  1992
# 13  1990

## now get the count for each column

c <- apply(veg_1, 2, n_distinct)
#c
# Program             Year           Period      Week Ending        Geo Level            State       State ANSI 
# 1               13                1                1                2                1                2 
# Ag District Ag District Code           County      County ANSI         Zip Code           Region   watershed_code 
# 1                1                1                1                1                6                1 
# Watershed        Commodity        Data Item           Domain  Domain Category            Value           CV (%) 
# 1                5               54               13              240             1271                1 

#c[c>1]


d <- names(c[c==1])
# #d
# [1] "Program"          "Period"           "Week Ending"      "State"            "Ag District"      "Ag District Code"
# [7] "County"           "County ANSI"      "Zip Code"         "watershed_code"   "Watershed"        "CV (%)"  
e <- names(c[c>1])
# #e
# [1] "Year"            "Geo Level"       "State ANSI"      "Region"          "Commodity"       "Data Item"      
# [7] "Domain"          "Domain Category" "Value"         

veg_2 <- select(veg_1, e)

colnames_2 <- colnames(veg_2)
#colnames_2

apply(veg_2, 2, n_distinct)

veg_3 <- dplyr::rename(veg_2, 
                       Geo = "Geo Level", 
                       State = "State ANSI",
                       Data = "Data Item",
                       Category = "Domain Category")

colnames_3 <- colnames(veg_3)
#colnames_3

#veg_3
#count unique items
unique(veg_3[,"Commodity"])

unique(veg_3[,"Data"]) %>% print(n=60)

unique(veg_3[,"Domain"])

unique(veg_3[,"Category"])

unique(veg_3[,"Value"])

vegdata <- separate(veg_3, Category, into = c("label", "quant"), sep=",")
# 
n_distinct(vegdata[,2])
# 
# 
unique(vegdata[,"label"]) %>% print(n=30)

RistrictUseChemical_ <- filter(vegdata, label=="RESTRICTED USE CHEMICAL")

RistrictUseChemical_1 <- RistrictUseChemical_ %>% select(label, quant) %>% unique()

#get the data for each restricted chemical

RistrictUseChemical_2 <- RistrictUseChemical_1 %>% select(-label) %>% 
  separate(quant, into = c("a", "ID"), sep = "=") %>% 
  separate(a, into = c("D", "Name"), sep = "[()]") %>% 
  select(-D) %>% 
  separate(ID, into = c("ID", "D1"), sep = "[)]") %>% 
  select(-D1)
#refer to the code on the website

RistrictUseChemical_1 %>% print(n=30)


#vegdata$quant

vegdata1 <- separate(vegdata, quant, into=c("Treat","Name"), sep = ":")

vegdata2 <- vegdata1 %>% filter(!Value %in% c("(D)",NA,"(Z)","(NA)"))

vegdata2 <- vegdata2 %>% select(-Domain)

vegdata2 <- vegdata2 %>% separate(Data, into = c("a", "Measurement"), sep = "-")

vegdata2 <- vegdata2 %>% select(-a)

vegdata2 <- vegdata2 %>% separate(Measurement, into = c("Measurement", "Unit of Measurement"), sep = ",")

vegdata3 <- vegdata2 %>% separate(Name, into = c("a", "ID"), sep = "=") %>% 
  separate(a, into = c("D", "Name"), sep = "[()]") %>% 
  select(-D) %>% 
  separate(ID, into = c("ID", "D1"), sep = "[)]") %>% 
  select(-D1)

#save the data
write.csv(vegdata3 ,"vegdatatidy.csv",row.names = FALSE)

write.csv(RistrictUseChemical_2, "RestrictUseChemical.csv",row.names=F)

vegdata3$Value <- as.numeric(vegdata3$Value)

#get the data with toxicity info in it
chemical_tox <- read.csv("chemical_tox.csv")

chemical_tox <- as.tibble(chemical_tox)

chemical_tox$X <- as.integer(chemical_tox$X)

chemical_tox$Name <- as.character(chemical_tox$Name)

chemical_tox$ID <- as.character(chemical_tox$ID)

chemical_tox <- chemical_tox %>% select(-ID)

#brocoli
broc <- vegdata3 %>% filter(label == "RESTRICTED USE CHEMICAL", Commodity == "BROCCOLI", `Unit of Measurement`==" MEASURED IN LB")

broc$Value <- as.numeric(broc$Value)

broc <- left_join(broc,chemical_tox,by="Name")

broc <- rename(broc,Real=Value,LD50=X)

broc <- broc %>% gather(Real , LD50 , key="Toxicity", value="value")

brocplot <- ggplot(broc, aes(x= Name, y=value )) + 
  geom_bar(stat="identity",position = "dodge",aes(fill=Toxicity)) + 
  labs(y = "Values(LB) ",x = "Chemical Name") +
  coord_flip()+
  labs(title="LD50(mg/kg) and Real content(lb) of Brocoli")



#Cauliflower

Caul <- vegdata3 %>% filter(label == "RESTRICTED USE CHEMICAL", Commodity == "CAULIFLOWER", `Unit of Measurement`==" MEASURED IN LB")

Caul$Value <- as.numeric(Caul$Value)

Caul <- left_join(Caul,chemical_tox,by="Name")

Caul <- rename(Caul,Real=Value,LD50=X)

Caul <- Caul %>% gather(Real , LD50 , key="Toxicity", value="value")

#plot the Cauliflower 
caulplot <- ggplot(Caul, aes(x= Name, y=value )) + 
  geom_bar(stat="identity",position = "dodge",aes(fill=Toxicity)) + 
  labs(y = "Values(LB) ",x = "Chemical Name") +
  coord_flip()+
  labs(title="LD50(mg/kg) and Real content(lb) of Cauliflower")

