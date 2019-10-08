library(tidyverse)
library(ggplot2)
library(knitr)
library(rmarkdown)
library(stargazer)
library(readr)
library(broom)

hcc_data_complete_balanced <- read_csv("hcc-dataset/hcc-data-complete-balanced.csv", 
                                       col_types = cols(AFP = col_number(), 
                                                        AHT = col_logical(), 
                                                        ALP = col_number(), 
                                                        ALT = col_number(), 
                                                        AST = col_number(), 
                                                        Age = col_number(), 
                                                        Alcohol = col_logical(), 
                                                        CRI = col_logical(), 
                                                        Cirrhosis = col_logical(), 
                                                        Class = col_logical(), 
                                                        Creatinine = col_number(), 
                                                        Diabetes = col_logical(), 
                                                        Dir_Bil = col_number(),
                                                        Endemic = col_logical(), 
                                                        Ferritin = col_number(),
                                                        GGT = col_number(), 
                                                        Gender = col_logical(),
                                                        Grams_day = col_number(), 
                                                        HBcAb = col_logical(),
                                                        HBeAg = col_logical(), 
                                                        HBsAg = col_logical(),
                                                        HCVAb = col_logical(), 
                                                        HIV = col_logical(),
                                                        Hallmark = col_logical(), 
                                                        Hemochro = col_logical(),
                                                        INR = col_number(), 
                                                        Iron = col_number(),
                                                        Major_Dim = col_number(), 
                                                        Metastasis = col_logical(),
                                                        NASH = col_logical(), 
                                                        Nodule = col_number(),
                                                        Obesity = col_logical(),
                                                        PHT = col_logical(),
                                                        PVT = col_logical(), 
                                                        Packs_year = col_number(),
                                                        Sat = col_number(), 
                                                        Smoking = col_logical(),
                                                        Spleno = col_logical(), 
                                                        Symptoms = col_logical(),
                                                        TP = col_number(), 
                                                        Total_Bil = col_number(),
                                                        Varices = col_logical()))

hcc_fil <- hcc_data_complete_balanced %>% dplyr::select(Gender, Symptoms, HBsAg, HBeAg, HCVAb, Alcohol, HIV, Obesity, Diabetes, NASH, Smoking, Cirrhosis, PHT, PVT, Metastasis, ALT, AST, GGT, ALP, Creatinine, Dir_Bil, Total_Bil, Albumin, INR, Platelets)

VHB_aguda <- hcc_fil %>% filter(HBeAg != 0)
VHB_aguda_02 <- VHB_aguda %>% dplyr::select(Gender, Symptoms, HBeAg, HBsAg, HCVAb, ALT, AST, GGT, ALP, Creatinine, Dir_Bil, Total_Bil, Albumin, INR, Platelets)


VHB_cronica <- hcc_fil %>% filter(HBsAg != 0) %>% add_count()
VHB_cronica_02 <- VHB_cronica %>% dplyr::select(Gender, Symptoms, HBsAg, HBeAg, HCVAb, ALT, AST, GGT, ALP, Creatinine, Dir_Bil, Total_Bil, Albumin, INR, Platelets)

VHC <- hcc_fil %>% filter(HCVAb != 0) %>% add_count()
VHC_02 <- VHC %>% dplyr::select(Gender, Symptoms, HBsAg, HBeAg, HCVAb, ALT, AST, GGT, ALP, Creatinine, Dir_Bil, Total_Bil, Albumin, INR, Platelets)

alcohol <- hcc_fil %>% filter(Alcohol != 0)  %>% add_count()
alcochol_02 <- alcohol %>% dplyr::select(Gender, Symptoms, Alcohol, NASH, Smoking, Cirrhosis, ALT, AST, GGT, ALP, Creatinine, Dir_Bil, Total_Bil, Albumin, INR, Platelets)

HIV <- hcc_fil %>% filter(HIV != 0) %>% add_count()
HIV_02 <- HIV %>% dplyr::select(Gender, Symptoms, HIV, HBsAg, HCVAb, Alcohol, Obesity, Diabetes, NASH, Smoking, Cirrhosis, PHT, PVT, Metastasis, Albumin, INR, Platelets)

NASH <- hcc_fil %>% filter(NASH != 0) %>% add_count()
NASH_02 <- NASH %>% dplyr::select(Gender, Symptoms, NASH, Alcohol, Obesity, Diabetes, Smoking, Cirrhosis, PHT, PVT, Metastasis, GGT, ALP, Albumin)

##Cuantificaciones
Gender_n <- hcc_fil %>% count(Gender)

ggplot(Gender_n, aes(x = Gender, y= n)) +
  geom_bar(stat = "identity") +
  scale_fill_gradientn( colours = terrain.colors(6))

VHB_aguda_n <- hcc_fil %>% count(HBeAg)
ggplot(VHB_aguda_n, aes(x = HBeAg, y= n)) +
  geom_bar(stat = "identity") +
  scale_fill_gradientn( colours = terrain.colors(6))
  
VHB_cronica_n <- hcc_fil %>% count(HBsAg)
ggplot(VHB_cronica_n, aes(x = HBsAg, y= n)) +
  geom_bar(stat = "identity") +
  scale_fill_gradientn( colours = terrain.colors(6))

VHC_n <- hcc_fil %>% count(HCVAb)
ggplot(VHC_n, aes(x = HCVAb, y= n)) +
  geom_bar(stat = "identity") +
  scale_fill_gradientn(colours = terrain.colors(6))
                        
alcohol_n <- hcc_fil %>% count(Alcohol)
ggplot(alcohol_n, aes(x = Alcohol, y= n)) +
  geom_bar(stat = "identity") +
  scale_fill_gradientn(colours = terrain.colors(6))
                        
HIV_n <- hcc_fil %>% count(HIV)
ggplot(HIV_n, aes(x = HIV, y= n)) +
  geom_bar(stat = "identity") +
  scale_fill_gradientn(colours = terrain.colors(6))

NASH_n <- hcc_fil %>% count(NASH)
ggplot(NASH_n, aes(x = NASH, y= n)) +
  geom_bar(stat = "identity") +
  scale_fill_gradientn(colours = terrain.colors(6))

sintomas_n <- hcc_fil %>% count(Symptoms)
ggplot(sintomas_n, aes(x = Symptoms, y= n)) +
  geom_bar(stat = "identity") +
  scale_fill_gradientn(colours = terrain.colors(6))

cirrhosis_n <- hcc_fil %>% count(Cirrhosis)

ggplot(hcc_fil, aes(x= ALT, y= GGT)) +
  geom_point(aes(color= Symptoms, shape= Metastasis)) + 
  theme_classic()

ggplot(hcc_fil, aes(x= ALP, y= GGT)) +
  geom_point(aes(color= Symptoms, shape= Metastasis)) + 
  theme_classic()

bind_01 <- bind_cols(alcohol_n, sintomas_n)


sum_01 <- hcc_fil %>% summarise(sum(Gender))


bind_02 <- bind_rows(alcohol_n, sintomas_n, NASH_n)

SA <- hcc_fil %>% filter(Alcohol == "TRUE", Symptoms == "TRUE", GGT > 100, ALT > 100, AST > 100)

t.test(GGT ~ Cirrhosis, data = SA)
summary(lm(GGT ~ Cirrhosis, data = SA))

formula <- lm(GGT ~ ALT + AST, data = SA)
summary(formula)
glance(formula)
tidy(formula)

ggplot(hcc_fil, aes(x= ALT + AST, y=GGT)) +
  geom_path(aes(group = GGT, color = Symptoms)) +
  geom_point(aes(color = Symptoms)) +
  theme_classic()

ggplot(SA, aes(x= ALT + AST, y=GGT)) +
  geom_path(aes(group = GGT, color = Symptoms)) +
  geom_point(aes(color = Symptoms)) +
  theme_classic()


dtc13 <- lm(uptake ~ I(log(conc)) + conc^2 + Type*Treatment, data = CO2)
dtc13_glance <- glance(dtc13)


