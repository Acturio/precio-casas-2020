####  Librerias ####
library(readr)
library(tidyr)
library(DataExplorer)
library(dplyr)
library(purrr)
library(stringr)
library(tidymodels)
library(tidypredict)


#### LECTURA DE ARCHIVOS ####

casas <- read_csv("data/casas_entrena.csv")
casas_prueba <- read_csv("data/casas_prueba.csv")



#### TRATAMIENTO ####
  names(casas) <- names(casas) %>% map_chr(str_replace_all," ", "_" ) 
  
  casas <- 
  casas %>% filter (Sale_Condition == 'Normal') %>% 
  mutate(Pool_QC = replace_na(Pool_QC, 0),
         Alley= replace_na(Alley, "Missing"), 
         Fence= replace_na(Fence, "Missing"),
         Fireplace_Qu= replace_na(Fireplace_Qu, "Missing"),
         Lot_Frontage=replace_na(Lot_Frontage, 0),
         Garage_Yr_Blt = if_else(is.na(Garage_Yr_Blt), Year_Built, Garage_Yr_Blt), 
         Garage_Type = replace_na (Garage_Type, "Missing"),
         Garage_Finish = replace_na( Garage_Finish,"Missing"), 
         Garage_Qual = replace_na(Garage_Qual, "Missing"),
         Garage_Cond =replace_na(Garage_Cond, "Missing"), 
         Bsmt_Qual =replace_na(Bsmt_Qual, "Missing"),
         Bsmt_Cond = replace_na(Bsmt_Cond, "Missing"), 
         Bsmt_Exposure =replace_na(Bsmt_Exposure, "Missing"),
         BsmtFin_Type_1 =replace_na(BsmtFin_Type_1, "Missing"),
         BsmtFin_Type_2 =replace_na(BsmtFin_Type_2, "Missing"), 
         Mas_Vnr_Type =replace_na(Mas_Vnr_Type, "Missing"),
         Mas_Vnr_Area = replace_na(Mas_Vnr_Area, 0), 
         Electrical = replace_na(Electrical, "Missing") ,
         Log_SalePrice = log(SalePrice)
    ) %>% 
    mutate_if(is.character, as.factor)
  
  
  names(casas_prueba) <- names(casas_prueba) %>% map_chr(str_replace_all," ", "_" ) 
  
  casas_prueba <- casas_prueba %>% 
  mutate(Pool_QC = replace_na(Pool_QC, 0),
         Alley= replace_na(Alley, "Missing"), 
         Fence= replace_na(Fence, "Missing"),
         Fireplace_Qu= replace_na(Fireplace_Qu, "Missing"),
         Lot_Frontage=replace_na(Lot_Frontage, 0),
         Garage_Yr_Blt = if_else(is.na(Garage_Yr_Blt), Year_Built, Garage_Yr_Blt), 
         Garage_Type = replace_na (Garage_Type, "Missing"),
         Garage_Finish = replace_na( Garage_Finish,"Missing"), 
         Garage_Qual = replace_na(Garage_Qual, "Missing"),
         Garage_Cond =replace_na(Garage_Cond, "Missing"), 
         Bsmt_Qual =replace_na(Bsmt_Qual, "Missing"),
         Bsmt_Cond = replace_na(Bsmt_Cond, "Missing"), 
         Bsmt_Exposure =replace_na(Bsmt_Exposure, "Missing"),
         BsmtFin_Type_1 =replace_na(BsmtFin_Type_1, "Missing"),
         BsmtFin_Type_2 =replace_na(BsmtFin_Type_2, "Missing"), 
         Mas_Vnr_Type =replace_na(Mas_Vnr_Type, "Missing"),
         Mas_Vnr_Area = replace_na(Mas_Vnr_Area, 0), 
         Electrical = replace_na(Electrical, "Missing") 
    ) %>% 
    mutate_if(is.character, as.factor)
  
  
  
  
  
#### ENTENDIMIENTO Y DESCRIPTIVO #### 
  
  # 1209 registros
  # 44 categoricas
  # 36 numericas

  # Analisis de correlacion numericas: 
  casas %>% 
    filter (Sale_Condition == 'Normal')  %>% 
    select (where(is.numeric)) %>% 
    plot_correlation(type = "all",
                     cor_args = list("use" = "pairwise.complete.obs"))
  
  casas %>%  
    filter (Sale_Condition == 'Normal')  %>% 
    select (where(is.numeric)) %>% 
    plot_scatterplot( by = "SalePrice", sampled_rows = 1000L)
  
  mat_cor <- casas %>% 
    select(where(is.numeric)) %>% 
    cor( use= "pairwise.complete.obs") %>% 
    round(2) 
  mat_cor [,36]
  
  # Distribuci?n de variable respuesta 
  casas %>% 
    filter (Sale_Condition == 'Normal')  %>% 
    select (SalePrice) %>% 
    plot_histogram()
  
  # Respuesta vs Gr_Liv_Area
  casas %>% 
  filter (Sale_Condition == 'Normal'  )  %>% 
    select (SalePrice,Gr_Liv_Area ) %>% 
    plot(col="blue")
  
  
  # Distribuci?n de categoricas:
  casas %>% 
    filter (Sale_Condition == 'Normal')  %>% 
    select (where(is.character)) %>% 
    plot_bar( )

  a<-casas %>% 
    filter (Sale_Condition == 'Normal')  %>% 
    select (SalePrice, where(is.character)) %>% 
    plot_correlation(type = "all", #Correlacion dummy
                     cor_args = list("use" = "pairwise.complete.obs"))
  
  a$data %>% as_tibble() %>% filter(Var2== 'SalePrice', abs(value) > .4)

  
  # Valores nulos: 
  
  casas %>% filter (Sale_Condition == 'Normal')  %>%  plot_missing()
  
#### MODELO 1 ####
  
  casas_df1 = casas %>% 
    dplyr::filter (Sale_Condition == 'Normal') %>% 
    dplyr::select (Log_SalePrice,
            #Variables numericas con correlacion > 0.5
            Overall_Qual,
            Year_Built  ,
            Year_Remod = 'Year_Remod/Add',
            Mas_Vnr_Area ,
            Total_Bsmt_SF ,     
            First_Flr_SF = '1st_Flr_SF',
            Second_Flr_SF = '2nd_Flr_SF',
            Gr_Liv_Area ,
            Full_Bath,
            Half_Bath,
            Bsmt_Full_Bath,
            Bsmt_Half_Bath,
            TotRms_AbvGrd , 
            Garage_Yr_Blt ,
            Garage_Cars ,
            Garage_Area,
            # Variables categoricas que en forma dummy cor > .4
            Exter_Qual,
            Foundation,
            #Heating,     Revisar colapsar variables
            Kitchen_Qual,
            Kitchen_AbvGr,
            Fireplaces,
            Fireplace_Qu, # Revisar colapsar categorías
            Yr_Sold,
            Enclosed_Porch,
            ThirdSsn_Porch = "3Ssn_Porch",
            Screen_Porch,
            Open_Porch_SF,
            Bldg_Type,
            MS_Zoning,
            Lot_Frontage,
            Lot_Shape,
            Land_Contour,
            Lot_Config,
            Land_Slope,
            Condition_1,
            Overall_Cond,
            Exter_Cond,
            Bsmt_Qual,
            Bsmt_Cond,
            BsmtFin_Type_1,
            BsmtFin_SF_1,
            BsmtFin_Type_2,
            Bsmt_Exposure,
            Heating_QC,
            Central_Air,
            Bedroom_AbvGr,
            Misc_Val,
            Neighborhood
            )

  casas %>% names()
  casas$Bsmt_Qual  %>% unique()
  casas$Exter_Qual %>% table(useNA = "always")

  set.seed(20180911)
  casas_split <- initial_split(casas_df1, prop = .7)
  casas_train <- training(casas_split)
  casas_test  <- testing(casas_split)
  
  modelo1 <-  linear_reg() %>%
    set_mode("regression") %>%
    set_engine("lm")
  
  casas_rec <- recipe(Log_SalePrice ~ . , data = casas_train) %>%
    step_mutate(TotalBaths = Full_Bath + 0.5 * Half_Bath + Bsmt_Full_Bath + Bsmt_Half_Bath * 0.5,
                Garage_Yr_Blt = if_else(is.na(Garage_Yr_Blt), Year_Built, Garage_Yr_Blt),
                Age_House = Yr_Sold - Year_Remod,
                TotalSF   = Gr_Liv_Area + Total_Bsmt_SF,
                Overall_Qual_2 = Overall_Qual^2,
                AvgRoomSF   = Gr_Liv_Area / TotRms_AbvGrd,
                PorchSF     = Enclosed_Porch + ThirdSsn_Porch + Open_Porch_SF,
                Porch       = factor(PorchSF > 0)) %>% 
    step_interact(~ Overall_Qual:TotalBaths) %>%
    step_interact(~ Overall_Cond:TotRms_AbvGrd) %>%
    step_interact(~ Overall_Qual:TotRms_AbvGrd) %>%
    step_interact(~ Overall_Qual:Gr_Liv_Area) %>% 
    step_interact(~ Overall_Qual:Overall_Cond) %>%
    step_interact(~ Overall_Cond:Age_House) %>%
    step_interact(~ Overall_Qual:Total_Bsmt_SF) %>%
    step_interact(~ TotalSF:Age_House) %>%
    step_interact(~ Second_Flr_SF:Bedroom_AbvGr) %>%
    step_interact(~ TotalSF:TotRms_AbvGrd) %>%
    step_interact(~ Age_House:TotRms_AbvGrd) %>%
    step_interact(~ Overall_Qual:Misc_Val) %>%
    step_ratio(Second_Flr_SF, denom = denom_vars(First_Flr_SF)) %>% 
    step_other(Neighborhood, threshold = 25) %>% 
    step_other(Exter_Cond, threshold = 40) %>% 
    step_other(Condition_1, threshold = 15) %>% 
    step_other(Land_Slope, threshold = 50) %>% 
    step_other(Land_Contour, threshold = 50) %>% 
    step_other(Lot_Shape, threshold = 500) %>% 
    #step_other(Foundation, threshold = 10) %>% 

    step_relevel(Bsmt_Qual, ref_level = "TA") %>% 
    step_normalize(all_predictors(), -all_nominal()) %>%
    step_dummy(all_nominal()) %>% 
    step_interact(~ matches("Bsmt_Qual"):Total_Bsmt_SF) %>%
    step_interact(~ matches("Bsmt_Qual"):TotRms_AbvGrd) %>%  
    step_interact(~ matches("Bsmt_Qual"):Bedroom_AbvGr) %>%
    step_interact(~ matches("Bsmt_Cond"):TotRms_AbvGrd) %>% 
    step_interact(~ matches("BsmtFin_Type_1"):BsmtFin_SF_1) %>%
    step_interact(~ matches("Kitchen_Qual"):Kitchen_AbvGr) %>%
    step_interact(~ matches("Heating_QC"):TotRms_AbvGrd) %>%
    step_interact(~ matches("Heating_QC"):TotalSF) %>%
    step_interact(~ matches("Heating_QC"):Second_Flr_SF) %>%
    step_rm(First_Flr_SF, Second_Flr_SF, Year_Remod, Yr_Sold,
            Bsmt_Full_Bath, Bsmt_Half_Bath, 
            Kitchen_AbvGr, BsmtFin_Type_1_Unf) %>% 
    prep()
  
  casa_juiced <- juice(casas_rec)
  
  lm_fit1 <- fit(modelo1, Log_SalePrice ~ ., casa_juiced)
  #lm_fit1 %>% tidy() %>% arrange(desc(std.error)) %>% as.data.frame()
  
  p_test <- predict(lm_fit1, bake(casas_rec, casas_test)) %>%
    bind_cols(casas_test) %>% dplyr::select(.pred, Log_SalePrice) %>% 
    dplyr::mutate(.pred=exp(.pred), Log_SalePrice = exp(Log_SalePrice)) %>% 
    dplyr::mutate(err = (log(1 + Log_SalePrice) - log(1 + .pred))^2)
  
  p_test %>% pull(err) %>% mean() %>% sqrt()
# 0.1289086 
0.08862085


p_test %>% ggplot(aes(x= Log_SalePrice, y=.pred)) + geom_point() 
  
  
  
#### PRUEBA KAGGLE####
  
  test <- casas_prueba %>% 
    select (#Variables numericas con correlacion > 0.5
            Overall_Qual,
            Year_Built  ,
            'Year_Remod/Add',
            Mas_Vnr_Area ,
            Total_Bsmt_SF ,     
            '1st_Flr_SF' ,
            Gr_Liv_Area ,
            Full_Bath,
            Half_Bath,
            Bsmt_Full_Bath,
            Bsmt_Half_Bath,
            TotRms_AbvGrd , 
            Fireplaces,
            Garage_Yr_Blt ,
            Garage_Cars ,
            Garage_Area,
            # Variables categoricas que en forma dummy cor > .4
            Exter_Qual,
            Foundation,
            Bsmt_Qual,
            BsmtFin_Type_1,
           # Heating,
            Kitchen_Qual,
            Bsmt_Qual,
            Fireplace_Qu) 
    
  train <- casas_df1
  
  
  casas_rec_KAGGLE <- recipe(Log_SalePrice ~ .  , data = train) %>%
    step_normalize(all_predictors(), -all_nominal()) %>%
    step_dummy(all_nominal()) %>% 
    prep()
  
  casa_juiced_KAGGLE <- juice(casas_rec_KAGGLE)
  
  lm_fit1_KAGGLE <- fit(modelo1, Log_SalePrice ~ ., casa_juiced_KAGGLE)
  lm_fit1_KAGGLE
  
  
  submission1 <- predict(lm_fit1_KAGGLE, bake(casas_rec_KAGGLE, test)) %>%
    bind_cols(test)  %>% mutate(SalePrice=exp(.pred)) 
   
   submission1_KAGGLE <- submission1 %>% 
                        mutate(id=row.names(submission1)) %>% 
                        select(id, SalePrice)
 
    write_csv(submission1_KAGGLE, "Kalizzy_sub1.csv")
  
  
  

  
  
  
  
  
