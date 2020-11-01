####  Librerias ####
library(readr)
library(tidyr)
library(DataExplorer)
library(dplyr)
library(purrr)
library(stringr)
library(tidymodels)
library(tidypredict)
library(forcats)


#### LECTURA DE ARCHIVOS ####

casas <- read_csv("data/casas_entrena.csv")
casas_prueba <- read_csv("data/casas_prueba.csv")

#### TRATAMIENTO ####
names(casas) <- names(casas) %>% map_chr(str_replace_all," ", "_" ) 

casas <- casas %>% 
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
         Bsmt_Full_Bath = replace_na(Bsmt_Full_Bath, 0), 
         Bsmt_Half_Bath = replace_na(Bsmt_Half_Bath, 0), 
         Electrical = replace_na(Electrical, "Missing") ,
         Heating_QC = replace_na(Heating_QC, "Missing"),
         Log_SalePrice = log(SalePrice),
         ID = 1:nrow(casas)) %>% filter (Sale_Condition == 'Normal') %>% 
  mutate_if(is.character, as.factor) %>% 
  rename(Year_Remod = 'Year_Remod/Add',
         First_Flr_SF = '1st_Flr_SF',
         Second_Flr_SF = '2nd_Flr_SF',
         ThirdSsn_Porch = "3Ssn_Porch")



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
         Bsmt_Full_Bath = replace_na(Bsmt_Full_Bath, 0), 
         Bsmt_Half_Bath = replace_na(Bsmt_Half_Bath, 0), 
         Electrical = replace_na(Electrical, "Missing"),
         Heating_QC = replace_na(Heating_QC, "Missing")
  ) %>% 
  mutate_if(is.character, as.factor) %>% 
  rename(Year_Remod = 'Year_Remod/Add',
         First_Flr_SF = '1st_Flr_SF',
         Second_Flr_SF = '2nd_Flr_SF',
         ThirdSsn_Porch = "3Ssn_Porch")


#### Variables  ####

casas_df1 = casas %>% 
  dplyr::filter (Sale_Condition == 'Normal') %>%
  dplyr::select(-Sale_Condition, -Misc_Feature, -SalePrice)

### Partición ###

set.seed(20180911)
casas_split <- initial_split(casas_df1, prop = .7)
casas_train <- training(casas_split)
casas_test  <- testing(casas_split)


### Receta #####

casas_rec <- recipe(Log_SalePrice ~ . , data = casas_df1) %>%
  update_role(ID, new_role = "id variable") %>% 
  step_filter(!ID %in% c(419, 404, 855, 427, 294, 249, 872, 311, 58, 272, 951, #
                         899, 981, 858, 324, 1159,  853, 213, 428)) %>% 
  step_mutate(TotalBaths = Full_Bath + Bsmt_Full_Bath + 0.5 * (Half_Bath + Bsmt_Half_Bath),
              Garage_Yr_Blt = if_else(is.na(Garage_Yr_Blt), Year_Built, Garage_Yr_Blt),
              Age_House = Yr_Sold - Year_Remod,
              TotalSF   = Gr_Liv_Area + Total_Bsmt_SF,
              Overall_Qual_2 = Overall_Qual^2,
              Overall_Qual_3 = Overall_Qual^3,
              AvgRoomSF   = Gr_Liv_Area / TotRms_AbvGrd,
              Porch_SF     = Enclosed_Porch + ThirdSsn_Porch + Open_Porch_SF,
              Porch       = factor(Porch_SF > 0),
              Pool = if_else(Pool_Area > 0,1,0)
  ) %>% 
  step_interact(~ Overall_Qual:TotalBaths) %>%
  step_interact(~ Overall_Cond:TotRms_AbvGrd) %>%
  step_interact(~ Overall_Qual:TotRms_AbvGrd) %>%
  step_interact(~ Overall_Qual:Gr_Liv_Area) %>% 
  step_interact(~ Overall_Qual:Overall_Cond) %>%
  step_interact(~ Overall_Cond:Age_House) %>%
  step_interact(~ Overall_Qual:Total_Bsmt_SF) %>%
  step_interact(~ Overall_Cond:Garage_Area) %>%
  step_interact(~ TotalSF:Age_House) %>%
  step_interact(~ Second_Flr_SF:Bedroom_AbvGr) %>%
  step_interact(~ TotalSF:TotRms_AbvGrd) %>%
  step_interact(~ Age_House:TotRms_AbvGrd) %>%
  step_interact(~ Overall_Qual:Misc_Val) %>%
  step_interact(~ Overall_Cond:Bedroom_AbvGr) %>%
  step_interact(~ Second_Flr_SF:First_Flr_SF) %>%
  step_ratio(Bedroom_AbvGr, denom = denom_vars(Gr_Liv_Area)) %>% 
  step_ratio(Second_Flr_SF, denom = denom_vars(First_Flr_SF)) %>% 
  step_mutate(Exter_Cond = fct_collapse(Exter_Cond, Good = c("TA", "Gd", "Ex")),
              Condition_1 = fct_collapse(Condition_1, 
                                         Artery_Feedr = c("Feedr", "Artery"), # Revisar 
                                         Railr = c("RRAn", "RRNn", "RRNe", "RRAe"),
                                         Norm = "Norm",
                                         Pos = c("PosN", "PosA")), #cambioV
              Land_Slope = fct_collapse(Land_Slope, Mod_Sev = c("Mod", "Sev")),
              Land_Contour = fct_collapse(Land_Contour, Low_HLS = c("Low","HLS"),
                                          Bnk_Lvl = c("Lvl","Bnk")),
              Lot_Shape = fct_collapse(Lot_Shape, IRREG = c("IR3", "IR2", "IR1")),
              #Kitchen_Qual = fct_collapse(Kitchen_Qual, Bad = c("Fa", "Po")),
              Bsmt_Cond = fct_collapse(Bsmt_Cond, Exc = c("Gd", "Ex")),
                                       
              Bsmt_Qual = fct_collapse(Bsmt_Qual, Missing = c("Missing", "Po")),
              BsmtFin_Type_1 = fct_collapse(BsmtFin_Type_1, Rec_BLQ = c("Rec", "BLQ")),
              BsmtFin_Type_2 = fct_collapse(BsmtFin_Type_2, Rec_BLQ = c("Rec", "BLQ", "LwQ")),
              Neighborhood = fct_collapse(Neighborhood, NoRidge_GrnHill = c("NoRidge", "GrnHill"),
                                          Crawfor_Greens = c("Crawfor", "Greens"),
                                          Blueste_Mitchel = c("Blueste", "Mitchel"),
                                          Blmngtn_CollgCr = c("Blmngtn", "CollgCr"),
                                          NPkVill_NAmes = c("NPkVill", "NAmes"),
                                          Veenker_StoneBr = c("Veenker", "StoneBr"),
                                          BrDale_IDOTRR = c("BrDale", "IDOTRR"),
                                          SWISU_Sawyer = c("SWISU", "Sawyer"),
                                          ClearCr_Somerst = c("ClearCr", "Somerst")),
              Fireplace_Qu = fct_collapse(Fireplace_Qu, Miss = c("Po", "Missing")),
             # Heating_QC = fct_collapse(Heating_QC, Miss = c("Po", "OthW")),
              Heating = fct_collapse(Heating, Grav_Wall = c("Grav", "Wall"),
                                     GasA_W = c("GasA", "GasW", "OthW")),
              MS_Zoning = fct_collapse(MS_Zoning, I_R_M_H = c("RM", "I (all)", "RH" )),
              Bldg_Type = fct_collapse(Bldg_Type, Du_Tu = c("Duplex", "Twnhs")),
              Foundation = fct_collapse(Foundation, Wood_Stone = c("Wood", "Stone")),
              Functional = fct_collapse(Functional, Min = c("Min1", "Min2"),  Maj = c("Maj1", "Maj2", "Mod"))
  ) %>% 
  step_relevel(Bsmt_Qual, ref_level = "TA") %>% 
  step_relevel(Exter_Cond, ref_level = "Good") %>% 
  step_relevel(Condition_1, ref_level = "Norm") %>%
  step_normalize(all_predictors(), -all_nominal()) %>%
  step_dummy(all_nominal()) %>% 
  step_interact(~ matches("Bsmt_Qual"):Total_Bsmt_SF) %>%
  step_interact(~ matches("Bsmt_Qual"):TotRms_AbvGrd) %>%  
  step_interact(~ matches("Bsmt_Qual"):Bedroom_AbvGr) %>%
  step_interact(~ matches("Bsmt_Cond"):TotRms_AbvGrd) %>% 
  step_interact(~ matches("BsmtFin_Type_1"):BsmtFin_SF_1) %>%
  step_interact(~ matches("BsmtFin_Type_1"):Total_Bsmt_SF) %>%
  step_interact(~ matches("Kitchen_Qual"):Kitchen_AbvGr) %>%
  step_interact(~ matches("Heating_QC"):TotRms_AbvGrd) %>%
  step_interact(~ matches("Heating_QC"):TotalSF) %>%
  step_interact(~ matches("Heating_QC"):Second_Flr_SF) %>%
  step_interact(~ matches("Neighborhood"):matches("Condition_1")) %>%
  step_rm(First_Flr_SF, Second_Flr_SF, Year_Remod, Yr_Sold,
          Bsmt_Full_Bath, Bsmt_Half_Bath, 
          Kitchen_AbvGr, BsmtFin_Type_1_Unf, 
          Total_Bsmt_SF, Kitchen_AbvGr, Overall_Qual, Pool_Area, ID,
          Bsmt_Qual_Gd_x_Total_Bsmt_SF_x_TotRms_AbvGrd,
          Heating_QC_TA_x_TotRms_AbvGrd_x_TotalSF_x_Second_Flr_SF,
          Bsmt_Qual_Fa_x_Total_Bsmt_SF_x_Bedroom_AbvGr,
          Gr_Liv_Area, Bsmt_Qual_Missing,
          Bsmt_Qual_Missing_x_Total_Bsmt_SF,
          Bsmt_Qual_Missing_x_TotRms_AbvGrd,
          Bsmt_Qual_Missing_x_Bedroom_AbvGr,
          Bsmt_Qual_Missing_x_TotRms_AbvGrd_x_Bedroom_AbvGr,
          BsmtFin_Type_1_Missing, 
          BsmtFin_Type_2_Missing,
          BsmtFin_Type_1_Missing_x_BsmtFin_SF_1,
          Porch_SF,
          BsmtFin_Type_1_Missing_x_BsmtFin_SF_1_x_Total_Bsmt_SF,
          BsmtFin_Type_1_Unf_x_BsmtFin_SF_1_x_Total_Bsmt_SF,
          Kitchen_Qual_Gd_x_Kitchen_AbvGr,
          Bsmt_Cond_Fa, Bsmt_Cond_TA,
          Sale_Type_Oth, Sale_Type_VWD,
          Pool_QC_Gd, Garage_Qual_Po, Electrical_Missing, Exter_Cond_Po
          
  ) %>% 
  prep()

### Ajuste de Receta

casa_juiced <- juice(casas_rec)

### Modelo 1 ###

modelo1 <-  linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

lm_fit1 <- fit(modelo1, Log_SalePrice ~ ., casa_juiced)

p_test <- predict(lm_fit1, bake(casas_rec, casas_test)) %>%
  bind_cols(casas_test) %>% #dplyr::select(.pred, Log_SalePrice) %>% 
  dplyr::mutate(.pred=exp(.pred), Log_SalePrice = exp(Log_SalePrice)) %>% 
  dplyr::mutate(err = (log(1 + Log_SalePrice) - log(1 + .pred))^2)

p_test %>% pull(err) %>% mean() %>% sqrt()



#regularización

modelo_simple <- linear_reg(mixture = tune(), penalty = tune()) %>% 
  set_mode("regression") %>%
  set_engine("glmnet")

wf_simple <- workflow() %>% 
  add_model(modelo_simple) %>% 
  add_recipe(casas_rec)

bf_set <- parameters(penalty(range = c(-3, 3), trans = log10_trans()),
                     mixture(range = c(0,1)))
bf_grid <- grid_regular(bf_set, levels = 10)

validacion_particion <- vfold_cv(casas_df1, v = 10)

metricas_vc <- tune_grid(wf_simple, 
                         resamples = validacion_particion, 
                         grid = bf_grid, 
                         metrics = metric_set(rmse, mae))

ggplot(metricas_vc %>% unnest(.metrics) %>% filter(.metric == "rmse"), 
       aes(x = penalty, y = .estimate)) + #geom_point() + 
  scale_x_log10() +
  geom_line(aes(color = id)) 
metricas_vc


metricas_resumen <- metricas_vc %>% collect_metrics()
metricas_vc %>% show_best(metric = "rmse")
(semi_mejor <- metricas_vc %>% select_by_one_std_err(metric = "rmse", desc(penalty)))
(mejor <- metricas_vc %>% select_best(metric = "rmse"))


modelo_final <- wf_simple %>%
  finalize_workflow(mejor) %>%
  fit(data = casas_train)

data_prediction <- modelo_final %>% 
  pull_workflow_prepped_recipe() %>% 
  bake(casas_test)

p_test <- predict(modelo_final, data_prediction) %>%
  bind_cols(data_prediction) %>% #dplyr::select(.pred, Log_SalePrice) %>% 
  dplyr::mutate(.pred=exp(.pred), Log_SalePrice = exp(Log_SalePrice)) %>% 
  dplyr::mutate(err = (log(1 + Log_SalePrice) - log(1 + .pred))^2)




modelo_reg <- linear_reg(mixture = 0.889, penalty = 0.001) %>% 
  set_mode("regression") %>%
  set_engine("glmnet")

lm_fit2 <- fit(modelo_reg, Log_SalePrice ~ ., casa_juiced)

p_test <- predict(lm_fit2, bake(casas_rec, casas_test)) %>%
  bind_cols(casas_test) %>% dplyr::select(.pred, Log_SalePrice) %>% 
  dplyr::mutate(.pred=exp(.pred), Log_SalePrice = exp(Log_SalePrice)) %>% 
  dplyr::mutate(err = (log(1 + Log_SalePrice) - log(1 + .pred))^2)

p_test[is.na(p_test$.pred),]

p_test %>% pull(err) %>% mean() %>% sqrt()


#################

test = casas_prueba %>% 
  dplyr::filter (Sale_Condition == 'Normal') %>%
  dplyr::select(-Sale_Condition, -Misc_Feature)


train <- casas_df1
casas_rec_KAGGLE <- casas_rec

casa_juiced_KAGGLE <- juice(casas_rec_KAGGLE)

lm_fit1_KAGGLE <- fit(modelo_reg, Log_SalePrice ~ ., casa_juiced_KAGGLE)
lm_fit1_KAGGLE %>% tidy()

submission1 <- predict(lm_fit1_KAGGLE, bake(casas_rec_KAGGLE, test)) %>%
  bind_cols(test)  %>% mutate(SalePrice=exp(.pred)) 



submission1_KAGGLE <- submission1 %>% 
  mutate(id=row.names(submission1)) %>% 
  select(id, SalePrice)

submission1[is.na(submission1$SalePrice),] %>% glimpse()
submission1_KAGGLE[is.na(submission1_KAGGLE$SalePrice),]



write_csv(submission1_KAGGLE, "Acturio_sub3.csv")  



