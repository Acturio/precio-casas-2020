####  Librerias ####
library(readr)
library(tidyr)
library(DataExplorer)
library(purrr)
library(stringr)
library(tidymodels)
#library(tidypredict)
library(magrittr)
library(dplyr)

#### FUNCIONES ####

{
step_nareplace <- function(recipe, ..., role = NA, trained = FALSE, skip = FALSE, columns = NULL, replace = 0, id = rand_id("nareplace")) {
  add_step(
    recipe,
    step_nareplace_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      skip = skip,
      id = id,
      replace = replace,
      columns = columns)
  )
}

step_nareplace_new <- function(terms, role, trained, skip, id, columns, replace) {
  step(
    subclass = "nareplace",
    terms = terms,
    role = role,
    trained = trained,
    skip = skip,
    id = id,
    columns = columns,
    replace = replace
  )
}

prep.step_nareplace <- function(x, training, info = NULL, ...) {

    col_names <- terms_select(x$terms, info = info)

    step_nareplace_new(
        terms = x$terms,
        role = x$role,
        trained = TRUE,
        skip = x$skip,
        id = x$id,
        columns = col_names,
        replace = x$replace
      )
}

bake.step_nareplace <- function(object, new_data, ...) {
  for (i in  object$columns) {
    if (any(is.na(new_data[, i])))
      new_data[is.na(new_data[, i]), i] <- object$replace
  }
  as_tibble(new_data)
}

print.step_nareplace <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Replacing NA values in ", sep = "")
    cat(format_selectors(x$terms, wdth = width))
    cat("\n")
    invisible(x)
  }

tidy.step_nareplace <- function(x, ...) {
  res <- simple_terms(x, ...)
  res$id <- x$id
  res
}
}

#### LECTURA DE ARCHIVOS ####

casas <- read_csv("data/casas_entrena.csv")
names(casas) <- names(casas) %>% map_chr(str_replace_all," ", "_" ) 

casas %<>% dplyr::mutate(SalePrice = log(SalePrice))

casas_prueba <- read_csv("data/casas_prueba.csv")
names(casas_prueba) <- names(casas_prueba) %>% map_chr(str_replace_all," ", "_" ) 

glimpse(casas)

###################

plot(casas$SalePrice, casas$Yr_Sold - casas$`Year_Remod/Add`)

casas %>% ggplot(aes(x = I((Yr_Sold - `Year_Remod/Add`)^2), y = log(SalePrice))) +
  geom_point()

casas$Garage_Type %>% table()

#### TRATAMIENTO ####

#mise::mise(vars = FALSE, figs = FALSE)

receta_casas <- casas %>% 
  recipe(SalePrice ~ .) %>% 
  step_filter(Sale_Condition == 'Normal') %>% 
  step_rename(
              First_Flr_SF = `1st_Flr_SF`,
              Second_Flr_SF = `2nd_Flr_SF`,
              Third_Ssn_Porch= `3Ssn_Porch`,
              Year_Remmod_Add = `Year_Remod/Add`
              ) %>% 
  step_unknown(Alley, new_level = "Unknown") %>% 
  step_unknown(Fence, new_level = "Unknown") %>% 
  step_unknown(Fireplace_Qu, new_level = "Unknown") %>% 
  step_unknown(Garage_Type, new_level = "Unknown") %>% 
  step_unknown(Garage_Finish, new_level = "Unknown") %>% 
  step_unknown(Garage_Qual, new_level = "Unknown") %>% 
  step_unknown(Garage_Cond, new_level = "Unknown") %>% 
  step_unknown(Bsmt_Qual, new_level = "Unknown") %>% 
  step_unknown(Bsmt_Cond, new_level = "Unknown") %>% 
  step_unknown(Bsmt_Exposure, new_level = "Unknown") %>% 
  step_unknown(BsmtFin_Type_1, new_level = "Unknown") %>% 
  step_unknown(BsmtFin_Type_2, new_level = "Unknown") %>% 
  step_unknown(Mas_Vnr_Type, new_level = "Unknown") %>% 
  step_unknown(Electrical, new_level = "Unknown") %>%  
  step_unknown(Pool_QC, new_level = "Unknown") %>%  
  step_unknown(Misc_Feature, new_level = "Unknown") %>%  
  step_nareplace(Lot_Frontage, replace = 0) %>% 
  step_nareplace(Mas_Vnr_Area, replace = 0) %>% 
  step_mutate(Garage_Yr_Blt = if_else(is.na(Garage_Yr_Blt), Year_Built, Garage_Yr_Blt),
              TotalBaths = Full_Bath + 0.5 * Half_Bath + Bsmt_Full_Bath + Bsmt_Half_Bath * 0.5,
              Age_House = Yr_Sold - Year_Built,
              TotalSF     = Gr_Liv_Area + Total_Bsmt_SF,
              AvgRoomSF   = Gr_Liv_Area / TotRms_AbvGrd,
              Fireplaces_Cat = factor(if_else(Fireplaces > 0,1,0)),
              Overall_Qual_2 = Overall_Qual^2
            ) %>% 
  step_interact(~ Overall_Qual:Gr_Liv_Area) %>%
  step_interact(~ Overall_Qual:TotRms_AbvGrd) %>%
  step_interact(~ Overall_Qual:TotalBaths) %>%
  step_ratio(Second_Flr_SF, denom = denom_vars(First_Flr_SF)) %>% 
  step_relevel(Bsmt_Qual, ref_level = "TA") %>% 
#  step_other(Neighborhood, threshold = 50) %>% 
  step_other(Exterior_1st, threshold = 20) %>% 
  step_other(Exterior_2nd, threshold = 20) %>% 

  step_normalize(all_predictors(), -all_nominal()) %>%
  step_rm(Neighborhood, Utilities, Garage_Type, Mas_Vnr_Type, 
          Exterior_1st, Exterior_2nd, MS_SubClass) %>% 
  step_dummy(all_nominal()) %>%
  step_interact(~ matches("Bsmt_Qual"):Total_Bsmt_SF) %>% 
  step_nzv(all_predictors()) %>% 
  
  step_rm(Garage_Cond_Unknown, Garage_Qual_Unknown, #Gr_Liv_Area, Total_Bsmt_SF,
        Full_Bath, Half_Bath, Bsmt_Full_Bath, Bsmt_Half_Bath,
        First_Flr_SF, Second_Flr_SF, Yr_Sold,
        Lot_Config_Inside, Fence_Unknown,
        Heating_QC_Gd, Bsmt_Exposure_Mn, Condition_1_Norm, BsmtFin_Type_1_BLQ,
        BsmtFin_Type_2_Unf, Garage_Finish_RFn, BsmtFin_SF_2,
        BsmtFin_Type_1_Rec, Fence_MnPrv, Bldg_Type_TwnhsE,
        Lot_Config_CulDSac, House_Style_SLvl, Exter_Cond_Gd,
        BsmtFin_Type_1_LwQ, House_Style_X1Story) %>% 
  prep() # Falta agregar la edad de casa y de la última remodelación

casas$Mas_Vnr_Type %>% table()

### SPLIT DATA ###

set.seed(20180911)

casas_split <- initial_split(casas, prop = .7)

casas_train <- training(casas_split)
casas_test <- testing(casas_split)

### TRAIN MODEL ###

# modelo_lineal <-  linear_reg(mode = "regression", penalty = 0.00085, mixture = 0.825) %>%
#     set_engine("glmnet") %>% translate() %>% 
#     fit(SalePrice ~ ., juice(receta_casas))

#juice(receta_casas)  %>%  plot_missing()

#modelo_lineal <-  linear_reg(mode = "regression",) %>%
#    set_engine("lm") %>% translate() %>% 
#    fit(LogSales ~ . - SalePrice, juice(receta_casas))

# modelo_lineal <-  linear_reg(mode = "regression", penalty = tune(), mixture = tune()) %>%
#   set_engine("glmnet") %>% translate() %>% 
#   fit(SalePrice ~ ., juice(receta_casas))

modelo_lineal <- linear_reg(mode = "regression", penalty = tune(), mixture = 0.75) %>%
  set_engine("glmnet") %>% translate() 

wf <- workflow() %>% 
  add_model(modelo_lineal) %>% 
  add_recipe(receta_casas)

params <- parameters(penalty(range = c(-15,10), trans = log10_trans()))
gd_regular <- grid_regular(params, levels = 100)

validation_partition <- vfold_cv(casas_train, v = 10)

metricas_vc <- tune_grid(wf,
                         resamples = validation_partition,
                         grid = gd_regular,
                         metrics = metric_set(rmse, mae))

ggplot(metricas_vc %>% unnest(.metrics) %>% filter(.metric == "rmse"), 
       aes(x = penalty, y = .estimate)) + geom_point() +
  scale_x_log10()

metricas_resumen <- metricas_vc %>% collect_metrics() %>% 
  arrange(mean)
metricas_vc %>% show_best(metric = "rmse")

ggplot(metricas_resumen %>% filter(.metric == "rmse"), 
       aes(x = penalty, y = mean, ymin = mean - std_err, ymax = mean + std_err)) +
  geom_linerange() +
  geom_point(colour = "red") +
  scale_x_log10()

##############


modelo_lineal <-  linear_reg(mode = "regression", penalty = 0.003, mixture = 0.75) %>%
    set_engine("glmnet") %>% translate() %>%
    fit(SalePrice ~ ., juice(receta_casas))

modelo_lineal
#tidy(modelo_lineal) %>% as.data.frame() %>% arrange(desc(p.value))
tidy(modelo_lineal) %>% as.data.frame() %>% arrange(desc(abs(estimate)))
    

p_test <- predict(modelo_lineal, bake(receta_casas, casas_test)) %>% 
          bind_cols(., bake(receta_casas, casas_test)) %>% mutate(.pred=exp(.pred)) %>% 
          mutate(SalePrice = exp(SalePrice)) %>% 
          mutate(err = (log(1+SalePrice) - log(1 + .pred))^2)

p_test %>% mutate(dif = abs(SalePrice - .pred)) %>% arrange(desc(dif)) %>% 
  dplyr::select(SalePrice, .pred, dif, err) %>% dplyr::filter(dif < 583850) %>%
  pull(err) %>% mean() %>% sqrt()

p_test %>% mutate(dif = abs(SalePrice - .pred)) %>% arrange(desc(dif)) %>% 
  dplyr::select(SalePrice, .pred, dif) %>% #dplyr::filter(dif < 583850) %>% #as.data.frame() %>% 
ggplot(aes(x= SalePrice, y = .pred)) + geom_point() + geom_smooth(method = "lm")


#####################################
juice(receta_casas) %>% glimpse()

modelo_RF <-  rand_forest(mtry = 30, trees = 50000, min_n = 5) %>% 
set_engine("ranger", importance = "impurity_corrected", verbose = T) %>% 
set_mode("regression") %>% 
translate() %>% 
fit(SalePrice ~ ., juice(receta_casas))

modelo_RF
tibble(variable = reorder(names(modelo_RF$fit$variable.importance),modelo_RF$fit$variable.importance), 
       value = modelo_RF$fit$variable.importance) %>% 
  arrange(desc(value)) %>% 
  ggplot(aes(x = variable, y = value)) +
  geom_bar(fill = "blue", stat = "identity") +
  theme(text = element_text(size = 8)) +
  coord_flip()



p_test <- predict(modelo_RF, bake(receta_casas, casas_test)) %>% 
          bind_cols(., bake(receta_casas, casas_test)) %>% mutate(.pred=exp(.pred)) %>% 
          mutate(SalePrice = exp(SalePrice)) %>% 
          mutate(err = (log(1+SalePrice) - log(1 + .pred))^2)

p_test$err %>% mean() %>% sqrt()
p_test %>% mutate(dif = abs(SalePrice - .pred)) %>% dplyr::filter(dif < 100000) %>% 
  pull(err) %>% mean() %>% sqrt()

p_test %>% mutate(dif = abs(SalePrice - .pred)) %>% arrange(desc(dif)) %>% 
  dplyr::select(SalePrice, .pred, dif) %>% #dplyr::filter(dif < 100000) %>% #as.data.frame() %>% 
ggplot(aes(x= SalePrice, y = .pred)) + geom_point() + geom_smooth(method = "lm")


casas %>% filter(SalePrice == 183850)
casas$Tot_Rms_Abv_Grd
casas$TotRms_AbvGrd %>% summary()
