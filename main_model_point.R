############# model ####################################################################################################
rm(list=ls())
getwd()
setwd('E:/AND')
mod_data = read.csv('ab_sp.csv')
min(mod_data$Coscinodiscus.traducens)
hist(mod_data$Coscinodiscus.traducens)
median(mod_data$Coscinodiscus.traducens)
PA = ifelse(mod_data$Coscinodiscus.traducens>8,1,0)
mod_data = cbind(mod_data,PA)

# get Presence/Absence data.  
resp_v_name <- 'PA'
resp_v <- mod_data[, resp_v_name]

# get coordinate data
xy <- mod_data[, c('Longitude', 'Latitude')]

# Step-2: Multicolinerity Checking
p1 = as.data.frame(st)
current_df <- na.omit(p1)
current_df 
cor_current = cor(mod_data[, c("Temp", "Sal", "Silicate", "Nitrite", "Phosphate")])
corrplot(cor_current, method = 'color', type = 'upper', diag = FALSE)

# VIF Checking 
collin = vifstep(mod_data[, c("Temp", "Sal", "Silicate", "Nitrite", "Phosphate")], th=10)
vif<-collin@results
corMatrix<-collin@corMatrix
levelplot(corMatrix)



# Formatting data for model
diatom_data <- BIOMOD_FormatingData(
  resp.var = mod_data$PA,
  expl.var = mod_data[, c("Temp", "Sal", "Silicate", "Nitrite", "Phosphate")],
  resp.xy = xy,
  resp.name = "Random_Species"
)

plot(diatom_data)

# modelling options 
option_1= BIOMOD_ModelingOptions(
    GLM = list(type = 'quadratic', interaction.level = 1),
    GBM = list(n.trees = 1000),
    GAM = list(algo = 'GAM_mgcv')
  )

option_2= BIOMOD_ModelingOptions(GLM = list( myFormula = formula(
              "PA ~ Temp +log(Silicate) + poly(Sal,2) + Sal + Phosphate+ Nitrite+Silicate:Phosphate")),
                             GBM = list(n.trees = 1000),
                             GAM = list(algo = 'GAM_mgcv'))


# Cross validation methods k-folds

DiatomCV <- BIOMOD_cv(diatom_data, k = 10, repetition = 5, do.full.models = TRUE,
                        stratified.cv = FALSE, stratify = "both", balance = "pres")


# Model Running 
Diatom_model_1 <- 
  BIOMOD_Modeling(
    data = diatom_data,
    models = c("GLM", "GBM", "RF", "GAM",'ANN','FDA','CTA','SRE'),
    models.options = option_1,
    CV.strategy = DiatomCV,
    CV.nb.rep = 5,
    CV.perc = 0.8,
    OPT.data.type = 'binary.PA',
    OPT.strategy = 'bigboss',
    metric.eval = c('TSS','ROC'),
    NbRunEval = 2,
    DataSplit = 80,
    VarImport = 1,
    modeling.id = "demo1"
  )


Diatom_model_1 =
  BIOMOD_Modeling(
    data = diatom_data,
    models = c("GLM", "GBM", "RF", "GAM",'ANN','FDA','CTA','SRE'),
    models.options = option_2,
    CV.strategy = 'kfold',
    CV.nb.rep = 10,
    CV.perc = 0.9,
    OPT.data.type = 'binary.PA',
    OPT.strategy = 'bigboss',
    metric.eval = c('TSS','ROC','KAPPA'),
    DataSplit = 80,
    VarImport = 10,
    Prevalence = 0.6,
    modeling.id = "demo1"
  )

Diatom_ev = get_evaluations(Diatom_model_1)

Diatom_ev = get_evaluations(Diatom_model_2)

# model dimension score
dim(Diatom_ev)
dimnames(Diatom_ev)

# ROC and TSS data frame

models_scores_graph(Diatom_model_1, by = "models" ,
                    metrics = c("ROC","TSS"))

(eval.df <- t(data.frame(RUN_1 = Diatom_ev[ , 1, , 1, ],
                         RUN_2 = Diatom_ev[ , 1, , 2, ],
                         RUN_3 = Diatom_ev[ , 1, , 3, ])))





# Model Evaluation and its Score

models_scores_graph(
  Diatom_model_1, 
  by = "models", 
  metrics = c("ROC","TSS"), 
  xlim = c(0.5,1), 
  ylim = c(0.5,1)
)

models_scores_graph(
  Diatom_model_1, 
  by = "cv_run" , 
  metrics = c("ROC","TSS"), 
  xlim = c(0.1,1), 
  ylim = c(0.1,1)
)

models_scores_graph(
  Diatom_model_1, 
  by = "data_set", 
  metrics = c("ROC","TSS"), 
  xlim = c(0.1,1), 
  ylim = c(0.1,1)
)

# Check Important Variables

(Diatom_import_var <- get_variables_importance(Diatom_model_1))

plot(Diatom_import_var)

apply(Diatom_import_var, c(1,2), mean)


# Individual Model Response Plot

Diatom_glm <- BIOMOD_LoadModels(Diatom_model_1, models='GLM')
Diatom_gbm <- BIOMOD_LoadModels(Diatom_model_1, models='GBM')
Diatom_rf <- BIOMOD_LoadModels(Diatom_model_1, models='RF')
Diatom_gam <- BIOMOD_LoadModels(Diatom_model_1, models='GAM')
Diatom_ann <- BIOMOD_LoadModels(Diatom_model_1, models='ANN')
Diatom_cta <- BIOMOD_LoadModels(Diatom_model_1, models='CTA')
Diatom_fda <- BIOMOD_LoadModels(Diatom_model_1, models='FDA')
Diatom_sre <- BIOMOD_LoadModels(Diatom_model_1, models='SRE')


Diatom_gam_strip <- 
  biomod2::response.plot2(
    models  = Diatom_gam,
    Data = get_formal_data(Diatom_model_1,'expl.var'), 
    show.variables= get_formal_data(Diatom_model_1,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    legend = FALSE,
    display_title = FALSE,
    data_species = get_formal_data(Diatom_model_1,'resp.var')
  )


Diatom_glm_strip <- 
  biomod2::response.plot2(
    models  = Diatom_glm,
    Data = get_formal_data(Diatom_model_1,'expl.var'), 
    show.variables= get_formal_data(Diatom_model_1,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    legend = FALSE,
    display_title = FALSE,
    data_species = get_formal_data(Diatom_model_1,'resp.var')
  )

Diatom_rf_strip <- 
  biomod2::response.plot2(
    models  = Diatom_rf,
    Data = get_formal_data(Diatom_model_1,'expl.var'), 
    show.variables= get_formal_data(Diatom_model_1,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    legend = FALSE,
    display_title = FALSE,
    data_species = get_formal_data(Diatom_model_1,'resp.var')
  )

Diatom_gbm_strip <- 
  biomod2::response.plot2(
  models  = Diatom_gbm,
  Data = get_formal_data(Diatom_model_1,'expl.var'), 
  show.variables= get_formal_data(Diatom_model_1,'expl.var.names'),
  do.bivariate = FALSE,
  fixed.var.metric = 'median',
  legend = FALSE,
  display_title = FALSE,
  data_species = get_formal_data(Diatom_model_1,'resp.var')
)

Diatom_ann_strip <- 
  biomod2::response.plot2(
    models  = Diatom_ann,
    Data = get_formal_data(Diatom_model_1,'expl.var'), 
    show.variables= get_formal_data(Diatom_model_1,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    legend = T,
    col = c("blue", "red"),
    plot = T,
    display_title = TRUE,
    data_species = get_formal_data(Diatom_model_1,'resp.var')
  )

Diatom_sre_strip <- 
  biomod2::response.plot2(
    models  = Diatom_sre,
    Data = get_formal_data(Diatom_model_1,'expl.var'), 
    show.variables= get_formal_data(Diatom_model_1,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    legend = FALSE,
    display_title = FALSE,
    data_species = get_formal_data(Diatom_model_1,'resp.var')
  )


Diatom_fda_strip <- 
  biomod2::response.plot2(
    models  = Diatom_fda,
    Data = get_formal_data(Diatom_model_1,'expl.var'), 
    show.variables= get_formal_data(Diatom_model_1,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    legend = FALSE,
    display_title = FALSE,
    data_species = get_formal_data(Diatom_model_1,'resp.var')
  )

Diatom_cta_strip <- 
  biomod2::response.plot2(
    models  = Diatom_cta,
    Data = get_formal_data(Diatom_model_1,'expl.var'), 
    show.variables= get_formal_data(Diatom_model_1,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    legend = FALSE,
    display_title = FALSE,
    data_species = get_formal_data(Diatom_model_1,'resp.var')
  )



# For Model Plot and comparisons

mod_com =
  BIOMOD_LoadModels(
    Diatom_model_1, 
    models = c('RF','GBM')
  )

plot_cmp =
  biomod2::response.plot2(
    models  = mod_com,
    Data = get_formal_data(Diatom_model_1,'expl.var'), 
    show.variables= get_formal_data(Diatom_model_1,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    legend = T,
    display_title = T,
    col = c("blue", "red"),
    data_species = get_formal_data(Diatom_model_1,'resp.var')
  )


# For Best Comparison Plot
plot_cmp %>%
  mutate(
    species = pred.name %>% strsplit('_') %>% sapply(function(x) x[1]),
    pa.dat = pred.name %>% strsplit('_') %>% sapply(function(x) x[2]),
    cv.rep = pred.name %>% strsplit('_') %>% sapply(function(x) x[3]),
    model = pred.name %>% strsplit('_') %>% sapply(function(x) x[4])
  ) %>%
  ggplot(
    aes(
      x = expl.val,
      y = pred.val,
      colour = model,
      group = pred.name
    )
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(~ expl.name, scales = 'free_x') + 
  labs(
    x = '',
    y = 'probability of occurence',
    colour = 'model type'
  ) + 
  scale_fill_brewer(type = 'qual', palette = 4) +
  theme_minimal() +
  theme(
    legend.position = 'bottom'
  )













## run the ensemble models ----
Diatom_ensemble_models <- 
  BIOMOD_EnsembleModeling(
    modeling.output = Diatom_model_1,
    em.by = 'all',
    eval.metric = 'TSS',
    eval.metric.quality.threshold = 0.8,
    models.eval.meth = c('TSS','ROC'),
    prob.mean = FALSE,
    prob.cv = TRUE, 
    committee.averaging = TRUE,
    prob.mean.weight = TRUE,
    VarImport = 0
  )

# asessment the model quality

(Diatom_model_asessment <- get_evaluations(Diatom_ensemble_models))


# Model Projection by study Area

Diatom_models_proj_current <- 
  BIOMOD_Projection(
    modeling.output = Diatom_model_1,
    new.env = mod_data[, c("Temp", "Sal", "Silicate", "Nitrite", "Phosphate")],
    proj.name = "Species_Prediction",
    binary.meth = "TSS",
    do.stack = FALSE,
    xy.new.env = xy,
    clamping.mask = T
  )

plot(Diatom_models_proj_current, str.grep = 'GBM')
plot(Diatom_models_proj_current, str.grep = 'RF')


# Diatom_ensemble_models_proj_current <- 
#   BIOMOD_EnsembleForecasting(
#     EM.output = Diatom_ensemble_models,
#     projection.output = Diatom_models_proj_current,
#     binary.meth = "TSS",
#     do.stack = FALSE,
#     xy.new.env = xy,
#     compress = FALSE,
#     build.clamping.mask = FALSE
#   )













  
