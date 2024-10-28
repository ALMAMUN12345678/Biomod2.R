rm(list=ls())
getwd()
setwd('E:/CTD/Raster/new')
st = raster::stack(
  c(Temperature = 'E:/CTD/Raster/new/Temperature.nc',
    Salinity = 'E:/CTD/Raster/new/Salinity.nc',
    Silicate = 'E:/CTD/Raster/new/Silicate.nc',
    Nitrate = 'E:/CTD/Raster/new/Nitrate.nc',
    Phosphate = 'E:/CTD/Raster/new/Phosphate.nc'
    
  )
)

st1 = raster::stack(
  c(Temperature = 'E:/CTD/Raster/temp.nc',
    Salinity = 'E:/CTD/Raster/sal.nc',
    Silicate = 'E:/CTD/Raster/silicate.nc',
    Nitrate = 'E:/CTD/Raster/nitrate.nc',
    Phosphate = 'E:/CTD/Raster/phosphate.nc'
    
  )
)

plot(st)

# Presence Absence Data
mod_data = read.csv('ab_sp.csv')
PA = ifelse(mod_data$Coscinodiscus.traducens>6,1,0)
mod_data = cbind(mod_data,PA)


# Data Format with some random variables

PA_format = 
  BIOMOD_FormatingData(
    resp.var = mod_data$PA,
    resp.xy = mod_data[, c('Longitude', 'Latitude')],
    expl.var = st,
    resp.name = "Presence",
    PA.nb.rep = 2,
    PA.nb.absences = 500,
    PA.strategy = 'random'
  )

plot(PA_format)

# Modeling Options

PA_opt =BIOMOD_ModelingOptions(
    GLM = list(type = 'quadratic', interaction.level = 1),
    GBM = list(n.trees = 1000),
    GAM = list(algo = 'GAM_mgcv'))

# Specially for maxent
MAXENT.Phillips = list(path_to_maxent.jar = "E:/CTD/Raster/new/maxent.jar")
myBiomodOption <- BIOMOD_ModelingOptions(MAXENT.Phillips = list(path_to_maxent.jar = file.path(getwd(), "maxent.jar"),
                 maximumiterations = 200,
                 visible = FALSE,
                 linear = TRUE,
                 quadratic = TRUE,
                 product = TRUE,
                 threshold = TRUE,
                 hinge = TRUE,
                 lq2lqptthreshold = 80,
                 l2lqthreshold = 10,
                 hingethreshold = 15,
                 beta_threshold = -1,
                 beta_categorical = -1,
                 beta_lqp = -1,
                 beta_hinge = -1,
                 defaultprevalence = 0.5))

# Defining Model
PA_models =BIOMOD_Modeling(
    data = PA_format,
    models = c("GLM", "GBM", "RF", "GAM",'ANN','FDA','CTA','SRE','MAXENT.Phillips.2'),
    models.options = PA_opt,
    CV.strategy = 'kfold',
    CV.nb.rep = 2,
    CV.perc = 0.8,
    OPT.data.type = 'binary.PA',
    OPT.strategy = 'bigboss',
    metric.eval = c('TSS','ROC'),
    DataSplit = 80,
    VarImport = 3,
    modeling.id = "demo1"
  )


## get models evaluation scores
PA_models_scores = get_evaluations(PA_models)

PA_models_scores["TSS","Testing.data","RF",,]

# Model Dimensions
dim(PA_models_scores)
dimnames(PA_models_scores)

# TSS and ROC value 
(PA_df = t(data.frame(RUN_1 = PA_models_scores[ , 1, , 1, ],
                      RUN_2 = PA_models_scores[ , 1, , 2, ])))

## plot models evaluation scores
models_scores_graph(
  PA_models, 
  by = "models", 
  metrics = c("ROC","TSS"), 
  xlim = c(0.5,1), 
  ylim = c(0.5,1)
)


models_scores_graph(
  PA_models, 
  by = "cv_run" , 
  metrics = c("ROC","TSS"), 
  xlim = c(0.1,1), 
  ylim = c(0.1,1)
)

models_scores_graph(
  PA_models, 
  by = "data_set", 
  metrics = c("ROC","TSS"), 
  xlim = c(0.1,1), 
  ylim = c(0.1,1)
)

## check variable importance
(PA_models_var_import <- get_variables_importance(PA_models, as.data.frame = TRUE))

apply(PA_models_var_import, c(1,2), mean)

## individual models response plots
PA_glm = BIOMOD_LoadModels(PA_models, models='GLM')
PA_gbm = BIOMOD_LoadModels(PA_models, models='GBM')
PA_rf = BIOMOD_LoadModels(PA_models, models='RF')
PA_gam = BIOMOD_LoadModels(PA_models, models='GAM')
PA_ann = BIOMOD_LoadModels(PA_models, models='ANN')
PA_max = BIOMOD_LoadModels(PA_models, models='MAXENT.Phillips.2')
PA_fda = BIOMOD_LoadModels(PA_models, models='FDA')
PA_cta = BIOMOD_LoadModels(PA_models, models='CTA')
PA_sre = BIOMOD_LoadModels(PA_models, models='SRE')

# Individual Model Plot
glm_eval_strip = 
  biomod2::response.plot2(
    models  = PA_glm,
    Data = get_formal_data(PA_models,'expl.var'), 
    show.variables= get_formal_data(PA_models,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    legend = FALSE,
    display_title = FALSE,
    data_species = get_formal_data(PA_models,'resp.var'),
    col = c("blue", "red")
  )

gam_eval_strip =
  biomod2::response.plot2(
    models  = PA_gam,
    Data = get_formal_data(PA_models,'expl.var'), 
    show.variables= get_formal_data(PA_models,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    legend = FALSE,
    display_title = FALSE,
    data_species = get_formal_data(PA_models,'resp.var'),
    col = c("blue", "red")
  )

ann_eval_strip = 
  biomod2::response.plot2(
    models  = PA_ann,
    Data = get_formal_data(PA_models,'expl.var'), 
    show.variables= get_formal_data(PA_models,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    legend = FALSE,
    display_title = FALSE,
    data_species = get_formal_data(PA_models,'resp.var'),
    col = c("blue", "red")
  )


max_eval_strip =
  biomod2::response.plot2(
    models  = PA_max,
    Data = get_formal_data(PA_models,'expl.var'), 
    show.variables= get_formal_data(PA_models,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    legend = FALSE,
    display_title = FALSE,
    data_species = get_formal_data(PA_models,'resp.var'),
    col = c("blue", "red")
  )


fda_eval_strip =
  biomod2::response.plot2(
    models  = PA_fda,
    Data = get_formal_data(PA_models,'expl.var'), 
    show.variables= get_formal_data(PA_models,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    legend = FALSE,
    display_title = FALSE,
    data_species = get_formal_data(PA_models,'resp.var'),
    col = c("blue", "red")
  )

rf_eval_strip =
  biomod2::response.plot2(
    models  = PA_rf,
    Data = get_formal_data(PA_models,'expl.var'), 
    show.variables= get_formal_data(PA_models,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    legend = FALSE,
    display_title = FALSE,
    data_species = get_formal_data(PA_models,'resp.var'),
    col = c("blue", "red")
  )

cta_eval_strip =
  biomod2::response.plot2(
    models  = PA_cta,
    Data = get_formal_data(PA_models,'expl.var'), 
    show.variables= get_formal_data(PA_models,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    legend = FALSE,
    display_title = FALSE,
    data_species = get_formal_data(PA_models,'resp.var'),
    col = c("blue", "red")
  )


sre_eval_strip =
  biomod2::response.plot2(
    models  = PA_sre,
    Data = get_formal_data(PA_models,'expl.var'), 
    show.variables= get_formal_data(PA_models,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    legend = FALSE,
    display_title = FALSE,
    data_species = get_formal_data(PA_models,'resp.var'),
    col = c("blue", "red")
  )





# For Model Plot and comparisons

mod_com =
  BIOMOD_LoadModels(
    PA_models, 
    models = c('GBM','RF')
  )

plot_cmp =
  biomod2::response.plot2(
    models  = mod_com,
    Data = get_formal_data(PA_models,'expl.var'), 
    show.variables= get_formal_data(PA_models,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    legend = T,
    display_title = T,
    col = c("blue", "red"),
    data_species = get_formal_data(PA_models,'resp.var')
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

#  For Model Projections

PA_ensemble_models <- 
  BIOMOD_EnsembleModeling(
    modeling.output = PA_models,
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

# Model Evaluations
(PA_model_asessment <- get_evaluations(PA_ensemble_models))


PABiomomodProj <- BIOMOD_Projection(modeling.output = PA_models,
                                    new.env = st,
                                    proj.name = 'Species_Prediction',
                                    # xy.new.env = mod_data[, c('Longitude', 'Latitude')],
                                    selected.models = 'all', # all
                                    binary.meth = 'ROC',
                                    filtered.meth = 'TSS',
                                    compress = 'xz',
                                    clamping.mask = T,
                                    do.stack=T)


# Make a Projections plot
plot(PABiomomodProj, str.grep = 'CTA')
plot(PABiomomodProj, str.grep = 'MAXENT.Phillips.2')





