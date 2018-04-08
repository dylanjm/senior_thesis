library(tidyverse)
library(depmixS4)

full.model.dat <- read_rds(here::here("clean_data/working_clean.rds"))
reduced.model.dat <- read_rds(here::here("clean_data/reduced_clean.rds"))

# Set seed for reproducibility
set.seed(123)

##########################################
# Full Model
# Includes all variables
# - cmrmtspl
# - indpro
# - payems
# - rpi
# - t10y3m
# - vixcls
##########################################
# Regime switching model
full.mod <- depmix(list(cmrmtspl~1, indpro~1, payems~1, rpi ~ 1, t10y3m ~ 1, vixcls ~ 1), 
              family = list(gaussian(),
                            gaussian(),
                            gaussian(),
                            gaussian(),
                            gaussian(),
                            gaussian()), 
              nstates = 2, data = full.model.dat)

# Fit our Hidden Markov Model
fm.full <- depmixS4::fit(full.mod)

# Add date column to model output
posterior.prb <- depmixS4::posterior(fm.full) %>% 
  mutate(date = full.model.dat$date) 

# Write our output to .csv to save for later
write_csv(posterior.prb, here::here("clean_data/hmm_full_model_prob.csv"))
write_rds(fm.full, here::here("model_objects/full_model.rds"))

##########################################
# Reduced Model
# Includes 4 variables
# - cmrmtspl
# - indpro
# - payems
# - rpi
##########################################
# Regime switching model
reduced.mod <- depmix(list(cmrmtspl ~ 1, indpro ~ 1, payems ~ 1, rpi ~ 1), 
              family = list(gaussian(),
                            gaussian(),
                            gaussian(),
                            gaussian()), 
              nstates = 2, data = reduced.model.dat)

# Fit our Hidden Markov Model
fm.reduc <- depmixS4::fit(reduced.mod)

# Add date column to model output
posterior.prb <- depmixS4::posterior(fm.reduc) %>% 
  mutate(date = reduced.model.dat$date) 

# Write our output to .csv to save for later
write_csv(posterior.prb, here("clean_data/hmm_reduced_model_prob.csv"))
write_rds(fm.reduc, here::here("model_objects/reduced_model.rds"))

#################################
# MultiLayer Neural Network
# Tutorial followed at
# https://tensorflow.rstudio.com/keras/articles/about_keras_models.html
# Keras Cheetsheet inside resources/keras.pdf
#################################
library(keras)
# Data needs to be a matrix
x.data <- reduced.model.dat %>% 
  dplyr::select(cmrmtspl:rpi) %>% 
  na.omit() %>% 
  as.matrix()

y.data <- reduced.model.dat %>% 
  dplyr::select(recession) %>% 
  na.omit() %>% 
  as.matrix()

# Split into train and test ~ 60/40 Split
x.train <- x.data[1:368,]
x.test <- x.data[369:612,]
mean <- apply(x.data, 2, mean)
std <- apply(x.data, 2, sd)
x.train <- scale(x.train, center = mean, scale = std)

# Convert Economy States to Categorical
y.train <- y.data[1:368,]
y.test <- y.data[369:612,]
y.train <- to_categorical(y.train, 2)
y.test <- to_categorical(y.test, 2) 

# defining the model and layers
model <- keras_model_sequential()

model %>%
  layer_dense(units = 368, input_shape = c(4)) %>%
  layer_activation(activation = "relu") %>% 
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 68) %>% 
  layer_activation(activation = "relu") %>% 
  layer_dense(units = 2) %>% 
  layer_activation(activation = "sigmoid")

summary(model)

# compile (define loss and optimizer)
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

# train (fit)
history <- model %>% fit(
  x.train, y.train,
  epochs = 30, batch_size = 30,
  validation_split = 0.5
)

model %>% evaluate(x.test, y.test)

summary(model)

as.data.frame(history) %>% 
  write_csv(here::here("clean_data/nn_training.csv"))

