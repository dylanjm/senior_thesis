library(tidyverse)
library(patchwork)
library(here)
library(ggthemes)

# Data we'll need for visualization
raw.dat <- read_rds(here::here("raw_data/initial_raw.rds"))
working.dat <- read_rds(here::here("clean_data/working_clean.rds"))
recession.dat <- read_rds(here::here("clean_data/nber_recession.rds"))
all.recession.dat <- read_rds(here::here("clean_data/all_nber_recession.rds"))

# Model Output Data
hmm.full.probs <- read_csv(here::here("clean_data/hmm_model_prob.csv"))
hmm.reduced.probs <- read_csv(here::here("clean_data/hmm_reduced_model_prob.csv"))
nn.training.dat <- read_csv(here::here("clean_data/nn_training.csv")) 

# Rename our facet labels w/out changing data
facet.labels <- c("cmrmtspl" = "Real Manufacturing & Trade Sales",
                  "indpro" = "Industrial Production",
                  "payems" = "Nonfarm Payroll",
                  "rpi" = "Real Personal Income",
                  "t10y3m" = "10 Year - 3 Month Treasury Spread",
                  "vixcls" = "VIX")

# Facet Plot of the first 4 indicators 1990-2018
log.index <- working.dat %>% 
  gather(key = "index", "log_returns", cmrmtspl:rpi) %>% 
  ggplot(aes(x = date, y = log_returns)) + 
  geom_line(size = .4) + 
  geom_rect(data = recession.dat,
            aes(xmin = peak,
                xmax = trough,
                ymin = y1,
                ymax = y2),
            color = "grey",
            alpha = .2,
            inherit.aes = FALSE) +
  facet_wrap(~ index, 
             scales = "free_y", 
             ncol = 1,
             labeller = as_labeller(facet.labels)) + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Performance of Business Cycle Indicators",
       x = "Date", y = "Log Returns", 
       subtitle = "Indices established by NBER Annual Journal (1989) - Gray Bars Establish Reccesionary Periods") 

# Facet Plot of Treasury Spread and Vix 1990-2018
norm.index <- working.dat %>% 
  gather("index", "value", t10y3m, vixcls) %>% 
  ggplot(aes(x = date, y = value)) + 
  geom_line(size = .4) +
  geom_rect(data = recession.dat,
            aes(xmin = peak,
                xmax = trough,
                ymin = y1,
                ymax = y2),
            color = "grey",
            alpha = .2,
            inherit.aes = FALSE) +
  facet_wrap(~ index, 
             scales = "free_y", 
             ncol = 1,
             labeller = as_labeller(facet.labels)) + 
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(x = "Date", y = "Index Value", 
       caption = expression(paste(italic("Data Source: "), 
                                  "Federal Reserve of St. Louis Economic Data 2018")))

# Combined Plot for both plots - USE IN PAPER
log.index + norm.index & 
  theme_classic() & 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Total Recession Plot for USA (1967-2018)
raw.dat %>% 
  ggplot(aes(x = date, y = recession)) + 
  geom_line() + 
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "United States Recessionary Periods (1967-2018)",
       x = "Year", y = "Recession", 
       subtitle = "NBER US Business Cycle Expansions and Contractions",
       caption = expression(paste(italic("Data Source: "),
                                  "http://www.nber.org/cycles.html"))) + 
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Histogram & Density Plots of Indicators
working.dat %>% 
  gather("index", "value", cmrmtspl:vixcls) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(aes(y=..density..),color = "white", fill = "#00203b", bins = 20) + 
  geom_density(aes(y=..density..), color = "firebrick2", size = .5) +
  facet_wrap(~ index, scales = "free", labeller = as_labeller(facet.labels)) +
  labs(title = "Density & Histogram Plot of Business Cycle Indicators",
       subtitle = "Data spanning 1990-2018",
       x = "Value", y = "Density") + 
  theme_classic()

# Probability of Recession Full Model 1990-2018
full.model.plot <- hmm.full.probs %>% 
  ggplot(aes(x = date, y = S1)) + 
  geom_line() + 
  geom_rect(data = recession.dat,
            aes(xmin = peak,
                xmax = trough,
                ymin = y1,
                ymax = y2),
            color = "grey",
            alpha = .2,
            inherit.aes = FALSE) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Probability of Regime 1 (Recessionary Period)",
       subtitle = "Full model including cmrmtspl, indpro, payems, rpi, t10y3m, vixcls",
       x = "Year", y = "Probability",
       caption = "Output created by Dynamic Factor Markov Switching Model") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Probability of Recession Reduced Model 1967-2018
hmm.reduced.probs %>% 
  ggplot(aes(x = date, y = S2)) + 
  geom_line() + 
  geom_rect(data = all.recession.dat,
            aes(xmin = peak,
                xmax = trough,
                ymin = y1,
                ymax = y2),
            color = "grey",
            alpha = .2,
            inherit.aes = FALSE) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Probability of Regime 1 (Recessionary Period)",
       subtitle = "Reduced model including cmrmtspl, indpro, payems, rpi",
       x = "Year", y = "Probability",
       caption = "Output created by Dynamic Factor Markov Switching Model") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Neural Network Training Plots
nn.training.dat %>% 
  ggplot(aes(x = epoch, y = value, group = data, fill = data)) +
  geom_point(pch = 21) + 
  geom_smooth(se = FALSE, aes(color = data)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual("Data", values = c("training" = "navyblue", "validation" = "firebrick2")) +
  scale_fill_manual("Data", values = c("training" = "navyblue", "validation" = "firebrick2")) +
  facet_wrap(~ metric, ncol = 1, scales = "free_y",
             labeller = as_labeller(c("acc" = "Accuracy", "loss" = "Loss"))) + 
  labs(title = "Neural Network Training & Validation Performance",
       x = "Epoch", y = "Value") +
  theme_classic()

raw.dat %>% 
  ggplot(aes(x = date, y = 1, fill = recession)) + 
  geom_tile(color = "#465573") + 
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_fill_gradient(low = "#465573", high = "yellow") + 
  guides(fill = FALSE) + 
  labs(title = "United State Recessions since 1967") + 
  theme_fivethirtyeight() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_blank())
