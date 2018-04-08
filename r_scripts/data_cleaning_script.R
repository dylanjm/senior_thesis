library(tidyverse)
library(lubridate)
library(here)

# These indices are the initial indices proposed by NBER
real.mnfg       <- read_csv(here::here("raw_data/CMRMTSPL.csv"))
industrial.prod <- read_csv(here::here("raw_data/INDPRO.csv"))
nonfarm.pay     <- read_csv(here::here("raw_data/PAYEMS.csv"))
real.income     <- read_csv(here::here("raw_data/RPI.csv"))

# For some reason there are "." saved in the data posionning the column type
# remove the "." and convert back to numeric.
treasury.spread <- read_csv(here::here("raw_data/T10Y3M.csv")) %>% 
  filter(T10Y3M != ".") %>% 
  mutate(T10Y3M = parse_number(T10Y3M))

# For some reason there are "." saved in the data posionning the column type
# remove the "." and convert back to numeric.
vix <- read_csv(here::here("raw_data/VIXCLS.csv")) %>% 
  filter(VIXCLS != ".") %>% 
  mutate(VIXCLS = parse_number(VIXCLS))

# Combine all individual data files into one .rds 
# make all headers lower case
# Add reccesionary periods
# Saved to raw_data/initial_raw.rds
list(real.mnfg, industrial.prod, nonfarm.pay, real.income, treasury.spread, vix) %>% 
  reduce(left_join, by = "DATE") %>% 
  rename_all(tolower) %>%
  mutate(recession = case_when(
    between(date, ymd("1969-12-01"), ymd("1970-11-01")) ~ 1,
    between(date, ymd("1973-11-01"), ymd("1975-03-01")) ~ 1,
    between(date, ymd("1980-01-01"), ymd("1980-07-01")) ~ 1,
    between(date, ymd("1981-07-01"), ymd("1982-11-01")) ~ 1,
    between(date, ymd("1990-07-01"), ymd("1991-03-01")) ~ 1,
    between(date, ymd("2001-03-01"), ymd("2001-11-01")) ~ 1,
    between(date, ymd("2007-12-01"), ymd("2009-06-01")) ~ 1,
    TRUE ~ 0
  )) %>% 
  write_rds("~/Desktop/econ_499/research_paper/raw_data/initial_raw.rds")

# Take the log difference of NBER Indices
# No need to do log returns on Treasury Spread & Vix data
# Drop all NA's which cuts out a lot of data
# **THIS IS THE WORKING DATA FOR MODELING**
read_rds("~/Desktop/econ_499/research_paper/raw_data/initial_raw.rds") %>% 
  mutate_at(vars(cmrmtspl:rpi), funs(c(NA,diff(log(.))))) %>% 
  na.omit() %>% 
  write_rds("~/Desktop/econ_499/research_paper/clean_data/working_clean.rds")

# Reduced Model Clean
# Essentially same data as above but without NA's removed
# This gives us data going all the way back to 1967
# We'll contrast this model with our full model
read_rds("~/Desktop/econ_499/research_paper/raw_data/initial_raw.rds") %>% 
  mutate_at(vars(cmrmtspl:rpi), funs(c(NA,diff(log(.))))) %>% 
  write_rds("~/Desktop/econ_499/research_paper/clean_data/reduced_clean.rds")

# Create a long table displaying peaks and troughs of NBER Reccesion periods
# This makes it a lot easier to plot geom_rect
# Instead of using binary value data as above
# y1 & y2 are set as -Inf, Inf so that the geom_rect fills the height 
# of the graphic. This is the reason for that. 
# **REMEMBER** This filters out the first few reccessions to match the
# data that includes VIX. If VIX is removed from model you can remove
# this filter statement. 
tibble(peak = c("1969-12-01","1973-11-01",
                "1980-01-01", "1981-07-01", 
                "1990-07-01", "2001-03-01", 
                "2007-12-01"), 
       trough = c("1970-11-01","1975-03-01",
                  "1980-07-01", "1982-11-01", 
                  "1991-03-01", "2001-11-01", 
                  "2009-06-01"), 
       y1 = c(-Inf,-Inf,-Inf,-Inf,-Inf,-Inf,-Inf),
       y2 = c(Inf,Inf,Inf,Inf,Inf,Inf,Inf)
) %>% 
  mutate_at(vars(peak, trough), parse_date) %>% 
  filter(peak >= ymd("1990-07-01")) %>% 
  write_rds(here::here("clean_data/nber_recession.rds"))

# I decided to just create a table that has the full
# expansion and retraction data.
tibble(peak = c("1969-12-01","1973-11-01",
                "1980-01-01", "1981-07-01", 
                "1990-07-01", "2001-03-01", 
                "2007-12-01"), 
       trough = c("1970-11-01","1975-03-01",
                  "1980-07-01", "1982-11-01", 
                  "1991-03-01", "2001-11-01", 
                  "2009-06-01"), 
       y1 = c(-Inf,-Inf,-Inf,-Inf,-Inf,-Inf,-Inf),
       y2 = c(Inf,Inf,Inf,Inf,Inf,Inf,Inf)
) %>% 
  mutate_at(vars(peak, trough), parse_date) %>% 
  write_rds(here::here("clean_data/all_nber_recession.rds"))
