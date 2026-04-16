library(ggplot2)
library(here)
library(dplyr)
library(tidyr)


# Belgium data
## Epidemiological data (Belgium, 2023) 
DALYs_RTI <- 43293.37
YLLs_RTI  <- 24868.04
YLDs_RTI  <- 18425.33
## Population
population <- 11697557

# Averting burden
clinical_effect <- 0.045  # 4.5% max effect (Tannvik et al.)

## Maximum avertable burden (upper bound)
DALYs_max_averted <- DALYs_RTI * clinical_effect
YLLs_max_averted  <- YLLs_RTI  * clinical_effect
YLDs_max_averted  <- YLDs_RTI  * clinical_effect

# Training system
## Training system parameters
trainees_per_training <- 20

coord_cost_per_session <- 10      # EUR
session_hours          <- 3
course_hours           <- 15
trainer_wage_hour      <- 55      # EUR/hour

trainer_efficacy <- 1.0  # paid trainers = 100%

sessions_per_training <- course_hours / session_hours


coverage <- 0.0357  # 2%, can be vector later

covered_population <- population * coverage
n_trainings <- covered_population / trainees_per_training
n_sessions  <- n_trainings * sessions_per_training
total_hours <- n_sessions * session_hours

## Base costs (before volunteer adjustment)
coordination_costs <- n_sessions * coord_cost_per_session
paid_trainer_costs <- total_hours * trainer_wage_hour

## Volunter/paid trainer mix + relative efficacy
volunteer_share <- seq(0, 1, by = 0.05)        # 0–100% volunteer hours
volunteer_eff   <- seq(0, 1, by = 0.05)        # 0–100% relative efficacy

grid <- expand.grid(
  volunteer_eff  = volunteer_eff,
  volunteer_share = volunteer_share
)

# Equations: training efficacy, DALYs averted, training cost, cost per DALY averted
grid <- grid %>%
  mutate(
    training_efficacy =
      (1 - volunteer_share) * 1 +
      volunteer_share * volunteer_eff,
    
    DALYs_averted =
      training_efficacy * clinical_effect * DALYs_RTI,
    
    training_cost =
      coordination_costs +
      (1 - volunteer_share) * paid_trainer_costs,
    
    
    cost_per_DALY = ifelse(
      DALYs_averted > 0,
      training_cost / DALYs_averted,
      NA_real_
    )
    
  )

# DALYs table
dalys_raw <- grid %>%
  select(volunteer_eff, volunteer_share, DALYs_averted) %>%
  mutate(
    volunteer_eff = volunteer_eff * 100,
    volunteer_share = paste0(volunteer_share * 100, "%")
  ) %>%
  pivot_wider(
    names_from  = volunteer_share,
    values_from = DALYs_averted
  )

# Cost per DALY table
cost_raw <- grid %>%
  select(volunteer_eff, volunteer_share, cost_per_DALY) %>%
  mutate(
    volunteer_eff = volunteer_eff * 100,
    volunteer_share = paste0(volunteer_share * 100, "%")
  ) %>%
  pivot_wider(
    names_from  = volunteer_share,
    values_from = cost_per_DALY
  )

# Save model outputs
saveRDS(
  list(
    data = dalys_raw,
    coverage = coverage
  ),
  file = "data_clean/dalys_raw.rds"
)

saveRDS(
  list(
    data = cost_raw,
    coverage = coverage
  ),
  file = "data_clean/cost_raw.rds"
)




