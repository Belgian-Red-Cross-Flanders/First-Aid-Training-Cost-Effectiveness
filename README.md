# Cost-effectiveness of Volunteer-led First Aid Training in Belgium

This repository contains the data and code used to estimate the cost-effectiveness of first aid training systems in Belgium, 
focusing on road traffic injuries (RTIs). The analysis combines epidemiological data, assumptions on clinical and behavioural effectiveness, 
and different training system configurations (paid and volunteer trainers).

## Project structure

The repository is organized as follows:

```         
├── scripts/        # R scripts for modelling and data processing
├── data_raw/       # Raw input data (external sources)
├── data_clean/     # Processed data and model outputs
├── outputs/        # Tables and figures used in the paper
```

## Requirements
The project is written in R.
Required packages:
ggplot2, here, dplyr, tidyr, eurostat, sf

## Model

The model estimates:

- DALYs averted by first aid training
- Total training costs
- Cost per DALY averted

across combinations of:

- training coverage
- volunteer trainer share
- volunteer trainer effectiveness

To reproduce results:


- Run data preprocessing:eu_nuts_density_pop.R
- Run main model script (coverage-specific):model_main.R
- Generate tables:table_generation.R

All data sources are public (GBD, Eurostat, Statbel)


### Population density data

Script:
```
scripts/eu_nuts_density_pop.R
```

This script:

- Retrieves Eurostat population and density data
- Aggregates NUTS-3 data to NUTS-1 regions
- Produces a population-weighted density table
- Overrides Belgian densities using official statistics (STATBEL)

Output:
```
data_clean/final_nuts1_population_density_2023.csv
```

## Outputs

The model generates:
### 1. DALYs averted table

- Rows: volunteer effectiveness
- Columns: volunteer share

Saved as:
```
data_clean/dalys_raw{coverage}.rds
```

### 2. Cost per DALY table

Same structure as DALYs table. 

Saved as:
```
data_clean/cost_raw{coverage}.rds
```

### 3. Total training cost table

- By volunteer share
- Independent of effectiveness



