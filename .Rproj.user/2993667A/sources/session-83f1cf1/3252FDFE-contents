# -------------------------------------------------
# Packages
# -------------------------------------------------
library(eurostat)
library(dplyr)
library(sf)

# -------------------------------------------------
# 1. NUTS-3 → NUTS-1 lookup
# -------------------------------------------------

nuts_lookup <- get_eurostat_geospatial(
  nuts_level = 3,
  year = 2024,
  resolution = "60"
) %>%
  st_drop_geometry() %>%
  transmute(
    nuts3        = geo,                 # ✅ MATCHES Eurostat tables
    nuts1        = substr(geo, 1, 3),
    country_code = CNTR_CODE
  )
    
    

# -------------------------------------------------
# 2. Population at NUTS-3 (2023)
# -------------------------------------------------
pop_nuts3 <- get_eurostat(
  "demo_r_pjangrp3",
  time_format = "raw"
) %>%
  filter(
    TIME_PERIOD == "2023",
    sex == "T",
    age == "TOTAL",
    nchar(geo) == 4   # NUTS-3
  ) %>%
  select(
    nuts3 = geo,
    population = values
  )

# -------------------------------------------------
# 3. Population density at NUTS-3 (2023)
# -------------------------------------------------
dens_nuts3 <- get_eurostat(
  "tgs00024",
  time_format = "raw"
) %>%
  filter(
    TIME_PERIOD == "2022",
    nchar(geo) == 4   # NUTS-3
  ) %>%
  select(
    nuts3 = geo,
    density = values
  )

# -------------------------------------------------
# 4. Aggregate density to NUTS-1 (population-weighted)
# -------------------------------------------------
dens_nuts1 <- pop_nuts3 %>%
  left_join(dens_nuts3, by = "nuts3") %>%
  mutate(
    country_code = substr(nuts3, 1, 2),
    nuts1        = substr(nuts3, 1, 3)
  ) %>%
  group_by(nuts1, country_code) %>%
  summarise(
    density = weighted.mean(density, population, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------------------------
# 5. Population at NUTS-1 (2023)
# -------------------------------------------------
pop_nuts1 <- get_eurostat(
  "demo_r_pjangrp3",
  time_format = "raw"
) %>%
  filter(
    TIME_PERIOD == "2023",
    sex == "T",
    age == "TOTAL",
    nchar(geo) == 3   # NUTS-1
  ) %>%
  transmute(
    nuts1        = geo,
    country_code = substr(geo, 1, 2),
    population   = values
  )

# -------------------------------------------------
# 6. NUTS-1 region names
# -------------------------------------------------
nuts1_names <- get_eurostat_geospatial(
  nuts_level = 1,
  year = 2024,
  resolution = "60"
) %>%
  st_drop_geometry() %>%
  transmute(
    nuts1        = NUTS_ID,
    nuts1_name   = NAME_LATN,
    country_code = CNTR_CODE
  )

# -------------------------------------------------
# 7. Country names
# -------------------------------------------------
country_names <- get_eurostat_geospatial(
  nuts_level = 0,
  year = 2024,
  resolution = "60"
) %>%
  st_drop_geometry() %>%
  transmute(
    country_code = CNTR_CODE,
    country_name = NAME_LATN
  )

# -------------------------------------------------
# 8. Final table (what you asked for)
# -------------------------------------------------
final_nuts1_table <- pop_nuts1 %>%
  left_join(dens_nuts1,  by = c("nuts1", "country_code")) %>%
  left_join(nuts1_names, by = c("nuts1", "country_code")) %>%
  left_join(country_names, by = "country_code") %>%
  select(
    country_name,
    nuts1,
    nuts1_name,
    population,
    density
  ) %>%
  arrange(country_name, nuts1)


final_nuts1_table <- final_nuts1_table %>%
  mutate(
    density = case_when(
      nuts1 == "BE1" ~ 7642,  # Brussels-Capital Region (Statbel 2023)
      nuts1 == "BE2" ~ 497,   # Flemish Region (Statbel 2023)
      nuts1 == "BE3" ~ 218,   # Walloon Region (Statbel 2023)
      TRUE ~ density          # all other regions unchanged
    )
  )


# Export final NUTS-1 table to CSV
write.csv(
  final_nuts1_table,
  file = "data_clean/final_nuts1_population_density_2023.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
