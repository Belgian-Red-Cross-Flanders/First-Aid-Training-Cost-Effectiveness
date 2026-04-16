library(dplyr)
library(tidyr)
library(gt)
library(scales)


# Assume:
# - First column = Relative efficacy of volunteers (%)
# - Remaining columns = Volunteer/paid trainer hours (%)


dalys_obj <- readRDS(here("data_clean", "dalys_raw.rds"))
cost_obj  <- readRDS(here("data_clean", "cost_raw.rds"))

dalys_raw <- dalys_obj$data
cost_raw  <- cost_obj$data

coverage  <- dalys_obj$coverage
coverage_txt <- percent(coverage, accuracy = 0.01)


out_dir <- here("outputs", "figures")

coverage_txt <- paste0(coverage*100,"%")

#--------------------------------------------------
# 3. Reshape to long format
#--------------------------------------------------
dalys_long <- dalys_raw %>%
  pivot_longer(
    cols = -1,
    names_to = "volunteer_share",
    values_to = "dalys"
  ) %>%
  rename(volunteer_eff = 1) %>%
  mutate(
    volunteer_eff  = volunteer_eff,
    volunteer_share = as.numeric(gsub("%", "", volunteer_share))
  )

cost_long <- cost_raw %>%
  pivot_longer(
    cols = -1,
    names_to = "volunteer_share",
    values_to = "cost_per_daly"
  ) %>%
  rename(volunteer_eff = 1) %>%
  mutate(
    volunteer_eff  = volunteer_eff,
    volunteer_share = as.numeric(gsub("%", "", volunteer_share))
  )

#--------------------------------------------------
# 4. Merge DALYs and cost
#--------------------------------------------------
combined <- left_join(
  dalys_long,
  cost_long,
  by = c("volunteer_eff", "volunteer_share")
)

#--------------------------------------------------
# 5. Subset to 20% intervals
#--------------------------------------------------

combined_20 <- combined %>%
  mutate(
    volunteer_eff   = round(volunteer_eff),
    volunteer_share = round(volunteer_share)
  ) %>%
  filter(
    volunteer_eff %% 20 == 0,
    volunteer_share %% 20 == 0
  )


#--------------------------------------------------
# 6. Build long table with two rows per cell
#--------------------------------------------------
table_long <- combined_20 %>%
  pivot_longer(
    cols = c(dalys, cost_per_daly),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      dalys = "DALYs averted",
      cost_per_daly = "Cost per DALY"
    ),
    volunteer_eff  = paste0(volunteer_eff, "%"),
    volunteer_share = paste0(volunteer_share, "%")
  )

#--------------------------------------------------
# 7. Pivot back to wide for gt
#--------------------------------------------------
table_wide <- table_long %>%
  pivot_wider(
    names_from = volunteer_share,
    values_from = value
  )

#--------------------------------------------------
# 8. Create DALYs-only helper table (for coloring)
#--------------------------------------------------
dalys_only <- table_wide %>%
  filter(metric == "DALYs averted")

#--------------------------------------------------
# 9. Build gt table
#--------------------------------------------------
vol_cols <- setdiff(
  names(table_wide),
  c("metric", "volunteer_eff")
)

gt_table <- table_wide %>%
  gt(
    rowname_col = "metric",
    groupname_col = "volunteer_eff",
    row_group_as_column = TRUE
  ) %>%
  
  tab_header(
    title = "DALYs averted and cost per DALY"
  ) %>%
  
  tab_stubhead(label = "Relative volunteer efficacy") %>%
  
  cols_label(
    volunteer_eff = "Relative volunteer efficacy"
  ) %>%
  
  tab_spanner(
    label = "Volunteer trainer share",
    columns = all_of(vol_cols)
  ) %>%
  
  fmt_number(
    rows = metric == "DALYs averted",
    decimals = 1
  ) %>%
  
  fmt_currency(
    rows = metric == "Cost per DALY",
    currency = "EUR",
    decimals = 0
  ) %>%
  
  data_color(
    columns = all_of(vol_cols),
    rows = metric == "DALYs averted",
    colors = scales::col_numeric(
      palette = c("#d73027", "#fee08b", "#1a9850"),
      domain = range(
        unlist(dalys_only %>% select(all_of(vol_cols))),
        na.rm = TRUE
      )
    )
  ) %>%
  
  tab_source_note(
    
    paste0(
      "Training ", coverage_txt,
      " of the population of Belgium, assuming a maximum efficacy of 4.5% in first aid interventions."
    )
    
  )


#--------------------------------------------------
# 10. Export
#--------------------------------------------------

gtsave(
  gt_table,
  filename = paste0("compound_DALYs_costs_",coverage,".png"),
  path = out_dir,
  zoom = 2   # improves resolution for publication
)

