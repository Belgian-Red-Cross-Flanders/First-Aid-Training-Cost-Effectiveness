library(readxl)
library(dplyr)
library(tidyr)
library(gt)
library(scales)
library(openxlsx)

#--------------------------------------------------
# 1. File path
#--------------------------------------------------
file_path <- "C:/Users/MCASTRO/Rode Kruis-Vlaanderen/AH-Third Pillar Research Center - General/Paper 20 - First Aid/draft3_calculations.xlsx"
out_dir <- dirname(file_path)

wb   <- loadWorkbook(file_path)
tbls <- getTables(wb, sheet = "Data 1%")

#--------------------------------------------------
# 2. Read tables
#--------------------------------------------------
dalys_range <- names(tbls)[tbls == "DALYs_averted"]
cost_range  <- names(tbls)[tbls == "Cost_per_DALY"]

dalys_raw  <- read_excel(file_path, range = dalys_range)
cost_raw_1 <- read_excel(file_path, sheet = "Data 1%", range = cost_range)
cost_raw_3 <- read_excel(file_path, sheet = "Data 3%", range = cost_range)

#--------------------------------------------------
# 3. Reshape to long format
#--------------------------------------------------
dalys_long <- dalys_raw %>%
  pivot_longer(-1, names_to = "volunteer_share", values_to = "dalys") %>%
  rename(volunteer_eff = 1) %>%
  mutate(
    volunteer_eff  = volunteer_eff * 100,
    volunteer_share = as.numeric(gsub("%", "", volunteer_share))
  )

cost_long_1 <- cost_raw_1 %>%
  pivot_longer(-1, names_to = "volunteer_share", values_to = "cost_per_daly") %>%
  rename(volunteer_eff = 1) %>%
  mutate(
    volunteer_eff  = volunteer_eff * 100,
    volunteer_share = as.numeric(gsub("%", "", volunteer_share))
  )

cost_long_3 <- cost_raw_3 %>%
  pivot_longer(-1, names_to = "volunteer_share", values_to = "cost_3") %>%
  rename(volunteer_eff = 1) %>%
  mutate(
    volunteer_eff  = volunteer_eff * 100,
    volunteer_share = as.numeric(gsub("%", "", volunteer_share))
  )

#--------------------------------------------------
# 4. Build COST RANGE lookup (1% – 3%)
#--------------------------------------------------
cost_range_lookup <- cost_long_1 %>%
  left_join(cost_long_3,
            by = c("volunteer_eff", "volunteer_share")) %>%
  filter(
    volunteer_eff %% 20 == 0,
    volunteer_share %% 20 == 0
  ) %>%
  mutate(
    volunteer_eff   = paste0(volunteer_eff, "%"),
    volunteer_share = paste0(volunteer_share, "%"),
    cost_range = ifelse(
      volunteer_eff == "0%" & volunteer_share == "100%",
      NA_character_,
      paste0(
        "€", comma(round(cost_per_daly, 0)),
        "–€", comma(round(cost_3, 0))
      )
    )
  ) %>%
  select(volunteer_eff, volunteer_share, cost_range)

#--------------------------------------------------
# 5. Merge DALYs with NUMERIC cost (1% only)
#    (this is the original, working structure)
#--------------------------------------------------
combined <- dalys_long %>%
  left_join(
    cost_long_1,
    by = c("volunteer_eff", "volunteer_share")
  )

#--------------------------------------------------
# 6. Subset to 20% intervals
#--------------------------------------------------
combined_20 <- combined %>%
  filter(
    volunteer_eff %% 20 == 0,
    volunteer_share %% 20 == 0
  )

#--------------------------------------------------
# 7. Build long table EXACTLY like 1‑coverage code
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
    volunteer_eff   = paste0(volunteer_eff, "%"),
    volunteer_share = paste0(volunteer_share, "%")
  )

#--------------------------------------------------
# 8. Pivot wide
#--------------------------------------------------
table_wide <- table_long %>%
  pivot_wider(
    names_from  = volunteer_share,
    values_from = value
  )


#--------------------------------------------------
# 8.5 Convert ALL display columns to character
#     (this is REQUIRED so cost ranges can be inserted)
#--------------------------------------------------
for (vs in setdiff(names(table_wide), c("metric", "volunteer_eff"))) {
  table_wide[[vs]] <- as.character(table_wide[[vs]])
}


#--------------------------------------------------
# 9. >>> REPLACE cost values WITH RANGES (KEY STEP)
#--------------------------------------------------

for (vs in unique(cost_range_lookup$volunteer_share)) {
  
  repl <- cost_range_lookup %>%
    filter(volunteer_share == vs) %>%
    arrange(volunteer_eff)
  
  table_wide[table_wide$metric == "Cost per DALY", vs] <- repl$cost_range
}


#--------------------------------------------------
# 10. DALYs‑only helper table for colouring
#--------------------------------------------------
dalys_only <- table_wide %>%
  filter(metric == "DALYs averted") %>%
  mutate(across(-c(metric, volunteer_eff), as.numeric))

#--------------------------------------------------
# 11. Build gt table
#--------------------------------------------------

format_dalys <- function(x) {
  ifelse(
    is.na(x),
    NA_character_,
    formatC(
      as.numeric(x),
      format = "f",
      digits = 1,
      big.mark = ",",
      decimal.mark = "."
    )
  )
}



for (vs in vol_cols) {
  idx <- table_wide$metric == "DALYs averted"
  table_wide[[vs]][idx] <- format_dalys(table_wide[[vs]][idx])
}

vol_cols <- setdiff(names(table_wide), c("metric", "volunteer_eff"))

gt_table <- table_wide %>%
  gt(
    rowname_col = "metric",
    groupname_col = "volunteer_eff",
    row_group_as_column = TRUE
  ) %>%
  tab_header(title = "DALYs averted and cost per DALY") %>%
  tab_stubhead(label = "Relative volunteer efficacy") %>%
  tab_spanner(
    label = "Volunteer trainer share",
    columns = all_of(vol_cols)
  ) %>%
  fmt_number(
    rows = metric == "DALYs averted",
    decimals = 1
  )  %>%
  tab_source_note(
    "Training 1–3% of the population of Belgium, assuming a maximum efficacy of 4.5% in first aid interventions at these training coverage values."
  )


dalys_palette <- scales::col_numeric(
  palette = c("#d73027", "#fee08b", "#1a9850"),
  domain = range(
    unlist(dalys_only %>% select(all_of(vol_cols))),
    na.rm = TRUE
  )
)

for (col in vol_cols) {
  
  vals <- dalys_only[[col]]
  
  for (i in seq_along(vals)) {
    
    gt_table <- gt_table %>%
      tab_style(
        style = cell_fill(color = dalys_palette(vals[i])),
        locations = cells_body(
          rows = (metric == "DALYs averted") &
            (volunteer_eff == dalys_only$volunteer_eff[i]),
          columns = all_of(col)
        )
      )
  }
}


#--------------------------------------------------
# 12. Export
#--------------------------------------------------
gtsave(
  gt_table,
  filename = "compound_DALYs_costs_range_1_3.png",
  path = out_dir,
  zoom = 2
)