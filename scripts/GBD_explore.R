# ============================================================
#   Global Burden of Disease – Cause-specific DALY plots
# ============================================================

library(tidyverse)
library(ggplot2)
library(readr)
library(scales)

# ------------------------------------------------------------
# Load data
# ------------------------------------------------------------
data_path  <- "C:/Users/MCASTRO/Documents/GitHub/First Aid/data_raw/individual_injuries_IHME-GBD_2023_DATA-2bfb54b4-1.csv"
output_dir <- "C:/Users/MCASTRO/Documents/GitHub/First Aid/outputs/figures"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

gbd <- read_csv(data_path, show_col_types = FALSE)

# ------------------------------------------------------------
# Filter: only DALYs, only selected causes, only 2023
# ------------------------------------------------------------
selected_causes <- c(
  "Falls","Self-harm","Transport injuries",
  "Environmental heat and cold exposure",
  "Other exposure to mechanical forces",
  "Pulmonary aspiration and foreign body in airway",
  "Fire, heat, and hot substances",
  "Physical violence by other means",
  "Drowning","Physical violence by sharp object",
  "Foreign body in eyes","Poisoning by other means",
  "Physical violence by firearm","Poisoning by carbon monoxide",
  "Foreign body in other body part",
  "Non-venomous animal contact","Unintentional firearm injuries",
  "Electrocution","Venomous animal contact",
  "Conflict and terrorism","Police conflict and executions"
)

gbd_clean <- gbd %>%
  filter(year == 2023,
         cause_name %in% selected_causes,
         measure_name == "DALYs (Disability-Adjusted Life Years)")   # <-- MAGIC FIX

# ------------------------------------------------------------
# Clean transformed dataframes for Number and Percent
# ------------------------------------------------------------
gbd_number <- gbd_clean %>%
  filter(metric_name == "Number") %>%
  mutate(
    val_plot   = val   / 1000,
    lower_plot = lower / 1000,
    upper_plot = upper / 1000,
    axis_label = "DALYs (thousands)",
    metric_clean = "number"
  )

gbd_percent <- gbd_clean %>%
  filter(metric_name == "Percent") %>%
  mutate(
    val_plot   = val   * 100,
    lower_plot = lower * 100,
    upper_plot = upper * 100,
    axis_label = "Percent of DALYs (%)",
    metric_clean = "percent"
  )

# ------------------------------------------------------------
# GBD plotting function
# ------------------------------------------------------------
plot_gbd <- function(df_metric, location) {
  
  df_loc <- df_metric %>%
    filter(location_name == location) %>%
    arrange(desc(val_plot)) %>%
    mutate(cause_name = factor(cause_name, levels = rev(unique(cause_name))))
  
  # Gridlines based on transformed values
  axis_range <- range(df_loc$lower_plot, df_loc$upper_plot)
  y_breaks <- pretty(axis_range, n = 12)
  
  p <- ggplot(df_loc, aes(x = cause_name, y = val_plot)) +
    geom_col(fill = "#2E86AB") +
    geom_errorbar(
      aes(ymin = lower_plot, ymax = upper_plot),
      width = 0.3,
      linewidth = 0.9
    ) +
    coord_flip() +
    scale_y_continuous(
      breaks = y_breaks,
      labels = comma_format()
    ) +
    labs(
      title = paste0("DALYs by Cause – ", location, " (", unique(df_loc$metric_clean), ")"),
      subtitle = "95% uncertainty interval",
      x = "Cause of Injury",
      y = unique(df_loc$axis_label)
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey80", linewidth = 0.3),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11)
    )
  
  filename <- paste0("DALYs_", unique(df_loc$metric_clean), "_", gsub(" ", "_", location), ".png")
  ggsave(file.path(output_dir, filename), p, width = 10, height = 7, dpi = 300)
}

# ------------------------------------------------------------
# Run for all locations
# ------------------------------------------------------------
# for (loc in unique(gbd_clean$location_name)) {
#   plot_gbd(gbd_number,  loc)
#   plot_gbd(gbd_percent, loc)
# }

# ------------------------------------------------------------
# Belgium vs Global comparison (Percent of DALYs)
# ------------------------------------------------------------

plot_belgium_global_percent <- function(df_percent) {
  
  df_bg <- df_percent %>%
    filter(location_name %in% c("Belgium", "Global")) %>%
    mutate(
      cause_name = factor(cause_name),
      location_name = factor(location_name, levels = c("Global", "Belgium"))
    )
  
  # Order causes by Global percentage
  order_levels <- df_bg %>%
    filter(location_name == "Global") %>%
    arrange(val_plot) %>%
    pull(cause_name)
  
  df_bg <- df_bg %>%
    mutate(cause_name = factor(cause_name, levels = order_levels))
  
  p <- ggplot(df_bg, aes(x = cause_name, y = val_plot, fill = location_name)) +
    geom_col(position = position_dodge(width = 0.8)) +
    coord_flip() +
    scale_y_continuous(
      breaks = pretty(df_bg$val_plot, n = 10),
      labels = comma_format()
    ) +
    labs(
      title = "Percentage of DALYs by Injury Cause",
      subtitle = "Belgium vs Global (2023)",
      x = "Cause of Injury",
      y = "Percent of DALYs (%)",
      fill = "Location"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
  
  ggsave(
    file.path(output_dir, "DALYs_percent_Belgium_vs_Global.png"),
    p, width = 10, height = 7, dpi = 300
  )
}

# Run it
# plot_belgium_global_percent(gbd_percent)


# ------------------------------------------------------------
# Income groups comparison (Percent of DALYs)
# ------------------------------------------------------------

income_groups <- c(
  "World Bank Low Income",
  "World Bank Lower Middle Income",
  "World Bank Upper Middle Income",
  "World Bank High Income"
)

plot_income_groups_percent <- function(df_percent) {
  
  df_inc <- df_percent %>%
    filter(location_name %in% income_groups) %>%
    mutate(location_name = factor(location_name, levels = income_groups))
  
  # Order causes by High Income share
  order_levels <- df_inc %>%
    filter(location_name == "World Bank High Income") %>%
    arrange(val_plot) %>%
    pull(cause_name)
  
  df_inc <- df_inc %>%
    mutate(cause_name = factor(cause_name, levels = order_levels))
  
  p <- ggplot(df_inc, aes(x = cause_name, y = val_plot, fill = location_name)) +
    geom_col(position = position_dodge(width = 0.85)) +
    coord_flip() +
    scale_y_continuous(
      breaks = pretty(df_inc$val_plot, n = 10),
      labels = comma_format()
    ) +
    labs(
      title = "Percentage of DALYs by Injury Cause",
      subtitle = "Comparison across World Bank income groups (2023)",
      x = "Cause of Injury",
      y = "Percent of DALYs (%)",
      fill = "Income group"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
  
  ggsave(
    file.path(output_dir, "DALYs_percent_income_groups.png"),
    p, width = 11, height = 8, dpi = 300
  )
}

# Run it
# plot_income_groups_percent(gbd_percent)

# ============================================================
# Belgium: Top 10 injury causes by DALYs
# (restricted to selected_causes)
# Stacked YLLs + YLDs with DALY uncertainty interval
# ============================================================

gbd_belgium_all <- gbd %>%
  filter(
    year == 2023,
    location_name == "Belgium",
    cause_name %in% selected_causes,          # ✅ KEY FIX
    measure_name %in% c(
      "DALYs (Disability-Adjusted Life Years)",
      "YLLs (Years of Life Lost)",
      "YLDs (Years Lived with Disability)"
    ),
    metric_name == "Number"
  ) %>%
  mutate(
    value_k = val / 1000,
    lower_k = lower / 1000,
    upper_k = upper / 1000,
    metric_type = case_when(
      measure_name == "YLLs (Years of Life Lost)"               ~ "YLLs",
      measure_name == "YLDs (Years Lived with Disability)"      ~ "YLDs",
      measure_name == "DALYs (Disability-Adjusted Life Years)"  ~ "DALYs"
    )
  )

# ------------------------------------------------------------
# Identify TOP 10 *within selected causes* by DALYs
# ------------------------------------------------------------

top10_causes <- gbd_belgium_all %>%
  filter(metric_type == "DALYs") %>%
  arrange(desc(value_k)) %>%
  slice(1:10) %>%
  pull(cause_name)

# ------------------------------------------------------------
# Stacked data: YLLs + YLDs
# ------------------------------------------------------------

stack_data <- gbd_belgium_all %>%
  filter(
    cause_name %in% top10_causes,
    metric_type %in% c("YLLs", "YLDs")
  ) %>%
  mutate(
    cause_name = factor(cause_name, levels = rev(top10_causes)),
    metric_type = factor(metric_type, levels = c("YLLs", "YLDs"))
  )

# ------------------------------------------------------------
# DALY uncertainty bars (total)
# ------------------------------------------------------------

daly_uncertainty <- gbd_belgium_all %>%
  filter(
    cause_name %in% top10_causes,
    metric_type == "DALYs"
  ) %>%
  mutate(
    cause_name = factor(cause_name, levels = rev(top10_causes))
  )

# ============================================================
# Plot
# ============================================================

p <- ggplot() +
  geom_col(
    data = stack_data,
    aes(x = cause_name, y = value_k, fill = metric_type),
    width = 0.7
  ) +
  geom_errorbar(
    data = daly_uncertainty,
    aes(
      x = cause_name,
      ymin = lower_k,
      ymax = upper_k,
      linetype = "95% uncertainty interval"
    ),
    color = "black",
    width = 0.25,
    linewidth = 0.8
  ) +
  coord_flip() +
  scale_y_continuous(
    name = "DALYs (thousands)",
    breaks = seq(0, max(daly_uncertainty$upper_k), by = 100),
    minor_breaks = seq(0, max(daly_uncertainty$upper_k), by = 50),
    labels = comma_format()
  ) +
  scale_fill_manual(
    values = c(
      "YLLs" = "#E74C3C",
      "YLDs" = "#27AE60"
    ),
    name = "DALY component"
  ) +
  scale_linetype_manual(
    values = c("95% uncertainty interval" = "solid"),
    name = ""
  ) +
  labs(
    title = "DALYs by Cause – Belgium, 2023",
    x = "Cause of Injury"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey70", linewidth = 0.4),
    panel.grid.minor.x = element_line(color = "grey85", linewidth = 0.25),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11),
    
    # Legend inside plot
    legend.position = c(0.97, 0.12),       # x, y in npc coordinates
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(
      fill = alpha("white", 0.75),
      color = alpha("white", 0.75)
    ),
    legend.key = element_blank(),
    # legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10)
  )

ggsave(
  file.path(output_dir, "DALYs_Stacked_YLLs_YLDs_Belgium_Top10_SelectedCauses.png"),
  p,
  width = 11,
  height = 8,
  dpi = 300
)