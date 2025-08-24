library(tidyverse)

# Load raw data
df_raw <- readRDS("./data/cn_us_klems_6323_n2_va.RDS") 

df_raw <- df_raw |> 
 mutate(country = if_else(country == "CN", "Canada", "United States")) |> 
 rename(industry_name = industry) |> 
 select(-c(mfp_gr_resid, cpi, pop, qi_mfp_resid)) |> 
 select(!ends_with("_gr") & !ends_with("_avgshr"))

# Concordance lookup: map from lower-case variable names to user-friendly descriptions
var_lookup <- tribble(
 ~var_name, ~var_description,
 "qi_cntr_kh", "Contribution of Capital Intensity",
 "qi_cntr_kith", "Contribution of ICT Capital Intensity",
 "qi_cntr_knith", "Contribution of non-ICT Capital Intensity",
 "qi_cntr_lh", "Contribution of Labour Composition",
 "qi_cntr_uh", "Contribution of intermediate Inputs",
 "pi_va", "Price Index of GDP",
 "pi_e", "Price Index of Energy Input",
 "pi_k", "Price Index of Capital Services",
 "pi_l", "Price Index of Labour Input",
 "pi_m", "Price Index of Material Input",
 "pi_s", "Price Index of Services Input",
 "pi_u", "Price Index of Intermediate Inputs",
 "pi_go", "Price Index of Gross Output",
 "pi_z", "Price Index of Net Capital Stock",
 "qi_va", "Quantity Index of GDP",
 "qi_e", "Quantity Index of Energy Input",
 "qi_h", "Quantity Index of Hours Worked",
 "qi_h2", "Quantity Index of Hours Worked - Primary/Secondary Education",
 "qi_h3", "Quantity Index of Hours Worked - Post-secondary Education",
 "qi_h4", "Quantity Index of Hours Worked - University Degree or Above",
 "qi_k", "Quantity Index of Capital Services",
 "qi_kit", "Quantity Index of ICT Capital Services",
 "qi_knit", "Quantity Index of Non-ICT Capital",
 "qi_kl", "Quantity Index of Labour & Capital",
 "qi_klu", "Quantity Index of All Inputs",
 "qi_kh", "Index of Capital Intensity",
 "qi_kith", "Index of ICT Capital Intensity",
 "qi_knith", "Index of non-ICT Capital Intensity",
 "qi_lh", "Index of Labour Composition",
 "qi_l", "Quantity Index of Labour",
 "qi_l2", "Labour Input - Primary/Secondary",
 "qi_l3", "Labour Input - Post-secondary",
 "qi_l4", "Labour Input - University",
 "qi_m", "Quantity Index of Material Input",
 "qi_s", "Quantity Index of Services Input",
 "qi_u", "Quantity Index of Intermediate Inputs",
 "qi_go", "Quantity Index of Gross Output",
 "qi_z", "Quantity Index of Net Capital Stock",
 "qi_lp_va", "Real GDP per Hour Worked",
 "qi_kp_va", "Real GDP per Unit of Capital Service",
 "qi_lp_go", "Real Gross Output per Hour Worked",
 "qi_mfp_va", "Multifactor Productivity (GDP)",
 "qi_mfp_go", "Multifactor Productivity (Output)",
 "v_va", "GDP in Current Prices (Millions)",
 "v_e", "Cost of Energy (Millions)",
 "v_k", "Cost of Capital (Millions)",
 "v_kit", "Cost of ICT Capital (Millions)",
 "v_knit", "Cost of non-ICT Capital (Millions)",
 "v_l", "Cost of Labour (Millions)",
 "v_l2", "Labour Compensation - Primary/Secondary",
 "v_l3", "Labour Compensation - Post-secondary",
 "v_l4", "Labour Compensation - University",
 "v_m", "Cost of Material (Millions)",
 "v_s", "Cost of Services (Millions)",
 "v_u", "Cost of Intermediate Inputs (Millions)",
 "v_go", "Gross Output in Current Prices (Millions)",
 "ttl_cost_va", "Total Cost of Capital and Labour (Millions)",
 "s_l_va", "Labour Share of Total Cost",
 "s_k_va", "Capital Share of Total Cost",
 "qi_mfp", "Index of Multifactor Productivity",
 "qi_lp", "Index of Labour Productivity",
 "qi_kp", "Index of Capital Productivity",
 "v_h", "Hours Worked (Millions)",
 
)

# Filter concordance to match existing columns in your dataset
vars_in_data <- names(df_raw)
valid_vars <- var_lookup %>% filter(var_name %in% vars_in_data)

# Rename those columns using the concordance
df_clean <- df_raw %>%
 rename_with(
  .fn = ~ valid_vars$var_description[match(., valid_vars$var_name)],
  .cols = all_of(valid_vars$var_name)
 )


#saveRDS(df_clean , "./df_clean.RDS")
row.names(df_clean) <- NULL
write.csv(df_clean , "app/df_clean.csv", row.names = FALSE, fileEncoding = "UTF-8")
#df_clean <- readRDS("./df_clean.RDS")

# ---- Example filtering and indexing settings (can be dynamic in a Shinylive app) ----
target_variable <- "Index of Labour Productivity"  # <- must match a renamed variable
start_year <- 1963
end_year <- 2023
index_year <- 1963
selected_industry <- "Business sector"

# ---- Prepare data for plotting ----
df_plot <- df_clean %>%
 filter(year >= start_year,
        year <= end_year,
        industry_name == selected_industry) %>%
 select(year, country, industry_name, value = all_of(target_variable)) %>%
 group_by(country) %>%
 mutate(index_value = value[year == index_year],
        value_indexed = 100 * value / index_value) %>%
 ungroup()

# ---- ggplot line chart ----
ggplot(df_plot, aes(x = year, y = value_indexed, color = country)) +
 geom_line(linewidth = 1.2) +
 scale_color_manual(values = c("Canada" = "red", "United States" = "blue")) +
 labs(
  title = paste0(target_variable, " (Index = 100 in ", index_year, ")"),
  subtitle = paste("Industry:", selected_industry),
  x = "Year", y = "Index (Base Year = 100)", color = "Country"
 ) +
 theme_minimal(base_size = 14)









################################################################################
library(tidyverse)

# Load raw data
df_raw <- read_csv("./data/cn_us_mfp.csv")

# Concordance lookup: map from lower-case variable names to user-friendly descriptions
var_lookup <- tribble(
 ~var_name, ~var_description,
 "contrk_lpv", "Contribution of Capital Intensity",
 "contrl_lpv", "Contribution of Labour Composition",
 "contru_lpv", "Contribution of intermediate Inputs",
 "contrk_lpa", "Contribution of Capital Intensity",
 "contrl_lpa", "Contribution of Labour Composition",
 "ifpa", "Price Index of GDP",
 "ifpe", "Price Index of Energy Input",
 "ifpk", "Price Index of Capital Services",
 "ifpl", "Price Index of Labour Input",
 "ifpm", "Price Index of Material Input",
 "ifps", "Price Index of Services Input",
 "ifpu", "Price Index of Intermediate Inputs",
 "ifpv", "Price Index of Gross Output",
 "ifpz", "Price Index of Net Capital Stock",
 "ifqa", "Quantity Index of GDP",
 "ifqe", "Quantity Index of Energy Input",
 "ifqh", "Hours Worked (Millions)",
 "ifqh2", "Hours Worked - Primary/Secondary Education",
 "ifqh3", "Hours Worked - Post-secondary Education",
 "ifqh4", "Hours Worked - University Degree or Above",
 "ifqk", "Quantity Index of Capital Services",
 "ifqk2", "Quantity Index of ICT Capital",
 "ifqk3", "Quantity Index of Non-ICT Capital",
 "ifqkl", "Quantity Index of Labour & Capital",
 "ifqklu", "Quantity Index of All Inputs",
 "ifqkh", "Index of Capital Intensity",
 "ifqlh", "Index of Labour Composition",
 "ifql", "Quantity Index of Labour",
 "ifql2", "Labour Input - Primary/Secondary",
 "ifql3", "Labour Input - Post-secondary",
 "ifql4", "Labour Input - University",
 "ifqlq", "Labour Quality Index",
 "ifqm", "Quantity Index of Material Input",
 "ifqs", "Quantity Index of Services Input",
 "ifqu", "Quantity Index of Intermediate Inputs",
 "ifqv", "Quantity Index of Gross Output",
 "ifqz", "Quantity Index of Net Capital Stock",
 "lpa", "Real GDP per Hour Worked",
 "kpa", "Real GDP per Unit of Capital Service",
 "lpv", "Real Gross Output per Hour Worked",
 "mfpa", "Multifactor Productivity (GDP)",
 "mfpv", "Multifactor Productivity (Output)",
 "paa", "GDP in Current Prices (Millions)",
 "pee", "Cost of Energy (Millions)",
 "pkk", "Cost of Capital (Millions)",
 "pkk2", "Cost of ICT Capital (Millions)",
 "pkk3", "Cost of non-ICT Capital (Millions)",
 "pll", "Cost of Labour (Millions)",
 "pll2", "Labour Compensation - Primary/Secondary",
 "pll3", "Labour Compensation - Post-secondary",
 "pll4", "Labour Compensation - University",
 "pmm", "Cost of Material (Millions)",
 "pss", "Cost of Services (Millions)",
 "puu", "Cost of Intermediate Inputs (Millions)",
 "pvv", "Gross Output in Current Prices (Millions)",
 "cost_a", "Total Cost of Capital and Labour (Millions)",
 "alpha_la", "Labour Share of Total Cost",
 "alpha_ka", "Capital Share of Total Cost"
)


# Filter concordance to match existing columns in your dataset
vars_in_data <- names(df_raw)
valid_vars <- var_lookup %>% filter(var_name %in% vars_in_data)

# Rename those columns using the concordance
df_clean <- df_raw %>%
 rename_with(
  .fn = ~ valid_vars$var_description[match(., valid_vars$var_name)],
  .cols = all_of(valid_vars$var_name)
 )

df_clean <- df_clean |> 
 mutate(country = if_else(country == "CN", "Canada", "United States"))

# df_clean <- df_clean |> 
# group_by(country, naics) %>%
#  mutate(delta_ifqkh = 100 * delta_ifqkh / delta_ifqkh[year == 2017]) %>%
#  ungroup()

df_clean <- df_clean |> 
 select(!starts_with("delta_")) |> 
select(!starts_with("cost_")) |> 
select(!starts_with("alpha_")) |> 
 rename(industry_name = industry) |> 
 filter(year >= 1987)

df <- as.data.frame(unique(df_clean$industry_name))

names(df_clean)

saveRDS(df_clean , "./df_clean.RDS")
df_clean <- readRDS("./df_clean.RDS")

# ---- Example filtering and indexing settings (can be dynamic in a Shinylive app) ----
target_variable <- "Real GDP per Hour Worked"  # <- must match a renamed variable
start_year <- 1987
end_year <- 2023
index_year <- 2000
selected_industry <- "Business sector"

# ---- Prepare data for plotting ----
df_plot <- df_clean %>%
 filter(year >= start_year,
        year <= end_year,
        industry_name == selected_industry) %>%
 select(year, country, industry_name, value = all_of(target_variable)) %>%
 group_by(country) %>%
 mutate(index_value = value[year == index_year],
        value_indexed = 100 * value / index_value) %>%
 ungroup()

# ---- ggplot line chart ----
ggplot(df_plot, aes(x = year, y = value_indexed, color = country)) +
 geom_line(linewidth = 1.2) +
 scale_color_manual(values = c("Canada" = "red", "United States" = "blue")) +
 labs(
  title = paste0(target_variable, " (Index = 100 in ", index_year, ")"),
  subtitle = paste("Industry:", selected_industry),
  x = "Year", y = "Index (Base Year = 100)", color = "Country"
 ) +
 theme_minimal(base_size = 14)
