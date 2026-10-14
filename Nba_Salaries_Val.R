install.packages(c("mgcv", "ggrepel"))
library(tidyverse)
library(hoopR)
library(mgcv)
library(ggrepel)
library(tidyverse)
library(hoopR)

# Import Bball Ref salaries

salaries_2025_26 <- read_csv("bref_salaries_2025_26.csv", skip = 1) %>%
  select(Player = `Player`, Salary_Raw = `2025-26`) %>%
  filter(!is.na(Player) & Player != "Player") %>%
  mutate(
    Player = str_trim(gsub("\\*", "", Player)),
    salary = as.numeric(gsub("[\\$,]", "", Salary_Raw))
  ) %>%
  filter(!is.na(salary) & salary > 0) %>%
  select(Player, salary) %>%
  distinct(Player, .keep_all = TRUE)

print(paste("Successfully loaded", nrow(salaries_2025_26), "player salaries!"))
head(salaries_2025_26)

#Fetch player box scores and per-36 

player_raw2 <- hoopR::load_nba_player_box(2026)

player_perf <- player_raw2 %>%
  group_by(athlete_id, athlete_display_name) %>%
  summarize(
    Games = n(),
    Minutes = sum(as.numeric(minutes), na.rm = TRUE),
    PTS = sum(as.numeric(points), na.rm = TRUE),
    AST = sum(as.numeric(assists), na.rm = TRUE),
    REB = sum(as.numeric(rebounds), na.rm = TRUE),
    STL = sum(as.numeric(steals), na.rm = TRUE),
    BLK = sum(as.numeric(blocks), na.rm = TRUE),
     .groups = "drop"
  ) %>%
  filter(Minutes >= 150) %>%
  mutate(
    PTS_36 = (PTS / Minutes) * 36,
    AST_36 = (AST / Minutes) * 36,
    REB_36 = (REB / Minutes) * 36,
    STL_36 = (STL / Minutes) * 36,
    BLK_36 = (BLK / Minutes) * 36,
    
#Per-36 weights    
    
    Prod_Index = PTS_36 + (1.2 * AST_36) + (0.8 * REB_36) + (1.5 * BLK_36) + (1.5 * STL_36)
  )

# Join performance data with imported salaries

player_data_2026 <- player_perf %>%
  inner_join(salaries_2025_26, by = c("athlete_display_name" = "Player")) %>%
  mutate(
    Salary_Millions = salary / 1e6,
    log_salary = log(Salary_Millions)
  ) %>%
  filter(Salary_Millions > 1.0)

#General Additive Model:

gam_model_2026 <- gam(
  log_salary ~ s(Minutes, k = 5) + s(Prod_Index, k = 5),
  data = player_data_2026,
  method = "REML"
)

summary(gam_model_2026)

# Predictions and conversion from log scale

player_data_2026 <- player_data_2026 %>%
  mutate(
    log_pred_salary = predict(gam_model_2026, newdata = .),
    Predicted_Salary_Millions = exp(log_pred_salary),
    
    Surplus_Millions = Predicted_Salary_Millions - Salary_Millions
  )

# Top 10 most underpaid players

player_data_2026 %>%
  arrange(desc(Surplus_Millions)) %>%
  select(athlete_display_name, Salary_Millions, Predicted_Salary_Millions, 
  Surplus_Millions, Prod_Index) %>%
  head(10)

# Data Visualization

ggplot(player_data_2026, aes(x = Prod_Index, y = Salary_Millions)) + 
  geom_point(alpha = 0.6, color = "dodgerblue", size = 2) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5), color = "red2", linetype = "dashed", se = FALSE) +
  geom_text_repel(
    data = player_data_2026 %>% filter(Surplus_Millions > 18 | Surplus_Millions < -22),
    aes(label = athlete_display_name),
    size = 3.5,
    box.padding = 0.5,
    max.overlaps = 15
  ) +
  
  labs(
    title = "Nba Player Fair Market Value vs. Actual Salary (2025-26)", 
    subtitle = "GAM Model Estimates vs. Actual Contract Pay Scale", 
    x = "Production Index (Weighted Per-36 Box Score)",
    y = "Actual Salary ($ Millions)",
    caption = "Source: hoopR & Basketball-Reference Data Pipeline"
  ) + theme_minimal(base_size = 12)+
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )
