library(dplyr)
library(ggplot2)
library(readr)

# CHARGEMENT DES DONNÉES PRÊTES
afcon <- read_csv("data/afcon_results.csv", show_col_types = FALSE)

# --- GRAPHIQUE 1 : ÉVOLUTION TEMPORELLE ---
p1 <- ggplot(afcon, aes(x = year, fill = comp_type)) +
  geom_histogram(binwidth = 2, position = "stack", color = "white") +
  scale_fill_manual(values = c("finals" = "#FF9933", "qualification" = "#009900"),
                    labels = c("Phase Finale", "Qualifications")) +
  labs(title = "1. Expansion historique de la CAN",
       x = "Année", y = "Nombre de matchs", fill = "Type") +
  theme_minimal() +
  theme(legend.position = "top")

#print(p1)

# --- GRAPHIQUE 2 : TENDANCE DES BUTS ---
stats_buts <- afcon %>%
  group_by(year, comp_type) %>%
  summarise(avg_goals = mean(total_goals), .groups = 'drop')

p2 <- ggplot(stats_buts, aes(x = year, y = avg_goals, color = comp_type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.2) + 
  scale_color_manual(values = c("finals" = "#FF9933", "qualification" = "#009900"),
                     labels = c("Phase Finale", "Qualifications")) +
  labs(title = "2. Évolution de la moyenne de buts",
       x = "Année", y = "Buts par match", color = "") +
  theme_minimal()

#print(p2)

# --- VARIABLES COMMUNES : périodes (pour Graph 3 et 4) ---
afcon <- afcon %>%
  mutate(
    neutral = as.logical(neutral),
    period = case_when(
      year < 1980 ~ "Avant 1980",
      year >= 1980 & year <= 1999 ~ "1980-1999",
      year >= 2000 & year <= 2009 ~ "2000-2009",
      year >= 2010 ~ "2010+",
      TRUE ~ NA_character_
    ),
    period = factor(period, levels = c("Avant 1980", "1980-1999", "2000-2009", "2010+")),
    neutral_label = if_else(neutral, "Terrain neutre", "Non neutre")
  )

# --- GRAPHIQUE 3 : AVANTAGE DU DOMICILE (taux H/D/A) ---
# Séparé par neutral (TRUE/FALSE) et par période

home_adv <- afcon %>%
  filter(!is.na(period), !is.na(result), !is.na(neutral)) %>%
  mutate(
    outcome = factor(
      result,
      levels = c("H", "D", "A"),
      labels = c("Victoire home", "Nul", "Victoire away")
    )
  ) %>%
  group_by(period, neutral_label, outcome) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(period, neutral_label) %>%
  mutate(rate = n / sum(n)) %>%
  ungroup()

p3 <- ggplot(home_adv, aes(x = period, y = rate, fill = outcome)) +
  geom_col(color = "white") +
  facet_wrap(~ neutral_label) +
  scale_y_continuous(
    labels = function(x) paste0(round(100 * x), "%"),
    limits = c(0, 1)
  ) +
  scale_fill_manual(
    values = c(
      "Victoire home" = "#009900",
      "Nul"          = "#FF9933",
      "Victoire away"= "#8B5A2B"
    )
  ) +
  labs(
    title = "3. Avantage du domicile : répartition des résultats",
    x = "Période",
    y = "Part des matchs",
    fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p3)



# --- GRAPHIQUE 4 : ECART DE NIVEAU (boxplot de |diff buts|) ---
# abs_goal_diff = |home_score - away_score|
# Par période, comparé finals vs qualification

# Si jamais abs_goal_diff n'existe pas dans votre CSV, décommentez :
afcon <- afcon %>% mutate(abs_goal_diff = abs(home_score - away_score))

# --- GRAPHIQUE 4 (VARIÉ) : CLEVELAND DOT PLOT des écarts de buts ---

# --- GRAPHIQUE 4 (VARIÉ + TRÈS VISUEL) : HEATMAP des écarts de buts (%) ---

# --- GRAPHIQUE 4 (HEATMAP) : écarts de buts (%) avec somme = 100% par période ---

gap_dist <- afcon %>%
  filter(!is.na(period), !is.na(abs_goal_diff), !is.na(comp_type)) %>%
  mutate(
    gap_class = case_when(
      abs_goal_diff == 0 ~ "0",
      abs_goal_diff == 1 ~ "1",
      abs_goal_diff == 2 ~ "2",
      abs_goal_diff >= 3 ~ "3+",
      TRUE ~ NA_character_
    ),
    gap_class = factor(gap_class, levels = c("0", "1", "2", "3+"))
  ) %>%
  count(period, comp_type, gap_class, name = "n") %>%
  group_by(period, comp_type) %>%
  mutate(
    rate = n / sum(n),

    # Pourcentages "bruts"
    pct_raw = 100 * rate,
    pct_floor = floor(pct_raw),
    frac = pct_raw - pct_floor,

    # Ajustement pour que la somme des % entiers fasse exactement 100
    remainder = 100 - sum(pct_floor)
  ) %>%
  arrange(desc(frac), .by_group = TRUE) %>%
  mutate(
    pct_int = pct_floor + if_else(row_number() <= remainder, 1, 0)
  ) %>%
  arrange(gap_class, .by_group = TRUE) %>%
  ungroup()

# Vérification : somme des taux = 1 (100%) par (period, comp_type)
check_sum <- gap_dist %>%
  group_by(period, comp_type) %>%
  summarise(sum_rate = sum(rate), sum_pct = sum(pct_int), .groups = "drop")
print(check_sum)

p4 <- ggplot(gap_dist, aes(x = period, y = gap_class, fill = pct_int / 100)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(pct_int, "%")), size = 3) +
  facet_wrap(
    ~ comp_type, nrow = 1,
    labeller = as_labeller(c(finals = "Phase Finale", qualification = "Qualifications"))
  ) +
  scale_fill_gradientn(
    colours = c("#0B6623", "#F4C430", "#B22222"),  # vert -> or -> rouge
    labels = function(x) paste0(round(100 * x), "%"),
    limits = c(0, 1)
  ) +
  labs(
    title = "Matchs serrés : écarts de buts (%)",
    x = "Période",
    y = "|diff| (buts)",
    fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p4)

# --- GRAPHIQUE 5 :TOP 10 DES VAINQUEURS ---
victoires <- afcon %>%
  mutate(winner = case_when(
    result == "H" ~ home_team,
    result == "A" ~ away_team,
    TRUE ~ NA_character_
  )) %>%
  mutate(winner = case_when(
    winner == "United Arab Republic" ~ "Egypt",
    winner == "Ivory Coast" ~ "Côte d'Ivoire",
    winner == "Zaire" ~ "DR Congo",
    TRUE ~ winner
  )) %>%
  filter(!is.na(winner)) %>%
  count(winner, sort = TRUE) %>%
  head(10)
p5 <- ggplot(victoires, aes(x = reorder(winner, n), y = n, fill = n)) +
  geom_col() + 
  coord_flip() + 
  scale_fill_gradient(low = "#FFE5CC", high = "#FF9933") +
  geom_text(aes(label = n), hjust = 1.2, color = "white", fontface = "bold") + 
  labs(title = "5. Les Rois de la Victoire",
       x = "", y = "Nombre de victoires") +
  theme_minimal() +
  theme(legend.position = "none") 

#print(p5)

verif_classement <- afcon %>%
  mutate(winner = case_when(
    result == "H" ~ home_team,
    result == "A" ~ away_team,
    TRUE ~ NA_character_
  )) %>%
  mutate(winner = case_when(
    winner == "United Arab Republic" ~ "Egypt",  
    winner == "Ivory Coast" ~ "Côte d'Ivoire",   
    winner == "Zaire" ~ "DR Congo",              
    TRUE ~ winner
  )) %>%
  filter(!is.na(winner)) %>%
  count(winner, sort = TRUE)

#print(head(verif_classement, 15))