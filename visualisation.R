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

print(p1)

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

print(p2)

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
  scale_fill_gradient(low = "#4facfe", high = "#00f2fe") + 
  geom_text(aes(label = n), hjust = 1.2, color = "white", fontface = "bold") + 
  labs(title = "5. Les Rois de la Victoire",
       x = "", y = "Nombre de victoires") +
  theme_minimal() +
  theme(legend.position = "none") 

print(p5)

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

print(head(verif_classement, 15))