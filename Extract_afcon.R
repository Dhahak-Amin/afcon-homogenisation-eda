packages <- c("readr", "dplyr", "stringr", "lubridate")

to_install <- setdiff(packages, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install)

invisible(lapply(packages, library, character.only = TRUE))

url_results <- "https://raw.githubusercontent.com/martj42/international_results/master/results.csv"
results <- readr::read_csv(url_results, show_col_types = FALSE)

required_cols <- c(
  "date", "home_team", "away_team", "home_score", "away_score",
  "tournament", "city", "country", "neutral"
)
stopifnot(all(required_cols %in% names(results)))

tourn_afcon <- results %>%
  distinct(tournament) %>%
  filter(str_detect(tournament, fixed("African Cup of Nations", ignore_case = TRUE))) %>%
  arrange(tournament) %>%
  pull(tournament)

stopifnot(length(tourn_afcon) > 0)

afcon <- results %>%
  filter(tournament %in% tourn_afcon) %>%
  mutate(
    date = as.Date(date),
    year = lubridate::year(date),
    decade = (year %/% 10) * 10,
    comp_type = if_else(
      str_detect(tournament, regex("qualif", ignore_case = TRUE)),
      "qualification",
      "finals"
    ),
    total_goals = home_score + away_score,
    goal_diff = home_score - away_score,
    abs_goal_diff = abs(goal_diff),
    result = case_when(
      home_score > away_score ~ "H",
      home_score < away_score ~ "A",
      TRUE ~ "D"
    )
  )

dir.create("data", showWarnings = FALSE, recursive = TRUE)
readr::write_csv(afcon, "data/afcon_results.csv")

message("AFCON dataset saved: data/afcon_results.csv")
message("Rows: ", nrow(afcon))
message("Years: ", min(afcon$year, na.rm = TRUE), " to ", max(afcon$year, na.rm = TRUE))
message("Tournaments included: ", paste(tourn_afcon, collapse = " | "))
