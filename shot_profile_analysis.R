library(tidyverse)

# ==================================================
# Load Data
# ==================================================

shots <- read_csv("~/Desktop/Sports Projects/archive/NBA_2024_Shots.csv")

# Quick check
names(shots)
glimpse(shots)


# ==================================================
# Question 1: Who Takes the Most Shots?
# ==================================================

top10 <- shots %>%
  count(PLAYER_NAME, sort = TRUE) %>%
  head(10)

top10

ggplot(top10,
       aes(x = reorder(PLAYER_NAME, n),
           y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 NBA Players by Shot Attempts (2023-24)",
    x = "Player",
    y = "Shot Attempts"
  )


# ==================================================
# Question 2: How Do Star Players Differ in Shot Profile?
# ==================================================

star_players <- shots %>%
  filter(PLAYER_NAME %in% c(
    "Stephen Curry",
    "LeBron James",
    "Giannis Antetokounmpo"
  ))

shot_profile_pct <- star_players %>%
  count(PLAYER_NAME, ZONE_RANGE) %>%
  group_by(PLAYER_NAME) %>%
  mutate(
    pct = n / sum(n)
  )

shot_profile_pct

ggplot(shot_profile_pct,
       aes(x = ZONE_RANGE,
           y = pct,
           fill = PLAYER_NAME)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Shot Profile Comparison (Percent of Total Shots)",
    x = "Shot Distance Range",
    y = "Percent of Shots"
  )


# ==================================================
# Question 3: Efficiency vs Volume
# ==================================================

player_efficiency <- shots %>%
  group_by(PLAYER_NAME) %>%
  summarise(
    total_shots = n(),
    made_shots = sum(EVENT_TYPE == "Made Shot"),
    fg_pct = made_shots / total_shots
  ) %>%
  filter(total_shots >= 500)

player_efficiency

ggplot(player_efficiency,
       aes(x = total_shots,
           y = fg_pct)) +
  geom_point() +
  geom_text(
    data = player_efficiency %>%
      filter(PLAYER_NAME %in% c(
        "Stephen Curry",
        "LeBron James",
        "Giannis Antetokounmpo",
        "Luka Doncic"
      )),
    aes(label = PLAYER_NAME),
    vjust = -0.5
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "NBA Player Efficiency vs Shot Volume (2023-24)",
    x = "Total Shot Attempts",
    y = "Field Goal Percentage"
  )

# ==========================================
# Question 4: Most Efficient High-Volume Scorers
# ==========================================

top_efficiency <- player_efficiency %>%
  filter(total_shots > 500) %>%
  arrange(desc(fg_pct)) %>%
  head(15)

ggplot(top_efficiency,
       aes(x = reorder(PLAYER_NAME, fg_pct),
           y = fg_pct)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Most Efficient High-Volume Scorers",
    subtitle = "Players with 500+ shot attempts during the 2023-24 NBA season",
    x = NULL,
    y = "Field Goal Percentage"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_blank()
  )