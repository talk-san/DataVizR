print("Homework 4")
print("Tariel Hakobyan")

library(ggplot2)
library(dplyr)

# Part 5: Overall performance (R and Python)
# • Define unique color for each team per season. For each season create horizontal bar plot using total
# number of points. Highlighting the winner with the unique color that you assigned to it. Save all
# graphs in pdf. (R)
# • Redo the same task in python. But instead of total points use goal difference. Use same logic for colors
# as in first part. (Python)

bundesliga2 <- read.csv("bundesliga2.csv", stringsAsFactors = FALSE)

unique_seasons <- sort(unique(bundesliga2$SEASON))

for (season in unique_seasons) {
  season_df <- bundesliga2 %>% 
    filter(SEASON == season) %>% 
    arrange(POSITION)
  
  n_teams <- nrow(season_df)
  
  team_colors <- rainbow(n_teams)
  names(team_colors) <- season_df$TEAM
  
  season_df <- season_df %>%
    mutate(fill_color = ifelse(POSITION == 1, team_colors[TEAM], "lightgray"))
  
  p <- ggplot(season_df, aes(x = reorder(TEAM, POINTS), y = POINTS, fill = fill_color)) +
    geom_bar(stat = "identity") +
    scale_fill_identity() +
    coord_flip() +
    labs(title = paste("Season", season, "- Total Points per Team"),
         x = "Team",
         y = "Total Points") +
    theme_minimal() +
    theme(text = element_text(size = 12))
  
  print(p)
}

