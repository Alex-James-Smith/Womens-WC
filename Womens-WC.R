## Libraries
library(tidyverse)
library(magrittr)
library("scales")

## Import Data
wwc_outcomes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
squads <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/squads.csv")
codes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/codes.csv")

dplyr::left_join(wwc_outcomes, codes, by = "team")

# as_factor(squads$club)
# levels(as_factor(wwc_outcomes$team))

## Top 10 professional clubs which players come from
squads %>% group_by(club) %>% 
  summarise(Members = n()) %>% 
  arrange(desc(Members)) %>% top_n(10) %>%
  ggplot(aes(fct_reorder(club, Members), Members)) + 
  geom_col(aes(fill = Members)) + coord_flip() + 
  ylim(0, 15)+ 
  labs(x = "Professional Club") + 
  scale_fill_viridis_c() + 
  theme(panel.background = element_blank())

## Distribution of player age
squads %>% group_by(age) %>% summarise(Members = n()) %>% ggplot(aes(age, Members)) + 
  geom_col()


# wwc_outcomes %>% group_by(win_status) %>% summarise(Number = n())
# wwc_outcomes %>% group_by(team) %>% summarise(Games = n()) %>% arrange(desc(Games))

## Proportion of Games won/tied/lost in Women's World Cup 1991 - 2019, sorted by total number of games played at WWC
wwc_outcomes %>% group_by(team) %>% mutate(games = n()) %>% ggplot(aes(fct_rev(fct_infreq(team)))) + 
  geom_bar(aes(fill = win_status), position = position_fill()) + coord_flip() + 
  labs(x = "Team", y = NULL) +
  scale_y_continuous(labels = scales::percent) + 
  theme(panel.background = element_blank())

# wwc_outcomes$round <- as_factor(wwc_outcomes$round)

## Relevel Round factors
wwc_outcomes$round %<>% fct_relevel("Group", "Round of 16")
squads$pos %<>% fct_relevel("GK", "DF", "MF")

## Percent of goals scored by position
squads %>% group_by(pos) %>% summarise(total_goals = sum(goals, na.rm = TRUE)) %>%
  mutate(pct_goals = total_goals/sum(total_goals)) %>% 
  ggplot(aes(pos, pct_goals), label = pos) +
  geom_col() + 
  geom_text(aes(label = percent(pct_goals)), colour = "grey40", vjust = -0.6) +
  labs(title = "Percent of goals scored by position", 
       caption = "Source: data.world \n #TidyTuesday", 
       x = "Position") + 
  scale_y_continuous(name = "Percent goals scored", 
                     labels = scales::percent,
                     limits = c(0, 0.6)) +
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey30"),
        panel.grid.minor.y = element_line(colour = "grey80"), 
        panel.grid.major.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(vjust = 10),
        plot.title = element_text(size = 24))
