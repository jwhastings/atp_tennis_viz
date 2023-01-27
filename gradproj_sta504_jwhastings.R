library(ggthemes)
library(httr)
library(lubridate)
library(tidyverse)
library(gganimate)
library(kableExtra)

#################################
# Download ATP data from Github #
#################################

# https://github.com/JeffSackmann/tennis_atp

# https://stackoverflow.com/questions/25485216/how-to-get-list-files-from-a-github-repository-folder-using-r
req <- GET("https://api.github.com/repos/JeffSackmann/tennis_atp/git/trees/master?recursive=1")
stop_for_status(req)
file_list <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)

# source of all .csv files
github_master_url <- "https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/"

# https://stackoverflow.com/questions/11433432/how-to-import-multiple-csv-files-at-once
atp_tennis_raw_data <-
  as_tibble(file_list[str_detect(file_list, "atp_matches_([:digit:]{4}).csv")]) %>%
  mutate(file_path = paste0(github_master_url, value)) %>%
  select(file_path) %>%
  map_df( ~ read_csv(., col_type = cols(.default = "c")))

write_csv(atp_tennis_raw_data, "atp_matches.csv")

#######################
# Import all ATP data #
#######################

# common column types for .csv files of interest
c_types <-
  c(
    tourney_id    = "c",
    tourney_name  = "f",
    surface       = "f",
    draw_size     = "i",
    tourney_level = "f",
    tourney_date  = "c",
    match_num     = "c", 
    
    winner_id    = "c",
    winner_seed  = "f",
    winner_entry = "f",
    winner_name  = "c",
    winner_hand  = "f",
    winner_ht    = "d",
    winner_ioc   = "f",
    winner_age   = "d",
    
    loser_id    = "c",
    loser_seed  = "f",
    loser_entry = "f",
    loser_name  = "c",
    loser_hand  = "f",
    loser_ht    = "d",
    loser_ioc   = "f",
    loser_age   = "d",
    
    score   = "c",
    best_of = "i",
    round   = "f",
    minutes = "i",
    
    w_ace     = "i",
    w_df      = "i",
    w_svpt    = "i",
    w_1stIn   = "i",
    w_1stWon  = "i",
    w_2ndWon  = "i",
    w_SvGms   = "i",
    w_bpSaved = "i",
    w_bpFaced = "i",
    
    l_ace     = "i",
    l_df      = "i",
    l_svpt    = "i",
    l_1stIn   = "i",
    l_1stWon  = "i",
    l_2ndWon  = "i",
    l_SvGms   = "i",
    l_bpSaved = "i",
    l_bpFaced = "i",
    
    winner_rank        = "i",
    winner_rank_points = "i",
    loser_rank         = "i",
    loser_rank_points  = "i"
  )

# import data
atp_tennis <- read_csv("atp_matches.csv", col_types = c_types) %>%
  mutate(tourney_name = recode_factor(tourney_name,
                                      "Australian Open" = "Australian Open",
                                      "Roland Garros" = "French Open",
                                      "Wimbledon" = "Wimbledon",
                                      "Us Open" = "US Open"), 
         tourney_date = ymd(tourney_date),
         tourney_year = year(tourney_date))

################
# Process data #
################

big_3_names <- c("Roger Federer", "Rafael Nadal", "Novak Djokovic")

# subset of only grand slam matches, reformat a bit
gs_matches <- atp_tennis %>%
  filter(tourney_level == "G") %>%
  select(
    tourney_id,
    tourney_name,
    surface,
    tourney_date,
    tourney_year,
    winner_id,
    winner_name,
    winner_age,
    loser_id,
    loser_name,
    loser_age,
    round
  ) %>%
  
  unite(winner_id, winner_name, col = "winner", sep = ";") %>%
  unite(loser_id,  loser_name,  col = "loser",  sep = ";") %>%
  
  pivot_longer(cols = c(winner, loser),
               names_to = "outcome",
               values_to = "player") %>%
  
  separate(player,
           into = c("player_id", "player_name"),
           sep = ";") %>%
  
  mutate(player_age = if_else(outcome == "winner", winner_age, loser_age)) %>%
  
  select(-c(winner_age, loser_age)) %>%
  group_by(player_id, player_name) %>%
  arrange(tourney_date) %>%
  
  mutate(floor_age = floor(player_age),
         win = if_else(round == "F" & outcome == "winner",
                       1, 0)) %>%
  
  arrange(player_id, player_name, tourney_date) %>%
  mutate(cum_gs_wins = cumsum(win),
         total_gs_wins = sum(win),
         double_digits = if_else(total_gs_wins >= 10, TRUE, FALSE),
         big_3 = if_else(player_name %in% big_3_names,
                         TRUE, FALSE)
  )

# get names of players who have won at least 1 grand slam
gs_winners <- filter(gs_matches, win > 0) %>%
  distinct(player_id, player_name)

gs_matches_winners <- inner_join(gs_matches, gs_winners, by = c("player_id", "player_name"))

gs_total_wins <- gs_matches_winners %>%
  arrange(desc(tourney_date)) %>%
  slice_head(n = 1)

by_var <- "big_3"

big_3_colors <- c("#1E8FD5", # Djokovic
                  "#E3783B", # Nadal
                  "#A2C128") # Federer

big_3_colors_dark <- c("#1978B3", # Djokovic
                       "#A14917", # Nadal
                       "#80981F") # Federer
#########
# Plots #
#########

## GS wins by age

# by age
lplot1 <- ggplot() +
  # not big 3
  geom_line(
    data = filter(gs_matches_winners, !(!!sym(by_var))),
    aes(x = floor_age, y = cum_gs_wins, group = player_name),
    color = "dark gray"
  ) +
  geom_point(
    data = filter(gs_total_wins, !(!!sym(by_var))),
    aes(x = floor_age, y = total_gs_wins, group = player_name),
    color = "dark gray"
  ) +
  # big 3
  geom_line(
    data = filter(gs_matches_winners, !!sym(by_var)),
    aes(
      x = floor_age,
      y = cum_gs_wins,
      color = player_name,
      group = player_name
    ),
    size = 1,
    alpha = 0.7
  ) +
  geom_point(
    data = filter(gs_total_wins, !!sym(by_var)),
    aes(
      x = floor_age,
      y = total_gs_wins,
      color = player_name,
      group = player_name
    ),
    alpha = 1
  ) +
  scale_color_manual(values = big_3_colors) +
  theme_few() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Major Wins by Age",
       x = "Age",
       y = "Major Wins",
       color = "")

lplot1

## GS wins by year

# by year
lplot2 <- ggplot() +
  # category: false
  geom_line(
    data = filter(gs_matches_winners, !(!!sym(by_var))),
    aes(
      x = tourney_year,
      y = cum_gs_wins,
      group = player_name
    ),
    color = "dark gray"
  ) +
  geom_point(
    data = filter(gs_total_wins, !(!!sym(by_var))),
    aes(
      x = tourney_year,
      y = total_gs_wins,
      group = player_name
    ),
    color = "dark gray"
  ) +
  geom_text(data = filter(gs_total_wins, !(!!sym(by_var)),
                          total_gs_wins >= 10),
            aes(x = tourney_year, y = total_gs_wins,
                label = player_name),
            vjust = -1) +
  # category: true
  geom_line(
    data = filter(gs_matches_winners, !!sym(by_var)),
    aes(
      x = tourney_year,
      y = cum_gs_wins,
      color = player_name,
      group = player_name
    ),
    size = 1,
    alpha = 0.7
  ) +
  geom_point(
    data = filter(gs_total_wins, !!sym(by_var)),
    aes(
      x = tourney_year,
      y = total_gs_wins,
      color = player_name,
      group = player_name
    ),
    alpha = 1
  ) +
  scale_color_manual(values = big_3_colors) +
  theme_few() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Major Wins by Year",
       x = "Year",
       y = "Major Wins",
       color = "")

lplot2

## GS wins comparison: big 3 vs. others since 2003

# grand slam finals since 2003
gs_final_2003 <- filter(atp_tennis,
                        tourney_level == "G",
                        round == "F",
                        tourney_year >= 2003) %>%
  mutate(
    big_3 = case_when(
      winner_name == "Roger Federer" ~ winner_name,
      winner_name == "Rafael Nadal" ~ winner_name,
      winner_name == "Novak Djokovic" ~ winner_name,
      TRUE ~ "ZZOther"
    )
  )


tiles <- ggplot(data = gs_final_2003) +
  geom_tile(aes(x = tourney_name, y = tourney_year, fill = big_3), color = "black") +
  geom_text(aes(x = tourney_name, y = tourney_year, label = winner_name),
            fontface = "bold") +
  scale_y_reverse(breaks = seq(2003, 2022)) +
  coord_cartesian(expand = FALSE) +
  scale_fill_manual(values = c(big_3_colors, "#A9A9A9")) +
  theme_few() +
  theme(axis.ticks = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Major Winners since 2003",
       x = "",
       y = "Year")

tiles

## big 3 wins comparison by slam

slam_colors <- c("#0091D2", # AO
                 "#C85A19", # RG
                 "#266C40", # Wimbledon
                 "#00288C") # USO

gs_wins_b3 <-
  filter(gs_matches,
         round == "F",
         outcome == "winner",
         player_name %in% big_3_names) %>%
  group_by(tourney_name, player_name) %>%
  summarize(win = n(), .groups = "keep")

bplot1 <- ggplot(data = gs_wins_b3) +
  geom_col(aes(x = player_name, y = win, fill = tourney_name), color = "black", position = position_dodge2()) +
  geom_text(aes(x = player_name, y = win, label = win), position = position_dodge2(0.9), vjust = -1, fontface = "bold") +
  scale_y_continuous(limits = c(0, 15)) +
  scale_fill_manual(values = slam_colors) +
  coord_cartesian(expand = FALSE) +
  theme_few() +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        panel.border = element_blank(),
        panel.spacing.y = unit(0, "cm"),
        plot.margin = margin(5, 5, 5, -10, "point"),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Big Three Wins by Major",
       x = "",
       y = "",
       fill = "")

bplot1

## Head to head (all matches)

federer_win <- atp_tennis %>%
  select(winner_name, loser_name) %>%
  filter(winner_name == "Roger Federer",
         loser_name %in% c("Novak Djokovic", "Rafael Nadal")) %>%
  group_by(loser_name) %>%
  summarize(win = n())

federer_loss <- atp_tennis %>%
  select(winner_name, loser_name) %>%
  filter(winner_name %in% c("Novak Djokovic", "Rafael Nadal"),
         loser_name == "Roger Federer",
  ) %>%
  group_by(winner_name) %>%
  summarize(loss = n())

federer <-
  full_join(federer_win, federer_loss, by = c("loser_name" = "winner_name")) %>%
  rename(opponent = loser_name) %>%
  mutate(name = "Roger Federer")

##

nadal_win <- atp_tennis %>%
  select(winner_name, loser_name) %>%
  filter(winner_name == "Rafael Nadal",
         loser_name %in% c("Novak Djokovic", "Roger Federer")) %>%
  group_by(loser_name) %>%
  summarize(win = n())

nadal_loss <- atp_tennis %>%
  select(winner_name, loser_name) %>%
  filter(winner_name %in% c("Novak Djokovic", "Roger Federer"),
         loser_name == "Rafael Nadal",
  ) %>%
  group_by(winner_name) %>%
  summarize(loss = n())

nadal <-
  full_join(nadal_win, nadal_loss, by = c("loser_name" = "winner_name")) %>%
  rename(opponent = loser_name) %>%
  mutate(name = "Rafael Nadal")

##

djokovic_win <- atp_tennis %>%
  select(winner_name, loser_name) %>%
  filter(winner_name == "Novak Djokovic",
         loser_name %in% c("Rafael Nadal", "Roger Federer")) %>%
  group_by(loser_name) %>%
  summarize(win = n())

djokovic_loss <- atp_tennis %>%
  select(winner_name, loser_name) %>%
  filter(winner_name %in% c("Rafael Nadal", "Roger Federer"),
         loser_name == "Novak Djokovic",
  ) %>%
  group_by(winner_name) %>%
  summarize(loss = n())

djokovic <-
  full_join(djokovic_win, djokovic_loss, by = c("loser_name" = "winner_name")) %>%
  rename(opponent = loser_name) %>%
  mutate(name = "Novak Djokovic")

win_loss <- bind_rows(djokovic, federer, nadal) %>%
  relocate(name, .before = everything()) %>%
  mutate(winloss = paste0(win, "-", loss)) %>%
  select(-win, -loss) %>%
  arrange(name, opponent) %>%
  pivot_wider(names_from = opponent, values_from = winloss) %>%
  select(name, `Novak Djokovic`, `Rafael Nadal`, `Roger Federer`) %>%
  replace_na(list(`Novak Djokovic` = "", `Rafael Nadal` = "", `Roger Federer` = ""))

table <- win_loss %>% column_to_rownames(var = "name") %>%
  kbl(align = "ccc") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "bordered")) %>%
  #kable_styling() %>%
  column_spec(1, bold = T, background = "#F0F0F0") %>%
  row_spec(0, bold = T) %>%
  row_spec(1, color = "#1978B3") %>%
  row_spec(2, color = "#A14917") %>%
  row_spec(3, color = "#80981F")

table
