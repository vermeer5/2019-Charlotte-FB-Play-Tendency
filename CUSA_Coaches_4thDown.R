if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}
pacman::p_load(tidyverse, animation, glue, zoo, ggimage)
pacman::p_load_current_gh("saiemgilani/cfbfastR")

library(dplyr)
# usethis::edit_r_environ() to save API key into .Renviron file

# loading data for season 2014-2020 (takes about 100 seconds)
tictoc::tic()
pbp <- data.frame()
seasons <- 2014:2020
progressr::with_progress({
  future::plan("multisession")
  pbp <- cfbfastR::load_cfb_pbp(seasons)
})
tictoc::toc()

# loading coaches info using cfbd_coaches function
coaches <- purrr::map_dfr(seasons,function(x){cfbfastR::cfbd_coaches(year = x)})

# loading school conference info using cfbd_team_info & left join on school
team_info <- cfbfastR::cfbd_team_info()
coaches <- coaches %>% 
  dplyr::left_join(team_info, by="school")
coaches <- coaches %>%
  dplyr::mutate(coach = paste(first_name, last_name, sep = " ")) %>%
  dplyr::filter(games >= 6) %>%
  dplyr::select(coach, school, year, conference)

# adding coaches to pbp dataframe
pbp <- pbp %>%
  dplyr::inner_join(coaches, by = c("offense_play" = "school", "year" = "year"))

# filter for fourth down plays only
down4 <- pbp %>%
  dplyr::filter(down == 4) %>%
  dplyr::mutate(fga = ifelse(str_detect(play_type, "Field Goal"),
                             1, 0),
                punt = ifelse(play_type == "Punt", 1, 0),
                attempt = ifelse(rush == 1 | pass == 1, 1, 0),
                play = dplyr::case_when(fga == 1 ~ "FG Attempt",
                                        punt == 1 ~ "Punt",
                                        attempt == 1 ~ "Go"))

# Make graphs of coaches 4th Down Tendencies
down4 %>%
  dplyr::filter(!is.na(play)) %>%
  dplyr::filter(coach %in% c("Will Healy", "Dana Dimel", "Bill Clark", "Tyson Helton", "Doc Holliday",
                             "Skip Holtz", "Seth Littrell", "Rick Stockstill", "Willie Taggart", "Jeff Traylor")) %>%
  dplyr::filter(distance <= 5, distance > 0) %>%
  ggplot(aes(x = distance, y = 100 - yards_to_goal, color = play)) +
  geom_point(size = 2.5) +
  geom_jitter() +
  facet_wrap(. ~ coach, ncol = 5) +
  theme_bw() +
  labs(x = "Yards to Go",
       title = "C-USA Coaches's Fourth Down Tendencies | CFP Era",
       subtitle = "Data from @cfbfastR",
       color = "Decision") +
  scale_y_continuous(labels = c("Own 20", "Own 40", "Opp 40",
                                "Opp 20", "Endzone"),
                     breaks = c(20, 40, 60, 80, 100)) +
  theme(axis.title.y = element_blank())
