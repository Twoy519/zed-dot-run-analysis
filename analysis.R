library(googlesheets4)
library(ggplot2)
library(dplyr)
library(corrplot)

df <- read_sheet("https://docs.google.com/spreadsheets/d/1OO7wvFKavCydciNxWOxeI5CkpbpQyrGUntNYoFGT90k/edit#gid=0")

df <- df %>% 
  mutate(
    parents.father.win_rate = ifelse(parents.father.win_rate == -1, NA, parents.father.win_rate),
    parents.father.number_of_races = ifelse(parents.father.number_of_races == -1, NA, parents.father.number_of_races),
    parents.mother.win_rate = ifelse(parents.mother.win_rate == -1, NA, parents.mother.win_rate),
    parents.mother.number_of_races = ifelse(parents.mother.number_of_races == -1, NA, parents.mother.number_of_races)
  )

df <- df %>% 
  mutate(
    race_adjusted_win_rate = win_rate * (number_of_races / sum(number_of_races))
  )

ggthemr::ggthemr(palette='pale', type='outer')
ggplot(
  df %>% 
    group_by(
      bloodline,
      breed_type,
      genotype
    ) %>% 
    summarise(
      num_races = sum(number_of_races),
      mean_win_rate = mean(win_rate) / 100,
      num_horses = n()
    ) %>% 
    filter(num_races > 100),
  aes(
    x = num_races,
    y = mean_win_rate
  )
) +
  geom_point(aes(size = num_horses), alpha = 0.6) +
  theme(
    text = element_text(size = 20),
    legend.position = 'top',
    legend.key.width=unit(2,"cm")
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy=1),
    breaks = seq(0,1,.1),
    limits = c(0,.4)
  ) +
  scale_x_log10(labels = scales::comma_format(accuracy=1), limits = c(100,1e6)) +
  ggrepel::geom_label_repel(
    data = . %>% filter(mean_win_rate > .1),
    size = 4,
    aes(
      label=glue::glue('{bloodline} - {breed_type} - {genotype}\nraces: {scales::comma(num_races, accuracy = 1)}\nwin_rate: {scales::percent(mean_win_rate, accuracy = 1)}'),
      color = paste0(bloodline,breed_type,genotype)
    ),
    # force = 150
    # segment.square  = TRUE,
    segment.inflect = TRUE,
    force             = 25,
    nudge_y           = 0.1,
    label.size = .2,
    # max.overlaps = 1,
    # nudge_x           = 0.15,
    direction         = "x",
    hjust             = 0,
    segment.size      = 0.5,
    segment.curvature = -0.1,
    box.padding = .9,
  ) +
  labs(
    title = 'Win Rate by Bloodline, Breed Type and Genesis',
    x = "Number of Races",
    y = "Average Win Rate",
    size = "Number of Horses in Group"
  ) +
  scale_size_binned(n.breaks = 4, range = c(1,10)) +
  guides(color=FALSE)


ggplot(
  df %>%
    filter(number_of_races > 50) %>% 
    mutate(grp = paste0(bloodline,': ', breed_type)) %>% 
    group_by(grp) %>% 
    summarise(
      p10 = quantile(win_rate / 100 , 0.10),
      p50 = median(win_rate / 100 ),
      p90 = quantile( win_rate / 100 , 0.90),
      p95 = quantile( win_rate / 100 , 0.95),
      n = n()
    ) %>% 
    mutate(
      grp_o = forcats::fct_reorder(
        grp,
        n
      )
    ),
  aes(
    x = grp_o
  )
) +
  ggeconodist::geom_econodist(
    aes(
      ymin = p10,
      median = p95,
      ymax = p90
    ),
    stat="identity",
  ) +
  coord_flip() +
  theme(
    text = element_text(size =20)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy=1),
    limits = c(0,.22)
  ) +
  labs(
    y = 'Win Rate',
    x = element_blank()
  )
  # facet_grid(
  #   bloodline ~ .
  # ) +
  # theme(
  #   axis.text.x = element_text(angle=90, hjust = 1)
  # )

ggplot(
  df %>%
    filter(
      number_of_races > 50,
      paste0(bloodline,': ', breed_type) %in% c('Buterin: elite', 'Finney: elite')
      ) %>% 
    mutate(grp = paste0(bloodline,': ', breed_type)) %>% 
    group_by(genotype, grp) %>% 
    summarise(
      p10 = quantile(win_rate / 100 , 0.10),
      p50 = median(win_rate / 100 ),
      p90 = quantile( win_rate / 100 , 0.90),
      p95 = quantile( win_rate / 100 , 0.95),
      n = n()
    ) %>% 
    mutate(
      grp_o = forcats::fct_reorder(
        genotype,
        n
      )
    ),
  aes(
    x = grp_o
  )
) +
  ggeconodist::geom_econodist(
    aes(
      ymin = p10,
      median = p95,
      ymax = p90
    ),
    stat="identity",
  ) +
  coord_flip() +
  theme(
    text = element_text(size =20)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy=1),
    limits = c(0,.22)
  ) +
  labs(
    y = 'Win Rate',
    x = element_blank()
  ) +
  facet_grid(
    . ~ grp
  )
# theme(
#   axis.text.x = element_text(angle=90, hjust = 1)
# )


df %>%
  filter(
    number_of_races >= 10 &
      !(is.na(parents.father.win_rate)) &
      parents.father.number_of_races >= 10 &
      !(is.na(parents.mother.win_rate)) &
      parents.mother.number_of_races >= 10
  ) %>% 
  select( win_rate, parents.father.win_rate, parents.mother.win_rate) %>% 
  cor(method='pearson') -> M

Mut <- M[upper.tri(M)]

corrplot(M, method='color')

ggplot(
  df %>%
    filter(
      number_of_races >= 10 &
        !(is.na(parents.father.win_rate)) &
        parents.father.number_of_races >= 10 &
        !(is.na(parents.mother.win_rate)) &
        parents.mother.number_of_races >= 10
    ) %>% 
    mutate(
      two_good_parents = parents.father.win_rate >= .1 & parents.mother.win_rate >= .1
    ),
  aes(
    x = two_good_parents,
    y = win_rate,
    color = two_good_parents,
    size = number_of_races
  )
) +
  # geom_boxplot(alpha = .1, fill = NA) +
  geom_jitter(alpha = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  theme(
    text = element_text(size = 20),
    axis.text.x = element_blank(),
    legend.position = 'none'
  ) +
  labs(
    subtitle = "Red = Horses whose parents both had win rates >= 10%",
    x= element_blank(),
    y = "Win Rate"
  ) +
  scale_fill_manual(values=c("#3498db", "#db3445")) +
  scale_color_manual(values=c("#3498db", "#db3445")) +
  scale_size_continuous(range = c(2,10)) +
  ggrepel::geom_label_repel(
    data = . %>% filter(win_rate >= .5),
    aes(label = id)
  )
  
  
ggplot(
  df %>%
    filter(
      number_of_races >= 10 &
        !(is.na(parents.father.win_rate)) &
        parents.father.number_of_races >= 10
    ),
  aes(
    x = win_rate,
    y = parents.father.win_rate,
  )
) +
  geom_point() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20)) +
  labs(
    title = "Father's Win Rate Doesn't Correlate With Offspring Win Rate",
    subtitle = "Each Point = 1 Horse.\nFiltered to only horses and fathers with >= 10 races",
    x = "Win Rate",
    y = "Father's Win Rate"
  )
  
ggplot(
  df %>%
    filter(
      number_of_races >= 10 &
        !(is.na(parents.mother.win_rate)) &
        parents.mother.number_of_races >= 10
    ),
  aes(
    x = win_rate,
    y = parents.mother.win_rate,
  )
) +
  geom_point() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20)) +
  labs(
    title = "Neither does Mother's",
    subtitle = "Each Point = 1 Horse.\nFiltered to only horses and mothers with >= 10 races",
    x = "Win Rate",
    y = "Mother's Win Rate"
  )



