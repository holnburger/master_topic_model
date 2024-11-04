library(tidyverse)
library(feather)
library(glue)
library(janitor)
library(ggpage)

# get the rated segments
topic_of_rated_segments <- read_csv("data/clean/models/topic_export_300.csv") %>%
  clean_names() %>%
  distinct(rede_id, paragraph, .keep_all = TRUE) %>%
  # filter out the topics "Anrede", -1 and 0
  filter(!topic %in% c(-1, 0, 10))

# we need paragraphs to get additional data: date and party
mdb_paragraph_metadata <- read_feather("data/clean/mdb_paragraphs.feather") %>%
  select(datum, sitzung, wahlperiode, rede_id, paragraph) %>%
  distinct()


# mdb overview speeches
overview <- read_rds("data/clean/overview.RDS") %>%
  filter(!is.na(redner_fraktion)) %>%
  count(wahlperiode, redner_fraktion)

topics <- topic_of_rated_segments %>%
  distinct(custom_name, topic) %>%
  mutate(custom_name = if_else(custom_name == "Politik und Verantwortung", 
                               "Verantwortungsvolle Politik",
                               custom_name)) %>%
  rename(topic_label = custom_name)

topics_per_speech <- topic_of_rated_segments %>%
  select(rede_id, topic, paragraph) %>%
  # filter out the topics "Anrede", -1 and 0
  filter(!topic %in% c(-1, 0, 10))

# see what topics are relevant in certain times
topics_metadata <- topic_of_rated_segments %>%
  left_join(mdb_paragraph_metadata, by = c("rede_id", "paragraph")) %>%
  rename(label = custom_name)

# show a few examples for certain topics
topics_metadata %>%
  filter(topic %in% c(14, 21)) %>%
  mutate(month = floor_date(datum, "month")) %>%
  count(month, label) %>%
  ggplot(aes(month, n, colour = label)) +
  geom_line() +
  labs(title = "Bundestagsdebatten zu den Themen \"Impfpflicht\" und \"Pandemie und Infektionsschutzmaßnahmen\"",
       subtitle = "Anzahl der Redeabschnitte pro Monat welche den Topics \"COVID-19 Impfpflicht Debatte\"\noder \"Pandemie und Infektionsschutzmaßnahmen\" zugeordnet werden können",
       x = "Monat", y = "Anzahl Redeabschnitte", colour = "Topic") +
  theme_classic() +
  theme(legend.position = "bottom") +
  annotate("text", x = as.Date("2018-10-01"), y = 50, label = "Debatte über Masernimpfpflicht") +
  annotate("curve", x = as.Date("2018-10-01"), y = 60, xend = as.Date("2019-10-01"), yend = 80, linewidth = 0.3, curvature = -0.2, 
           arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = as.Date("2020-08-01"), y = 240, label = "Debatten über Impfstrategie\nund Einkauf möglicher Impfstoffe Ende 2020/Anfang 2021", hjust = 1) +
  annotate("curve", x = as.Date("2020-08-01"), y = 220, xend = as.Date("2020-12-01"), yend = 190, linewidth = 0.3, curvature = -0.2, 
           arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = as.Date("2022-08-01"), y = 240, label = "Debatten über Corona-Impfpflicht-\nInitiativen im April 2022", hjust = 0) +
  annotate("curve", x = as.Date("2022-10-01"), y = 220, xend = as.Date("2022-05-01"), yend = 190, linewidth = 0.3, curvature = -0.2, 
           arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = as.Date("2022-04-01"), y = 350, label = "Debatten über Impfpflicht\nmit ersten Vorschlägen im Januar 2022", hjust = 0) +
  annotate("curve", x = as.Date("2022-06-01"), y = 370, xend = as.Date("2022-02-01"), yend = 380, linewidth = 0.3, curvature = 0.2, 
           arrow = arrow(length = unit(0.2, "cm"))) 

ggsave("document/figures/vaccination_debate.pdf", width = 15, height = 9)
