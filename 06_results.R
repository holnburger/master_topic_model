library(tidyverse)
library(janitor)
library(tidycomm)
library(forcats)
library(ggtext)
library(glue)
library(markdown)
library(marquee)
library(knitr)
library(kableExtra)
library(feather)
library(ggforce)
library(ggridges)
library(patchwork)

raters_data <- read_csv("data/crowdbucket/raters/swipes_data.csv")
authors_data <- read_csv("data/crowdbucket/authors/swipes_data.csv")

control_ids <- read_csv("data/clean/control_item_ids.csv")

finished_users <- raters_data %>% 
  left_join(control_ids, by = c("user_defined_id" = "id")) %>% 
  count(control, swiped_by_uid) %>% 
  filter(control == TRUE & n == 50) %>%
  select(user_id = swiped_by_uid)

# create result table of agreed and disagreed to number of items 
item_ratings <- authors_data %>%
  bind_rows(raters_data) %>%
  filter(!str_detect(user_defined_id, "trap")) %>%
  mutate(rating = case_when(
    swipe == "hard left" ~ -2L,
    swipe == "left" ~ -1L,
    swipe == "down" ~ 0L,
    swipe == "right" ~ 1L,
    swipe == "hard right" ~ 2L
  )) %>%
  group_by(user_defined_id) %>%
  summarise(mean = mean(rating), n = n())

total_items <- item_ratings %>% nrow()

# check if there is a normal distribution
mean(item_ratings$mean)
sd(item_ratings$mean)

# no normal distribution, triangular distribution
item_ratings %>% 
  ggplot(aes(x = mean)) + 
  geom_histogram(bins = 5)

# create table data for total agreement to items
item_ratings_table <- item_ratings %>%
  ungroup() %>%
  summarise("Stimme überhaupt nicht zu" = sum(mean <= -1.5),
            "Stimme nicht zu" = sum(mean > -1.5 & mean <= -0.5), 
            "Weder/noch" = sum(mean > -0.5 & mean < 0.5),
            "Stimme zu" = sum(mean >= 0.5 & mean < 1.5),
            "Stimme voll und ganz zu" = sum(mean >= 1.5)) %>%
  pivot_longer(everything(),
               names_to = "Bewertung", values_to = "n") %>% 
  mutate(percent = scales::percent(n / total_items, 
                                   big.mark = ".", decimal.mark = "," )) %>%
  kbl(booktabs = TRUE, caption = "Bewertung der Labels der Stichprobe (784 Dokumente)",
      format = "latex", escape = TRUE, position = "htbp", linesep = "", 
      format.args = list(big.mark = ".", decimal.mark = ","),
      label = "results-items",
      col.names = c("Bewertung", "n", ""))

writeLines(item_ratings_table, "document/tables/results_item_table.tex")

results_total <- item_ratings %>%
  ungroup() %>%
  summarise("Ablehnung" = sum(mean <= -0.5), 
            "Weder/noch" = sum(mean > -0.5 & mean < 0.5),
            "Zustimmung" = sum(mean >= 0.5))

# shorter table for documentation of total agreement/disagreement
ratings_table <- results_total %>%
  pivot_longer(everything(),
               names_to = "Bewertung", values_to = "n") %>% 
  mutate(percent = scales::percent(n / total_items, accuracy = 0.1, 
                                   big.mark = ".", decimal.mark = ",")) %>% 
  select(Bewertung, n, percent) %>%
  kbl(booktabs = TRUE, caption = "Ergebnis der Stichprobenbewertung",
      format = "latex", escape = TRUE, position = "htbp", linesep = "", 
      label = "results",
      format.args = list(big.mark = ".", decimal.mark = ","),
      col.names = c("Bewertung", "n", ""))

writeLines(ratings_table, "document/tables/results_table.tex")

# calculate the confidence interval based on doering_2023
p <- results_total %>% pull(Zustimmung) / total_items

total_items * p * (1 - p)

# we can calculate the confidence level of the population
z <- 1.96

# lower value
p - z * sqrt((p * (1 - p))/ total_items)

# upper value
p + z * sqrt((p * (1 - p))/ total_items)

# calculate the total agreement with the topic model
authors_data %>%
  bind_rows(raters_data) %>%
  mutate(rating = case_when(
    swipe == "hard left" ~ 0L,
    swipe == "left" ~ 0L,
    swipe == "down" ~ 1L,
    swipe == "right" ~ 2L,
    swipe == "hard right" ~ 2L
  )) %>%
  filter(!str_detect(user_defined_id, "trap")) %>%
  # filter out user that had a high value of disagreement
  # filter(!swiped_by_uid %in% c("p18jOvqr5rOW", "Pz8w1l97lplx")) %>%
  count(swipe, sort = TRUE) %>%
  mutate(per = round(n/sum(n),2))

# calculate the inter coder reliability for the authors
autors_table_data <- authors_data %>%
  mutate(rating = case_when(
    swipe == "hard left" ~ 0L,
    swipe == "left" ~ 0L,
    swipe == "down" ~ 1L,
    swipe == "right" ~ 2L,
    swipe == "hard right" ~ 2L
  )) %>%
  select(id = user_defined_id, user_id = swiped_by_uid, rating) %>%
  # only control items
  # left_join(control_ids, by = "id") %>%
  # filter(control == TRUE) %>%
  select(id, user_id, rating) %>%
  test_icr(unit_var = id, coder_var = user_id, levels = c("rating" = "nominal"), 
           lotus = TRUE, na.omit = TRUE) %>%
  select(Kodierer = n_Coders, Dokumente = n_Units, Übereinstimmung = Agreement, 
         "Krippendorffs $\\alpha$" = Krippendorffs_Alpha, Lotus)

# calculate the inter coder reliability for the raters
# together with the authors
raters_table_data <- raters_data %>%
  bind_rows(authors_data) %>%
  mutate(rating = case_when(
    swipe == "hard left" ~ 0L,
    swipe == "left" ~ 0L,
    swipe == "down" ~ 1L,
    swipe == "right" ~ 2L,
    swipe == "hard right" ~ 2L
  )) %>%
  select(id = user_defined_id, user_id = swiped_by_uid, rating) %>%
  # only control items
  left_join(control_ids, by = "id") %>%
  filter(control == TRUE) %>%
  select(id, user_id, rating) %>%
  test_icr(unit_var = id, coder_var = user_id, levels = c("rating" = "nominal"), 
           lotus = TRUE, na.omit = TRUE) %>%
  select(Kodierer = n_Coders, Dokumente = n_Units, Übereinstimmung = Agreement, 
         "Krippendorffs $\\alpha$" = Krippendorffs_Alpha, Lotus)

# Create tables for authors and raters values
icr_table <- autors_table_data %>%
  bind_rows(raters_table_data) %>%
  kbl(caption = "Auswertung der Inter-Kodier-Reliabilität zwischen den Autor:innen und den Teilnehmenden",
      booktabs = TRUE, format = "latex", digits = 2, escape = FALSE,
      format.args = list(big.mark = ".", decimal.mark = ",")) %>%
  kable_styling() %>%
  pack_rows("Autor:innen", 1, 1) %>%
  pack_rows("Teilnehmende und Autor:innen", 2, 2)

writeLines(icr_table, "document/tables/icr_table.tex")
  
# evaluate how the items have been rated
# this is an old calculation, we switched to the new calculation below
evaluated_data <- raters_data %>%
  bind_rows(authors_data) %>%
  mutate(rating = case_when(
    swipe == "hard left" ~ -2L,
    swipe == "left" ~ -1L,
    swipe == "down" ~ 0L,
    swipe == "right" ~ 1L,
    swipe == "hard right" ~ 2L
  )) %>%
  # remove traps
  filter(!str_detect(user_defined_id, "trap")) %>%
  # calculate the mean per item_label
  group_by(item_label) %>%
  summarise(mean = mean(rating), sd = sd(rating), n = n()) %>%
  arrange(-mean) %>%
  mutate(mean_outlier = if_else(mean < 1, TRUE, FALSE)) %>%
  mutate(sd_outlier = if_else(sd > 1, TRUE, FALSE))

# calculate the agreement to each label based on evaluating each document
evaluated_data_based_on_item_mean <- authors_data %>%
  bind_rows(raters_data) %>%
  mutate(rating = case_when(
    swipe == "hard left" ~ -2L,
    swipe == "left" ~ -1L,
    swipe == "down" ~ 0L,
    swipe == "right" ~ 1L,
    swipe == "hard right" ~ 2L
  )) %>%
  # remove traps
  filter(!str_detect(user_defined_id, "trap")) %>%
  # calculate the mean by id
  group_by(user_defined_id) %>%
  summarise(mean_rating = mean(rating), item_label = first(item_label)) %>%
  # calculate the mean per item_label
  group_by(item_label) %>%
  summarise(mean = mean(mean_rating), sd = sd(mean_rating), n = n()) %>%
  arrange(-mean) %>%
  mutate(mean_outlier = if_else(mean < 1, TRUE, FALSE)) %>%
  mutate(sd_outlier = if_else(sd > 1, TRUE, FALSE))

# figure for our document
evaluated_data_based_on_item_mean %>%
  ggplot(aes(fct_reorder(item_label, mean), mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd, width=.2)) +
  coord_flip()

# reject topics if the mean is under 1 and the sd is bigger than 1
rejected_topics <- evaluated_data_based_on_item_mean %>%
  filter(mean < 1 | sd > 1)

# check for rejected topics
raters_data %>%
  bind_rows(authors_data) %>%
  filter(item_label %in% evaluated_data_based_on_item_mean$item_label) %>%
  filter(!str_detect(user_defined_id, "trap")) %>%
  arrange(desc(user_defined_id), desc(item_label)) 

# corrected topics base on authors classification
corrected_topics <- evaluated_data_based_on_item_mean %>%
  mutate(item_label = str_trim(item_label)) %>%
  mutate(corrected_label = case_when(
    # item_label == "Abschaffung § 219a StGB" ~ "Regelungen zu Schwangerschaft und § 219a StGB",
    item_label == "Aufarbeitung der DDR-Diktatur" ~ "Aufarbeitung der deutschen Geschichte",
    item_label == "Bundeswehreinsatz in Mali" ~ "Bundeswehreinsätze in Westafrika",
    item_label == "Deutsch-Französische Partnerschaft und Integration" ~ "Deutsch-Französische Beziehung",
    item_label == "Forschungsförderung und Wissenschaftsfreiheit" ~ "Forschung und Wissenschaft",
    item_label == "Friedrich Merz und CDU-Debatte" ~ "Friedrich Merz",
    item_label == "Hartz IV und Existenzsicherung" ~ "Arbeitslosigkeit und Existenzsicherung",
    item_label == "Internationale Sicherheitsstrategie und UN-Sicherheitsrat" ~ "Außen- und Sicherheitspolitik",
    item_label == "AfD und parlamentarische Prozesse" ~ "AfD im Parlament",
    item_label == "Flüchtlingsintegration aus der Ukraine" ~ "Geflüchtete aus der Ukraine",
    item_label == "Bekämpfung sexuellen Kindesmissbrauchs" ~ "Bekämpfung sexuellen Kindesmissbrauchs und Jugendstrafrecht",
    item_label == "FDP-Partei und Koalitionspolitik" ~ "FDP im Parlament",
    item_label == "Anträge der Linksfraktion" ~ "Linksfraktion im Parlament",
    item_label == "Maritime Sicherheit NATO Sea Guardian" ~ "Maritime Sicherheit",
    item_label == "5G-Netzausbau und Sicherheit" ~ "Breitbandausbau und Mobilfunksicherheit",
    item_label == "NetzDG und Online-Hasskriminalität" ~ "NetzDG und Internet-Regulierungen",
    item_label == "Plastikmüll-Reduktion und Kreislaufwirtschaft" ~ "Müllreduktion und Kreislaufwirtschaft",
    item_label == "Stärkung der Bundespolizei und Polizeibeauftragter" ~ "Sicherheitsbehörden und Polizeibeauftragte",
    item_label == "Türkei-NATO-Außenpolitik" ~ "Türkei und Syrien",
    item_label == "Umsatzsteuersenkung für Gastronomie" ~ "Umsatzsteuer",
    item_label == "UNMISS und Südsudan-Hilfseinsätze" ~ "Sudan und Südsudan",
    item_label == "Unternehmenssteuerreform und Steuergerechtigkeit" ~ "Steuerpolitik",
    item_label == "Verteidigung der Demokratie" ~ "Demokratische Werte",
    item_label == "Weihnachtsgrüße im Bundestag" ~ "Weihnachtsgrüße und Grüße zum Jahresende",
    item_label == "Kritik und Forderungen an Bundesregierung" ~ "Forderungen an Bundesregierung und Bund-Länder-Diskussion",
    item_label == "COVID-19 Impfpflicht Debatte" ~ "Impfpflicht Debatte"
    )) %>%
  mutate(keep_label = case_when(
    item_label == "Geschlechtergleichstellung und Gewaltprävention" ~ TRUE,
    item_label == "Olaf Scholz' Kanzlerschaft und Führung" ~ TRUE,
    item_label == "ÖPNV-Finanzierung und Verkehrswende" ~ TRUE,
    item_label == "Pflegereform und Arbeitsbedingungen" ~ TRUE,
    item_label == "Postgesetz und Zustellungsdienstleistungen" ~ TRUE,
    item_label == "Rezession und Wirtschaftspolitik" ~ TRUE,
    item_label == "Deutsch-Israelische Beziehungen und Sicherheit" ~ TRUE,
    item_label == "CDU/CSU-Fraktion Anträge und Positionen" ~ TRUE,
    item_label == "Religionsfreiheit und Extremismus" ~ TRUE,
    item_label == "Bildungskooperation und Föderalismus" ~ TRUE,
    item_label == "Europas Zukunft und Solidarität" ~TRUE
    )) %>%
  mutate(delete_topic = case_when(
    item_label == "Anrede in Reden" ~ TRUE,
    item_label == "Gesetzesverabschiedung und Zustimmung" ~ TRUE,
    item_label == "Koalitionsvertrag und -verhandlungen" ~ TRUE,
    item_label == "Politische Identität und Zukunftsvisionen" ~ TRUE,
    item_label == "Verantwortungsvolle Politik" ~ TRUE,
  )) %>%
  mutate(need_modification = case_when(
    item_label == "Bekämpfung sexuellen Kindesmissbrauchs" ~ TRUE,
    item_label == "Kritik und Forderungen an Bundesregierung" ~ TRUE,
    # item_label == "Musterfeststellungsklage und Verbraucherschutz" ~ TRUE,
    item_label == "Türkei-NATO-Außenpolitik" ~ TRUE,
    item_label == "Unternehmenssteuerreform und Steuergerechtigkeit" ~ TRUE
  ))

# create a new chart which represents all the data
# split into two groups
corrected_topics %>%
  group_by(chunk = (row_number()-1) %/% (n()/2)) %>%
  # mark every modified label with a superscript 1
  mutate(item_label = if_else(is.na(corrected_label),
                              item_label,
                              marquee_glue("{corrected_label}{.sup 1}"))) %>%
  # mark every kept label with a superscript 2
  mutate(item_label = if_else(is.na(keep_label),
                              item_label,
                              marquee_glue("{item_label}{.sup 2}"))) %>%
  # mark every kept label with a superscript 3
  mutate(item_label = if_else(is.na(need_modification),
                              item_label,
                              marquee_glue("{item_label}{.sup ,3}"))) %>%
  # strike through all labels that will be dropped
  mutate(item_label = if_else(is.na(delete_topic), 
                              item_label,
                              marquee_glue("~~{item_label}~~")
                              )) %>%
  ggplot(aes(mean, fct_reorder(item_label, mean))) +
  geom_errorbar(aes(xmin = mean-sd, xmax = mean+sd, width=.4, colour = sd_outlier, linetype = sd_outlier)) +
  geom_point(aes(shape = mean_outlier, colour = mean_outlier)) +
  labs(title = "Qualität der Labels von 137 Topics der analysierten Bundestagsdebatten des 19. und 20. Bundestagsn", 
       subtitle = "Auswertung der Zustimmung zum Label mittels Crowdbucket, absteigend von höchster Zustimmung zu niedrigster Zustimmung",
       caption = "Die Bewertung wurden in numerische Werte umgewandelt:<br> 
       Stimme überhaupt nicht zu (-2), Stimme nicht zu (-1), Weder/noch (0), Stimme zu (1), Stimme voll und ganz zu (2)<br>
       Labels ohne eine durchschnittliche Zustimmung (Mittelwert < 1) und mit einer hohen Standardabweichung (SD > 1) sind hervorgehoben und wurden durch die Autor:innen überprüft.<br>
       Nach Überprüfung wurden die Labels entweder angepasst, eine Überarbeitung angeregt oder die Labels verworfen.<br>
       ^1^ Angepasstes Label nach Überprüfung durch die Autor:innen<br>
       ^2^ Label ohne Anpassung nach Prüfung übernommen<br>
       ^3^ Label übernommen, aber eine Modifizierung etwa durch eine Aufteilung der Topics wäre sinnvoll",
       y = NULL, x = NULL) +
  facet_wrap(~chunk, scales = "free_y") +
  theme_minimal() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    # using margin correction based on 
    axis.text.y = element_marquee(color = "black", style = classic_style() %>% modify_style("base", margin = trbl(0))),
    plot.caption = element_markdown(hjust = 0),
    legend.position = "none"
  ) + 
  scale_color_manual(values = c("black", "red")) +
  scale_linetype_manual(values = c("solid", "dashed"))

ggsave(file = "document/figures/results_topics.pdf", width = 250, height = 350, units = "mm", dpi = 300, device = cairo_pdf)


# create a table with the changed topic labels and their previous label
corrected_topics_table <- corrected_topics %>%
  filter(!is.na(corrected_label)) %>%
  mutate(arrow = "\\textrightarrow") %>%
  select("Label generiert durch ChatGPT" = item_label, " " = arrow, 
         "Label modifiziert durch die Autor:innen" = corrected_label) %>%
  kbl(booktabs = TRUE, caption = "Veränderte Topic-Label nach Überprüfung durch die Autor:innen",
      format = "latex", escape = FALSE, linesep = "", label = "corrected_topics",  
      format.args = list(big.mark = ".", decimal.mark = ",")) %>%
  kable_styling(font_size = 10, latex_options = "HOLD_position")

writeLines(corrected_topics_table, "document/tables/corrected_topics_table.tex")

# create a dataset with only the correct labels and remove labels that are dropped
label_correction <- corrected_topics %>% 
  mutate(corrected_label = if_else(is.na(corrected_label), item_label, corrected_label)) %>%
  filter(is.na(delete_topic)) %>%
  select(item_label, corrected_label)

# load in all the documents with corrected labels
all_rated_items <- read_csv("data/clean/models/topic_export_300.csv") %>%
  clean_names() %>%
  # keep only items with accepted labels
  right_join(label_correction, by = c("custom_name" = "item_label")) %>%
  # drop old label
  select(-custom_name)

# load in the metadata to each speech
paragraph_metadata <- read_feather("data/clean/mdb_paragraphs.feather")

# check documents where one of the raters swiped left that haven't been reviewed yet
authors_data %>% 
  filter(!str_detect(user_defined_id, "trap")) %>%
  count(item_label, swipe) %>% 
  filter(swipe == "left" | swipe == "hard left") %>% 
  distinct(item_label, .keep_all = TRUE) %>% 
  anti_join(rejected_topics, by = "item_label")

# see the total agreement to labels from authors and raters
authors_data %>% 
  bind_rows(raters_data) %>% 
  filter(!str_detect(user_defined_id, "trap")) %>% 
  filter(swipe != "down") %>%
  mutate(rating = case_when(
    str_detect(swipe, "right") ~ "agree",
    str_detect(swipe, "left") ~ "disagree"
  )) %>%
  group_by(item_label) %>%
  count(rating) %>%
  mutate(perc = n / sum(n)) %>%
  filter(rating == "agree") %>% 
  arrange(desc(perc)) %>%
  filter(perc < 1)

# write the corrected label data and metadata
topic_model <- read_csv("data/clean/models/topic_export_300.csv") %>%
  clean_names()

# create a df with the corrected and old labels
corr_labels <- corrected_topics %>%
  filter(is.na(delete_topic)) %>%
  mutate(corr_label = if_else(!is.na(corrected_label), corrected_label, item_label)) %>%
  select(item_label, corr_label)

# create the final dataframe
final_data_paragraphs <- topic_model %>%
  right_join(corr_labels, by = c("custom_name" = "item_label")) %>%
  select(rede_id, label = corr_label, topic, paragraph) %>%
  distinct(rede_id, paragraph, .keep_all = TRUE)

# read in the metadata
mdb_speech_metadata <- read_feather("data/clean/mdb_speeches.feather") %>%
  distinct(rede_id, .keep_all = TRUE)

# create the export data for osf.io
write_csv(mdb_speech_metadata, "export/speeches_metadata.csv")
write_csv(final_data_paragraphs, "export/paragraphs_topics.csv")


# create an overview of topics in parliament
prepared_data <- final_data_paragraphs %>% 
  left_join(mdb_speech_metadata) %>%
  filter(!is.na(label)) %>%
  mutate(quarter = floor_date(datum, "quarter")) %>% 
  group_by(label, quarter) %>%
  mutate(quarter_with_most_docs = quarter[which.max(table(quarter))]) %>%
  ungroup() %>%
  mutate(label = fct_reorder(label, -as.numeric(quarter_with_most_docs)))

total_labels <- n_distinct(prepared_data$label)
# create 4 plots later and stitch them together
labels_per_plot <- ceiling(total_labels / 4)


plot1 <- prepared_data %>%
  filter(label %in% levels(label)[1:labels_per_plot]) %>%
  ggplot(aes(datum, y = label)) + 
  geom_density_ridges(rel_min_height = 0.02, bandwidth = 50) +
  theme_minimal() +
  scale_x_date(limits = as.Date(c("2017-10-24", "2024-03-31"))) +
  labs(x = NULL, y = NULL) +
  scale_y_discrete(limits=rev) + 
  theme(plot.margin = margin(5, 5, 5, 20))

plot2 <- prepared_data %>%
  filter(label %in% levels(label)[(labels_per_plot + 1):(2 * labels_per_plot)]) %>%
  ggplot(aes(datum, y = label)) + 
  geom_density_ridges(rel_min_height = 0.02, bandwidth = 50) +
  theme_minimal() +
  scale_x_date(limits = as.Date(c("2017-10-24", "2024-03-31"))) +
  labs(x = NULL, y = NULL) +
  scale_y_discrete(limits=rev) + 
  theme(plot.margin = margin(5, 5, 5, 20))

plot3 <- prepared_data %>%
  filter(label %in% levels(label)[(2 * labels_per_plot + 1):(3 * labels_per_plot)]) %>%
  ggplot(aes(datum, y = label)) + 
  geom_density_ridges(rel_min_height = 0.02, bandwidth = 50) +
  theme_minimal() +
  scale_x_date(limits = as.Date(c("2017-10-24", "2024-03-31"))) +
  labs(x = NULL, y = NULL) +
  scale_y_discrete(limits=rev) + 
  theme(plot.margin = margin(5, 5, 5, 20))

plot4 <- prepared_data %>%
  filter(label %in% levels(label)[(3 * labels_per_plot + 1):total_labels]) %>%
  ggplot(aes(datum, y = label)) + 
  geom_density_ridges(rel_min_height = 0.02, bandwidth = 50) +
  theme_minimal() +
  scale_x_date(limits = as.Date(c("2017-10-24", "2024-03-31"))) +
  labs(x = NULL, y = NULL)  +
  scale_y_discrete(limits=rev) + 
  theme(plot.margin = margin(5, 5, 5, 20))


(plot4 | plot3 ) + plot_layout(guides = "collect") +
  plot_annotation(
    title = 'Topics des 19. und 20. Bundestags',
    subtitle = 'Zeitliche Verteilung der Dichte von Redebeiträgen zu ermittelten Topics im Deutschen Bundestag (2017 - März 2024)',
    caption = "Grafik 1 von 2"
  )

ggsave("document/figures/overview_bundestag_1.pdf", width = 340, 
       height = 210, units = "mm", dpi = "retina")

(plot2 | plot1 ) + plot_layout(guides = "collect") +
  plot_annotation(
    title = 'Topics des 19. und 20. Bundestags',
    subtitle = 'Zeitliche Verteilung der Dichte von Redebeiträgen zu ermittelten Topics im Deutschen Bundestag (2017 - März 2024)',
    caption = "Grafik 2 von 2"
  )

ggsave("document/figures/overview_bundestag_2.pdf", width = 340, 
       height = 210, units = "mm")
