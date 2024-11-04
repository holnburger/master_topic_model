library(xml2)
library(rvest)
library(tidyverse)
library(feather)
library(glue)
library(knitr)
library(kableExtra)

# clean mdb summary data ----------
file_raw <- read_xml("data/raw/mdb/MDB_STAMMDATEN.XML")

get_data_mdb <- function(x){
  id <- x %>% xml_find_all("//ID") %>% xml_text()
  geschlecht <- x %>% xml_find_all("//ID/following-sibling::BIOGRAFISCHE_ANGABEN/GESCHLECHT") %>% xml_text()
  geburtstag <- x %>% xml_find_all("//ID/following-sibling::BIOGRAFISCHE_ANGABEN/GEBURTSDATUM") %>% xml_text() %>% dmy()
  partei <- x %>% xml_find_all("//ID/following-sibling::BIOGRAFISCHE_ANGABEN/PARTEI_KURZ") %>% xml_text()
  # storing the values as list and accessing the first element of said list as new tibble prevents
  # storing them as pointers which will not be saved in an RDS file
  wahlperioden <- x %>% xml_find_all("//ID/following-sibling::WAHLPERIODEN") %>% map(list)
  namen <- x %>% xml_find_all("//ID/following-sibling::NAMEN") %>% map(list)
  
  tibble(id, geschlecht, geburtstag, partei, wahlperioden, namen)
}


mdb_data <- get_data_mdb(file_raw) %>%
  mutate(fraktion = map(wahlperioden, 
                        ~xml_find_all(.[[1]], ".//INSTITUTIONEN/INSTITUTION/INSART_LANG[contains(.,'Fraktion/Gruppe')]") %>%
                          xml_parent() %>% html_elements("INS_LANG") %>% xml_text())) %>%
  mutate(fraktion_mitglied_von = map(wahlperioden,
                                     ~xml_find_all(.[[1]], ".//INSTITUTIONEN/INSTITUTION/INSART_LANG[contains(.,'Fraktion/Gruppe')]") %>%
                                       xml_parent() %>% html_elements("MDBINS_VON") %>% xml_text())) %>%
  mutate(fraktion_mitglied_bis = map(wahlperioden,
                                     ~xml_find_all(.[[1]], ".//INSTITUTIONEN/INSTITUTION/INSART_LANG[contains(.,'Fraktion/Gruppe')]") %>%
                                       xml_parent() %>% html_elements("MDBINS_BIS") %>% xml_text())) %>%
  mutate(wahlperioden = map(wahlperioden, ~html_elements(.[[1]], "WP") %>% xml_text() %>% as.integer())) %>%
  mutate(anzahl_wahlperioden = map(wahlperioden, ~length(.)) %>% unlist()) %>%
  mutate(nachname = map(namen, ~html_elements(.[[1]], "NACHNAME") %>% xml_text())) %>%
  mutate(vorname = map(namen, ~html_elements(.[[1]], "VORNAME") %>% xml_text()))

# save MdB summary data
write_rds(mdb_data , "data/clean/mdb_data.RDS")


# functions to transfer mdb protocols from xml to tibble -----------
get_speeches_df <- function(xml) {
  raw <- xml %>% map(list)
  
  rede <- xml %>% xml_text()
  id <- xml %>% html_element("redner") %>% xml_attr("id")
  vorname <- xml %>% html_element("vorname") %>% xml_text()
  nachname <- xml %>% html_element("nachname") %>% xml_text()
  fraktion <- xml %>% html_element("fraktion") %>% xml_text()
  rolle <- xml %>% html_element("rolle_kurz") %>% xml_text()
  typ <- xml %>% xml_name()
  status <- xml %>% xml_attr("klasse")

  
  tibble(raw, rede, id, vorname, nachname, fraktion, rolle, typ, status) %>%
    mutate(rede_id = map(raw, ~xml_parent(.[[1]]) %>% xml_attr("id")) %>% as.character()) %>%
    select(-raw) %>%
    mutate(status = ifelse(typ == "kommentar", typ, status)) %>%
    mutate(status = ifelse(typ == "name", "präsidium", status)) %>%
    mutate(fraktion = case_when(
      typ == "name"       ~ "präsidium",
      !is.na(rolle)       ~ "andere",
      TRUE                ~ fraktion)) %>%
    fill(id, vorname, nachname, fraktion) %>%
    mutate(präsidium = ifelse(fraktion == "präsidium", TRUE, FALSE)) %>%
    mutate(fraktion = ifelse(fraktion == "präsidium", NA, fraktion)) %>%
    filter(!status %in% c("T_NaS", "T_Beratung", "T_fett", "redner")) %>%
    filter(!typ %in% c("a", "fussnote", "sup")) %>%
    select(rede_id, rede, id, vorname, nachname, fraktion, präsidium, typ, status)
}


get_overview_df <- function(x){
  rede_id <- x %>% xml_attr("id")
  redner_id <- x %>% html_element("redner") %>% xml_attr("id")
  redner_vorname <- x %>% html_element("redner") %>% html_element("vorname") %>% xml_text()
  redner_nachname <- x %>% html_element("redner") %>% html_element("nachname") %>% xml_text()
  redner_fraktion <- x %>% html_element("redner") %>% html_element("fraktion") %>% xml_text()
  redner_rolle <- x %>% html_element("rolle_kurz") %>% xml_text()
  sitzung <- x %>% xml_find_first("//sitzungsnr") %>% xml_text() %>% as.integer()
  datum <- x %>% xml_find_first("//datum") %>% xml_attr("date") %>% lubridate::dmy()
  wahlperiode <- x %>% xml_find_first("//wahlperiode") %>% xml_text() %>% as.integer()
  
  tibble(rede_id, redner_id, redner_vorname, redner_nachname, redner_fraktion, redner_rolle, sitzung, datum, wahlperiode)
}


# set up the data --------------

prot_files <- list.files("data/raw/19/", full.names = TRUE) %>%
  append(list.files("data/raw/20/", full.names = TRUE))

prot_extract <- map(prot_files, ~read_html(.) %>% 
                      xml_find_all("//rede"))
class(prot_extract) <- "xml_nodeset"

# overview of speeches ---------------
prot_overview <- map_dfr(prot_extract, get_overview_df) %>%
  # only speeches till the end of the first quarter of 2024
  filter(datum <= "2024-03-31")

write_rds(prot_overview, "data/clean/overview.RDS")

# extracting speeches from the protocols -----
speech_extract <- map(prot_files, ~read_html(.) %>% 
                        # only speeches held, no speeches handed in via "anhang"
                        xml_find_all("//sitzungsverlauf//rede/*")) %>%
  # keep only of there is more than 0 elements
  keep(~length(.) > 0)

# cleaning up mistakes in the protocols ----
prot_speeches <- map_dfr(speech_extract, 
                         get_speeches_df) %>%
  mutate(fraktion = case_when(
    str_detect(fraktion, "BÜNDNIS") ~ "BÜNDNIS 90/DIE GRÜNEN",
    fraktion == "Bündnis 90/Die Grünen" ~ "BÜNDNIS 90/DIE GRÜNEN", 
    fraktion == "Bremen" ~ NA_character_,
    fraktion == "Fraktionslos" ~ "fraktionslos",
    TRUE ~ fraktion
  )) %>%
  # removing unnecessary whitespace
  mutate(fraktion = str_squish(fraktion)) %>%
  # removing one special case with a wrong party name
  mutate(fraktion = if_else(fraktion == "SPDCDU/CSU", "CDU/CSU", fraktion))

# Number of speeches in total, with everyone speaking at the bundestag (guests, 
# government, etc.)
n_all_speeches <- prot_speeches %>% count(rede_id) %>% nrow()

n_all_speeches %>% 
  scales::comma(big.mark = ".", 
                decimal.mark = ",") %>%
  writeLines("document/input/n_all_speeches.dat")

# Problem: This exceeds the github limit of max 50 mb, so we are saving this in
# multiple (10) separated files
# write_rds(prot_speeches, "data/clean/speeches.RDS")

prot_speeches %>%
  group_by(part = (row_number()-1) %/% (n()/10)) %>%
  group_split() %>%
  map2(1:10, ~write_rds(.x, paste0("data/clean/all_speeches/speeches_", .y, ".RDS")))

# check if it was working
prot_speeches <- map_dfr(list.files("data/clean/all_speeches", 
                                    pattern = "speeches", 
                                    full.names = TRUE), read_rds)


# we only want to extract the speeches without interjections
# and only speeches by mdb
mdb_speeches <- prot_speeches %>%
  filter(typ != "kommentar" & 
           fraktion != "andere" & 
           präsidium == FALSE
           # no greetings REDACTED
           # greetings often contain important parts of the speech
           # status != "J_1") %>%
  ) %>%
  group_by(rede_id, id, vorname, nachname, fraktion) %>%
  summarise(# we also create a column for paragraphs
            # this will later be used for topic modeling
            paragraph = list(rede), 
            rede = paste(rede, collapse = " ")
            ) %>%
  mutate(nchar = nchar(rede),
         words = str_count(rede, "\\w+"),
         nparagraphs = map_int(paragraph, length)) %>%
  # join with overview but only keep speeches from before 2024
  left_join(prot_overview %>% 
                select(rede_id, redner_id, datum, sitzung, wahlperiode),
                by = "rede_id") %>%
  filter(datum <= "2024-03-31")

# nr of speeches
n_speeches <- mdb_speeches %>% 
  ungroup() %>% 
  distinct(rede_id) %>% 
  nrow() 

# write nr of speeches for our document
n_speeches %>% 
  scales::comma(big.mark = ".", 
                decimal.mark = ",") %>%
  writeLines("document/input/n_speeches.dat")

mdb_paragraphs <- mdb_speeches %>% 
  ungroup() %>% 
  select(-rede) %>%
  unnest(paragraph) %>%
  mutate(nchar = nchar(paragraph), words = str_count(paragraph, "\\w+"))

# write nr of paragraphs for our document
mdb_paragraphs %>% 
  nrow() %>%
  scales::comma(big.mark = ".", decimal.mark = ",") %>%
  writeLines("document/input/n_paragraphs.dat")

# we split everything into paragraphs to process this via topic modeling
# a histogram gives us information about the length of the paragraphs
mdb_paragraphs %>% 
  ggplot(aes(words)) + 
  geom_histogram(binwidth = 1) +
  labs(title = "Wörter pro Redeabschnitt nach Datenbereinigung der Bundestagsreden",
       subtitle = 
         glue("Auswertung von insgesamt {n_speeches %>% scales::comma(big.mark = '.', decimal.mark = ',')} Bundestagsreden des 19. und 20. Bundestags"),
       x = "Wörter",
       y = "n"
  ) +
  theme_classic() +
  geom_vline(xintercept = 6, linetype = "dashed") +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","))

ggsave("document/figures/histogram_words_per_paragraph.pdf", 
       width = 8,
       height = 5)

# number of paragraphs and speeches after filtering
mdb_paragraphs %>%
  filter(words > 6) %>%
  nrow() %>%
  scales::comma(big.mark = ".", decimal.mark = ",") %>%
  writeLines("document/input/n_paragraphs_after_filtering.dat")

# we are writing the speeches and paragraphs as feather data to import it 
# to python later on 
# this won't be on github
write_feather(mdb_paragraphs %>% filter(words > 6),
              "data/clean/mdb_paragraphs.feather")

write_feather(mdb_speeches %>% 
                select(rede_id, id, vorname, nachname, fraktion, rede,
                       datum, sitzung, wahlperiode),
              "data/clean/mdb_speeches.feather")

# point instead of komma as big mark
point_format <- scales::comma_format(big.mark = ".", decimal.mark = ",")

# create a table with an overview of all paragraphs per periode
paragraphs_table_data <- mdb_paragraphs %>%
  group_by(wahlperiode, fraktion) %>%
  count() %>%
  # join the data with the number of speeches in total
  left_join(mdb_speeches %>% 
              filter(words > 6) %>% 
              ungroup() %>% 
              count(fraktion, wahlperiode, name = "n_speeches")) %>%
  group_by(wahlperiode) %>%
  arrange(desc(n), .by_group = TRUE) %>%
  ungroup() %>%
  select(-wahlperiode)

# add details that this faction is no longer existing
paragraphs_table_data$fraktion[13] <- paragraphs_table_data$fraktion[13] %>% 
  paste0(footnote_marker_number(1, "latex"))
paragraphs_table_data$fraktion[15] <- paragraphs_table_data$fraktion[15] %>% 
  paste0(footnote_marker_number(1, "latex"))
paragraphs_table_data$fraktion[16] <- paragraphs_table_data$fraktion[16] %>% 
  paste0(footnote_marker_number(1, "latex"))

# format the table
paragraphs_table_data %>%
  mutate(n = point_format(n), n_speeches = point_format(n_speeches)) %>%
  rename("Fraktion/Gruppe" = fraktion, "Redeabschnitte" = n, "Reden" = n_speeches) %>%
  kable(., "latex", caption = "Anzahl der Redeabschnitte und Reden in den jeweiligen Legislaturperioden", 
        label = "table:uebersicht_redeabschnitte",
        booktabs = TRUE,  align = "lrr", escape = FALSE) %>%
  kable_styling(font_size = 10) %>%
  pack_rows("19. Bundestag (2017 - 2021)", 1, 7) %>%
  pack_rows("20. Bundestag (2021 - 2025)\\textsuperscript{*}", 8, 16, escape = FALSE) %>%
  footnote(general = glue("Auswertung von insgesamt {mdb_paragraphs %>% filter(words > 6) %>% nrow() %>% point_format()} Redeabschnitten von Bundestagsabgeordneten."), 
           general_title = "Anmerkung: ",
           number = c("Die Fraktion DIE LINKE hat zum 6. Dezember 2023 seine Auflösung beschlossen. Sie konnte keine Fraktion mehr bilden und hat sich in die Gruppen BSW und Die Linke aufgeteilt."),
           symbol = c("Laufende Legistlaturperiode -- ausgewertete Protokolle bis 31. März 2024."),
           footnote_as_chunk = FALSE, threeparttable = TRUE) %>%
  write_file("document/tables/overview_paragraphs.tex")

