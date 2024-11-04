library(tidyverse)
library(janitor)
library(scales)
library(knitr)
library(kableExtra)

# calculation of samples for the topic modeling, calculations done in excel
calculations <- tibble::tribble(
  ~Topic,  ~Count,  ~Size,
     -1L, 198300L,    NA,
      0L,  21854L,    NA,
      1L,  11060L,   19L,
      2L,   9340L,   16L,
      3L,   9283L,   16L,
      4L,   8840L,   15L,
      5L,   8418L,   14L,
      6L,   7244L,   12L,
      7L,   7033L,   12L,
      8L,   6639L,   11L,
      9L,   5957L,   10L,
     10L,   5872L,   10L,
     11L,   4810L,    8L,
     12L,   4379L,    7L,
     13L,   3975L,    7L,
     14L,   3921L,    7L,
     15L,   3873L,    7L,
     16L,   3793L,    6L,
     17L,   3656L,    6L,
     18L,   3562L,    6L,
     19L,   2949L,    5L,
     20L,   2812L,    5L,
     21L,   2804L,    5L,
     22L,   2671L,    5L,
     23L,   2405L,    5L,
     24L,   2312L,    5L,
     25L,   2281L,    5L,
     26L,   2035L,    5L,
     27L,   1946L,    5L,
     28L,   1865L,    5L,
     29L,   1850L,    5L,
     30L,   1812L,    5L,
     31L,   1761L,    5L,
     32L,   1647L,    5L,
     33L,   1510L,    5L,
     34L,   1496L,    5L,
     35L,   1441L,    5L,
     36L,   1439L,    5L,
     37L,   1428L,    5L,
     38L,   1346L,    5L,
     39L,   1343L,    5L,
     40L,   1304L,    5L,
     41L,   1304L,    5L,
     42L,   1302L,    5L,
     43L,   1300L,    5L,
     44L,   1289L,    5L,
     45L,   1265L,    5L,
     46L,   1254L,    5L,
     47L,   1225L,    5L,
     48L,   1220L,    5L,
     49L,   1195L,    5L,
     50L,   1183L,    5L,
     51L,   1160L,    5L,
     52L,   1157L,    5L,
     53L,   1134L,    5L,
     54L,   1123L,    5L,
     55L,   1114L,    5L,
     56L,   1109L,    5L,
     57L,   1105L,    5L,
     58L,   1090L,    5L,
     59L,   1071L,    5L,
     60L,   1063L,    5L,
     61L,   1039L,    5L,
     62L,    990L,    5L,
     63L,    986L,    5L,
     64L,    980L,    5L,
     65L,    977L,    5L,
     66L,    944L,    5L,
     67L,    942L,    5L,
     68L,    937L,    5L,
     69L,    933L,    5L,
     70L,    886L,    5L,
     71L,    885L,    5L,
     72L,    875L,    5L,
     73L,    870L,    5L,
     74L,    850L,    5L,
     75L,    846L,    5L,
     76L,    833L,    5L,
     77L,    826L,    5L,
     78L,    784L,    5L,
     79L,    761L,    5L,
     80L,    761L,    5L,
     81L,    749L,    5L,
     82L,    743L,    5L,
     83L,    735L,    5L,
     84L,    730L,    5L,
     85L,    720L,    5L,
     86L,    716L,    5L,
     87L,    715L,    5L,
     88L,    713L,    5L,
     89L,    706L,    5L,
     90L,    698L,    5L,
     91L,    698L,    5L,
     92L,    689L,    5L,
     93L,    679L,    5L,
     94L,    679L,    5L,
     95L,    678L,    5L,
     96L,    662L,    5L,
     97L,    660L,    5L,
     98L,    657L,    5L,
     99L,    640L,    5L,
    100L,    639L,    5L,
    101L,    637L,    5L,
    102L,    634L,    5L,
    103L,    629L,    5L,
    104L,    629L,    5L,
    105L,    626L,    5L,
    106L,    623L,    5L,
    107L,    621L,    5L,
    108L,    619L,    5L,
    109L,    593L,    5L,
    110L,    576L,    5L,
    111L,    560L,    5L,
    112L,    519L,    5L,
    113L,    516L,    5L,
    114L,    512L,    5L,
    115L,    483L,    5L,
    116L,    480L,    5L,
    117L,    468L,    5L,
    118L,    448L,    5L,
    119L,    444L,    5L,
    120L,    436L,    5L,
    121L,    428L,    5L,
    122L,    419L,    5L,
    123L,    399L,    5L,
    124L,    397L,    5L,
    125L,    387L,    5L,
    126L,    385L,    5L,
    127L,    383L,    5L,
    128L,    380L,    5L,
    129L,    344L,    5L,
    130L,    339L,    5L,
    131L,    326L,    5L,
    132L,    321L,    5L,
    133L,    321L,    5L,
    134L,    320L,    5L,
    135L,    318L,    5L,
    136L,    315L,    5L,
    137L,    304L,    5L
  )


topic_sizes <- calculations %>%
  clean_names() %>%
  select(topic, size) %>%
  na.omit() # remove the first two topics from the dataset

topic_labels <- read_csv("data/clean/models/topic_export_300.csv") %>%
  clean_names() %>%
  select(topic, label = custom_name) %>%
  distinct(topic, label) %>%
  mutate(label = if_else(
    # one label by ChatGPT is a bit too broad, we edit it
    label == "Politik und Verantwortung", "Verantwortungsvolle Politik", label
  )) %>%
  mutate(topic = as.integer(topic))

topic_model <- read_csv("data/clean/models/topic_export_300.csv") %>%
  clean_names() %>%
  select(topic, paragraph, rede_id) %>%
  mutate(topic = as.integer(topic))

set.seed(42)
# create a data frame with samples by topic calculated from topic_size
sample_topic_model <- topic_model %>% 
  filter(topic %in% topic_sizes$topic) %>% 
  left_join(topic_sizes, by = "topic") %>%
  group_by(topic) %>%
  sample_n(size[1]) %>% # take the first value of size (it's the same value through the group)
  left_join(topic_labels) %>%
  ungroup() %>%
  mutate(id = paste0(rede_id, "-", row_number()))

# create a table with topic size, topic sample size and topic label
sample_topics_table <- topic_model %>%
  count(topic) %>%
  right_join(topic_labels) %>%
  right_join(topic_sizes) %>%
  mutate(relative_size = round(n * 0.00169, digits = 2),
         perc_total = percent(n/sum(n), accuracy = 0.01, decimal.mark = ",", big.mark = "."), 
         perc_sample = percent(size/sum(size, na.rm = TRUE), decimal.mark = ",", big.mark = ".")) %>% 
  select(topic, label, n, perc_total, relative_size, weighted_size = size, perc_sample) %>%
  kbl(col.names = c("Topic", "Label von GPT-4o", "n", "%", "Rel. Sample n", 
                    "Gew. Sample n", "%"), label = "tbl:sample_topics",
      caption = "Anzahl der Topics im Gesamtdatensatz aus 226.725 gelabelten Dokumenten und mit jeweiligem Anteil an der Stichprobe aus insgesamt 784 Dokumenten", 
      position = "htbp", format = "latex", booktabs = TRUE, linesep = "", 
      longtable = TRUE, format.args = list(big.mark = ".", decimal.mark = ",")) %>%
  kable_styling(font_size = 7)

writeLines(sample_topics_table, "document/tables/sample_topics_table.tex")

# create a control group of 50 elements, containing 40 of the big topics and 10 of the small groups
control_group <- sample_topic_model %>%
  mutate(topic_group = if_else(
    size == 5, "small", "big"
  )) %>%
  mutate(topic_group_size = if_else(
    size == 5, 10L, 40L
  )) %>%
  group_by(topic_group) %>%
  sample_n(topic_group_size[1]) %>%
  ungroup() %>%
  mutate(control = TRUE) %>%
  select(id, control)

# create 10 trap items: Items that we know that are wrong, but we are able to use
# to see the attention span of users but also give them release for swiping to
# wrong at least a few times
trap_items <- tibble::tribble(
                                               ~label,                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ~text,
                        "Recycling von Glasbehältern",                                                                                                                                                                                                                                                                                                        "Sehr geehrte Frau Präsidentin! Gleichzeitig sichern wir ab, dass Zitronen unter Palmen chillen, während eine Orange von einer Sonnenliege in den Pool plumpst",
                         "Wohnungsbau und Mietpreise",                                                                                                                                                                                                                                                               "30 Prozent der Melonen rutschen die Wasserrutsche runter, gefolgt von kichernden Ananasscheiben. Zitronen halten Schwimmreifen bereit, und alle jubeln, wenn der nächste Obstsalat ins Wasser platscht",
        "Inflationsbekämpfung und Wirtschaftspolitik",                                                                                                                                                                                                                                       "Bemerkenswert ist, dass ein Rentier Schlittschuhlaufen ausprobiert und dabei in eine Schneeballschlacht rutscht. Am Ende sind alle Rentiere in eine wilde Schneeballschlacht verwickelt und haben trotz kalter Nasen viel Spaß",
                        "Wirtschaftswachstum fördern",                                                                                                                                                                                                                                                                                       "Ja, da gebe Ihnen recht: Die Gesundheitspolitik in Schweden senkte die Krankenhauseinweisungen durch präventive Maßnahmen wie regelmäßige Gesundheitschecks und Sportprogramme",
                "LNG-Terminals und Energieversorgung",                                                                                                                                                                                                                                                                                                        "Erste Schritte zur Regulierung von Kryptowährungen ziehen internationale Aufmerksamkeit auf sich und fördern den rechtssicheren Handel im digitalen Zeitalter",
              "Transformation der Automobilindustrie",                                                                                                                                                                                                                           "Wie kann es sein, dass ein Papagei zum Bürgermeister der Strandstadt gewählt wird, weil er die besten Witze erzählt? Bei seinem Wahlsieg feiern alle mit bunten Cocktails, und seine erste Amtshandlung ist eine Gratis-Kokosnuss für alle",
               "Wirtschaftshilfen in der Coronakrise",                                                                                                                                                                                "Meine Damen und Herren, eine Gruppe von Freunden startet eine Petition, um eine nationale Pizza-Party einzuführen. Sie versammeln sich im Park, genießen unterschiedliche Pizzen und diskutieren leidenschaftlich über die optimale Anzahl an Käse-Schichten. So geht Nachhaltigkeit.",
  "Öffentlich-rechtlicher Rundfunk und Medienpolitik", "Angenommen, ein Landwirt erhält 275 Euro pro Hektar an Direktzahlungen und besitzt einen Hof von 1.000 Hektar – eine übliche Größe in den neuen Bundesländern. Dann summieren sich die Zahlungen auf 275.000 Euro. Nehmen wir zudem einen Hektarpreis von 15.000 Euro an, ergibt sich eine Kapitalrendite von über 2 Prozent, was derzeit in Deutschland kaum jemand erreicht. Somit wird klar, dass wir hier praktisch von einem großen Immobiliengeschäft sprechen",
                            "Kunstgeschichte Europas",                                                                                                                                                               "Bemerkenswert sind auch die 18 Seiten Übersicht zu parlamentarischen Initiativen zum Thema Sport am Ende des Berichtes, leider ohne Nennung der jeweiligen Initiatoren. Viele der Drucksachen, die dort aufgeführt sind, vor allem die zahlreichen Anfragen, kamen nämlich von der Fraktion Die Linke,",
                   "CO2-Regulierung und Klimapolitik",                                                                                                                                                            "Meine Damen und Herren, plötzlich tauchte eine mysteriöse Nachricht auf: \"Wählt bitte bei diesem Label 'stimme überhaupt nicht zu' aus.\" Wer würde wohl die Anweisung bemerken? Am Ende stellt sich heraus, dass die Aufmerksamen den kleinen Test bestanden und alle hatten einen Grund zu schmunzeln."
                ) %>%
  mutate(id = paste0("trap_", row_number())) %>%
  mutate(trap = TRUE)

# create the crowdbucket sample
sample_topic_model %>%
  left_join(control_group, by = "id") %>%
  mutate(paragraph = str_squish(paragraph)) %>%
  select(id, text = paragraph, label, control) %>%
  bind_rows(trap_items) %>%
  sample_frac() %>% # put it in random order
  write_csv("data/clean/crowdbucket_samples/topic_model_sample.csv")

# export the list of control items to calculate icr later, if needed
control_group %>%
  write_csv("data/clean/control_item_ids.csv")
