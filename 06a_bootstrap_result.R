library(boot)
library(tidyverse)

weighted_sample_size <- calculations <- tibble::tribble(
  ~topic,  ~data_size,  ~sample_size,
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

raters_data <- read_csv("data/crowdbucket/raters/swipes_data.csv")
authors_data <- read_csv("data/crowdbucket/authors/swipes_data.csv")
data <- read_csv("data/clean/models/topic_export_300.csv") %>%
  select(topic = Topic, label = CustomName) %>%
  distinct() %>%
  mutate(label = if_else(label == "Politik und Verantwortung", "Verantwortungsvolle Politik", label))

weighted_topics <- weighted_sample_size %>% 
  left_join(data, by = "topic")

data <- authors_data %>%
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
  summarise(mean = mean(rating), n = n(), label = first(item_label)) %>%
  left_join(weighted_topics, by = "label")


# weighted mean
weighted_mean <- function(data, indices) {
  sample_data <- data[indices, ]
  return(weighted.mean(sample_data$mean, sample_data$sample_size))
}

# bootstrap with weighted means
stratified_bootstrap <- function(data, R) {
  boot_means <- numeric(R)
  
  for (i in 1:R) {
    sample_data <- data %>%
      group_by(topic) %>%
      sample_n(size = first(sample_size), replace = TRUE) %>%
      ungroup()
    
    # calculation of weighted mean
    boot_means[i] <- weighted_mean(sample_data, 1:nrow(sample_data))
  }
  
  return(boot_means)
}

# number of bootstrap replications
R <- 1000

bootstrap_results <- stratified_bootstrap(data, R)

# calculation of 95% confidence interval
quantile(bootstrap_results, probs = c(0.025, 0.975))

# calculation of weighted mean
weighted.mean(data$mean, data$sample_size)

