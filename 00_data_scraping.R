library(tidyverse)
library(xml2)
library(rvest)

# download general mdb data
link_zip <- "https://www.bundestag.de/blob/472878/e207ab4b38c93187c6580fc186a95f38/mdb-stammdaten-data.zip"
dir.create("data/raw/mdb", recursive = TRUE)
download.file(link_zip, file.path("data/raw/mdb", basename(link_zip)))
unzip("data/raw/mdb/mdb-stammdaten-data.zip", exdir = "data/raw/mdb/")

# check the max number of protocols in the 19th bundestag
bt_19_website <- "https://www.bundestag.de/ajax/filterlist/de/services/opendata/543410-543410"
bt_20_website <- "https://www.bundestag.de/ajax/filterlist/de/services/opendata/866354-866354"
bt_19_last_protocol <- bt_19_website %>% 
  read_html() %>%
  xml_find_first("//strong") %>% 
  xml_text(trim = TRUE) %>%
  str_extract("\\d+")
bt_20_last_protocol <- bt_20_website %>%
  read_html() %>%
  xml_find_first("//strong") %>% 
  xml_text(trim = TRUE) %>%
  str_extract("\\d+")

prot_bt_19_websites <- paste0(bt_19_website, "?offset=", seq(0, bt_19_last_protocol, 5))
prot_bt_20_websites <- paste0(bt_20_website, "?offset=", seq(0, bt_20_last_protocol, 5))

# extract protocol links
get_prot_links <- function(x){
  x %>%
    read_html() %>%
    html_nodes(".bt-link-dokument") %>%
    html_attr("href") %>%
    paste0("https://www.bundestag.de", .)
}

# get all links to the protocols 19th Bundestag
prot_links <- map(prot_bt_19_websites, ~get_prot_links(.)) %>% unlist() %>% unique()
# download all protocols of the 19th Bundestag
dir.create("data/raw/19", recursive = TRUE)
prot_links %>% map(~download.file(., file.path("data/raw/19", basename(.))))

# get all links to the protocols 20th Bundestag
prot_links <- map(prot_bt_20_websites, ~get_prot_links(.)) %>% unlist() %>% unique()
# download all protocols of the 19th Bundestag
dir.create("data/raw/20", recursive = TRUE)
prot_links %>% map(~download.file(., file.path("data/raw/20", basename(.))))
