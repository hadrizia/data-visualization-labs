join_all_data <- function() {
library("tidyverse")
library(here)

conferences <-
  read.csv(
    here("ethnicity-gender-cs/crawler/raw_data/conferences.csv"),
    header = FALSE,
    stringsAsFactors = FALSE
  )

names(conferences) <- c(
  "conf_global_key",
  "conf_key",
  "year",
  "publisher",
  "conf_title",
  "conf_link",
  "cs",
  "de",
  "se",
  "th"
)

papers <-
  read.csv(
    here("ethnicity-gender-cs/crawler/raw_data/papers.csv"),
    header = FALSE,
    stringsAsFactors = FALSE
  )
names(papers) <-
  c(
    "paper_global_key",
    "paper_key",
    "conf_key",
    "paper_link",
    "paper_title",
    "pages",
    "citations"
  )

authors <-
  read.csv(
    here("ethnicity-gender-cs/crawler/raw_data/authors.csv"),
    header = FALSE,
    stringsAsFactors = FALSE
  )
names(authors) = c("author_paper_key", "position", "full_name")

ethnicity <-
  read.csv(
    here("ethnicity-gender-cs/crawler/raw_data/ethnicity.csv"),
    header = FALSE,
    stringsAsFactors = FALSE
  )
names(ethnicity) = c("full_name", "l0", "l1", "l2", "gender")

ethnicity_gender <- conferences %>%
  inner_join(papers, by = "conf_key") %>%
  inner_join(authors, by = c("paper_key" = "author_paper_key")) %>%
  inner_join(ethnicity, by = "full_name")

return(ethnicity_gender)
}

process_dataframe <- function(
  joined_data = readr::read_csv(here::here("ethnicity-gender-cs/data/joined_raw_data.csv"))) {
  
  library(tidyverse)
  
  joined_data <- joined_data %>% 
    mutate(gender = case_when(gender == '-' ~ 'Not specified',
                              gender == 'F' ~ "Female",
                              gender == 'M' ~ "Male"))
  data <- joined_data %>%
    group_by(gender) %>% 
    # summarise(cs_sum = sum(cs), 
    #           de_sum = sum(de), 
    #           se_sum = sum(se), 
    #           th_sum = sum(th),
    #           total = sum(cs_sum, de_sum, se_sum, th_sum)) %>% 
    gather(area, area_value, cs, de, se, th) %>% 
    mutate(area = case_when(area == 'cs' ~ 'Computer Science',
                            area == 'de' ~ 'Data Engineering',
                            area == 'se' ~ 'Software Engineering',
                            area == 'th' ~ 'Theory',
                            TRUE ~ '')) %>% 
  select(gender, area, area_value) %>% 
  filter(area_value == 1)
  
  position_num_pages <- joined_data %>% 
    filter(gender == "Female") %>% 
    select(paper_key, paper_title) %>% 
    group_by(paper_key) %>% 
    mutate(num_mulheres = n())
  
  return(joined_data)
    
} 
