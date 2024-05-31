setwd("d:\\Giovanni\\Downloads\\Nobel")

library(readxl)
library(tidyverse)
library(magrittr)
library(writexl)

### FINDING THOSE WHO WERE MOST NOMINATED BUT DIDN'T WIN ----

## Getting the Physics laureates
física <- read_excel('Laureates - Physics.xlsx') %>%
  select(-País) %>% # Removing country column
  filter(!is.na(Nome)) %>% # Removing NAs from the 'Nome' column
  fill(everything(), .direction = "down") # Replacing NAs for year

# Removing unwanted characters
física %<>%
  mutate(Nome = str_replace_all(Nome, c(", Jr." = "",
                                        ", Jr" = "",
                                        " Jr." = "",
                                        " Jr" = "",
                                        "Sir " = "",
                                        "-" = " ")),
         Nome = replace(Nome, Nome == 'William Henry Bragg', 'Henry Bragg'),
         Nome = replace(Nome, Nome == 'William Lawrence Bragg', 'Lawrence Bragg'),
         Nome = replace(Nome, Nome == 'J. Hans D. Jensen', 'Johannes Jensen'),
         Nome = replace(Nome, Nome == 'Maria Curie', 'Marie Curie'),
         Nome = replace(Nome, Nome == 'Antoine Henri Becquerel', 'Henri Becquerel'))

# Getting only first and last name
física %<>%
  mutate(Nome = str_split(Nome, " ") %>%
           map_chr(~ paste(.x[1], .x[length(.x)])),
         Ano.f = Ano) %>%
  select(-Ano)

## Getting the Chemistry laureates
química <- read_excel('Laureates - Chemistry.xlsx') %>%
  filter(!is.na(Nome)) %>% # Removing NAs from the 'Nome' column
  fill(everything(), .direction = "down") # Replacing NAs for year

# Removing unwanted characters
química %<>%
  mutate(Nome = str_replace_all(Nome, c(", Jr." = "",
                                        ", Jr" = "",
                                        " Jr." = "",
                                        " Jr" = "",
                                        "Sir " = "",
                                        "-" = " ")))

# Getting only first and last name
química %<>%
  mutate(Nome = str_split(Nome, " ") %>%
           map_chr(~ paste(.x[1], .x[length(.x)])),
         Ano.q = Ano) %>%
  select(-Ano)

## Getting the list of all nominees
base <- read_excel('Nominees.xlsx') %>%
  fill(everything(), .direction = "down") # Replacing NAs for year

# Removing unwanted characters
base %<>%
  mutate(Nome = str_replace_all(Nome, c(", Jr." = "",
                                        ", Jr" = "",
                                        " Jr." = "",
                                        " Jr" = "",
                                        "Sir " = "",
                                        "-" = " ")),
         Nome = replace(Nome, Nome == 'William Henry Bragg', 'Henry Bragg'),
         Nome = replace(Nome, Nome == 'William Lawrence Bragg', 'Lawrence Bragg'))

# Getting only first and last name
base %<>%
  mutate(Nome = str_split(Nome, " ") %>%
           map_chr(~ paste(.x[1], .x[length(.x)])))

# Changing some names
base %<>%
  mutate(Nome = replace(Nome, Nome == 'A Bohr', 'Aage Bohr'),
         Nome = replace(Nome, Nome == 'John Rayleigh', 'Lord Rayleigh'),
         Nome = replace(Nome, Nome == 'William Kelvin', 'Lord Kelvin'),
         Nome = replace(Nome, Nome == 'Prince Broglie', 'Louis Broglie'),
         Nome = replace(Nome, Nome == 'Patrick Blackett)', 'Patrick Blackett'),
         Nome = replace(Nome, Nome == 'Subramanyan Chandrasekhar', 'Subrahmanyan Chandrasekhar'),
         Nome = replace(Nome, Nome == 'Ferdinand Braun', 'Karl Braun'),
         Nome = replace(Nome, Nome == 'Aleksandr [Prockorov]', 'Alexander Prokhorov'))

# Getting all the nominees ranked
agregado <- base %>%
  group_by(Nome) %>%
  count()

write_xlsx(agregado, path = "agregado.xlsx")

# Findind the most wronged
injustiçados <- left_join(agregado, física, by = c('Nome')) %>%
  left_join(química, by = c('Nome')) %>%
  filter(is.na(Ano.f), is.na(Ano.q)) %>% # Selecting only those who did not win the Nobel Prize
  arrange(desc(n)) %>% # Rearranging from largest to smallest
  select(-c(Ano.f, Ano.q)) # Removing year column

write_xlsx(injustiçados, path = "injustiçados.xlsx")

### FINDING THOSE WHO WERE MOST NOMINATED UNTIL WINNING ----

agregado <- base %>%
  group_by(Ano, Nome) %>% # Grouping by name and year
  count()

teste <- left_join(física, agregado, by = c('Nome')) %>%
  filter(!is.na(Ano)) %>%
  mutate(dummy = if_else(Ano.f >= Ano, 1, 0)) %>%
  filter(dummy == 1) %>% # Taking off nominations after winning the prize
  group_by(Nome) %>%
  summarize(Total = sum(n)) %>%
  arrange(desc(Total)) # Rearranging from largest to smallest

write_xlsx(teste, path = "mais_vezes.xlsx")

### FINDING THOSE WHO WERE MOST TIMES NOMINATED AFTER WINNING ----

teste <- left_join(física, agregado, by = c('Nome')) %>%
  filter(!is.na(Ano)) %>%
  mutate(dummy = if_else(Ano.f >= Ano, 1, 0)) %>%
  filter(dummy == 0) %>% # Taking off nominations before winning the prize
  group_by(Nome) %>%
  summarize(Total = sum(n)) %>%
  arrange(desc(Total)) # Rearranging from largest to smallest

write_xlsx(teste, path = "mais_vezes_pós.xlsx")

### FINDING THOSE WHO WON BUT DIDN'T WERE NOMINATED ----

teste <- left_join(física, agregado, by = c('Nome')) %>%
  filter(Ano.f <= 1970,
         is.na(Ano))
