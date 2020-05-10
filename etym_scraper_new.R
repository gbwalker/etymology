
library(tidyverse)
library(stringr)
library(rvest)
library(utils)
library(plyr)

# Make an empty df for all of the scraped data.
df <- tibble(url = NA, word = NA, pos = NA, def = NA, italics = NA, related_words = NA) %>%
  na.omit()

# Seed word.
# First 20k: 'em
word = "'em"

# Start the clock.
start_time <- proc.time()

# Run for 10,000 words.
while (TRUE) {
  
  # Print a status update.
  print(word)
  
  # Create a URL.
  url <- URLencode(paste0("https://www.etymonline.com/word/", word))
  
  # Manually replace words with a slash.
  if (str_detect(word, "/")) {
    url <- URLencode(paste0("https://www.etymonline.com/word/", str_replace(word, "/", "%2F")))
  }
  
  # Read in the HTML data.
  page <- read_html(url)
  
  ###############################
  # Grab the content of interest.
  ###############################
  
  # Get the definition.
  def <- html_text(html_nodes(page, ".word__defination--2q7ZH"))
  
  pos <- str_replace(str_extract(html_text(html_nodes(page, ".word__name--TTbAA")), "\\([a-z]+"), "\\(", "")
  
  # Get the words to italicize later.
  italics <- paste0(html_text(html_nodes(page, ".foreign.notranslate")), collapse = ",")
  
  # Get related words.
  related_word_list <- html_text(html_nodes(page, ".related__word--3Si0N"))
  
  # If the list is too long, only use the set displayed on the page without the extra link.
  if (any(str_detect(related_word_list, "See all"))) {
    related_words <- paste0(related_word_list[1:length(related_word_list) - 1], collapse = ",")
  } else {
    related_words <- paste0(related_word_list, collapse = ",")
  }
  
  # Add new data to df
  new_data <- tibble(url = url, word = word, pos = pos, def = def, italics = italics, related_words = related_words)
  df <- rbind(df, new_data)
  
  # Get the next word.
  next_word_list <- html_text(html_nodes(page, ".alphabetical__word--3UeP2"))
  
  # Check the list of close links at the bottom.
  # Find the current word, and go to the next one.
  place <- match(word, next_word_list)
  
  # If the word is the last one in the list, break.
  if (length(next_word_list) == place[1]) {
    break
  }
  
  # Otherwise, continue with the next word.
  word <- next_word_list[place + 1]
}

# End the clock.
elapsed_time <- proc.time() - start_time
elapsed_mins <- as.integer(elapsed_time[3]) / 60
print(paste0(elapsed_mins, " minutes elapsed"))
print(paste0(elapsed_time[3] / nrow(df), " seconds per word, ", nrow(df), " words"))

# Save the result.
saveRDS(df, "df.Rds")
write_excel_csv(df, "df.csv")

##################
# Process results.
##################

# Pull out a date.

extract_date <- function(def) {
  
  long_date <- str_extract(def, "\\d\\d\\d\\d")
  short_date <- str_replace(str_extract(def, "\\d\\dc"), "c", "")
  
  long_loc <- str_locate(def, long_date)[1]
  short_loc <- str_locate(def, short_date)[1]
  
  # Use the four-digit date if it occurs first.
  first_date <- case_when(
    long_loc < short_loc | is.na(short_loc) ~ long_date,
    short_loc < long_loc | is.na(long_loc) ~ short_date,
    long_loc == short_loc ~ long_date
  )
  
  # Otherwise, use the short date.
  if (!is.na(first_date) & !is.na(short_date)) {
    if (first_date == short_date) {
      first_date <- case_when(
        str_detect(def, paste0("early ", first_date)) ~ paste0(as.integer(first_date) - 1, "00"),
        str_detect(def, paste0("mid-", first_date)) ~ paste0(as.integer(first_date) - 1, "50"),
        str_detect(def, paste0("late ", first_date)) ~ paste0(as.integer(first_date) - 1, "75"),
        TRUE ~ paste0(first_date, "00")
      )
    }
  }
  
  return(first_date)
}

# Create the new date column.
dates <- map(df$def, extract_date)
df$date <- unlist(dates)

# Reassign the part of speech column.
df$pos <- mapvalues(df$pos, 
          from = c("pron", "prep", "s", "adv", "n", "v", "adj", "interj", "conj", "i", "pl", "a", "objective", "possessive", "h", "article"),
          to = c("pronoun", "preposition", "root", "adverb", "noun", "verb", "adjective", "interjection", "conjunction", "root", "noun", "root", "objective", "possessive", "root", "article")
          )

# Convert date to integer.
df <- df %>% 
  mutate(date = as.integer(date))

# Pull out origin languages.
languages <- read_csv('languages.csv')
languages <- c(languages$list)

extract_languages <- function(def) {
  all_capitals <- unlist(str_extract_all(def, '[A-Z][a-z]+'))
  
  other_roots <- c("Proto-Indo-European",
                   "Old English",
                   "Middle English",
                   "Middle High German",
                   "Old High German",
                   "Old Norse",
                   "Middle Welsh",
                   "Old Church Slavonic",
                   "Serbo-Croatian",
                   "British English",
                   "American English",
                   "Proto-Germanic",
                   "Gothic",
                   "PIE",
                   "Old French",
                   "Frankish",
                   "Old Irish",
                   "Middle Dutch")
  
  manual_extraction <- unlist(lapply(def, str_extract, other_roots))
  manual_extraction <- unlist(lapply(manual_extraction, function(x) x[!is.na(x)]))
  manual_extraction <- str_replace(manual_extraction, "PIE", "Proto-Indo-European")
  
  # Find single-word languages in the language list.
  overlaps <- Reduce(intersect, list(languages, all_capitals))
  
  return (paste0(c(overlaps, manual_extraction), collapse = ","))
}

roots <- map(df$def, extract_languages)
df$roots <- unlist(roots)

# Combine duplicate entries.
duplicate_entries <- df %>% 
  filter(duplicated(df$word)) %>% 
  select(word) %>%
  c()

# Combine definitions, italics, related_words, and roots.
combine_rows <- function(col, separator = ",") {
  x <- unique(unlist(lapply(col, str_split, separator)))
  x <- paste0(unlist(lapply(x, str_trim)), collapse = separator)
  return (x)
}

for (entry in duplicate_entries$word) {
  
  # Get duplicate entries.
  df_duplicate <- df %>% filter(word == entry)
  
  # Remove existing entries.
  df <- df %>% filter(word != entry)
  
  defs <- combine_rows(df_duplicate$def, ";;")
  italics <- combine_rows(df_duplicate$italics)
  related_words <- combine_rows(df_duplicate$related_words)
  roots <- combine_rows(df_duplicate$roots)

  # Get the first row and assign new values.
  df_duplicate <- df_duplicate[1,]
  df_duplicate$def[1] <- defs
  df_duplicate$italics[1] <- italics
  df_duplicate$related_words[1] <- related_words
  df_duplicate$roots[1] <- roots
  
  # Add back in the extra row.
  df <- rbind(df, df_duplicate)
}

# saving the de-duplicated df.
saveRDS(df, "df_final_dedupe.rds")

########################
# Save the final version.
saveRDS(df, "df_final.Rds")
write_excel_csv(df, "df_final.csv")

