library(plyr)
library(tidyverse)
library(lubridate)
library(kableExtra)
library(vroom)
library(RNOmni)
library(scales)
library(glue)

# Import Data and Convert to CSV
# data <- vroom("data/WCA_export144_20211220T040002Z.tsv/WCA_export_Results.tsv", 
#                  col_select = c("eventId", "personId", "best", "average"))
# data2 <- data %>%
#   pivot_longer(-c(eventId:personId)) %>%
#   filter(value > 0) %>%
#   group_by(eventId, personId, name) %>%
#   summarize(min = min(value), .groups = "drop") %>%
#   mutate(min = min/100)
# 
# write_csv(data2, "data/wca_2020-12-20.csv")
data2 <- vroom("data/wca_2020-12-20.csv")

# Calculate ranks for each event
data3 <- data2 %>%
  group_by(eventId, name) %>%
  dplyr::mutate(x = RNOmni::RankNorm(min)) %>%
  dplyr::mutate(rank = cut(x, breaks = c(-Inf, seq(-3, 3, 0.3), Inf),
     labels = c("Master",
                paste0("D", 1:5),
                paste0("P", 1:5),
                paste0("G", 1:5),
                paste0("S", 1:5),
                "Bronze"))) %>%
  ungroup()

# Round numbers
data4 <- data3 %>%
  group_by(eventId, name, rank) %>%
  dplyr::summarize(lo = plyr::round_any(min(min), 0.1), 
                   hi = plyr::round_any(max(min), 0.1),
                   n = n(), .groups = "drop") %>%
  group_by(eventId, name) %>%
  dplyr::mutate(pct = n/sum(n)) %>%
  ungroup() %>%
  mutate(range = glue("{ lo }-{ hi }"))

# Extract % from 333 (used to approximate other events)
pct <- data4 %>%
  filter(eventId == "333" & name == "best") %>%
  mutate(cumsum = cumsum(pct),
         cumsum = percent(cumsum, accuracy = 0.1),
         pct = percent(pct, accuracy = 0.1)) %>%
  select(`%` = pct,
         Total = cumsum)

# Pivot wider
data4b <- data4 %>%
  select(eventId, name, rank, range) %>%
  pivot_wider(names_from = "name", values_from = "range") %>%
  setNames(c("Event", "Rank", "Average", "Single"))

# Format columns
data5 <- data4b %>%
  filter(!str_detect(Event, "bf")) %>%
  filter(!str_detect(Event, "mbo")) %>%
  rename(Avg = Average) %>%
  pivot_wider(names_from = "Event", values_from = c("Avg", "Single"),
              names_glue = "{.value} {Event}") %>% # {.value} {Event}
  add_column(pct, .after = "Rank")
newnames <- str_replace_all(names(data5), "Avg ", "")
newnames2 <- str_replace_all(newnames, "Single ", "")
data6 <- data5 %>%
  setNames(newnames2)

# Format to HTML table
data6 %>%
  kbl(caption = "WCA Ranking System by Mati & OtterCuber") %>%
  kable_classic(full_width = T, html_font = "Cambria") %>%
  add_header_above(c(" " = 3, "Average" = 16, "Single" = 16)) %>%
  kable_styling(bootstrap_options = "striped", font_size = 8) %>%
  row_spec(1, background = "#dff5f2") %>%
  row_spec(2:6, background = "#ffd1d0") %>%
  row_spec(7:11, background = "#f0ede5") %>%
  row_spec(12:16, background = "#cdefe0") %>%
  row_spec(17:21, background = "#8cc0c3") %>%
  row_spec(22, background = "#c4eda9") %>%
  column_spec(19, border_right = T) %>%
  footnote(general = "Using a 2021-12-20 export of the WCA data, the rank-based invese normal transform was applied to the best single and average times of all cubers per event. The 'Master' and 'Bronze' ranks are defined as less than or greater than three standard deviations from the mean, respectively, and the Diamond (D), Platinum (P), Gold (G), and Silver (S) ranks defined as successive 0.3 standard deviation intervals, starting from -3.0 to 2.7 to 2.7 and 3.0 standard deviations. For example, the `Diamond 1 (D1)` rank consists of cubers with a time between -3.0 and -2.7 standard deviations from the mean. The `%` column is an approximate (there are slight variations per event) indication of the percentage of cubers with times within the rank criteria, and the `Total` column is its cumulative sum.") %>%
  save_kable(file = "wca_ranking_system.html", self_contained = T)