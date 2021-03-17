library(tidyverse)
library(rvest)
library(stringi)
require(handlr)
library(lubridate)
library(zoo)
library(ggtext)
library(showtext)

font_add(family = "lemonada", regular = "fonts/lemonada/static/Lemonada-Regular.ttf")
font_add(family = "GOT", regular = "fonts/Game-of-Thrones/Game of Thrones.ttf")
font_add(family = "Nanum", regular = "fonts/NanumMyeongjo/NanumMyeongjo-Regular.ttf")
font_add(family = "dark11", regular = "fonts/Dark11/DARK11__.ttf")
font_add(family = "Kalam", regular = "fonts/Kalam/Kalam-Regular.ttf")
font_add(family = "drakon", regular = "fonts/drakon/drakon.ttf")

showtext_auto()

# ev_mentions <- function(url) {
#   w <- read_html(url)
#   pathway_data_html <- html_nodes(w, xml)
#   ev <- html_text(pathway_data_html)
#   z <- str_c(ev, collapse = T) 
#   biden <- str_count(z, "Biden")
#   trump <- str_count(z, "Trump")
#   clinton <- str_count(z, "Clinton")
#   list(biden = biden, 
#        trump = trump, 
#        clinton = clinton,
#        length = str_count(z))
# }
# 
# xml <-  ".top-box , .news-box li , p"

# ev_time_period <- as_date(mdy("11-30-14"):today())
# 
# year <- epiyear(ev_time_period)
# month <- months(ev_time_period, abbreviate = T)
# day <- format.Date(ev_time_period, "%d")
# urls <- glue::glue("https://www.electoral-vote.com/evp{year}/Pres/Maps/{month}{day}.html")
# url_senate <- glue::glue("https://www.electoral-vote.com/evp{year}/Senate/Maps/{month}{day}.html")

z <- readRDS("data/all-z-plus-missing")

# z <- z %>% mutate(quarters = quarters(date))
# 
# missing <- z %>% 
#   filter(length < 200)
# missing_year <- epiyear(missing$date)
# missing_month <- months(missing$date, abbreviate = T)
# missing_day <- format.Date(missing$date, "%d")
# urls_senate <- glue::glue("https://www.electoral-vote.com/evp{missing_year}/Senate/Maps/{missing_month}{missing_day}.html")
# 
# z_missing <- urls_senate %>%
#   map_df(possibly(ev_mentions,
#                   list(biden = NA,
#                        trump = NA,
#                        clinton = NA))) %>%
#   bind_cols(date = missing$date)
# 
# z <- urls %>%
#   map_df(possibly(ev_mentions,
#                   list(biden = NA,
#                        trump = NA,
#                        clinton = NA))) %>%
#   bind_cols(date = ev_time_period)



z1 <- z %>% 
  filter(length > 200) %>% 
  mutate(trump_7 = rollapply(trump, width=7, mean, na.rm = TRUE, align = "center", fill = NA),
         biden_7 = rollapply(biden, width=7, mean, na.rm = TRUE, align = "center", fill = NA),
         clinton_7 = rollapply(clinton, width=7, mean, na.rm = TRUE, align = "center", fill = NA)) %>% 
  mutate(trump_7 = ifelse(length < 200, NA, trump_7),
         biden_7 = ifelse(length < 200, NA, biden_7),
         clinton_7 = ifelse(length < 200, NA, clinton_7)) %>% 
  mutate(trump = ifelse(length < 200, NA, trump),
         biden = ifelse(length < 200, NA, biden),
         clinton = ifelse(length < 200, NA, clinton)) %>% 
  select(-length)

z2 <- z1 %>% 
  pivot_longer(-c(date, trump, biden, clinton), 
               names_to = "candidate_7", 
               values_to = "mentions_7") %>% 
  select(-c(trump, clinton, biden))

z3 <- z1 %>% 
  pivot_longer(-c(date, trump_7, biden_7, clinton_7), 
               names_to = "candidate", 
               values_to = "mentions") %>% 
  select(-c(trump_7, clinton_7, biden_7)) %>% 
  left_join(z2)

colour_stripe <- c("grey20", "grey30", "grey40", "grey50")
date_range_matrix <- matrix(as.numeric(seq.Date(from = min(z$date), 
                                                to = max(z$date), by = "quarter")), 
                            ncol = 2, byrow = TRUE)
date_range_df <- tibble::tibble(start = zoo::as.Date.numeric(date_range_matrix[,1]), 
                                end = zoo::as.Date.numeric(date_range_matrix[, 2]))
date_breaks <- c(date_range_df$start, date_range_df$end)
candidate_colours <- c("#bfd200", "#046c9a", "#972D15")
plot_title <- glue::glue("Daily EV Mentions for <i style= 'font-family: forum; color:{candidate_colours[1]}; font-size: 40px;'>Clinton</i>, ",
                         "<i style='font-family: Kalam; color:{candidate_colours[2]}; font-size: 40px;'>Biden</i>, ",
                         "and <b><i style = 'font-family:drakon; color:{candidate_colours[3]}; font-size: 40px;'>Trump</i></b>")

z3 %>% ggplot(aes(date, mentions, colour = candidate)) + 
  geom_line(aes(y=mentions_7, colour = candidate_7), show.legend = F, size = 1.5) +
  scale_color_manual(values = c(clinton_7 = candidate_colours[1], 
                                biden_7 = candidate_colours[2], 
                                trump_7 = candidate_colours[3])) +
  scale_x_yearqtr(breaks = date_breaks, 
                  lab = format(date_breaks, 
                               ifelse(month(date_breaks) == 11, "%b\n        %Y", "%b"))) +
  geom_point(data = z1, aes(y = trump), 
             colour = candidate_colours[3], 
             size = 0.5, 
             alpha = 0.5) +
  theme_minimal() +
  geom_rect(data = date_range_df, 
            aes(xmin = start, xmax = end,
                ymin = -Inf, ymax = Inf), 
            inherit.aes = FALSE,
            alpha = 0.4,
            fill = "grey90") +
  coord_cartesian(ylim = c(0, 80)) +
  labs(title = plot_title,
       x = "",
       y = "",
       caption = glue::glue("<i><span style = 'color:{candidate_colours[3]};'> &bull; </span></i>'s are Trump daily counts, lines are 7 day rolling means")) +
  theme(plot.title = element_markdown(size = 28, hjust = 0.5),
        text = element_text(family = "Ink Free", size = 18),
        plot.caption = element_markdown())

 saveRDS(z, "data/all-z-plus-missing")



 