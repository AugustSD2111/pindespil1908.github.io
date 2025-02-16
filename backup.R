library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(forcats)
library(knitr)

sti_billeder <- "C:/Users/augus/Desktop/pindespil/Jelly/pindespil1908.github.io/assets/img/"

opdateret <- Sys.Date()

data <- tribble(
  ~spiller, ~alder, ~position, 
  
  "Dalby",      "Ung",         "Keeper",
  "Aland",      "Gammel",      "Keeper",
  "KC",         "Gammel",      "Keeper",
  "Konrad",     "Ung",         "Keeper",
  
  "Jogge",      "Gammel",      "Forsvar",
  "Kav",        "Gammel",      "Forsvar",
  "Claes",      "Gammel",      "Forsvar",
  "Lau",        "Ung",         "Forsvar",
  "Fey",        "Gammel",      "Forsvar",
  "August",     "Gammel",      "Forsvar",
  "Pat",        "Ung",         "Forsvar",
  "Albert",     "Ung",         "Forsvar",
  
  "Gustav",     "Gammel",      "Midtbane",
  "Max",        "Ung",         "Midtbane",
  "Tietze",     "Gammel",      "Midtbane",
  "Sjølle",     "Ung",         "Midtbane",
  "Olabi",      "Ung",         "Midtbane",
  
  "Praygod",    "Ung",         "Angreb",
  "Otman",      "Ung",         "Angreb",
  "Thomas",     "Ung",         "Angreb",
  "Alexander",  "Ung",         "Angreb",
  "Nick",       "Gammel",      "Angreb",
  "Haris",      "Ung",         "Angreb",
  "Agge",       "Gammel",      "Angreb",
  "Shimal",     "Gammel",      "Angreb",
  "Biele",      "Gammel",      "Angreb",
  "Mikkel",     "Gammel",      "Angreb",
  "Trane",      "Gammel",      "Angreb"
)

pinde <- tribble(
  ~id, ~dato, ~spiltype, ~trøjer, ~joker, 
  ~vindere, 
  ~tabere,
  
  1, "2025-01-21", "Interval", 1, 0, 
  c("August", "Pat", "Nick", "Mikkel", "Biele", "Max", "Aland", "Thomas", "Trane", "Albert", NA),
  c("Lau", "Fey", "Gustav", "Sjølle", "Alexander", "Agge", "Shimal", "Praygod", "Otman", "Olabi", "Dalby"),
  
  2, "2025-01-21", "Skud", 0, 0, 
  c("Lau", "Fey", "Gustav", "Sjølle", "Alexander", "Agge", "Shimal", "Praygod", "Otman", "Olabi", "Dalby"),
  c("August", "Pat", "Nick", "Mikkel", "Biele", "Max", "Aland", "Thomas", "Trane", "Albert", NA),
  
  3, "2025-01-23", "Interval", 0, 0, 
  c("August", "Fey", "Shimal", "Nick", "Tietze", "Claes", "Mikkel", NA, NA),
  c("Lau", "Pat", "Albert", "Max", "Sjølle", "Olabi", "Praygod", "Otman", "Thomas"),
  
  4, "2025-01-28", "Interval", 0, 0,
  c("Gustav", "Aland", "Sjølle", "Otman", "Biele", "Albert", "Mikkel", "Lau", "Shimal", "Agge", "Fey", "Thomas"),
  c("August", "Dalby", "Trane", "Max", "Olabi", "Praygod", "Tietze", "Jogge", "Pat", "Alexander", "Claes", NA),
  
  5, "2025-02-03", "4v4", 1,0,
  c("August", "Dalby", "Nick", "Olabi", "Trane", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  c("Aland", "KC", "Konrad", "Haris", "Sjølle", "Lau", "Pat", "Mikkel", "Shimal", "Alexander", "Thomas", "Claes", "Kav", "Praygod", "Albert"),
  
  6, "2025-02-04", "Interval", 1, 0,
  c("Lau", "Kav", "Mikkel", "Praygod", "Trane", "Alexander", "Fey"),
  c("August", "Thomas", "Nick", "Jogge", "Max", "Agge", "Haris")
  
  
  
  
)

###
# HUSK AT OPDATERE PLOTS.RMD OGSÅ!
##



funktion <- function(input) {
  pinde <- pinde %>% 
    unnest(cols = c(vindere, tabere)) %>% 
    mutate(dato = as.Date(dato, tryFormats = c("%Y-%m-%d")))
  
  forkert <- c(setdiff(pinde$tabere, data$spiller), setdiff(pinde$vindere, data$spiller)) %>% na.omit
  
  if (length(forkert) > 0) {
    stop("idiot")
  } else {
    message("godt")
  }
  pinde
}

pinde <- funktion(pinde)

png(filename = paste0(sti_billeder, "pinde_spiller.png"), width = 1000, height = 1200)
pinde %>% 
  filter(!is.na(vindere)) %>% 
  group_by(vindere) %>% 
  summarise(pinde = n()) %>% 
  left_join(data, by = c("vindere"="spiller")) %>% 
  arrange(desc(pinde)) %>% 
  ggplot(aes(fct_reorder(vindere, pinde), pinde))+ 
  geom_point(aes(shape = alder, fill = position, color=position), size =12)+
  geom_segment(aes(x = fct_reorder(vindere, pinde), xend = fct_reorder(vindere, pinde),
                   y = 0, yend = pinde, color = position),
               lwd = 4) +
  geom_text(aes(label = pinde), color = "white", size = 8) +
  scale_shape_manual(values = c(21, 22), name = " ") +
  geom_hline(yintercept = 0) +
  coord_flip() +  
  theme_minimal() + 
  scale_x_discrete(name = "")+
  #scale_y_continuous(breaks = seq(0,2))+
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size = 16)) +
  labs(subtitle= paste0("Sidst opdateret: ", opdateret)) + theme(plot.background = element_rect(fill = 'lightgrey', colour = 'black'))
dev.off()

# posistions graf
png(filename = paste0(sti_billeder, "position_graf.png"), width = 1200, height = 1000)
pinde %>%
  pivot_longer(cols = c(vindere, tabere),
               names_to = "source",  # A temporary column for identifying col1/col2
               values_to = "spiller") %>%
  mutate(pind = ifelse(source == "vindere", "sejre", "tabte")) %>%  # Assign 1 if from col1, 0 if from col2
  select(-source)  %>%
  left_join(data, by = c("spiller"="spiller")) %>%
  filter(!is.na(spiller)) %>%
  group_by(id, position, pind) %>%
  summarise(vundne = n()) %>%
  rbind(data.frame(id = c(3, 3),
                   position = c("Keeper", "Keeper"),
                   pind = c("sejre", "tabte"),
                   vundne = c(1,1))) %>%
  pivot_wider(names_from = pind,
              values_from = vundne, values_fill = list(vundne = 0)) %>%
  mutate(gennemsnit = sejre/(tabte+sejre)*100) %>%
  ungroup() %>%
  group_by(position) %>%
  mutate(gnm = cumsum(sejre)/(cumsum(tabte)+cumsum(sejre))*100) %>%
  ungroup() %>%
  arrange(as.numeric(id), position) %>%
  ggplot(aes(id, gennemsnit, color = position, fill = position)) +
  geom_point(size = 5, position = position_dodge(width = 0.2)) +
  geom_point(aes(y = gnm, group = position), shape = 23, size = 7, na.rm = TRUE, position = position_dodge(width = 0.2)) +
  geom_line(aes(y = gnm, group = position, linetype = position),size = 2, na.rm = TRUE, position = position_dodge(width = 0.2)) +
  theme_minimal()+
 # scale_x_continuous(breaks = c(1,2,3)) +
  labs(
    x = "Pindespilsnummer",  # Custom x-axis label
    y = "win%"  # Custom plot title
  ) +
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size = 16))+ theme(plot.background = element_rect(fill = 'lightgrey', colour = 'black'))


#+
  # geom_segment(aes(
  #   x = dodge,  # Adjust x to align with dodge
  #   xend = dodge,  # Adjust xend to align with dodge
  #   y = gennemsnit,
  #   yend = gnm,
  #   group = position),
  #   arrow = arrow(length = unit(0.15, "cm")),
  #   size= 0.3,
  #   data = ~.x %>% filter(abs(gennemsnit - gnm) > 5), # Add a threshold for arrow drawing
  #   position = position_dodge(width = 0.2),
  #   na.rm = TRUE)
dev.off()


# ggplot(aes(x = as.numeric(pindespil_id), y = gennemsnit, color = position)) +
#   geom_point(size = 3, position = position_dodge(width = 0.2)) +
#   geom_point(aes(y = gnm, group = position), shape = 0, size = 0, na.rm = TRUE, position = position_dodge(width = 0.2)) +
#   geom_smooth(aes(y = gnm, group = position, fill = position), method = "loess", se = F, na.rm = TRUE,
#               alpha = 0.2, position = position_dodge(width = 0.2)) +
#   theme_minimal() + theme(legend.position = "bottom")
# #
# test2 %>% 
#   filter(!is.na(position)) %>% 
#   group_by(pindespil_id, position, udfald) %>%
#   summarise(vundne = n(), .groups = "drop") %>%
#   rbind(data.frame(pindespil_id = c("8", "8"), 
#                    position = c("keeper", "keeper"), 
#                    udfald = c("tabende_hold", "vindende_hold"),
#                    vundne = c(0,0))) %>%
#   pivot_wider(names_from = udfald,
#               values_from = vundne, values_fill = list(vundne = 0)) %>% 
#   mutate(gennemsnit = vindende_hold/(tabende_hold+vindende_hold)*100) %>% 
#   ungroup() %>%
#   group_by(position) %>% 
#   mutate(gnm = cumsum(vindende_hold)/(cumsum(tabende_hold)+cumsum(vindende_hold))*100) %>% 
#   ungroup() %>% 
#   arrange(as.numeric(pindespil_id))

pinde %>%
  pivot_longer(cols = c(vindere, tabere),
               names_to = "source",  # A temporary column for identifying col1/col2
               values_to = "spiller") %>%
  mutate(pind = ifelse(source == "vindere", "sejre", "tabte")) %>%  # Assign 1 if from col1, 0 if from col2
  select(-source)  %>%
  left_join(data, by = c("spiller"="spiller")) %>%
  filter(!is.na(spiller)) %>%
  group_by(id, alder, pind) %>%
  summarise(vundne = n()) %>%
  # rbind(data.frame(id = c(3, 3),
  #                  position = c("Keeper", "Keeper"),
  #                  pind = c("sejre", "tabte"),
  #                  vundne = c(1,1))) %>%
  pivot_wider(names_from = pind,
              values_from = vundne, values_fill = list(vundne = 0)) %>%
  mutate(gennemsnit = sejre/(tabte+sejre)*100) %>%
  ungroup() %>%
  group_by(alder) %>%
  mutate(gnm = cumsum(sejre)/(cumsum(tabte)+cumsum(sejre))*100) %>%
  ungroup() %>%
  arrange(as.numeric(id), alder) %>%
  ggplot(aes(id, gennemsnit, color = alder, fill = alder)) +
  geom_point(size = 5, position = position_dodge(width = 0.2)) +
  geom_point(aes(y = gnm, group = alder), shape = 23, size = 7, na.rm = TRUE, position = position_dodge(width = 0.2)) +
  geom_line(aes(y = gnm, group = alder, linetype = alder),size = 2, na.rm = TRUE, position = position_dodge(width = 0.2)) +
  theme_minimal()+
  # scale_x_continuous(breaks = c(1,2,3)) +
  labs(
    x = "Pindespilsnummer",  # Custom x-axis label
    y = "win%"  # Custom plot title
  ) +
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size = 16))+ theme(plot.background = element_rect(fill = 'lightgrey', colour = 'black'))




##

pinde %>%
  pivot_longer(cols = c(vindere, tabere),
               names_to = "source",  
               values_to = "spiller") %>%
  mutate(pind = ifelse(source == "vindere", 1, 0)) %>% 
  group_by(spiller) %>% 
  mutate(sejre = sum(pind),
         tabte = as.numeric(n())) %>% 
  select(spiller, sejre, tabte) %>% 
  distinct() %>% 
  filter(!is.na(spiller)) %>% 
  left_join(data, by="spiller") %>% 
  ggplot(aes(sejre, tabte)) + 
  geom_point() + geom_smooth(method = lm) +
  coord_flip() + scale_y_continuous("Antal træninger")

pinde %>% 
  mutate(trøjer_sejr = case_when(trøjer == 1 ~ "trojer",
                                 trøjer != 1 ~ "sort")) %>% 
  group_by(id) %>% 
  select(trøjer_sejr) %>% 
  distinct() %>%
  ungroup() %>% 
  ggplot(aes(x = trøjer_sejr)) + 
  geom_bar() +
  labs(x = "Trøjer Type", y = "Count")  # Custom axis labels

pinde %>% 
  mutate(trøjer_sejr = case_when(trøjer == 1 ~ "trojer",
                                 trøjer != 1 ~ "sort")) %>% 
  group_by(id) %>% 
  select(trøjer_sejr) %>% 
  distinct() %>%
  ungroup() %>% 
  ggplot(aes(x = trøjer_sejr)) + 
  geom_bar(width = 0.13, fill = "steelblue") +  # Narrower bars with color
  geom_point(stat = "count", aes(y = after_stat(count)), size = 15, shape = 21, fill = "white") +  # Circles at the top
  geom_text(stat = "count", aes(y = after_stat(count), label = after_stat(count)), vjust = 0.5, size = 7) +  # Labels above circles
  labs(x = "Trøjer Type", y = "Count") +
  theme_minimal()


