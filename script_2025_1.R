library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(forcats)

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
  c( "August", "Fey", "Shimal", "Nick", "Tietze", "Claes", "Mikkel", NA, NA),
  c("Lau", "Pat", "Albert", "Max", "Sjølle", "Olabi", "Praygod", "Otman", "Thomas")
)


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
  scale_y_continuous(breaks = seq(0,2))+
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size = 16)) +
  labs(subtitle= paste0("Sidst opdateret: ", opdateret))
dev.off()
  


