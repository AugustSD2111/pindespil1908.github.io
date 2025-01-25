# kode værk

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggbump)
library(jsonlite)
library(knitr)
library(gt)
library(DT)
library(formattable)
library(forcats)
library(kableExtra)

sti_billeder = "/Users/augus/Desktop/pindespil/Jelly/AugustSD2111.github.io/assets/img/"

# Indlæsning af data:
data <- fromJSON("C:/Users/augus/Desktop/pindespil/data1.json")


expand_json_to_df <- function(data) {
  
  # Initialize an empty list to store results
  expanded_rows <- list()
  
  # Loop through each item in the list (each JSON object)
  for (i in seq_along(data$pindespil_id)) {
    # Get the current values
    pindespil_id <- data$pindespil_id[[i]]
    dato <- data$dato[[i]]
    vindende_hold <- data$vindende_hold[[i]]
    tabende_hold <- data$tabende_hold[[i]]
    spiltype <- data$spiltype[[i]]
    trojer <- data$trojer[[i]]
    joker <- data$joker[[i]]
    
    # Find the maximum length between vindende_hold and tabende_hold
    max_len <- max(length(vindende_hold), length(tabende_hold))
    
    # Expand each vector to the same length (padding with NA if needed)
    vindende_hold <- c(vindende_hold, rep(NA, max_len - length(vindende_hold)))
    tabende_hold <- c(tabende_hold, rep(NA, max_len - length(tabende_hold)))
    
    # Repeat pindespil_id and dato to match the length
    pindespil_id <- rep(pindespil_id, max_len)
    dato <- rep(dato, max_len)
    spiltype <- rep(spiltype, max_len)
    trojer <- rep(trojer, max_len)
    joker <- rep(joker, max_len)
    # Combine the expanded rows into a data frame
    expanded_rows[[i]] <- data.frame(pindespil_id, 
                                     dato, 
                                     vindende_hold, 
                                     tabende_hold, 
                                     spiltype,
                                     trojer,
                                     joker, 
                                     stringsAsFactors = FALSE)
  }
  
  # Bind all the expanded rows into a single data frame
  expanded_df <- do.call(rbind, expanded_rows)
  
  return(expanded_df)
}

df_expanded <- expand_json_to_df(data)

test <- df_expanded %>% 
  pivot_longer(cols = c(vindende_hold, tabende_hold),
               names_to = "udfald",
               values_to = "spiller")

df <- data.frame(var = c(
  "August, gammel, forsvar",
  "Agge, gammel, midtbane",
  "Albert, gammel, forsvar",
  "Aland, gammel, keeper",
  "Banda, ung, angreb",
  "Bondo, gammel, midtbane",
  "Biele, gammel, midtbane",
  "Claes, gammel, forsvar",
  "Dalby, gammel, keeper",
  "Fey, gammel, forsvar",
  "Grube, gammel, forsvar",
  "Haris, ung, angreb",
  "Gulle, gammel, midtbane",
  "Lau, ung, forsvar",
  "Max, ung, midtbane",
  "Nico, ung, forsvar",
  "Nick, gammel, angreb",
  "Mikkel, gammel, angreb",
  "Noel, gammel, midtbane",
  "Shimal, gammel, angreb",
  "Semir, ung, midtbane",
  "Trane, gammel, angreb",
  "Kav, gammel, forsvar",
  "Walther, ung, midtbane",
  "Mathias, ung, midtbane",
  "Christian, gammel, midtbane",
  "Jokke, gammel, forsvar",
  "Thomas, ung, angreb",
  "Alexander, ung, angreb",
  "AndenKeeper, ung, keeper"
)) %>% 
  separate(var, into = c("name", "age", "position"), sep = ", ", remove = TRUE)

test2 <- test %>% 
  left_join(df, by = c("spiller"="name"))

test2$pindespil_id <- as.numeric(test2$pindespil_id)


png(filename = paste0(sti_billeder, "pinde_spiller.png"), width = 1000, height = 1200)
test2 %>% 
  filter(udfald == "vindende_hold", !is.na(spiller), spiller != "") %>% 
  group_by(spiller) %>%
  summarise(Pinde = n(), position = first(position), age = first(age), .groups = 'drop') %>% 
  ggplot(aes(fct_reorder(spiller, Pinde), Pinde)) +  # Order by Pinde in ascending order
  geom_segment(aes(x = fct_reorder(spiller, Pinde), xend = fct_reorder(spiller, Pinde),
                   y = 0, yend = Pinde, color = position),
               lwd = 2) +
  geom_point(aes(shape = age, fill = position, color = position), size = 7.5) +
  geom_text(aes(label = Pinde), color = "white", size = 5) +
  scale_shape_manual(values = c(21, 22), name = " ") +
  scale_x_discrete(name = "")+
  geom_hline(yintercept = 0) +
  coord_flip() +  # This flips the axes
  theme_light() + 
  theme(legend.title = element_blank())
dev.off()

### Position
# <div style="border-left: 630px solid black; height: 2px;"></div>
#   
#   
#   ```{r echo=FALSE, message=FALSE, warning=FALSE}
png(filename = paste0(sti_billeder, "position_rank.png"), width = 800, height = 600)
test2 %>% 
  filter(!is.na(position)) %>% 
  group_by(pindespil_id, position, udfald) %>%
  summarise(vundne = n(), .groups = "drop") %>%
  rbind(data.frame(pindespil_id = c("8", "8"), 
                   position = c("keeper", "keeper"), 
                   udfald = c("tabende_hold", "vindende_hold"),
                   vundne = c(0,0))) %>%
  pivot_wider(names_from = udfald,
              values_from = vundne, values_fill = list(vundne = 0)) %>% 
  ungroup() %>%
  group_by(position) %>% 
  mutate(gnm = cumsum(vindende_hold)/(cumsum(tabende_hold)+cumsum(vindende_hold))*100) %>% 
  ungroup() %>% 
  group_by(pindespil_id) %>% 
  mutate(rank = rank(-gnm, ties.method = "first")) %>% 
  arrange(pindespil_id) %>% 
  ggplot(aes(x = as.numeric(pindespil_id), y = as.numeric(rank), color = position)) + 
  geom_bump(size = 1.5) + 
  geom_point(size = 10) +
  geom_text(aes(label = paste0(round(gnm, 0), "%")), size = 2.5, color = "white") +
  scale_color_brewer(palette = "Dark2", guide = guide_legend(title = NULL)) +
  scale_y_reverse(breaks = c(4,3,2,1),
                  name = c(""),
                  labels = c("# 4", "# 3", "# 2", "# 1"), expand = c(0,.5)
  ) +
  scale_x_continuous(name ="", 
                     breaks = as.numeric(unique(test2$pindespil_id)), 
                     labels = tapply(test2$dato, test2$pindespil_id, unique))+
  theme_minimal() + 
  geom_hline(yintercept = c( 1.5, 2.5, 3.5)) +
  theme(
    #panel.grid = element_blank(),  # Remove grid lines
    axis.line.x = element_line(color = "white"),  # Add axis lines for clarity (optional)
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + theme(legend.position = "bottom")
dev.off()
# ```
# 
# ```{r}
png(filename = paste0(sti_billeder, "position_graf.png"), width = 800, height = 600)
test2 %>% 
  filter(!is.na(position)) %>% 
  group_by(pindespil_id, position, udfald) %>%
  summarise(vundne = n(), .groups = "drop") %>%
  rbind(data.frame(pindespil_id = c("8", "8"), 
                   position = c("keeper", "keeper"), 
                   udfald = c("tabende_hold", "vindende_hold"),
                   vundne = c(0,0))) %>%
  pivot_wider(names_from = udfald,
              values_from = vundne, values_fill = list(vundne = 0)) %>% 
  mutate(gennemsnit = vindende_hold/(tabende_hold+vindende_hold)*100) %>% 
  ungroup() %>%
  group_by(position) %>% 
  mutate(gnm = cumsum(vindende_hold)/(cumsum(tabende_hold)+cumsum(vindende_hold))*100) %>% 
  ungroup() %>% 
  arrange(as.numeric(pindespil_id)) %>%  
  ggplot(aes(x = as.numeric(pindespil_id), y = gennemsnit, color = position)) +
  geom_point(size = 3, position = position_dodge(width = 0.2)) +
  geom_point(aes(y = gnm, group = position), shape = 0, size = 0, na.rm = TRUE, position = position_dodge(width = 0.2)) +
  geom_smooth(aes(y = gnm, group = position, fill = position), method = "loess", se = F, na.rm = TRUE,
              alpha = 0.2, position = position_dodge(width = 0.2)) +
  theme_minimal() + theme(legend.position = "bottom")
dev.off()
#```


### Alder
# <div style="border-left: 630px solid black; height: 2px;"></div>
#   
#   
#   ```{r echo=FALSE, message=FALSE, warning=FALSE}
png(filename = paste0(sti_billeder, "alder_rank.png"), width = 1000, height = 800)
test2 %>% 
  filter(!is.na(age)) %>% 
  group_by(pindespil_id, age, udfald) %>%
  summarise(vundne = n(), .groups = "drop") %>%
  pivot_wider(names_from = udfald,
              values_from = vundne, values_fill = list(vundne = 0)) %>% 
  ungroup() %>% 
  group_by(age) %>% 
  mutate(gnm = cumsum(vindende_hold)/(cumsum(tabende_hold)+cumsum(vindende_hold))*100) %>% 
  ungroup() %>% 
  group_by(pindespil_id) %>% 
  mutate(rank = rank(-gnm, ties.method = "first")) %>% 
  ggplot(aes(x = as.numeric(pindespil_id), y = as.numeric(rank), color = age)) + 
  geom_bump(size = 1.5) + 
  geom_point(size = 10) +
  geom_text(aes(label = paste0(round(gnm, 0), "%")), size = 2.5, color = "white") +
  scale_color_brewer(palette = "Dark2", guide = guide_legend(title = NULL)) +
  scale_y_reverse(breaks = c(2,1),
                  name = c(""),
                  labels = c("# 2", "# 1"), expand = c(0,.5)
  ) +
  scale_x_continuous(name ="", 
                     breaks = as.numeric(unique(test2$pindespil_id)), 
                     labels = tapply(test2$dato, test2$pindespil_id, unique))+
  #scale_y_continuous(limits = c(1,3)) +
  theme_minimal() + 
  geom_hline(yintercept = c( 1.5)) +
  theme(
    #panel.grid = element_blank(),  # Remove grid lines
    axis.line.x = element_line(color = "white"),  # Add axis lines for clarity (optional)
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
dev.off()
#```


### Pindespilstype
#<div style="border-left: 630px solid black; height: 2px;"></div>
  
  
#  ```{r echo=FALSE, message=FALSE, warning=FALSE}
png(filename = paste0(sti_billeder, "spil_typer.png"), width = 1000, height = 800)
test2 %>%
  distinct(pindespil_id, spiltype) %>%
  count(spiltype) %>% 
  ggplot(aes(x = spiltype, y = n, fill = spiltype)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = 1.5, colour = "white", size = 15)+
  scale_fill_brewer(palette = "Set1") + 
  theme_void()
dev.off()
#```

### Trøjer
#<div style="border-left: 630px solid black; height: 2px;"></div>
  
  
#  ```{r echo=FALSE, message=FALSE, warning=FALSE}
test3 <- test2 %>% 
  filter(trojer != "") %>% 
  distinct(pindespil_id, trojer) %>% 
  count(trojer) %>% 
  mutate(xaks = c("hej"))

png(filename = paste0(sti_billeder, "trojer_vandt.png"), width = 1000, height = 800)
ggplot(test3, aes(x = xaks, y = n, fill=trojer)) + geom_bar(stat = "identity", position = "stack", width = .2) +
  geom_hline(yintercept = sum(test3$n/2), linetype = "dotted", color = "black", size = 1) +
  #geom_text(aes(y = 0.5, label = c(n)), nudge_y = c(0, sum(test3$n) - 1), colour = "white", size = 15)+
  #geom_text(label = c("hej", "dab"), colour = "white", size = 15) +
  scale_fill_brewer(palette = "Dark2") + 
  coord_flip() + theme_void()
dev.off()
#```

### Data
# <div style="border-left: 630px solid black; height: 2px;"></div>
#   
#   ```{r echo=FALSE, message=FALSE, warning=FALSE}
# data %>% select(-tabende_hold) %>% kable(format = "markdown", table.attr = "class='table table-striped'")
# ```
# 
# ```{r echo=FALSE, message=FALSE, warning=FALSE}
# df %>% arrange(name) %>%  kable(format = "html", table.attr = "class='table table-striped'")

