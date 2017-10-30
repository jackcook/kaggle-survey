library(data.table)
library(dplyr)
library(ggplot2)
library(tibble)

results <- as.tibble(fread("./data/multipleChoiceResponses.csv"))

# Get all column names that begin with "LearningPlatformUsefulness"
platforms <- grep("^LearningPlatformUsefulness", names(results), value=T)

names <- c()
popularities <- c()
scores <- c()

for (platform in platforms) {
    usefulness <- results %>%
        group_by_(platform) %>%
        count()
    
    # Popularity = the number of people who responded to this question
    popularity <- usefulness[[2]][2] + usefulness[[2]][3] + usefulness[[2]][4]
    
    # Usefulness = a weighted average determining the usefulness of this platform
    score <- (usefulness[[2]][2] * 0 + usefulness[[2]][3] * 0.5 + usefulness[[2]][4] * 1) / popularity
    
    names <- c(names, gsub("LearningPlatformUsefulness", "", platform))
    popularities <- c(popularities, popularity)
    scores <- c(scores, score)
}

scores_df <- data.frame(
    Popularity = popularities,
    Usefulness = scores,
    Name = names
)

ggplot(scores_df, aes(x = Usefulness, y = Popularity)) +
    ggtitle("Effectiveness of Learning Methods") +
    geom_point() +
    geom_smooth(method = lm, se = FALSE, color = "#333333", size = 0.5) +
    geom_text(aes(label = Name, family = "San Francisco Display"), nudge_y = 250) +
    theme(
        plot.background = element_rect(fill = "#eeeeee"),
        panel.background = element_rect(fill = "#eeeeee"),
        panel.grid.major = element_line(size = 0.4, linetype = "solid", color = "#cccccc"),
        panel.grid.minor = element_line(size = 0),
        plot.title = element_text(size = 20, family = "San Francisco Display", face = "bold", hjust = 0.5, margin = margin(b = 20)),
        axis.title = element_text(size = 14, family = "San Francisco Display", face = "bold"),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        axis.ticks = element_blank(),
        plot.margin = unit(c(1, 1.25, 1, 1), "cm")
    )
