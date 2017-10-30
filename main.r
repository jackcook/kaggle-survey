results <- as.tibble(fread("multipleChoiceResponses.csv"))
platforms <- grep("^LearningPlatformUsefulness", names(results), value=T)
names <- c()
popularities <- c()
scores <- c()
for (platform in platforms) {
    usefulness <- results %>%
        group_by_(platform) %>%
        count()
    
    popularity <- usefulness[[2]][2] + usefulness[[2]][3] + usefulness[[2]][4]
    score <- (usefulness[[2]][2] * 0 + usefulness[[2]][3] * 0.5 + usefulness[[2]][4] * 1) / popularity
    
    names <- c(names, gsub("LearningPlatformUsefulness", "", platform))
    popularities <- c(popularities, popularity)
    scores <- c(scores, score)
    
    print(paste(platform, " usefulness: ", score, sep = ""))
}

scores_df <- data.frame(
    popularity = popularities,
    usefulness = scores,
    name = names
)

ggplot(scores_df, aes(x=usefulness, y=popularity)) +
    geom_point() +
    geom_text(aes(label=name), vjust = 0)