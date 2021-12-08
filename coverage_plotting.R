library(ggplot2)
library(dplyr)

#Read in data
data_in <- read.csv("coverage.csv")

#Coverage plot
data_windows <- data_in %>%
  mutate(window = cut_interval(pos, length = 400)) %>%
  group_by(ID, window) %>%
  summarize(mean_cov = mean(cov))

ggplot(data_windows, aes(x = window, y = mean_cov)) +
  geom_boxplot(fill = "white") + 
  labs(title = "", 
       y = "Read Depth", 
       x = "Genome Position") + 
  theme_classic() + 
  theme(axis.title.y = element_text(vjust=1.2), 
        legend.position = "none", 
        text = element_text(size = 15), 
        axis.text.x = element_text(size = 5, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 10))

#Facet plot
data_IDs <- filter(data_in, ID %in% c("ZZYGJZ2A","ZZYGJZ2O","ZZYGJZ2P"))

# x axis is position of genome, y axis is coverage number
# each mini plot is a single sample/well
ggplot(data_IDs, aes(x = pos, y = cov, color = ID, group = ID)) + 
  geom_line() + 
  geom_smooth() + 
  theme_bw() + 
  labs(x = "Genome Position", 
       y = "Coverage", 
       color = "Sample") + 
  facet_wrap(.~ID)
