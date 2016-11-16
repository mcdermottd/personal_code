library(data.table) 
library(ggplot2)
library(scales)

# bar example
colors <- c("red", "orange", "light green", "dark green")
names(colors) <- c("Unsatisfactory", "Unsatisfactory with Admin Discretion", "Satisfactory", "Exceeds")

png("N:/delaware/de_growth_2015/qc/tva/rating_distribution/bar_chart_teacher_v2.png") 
ggplot(tva_bta, aes(Rating, fill = Rating)) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_fill_manual(values = colors) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Rating") + ylab("Percentage of Teachers") + ggtitle("Percentage of Teachers per Rating")
dev.off()

# histogram 1 example
png("N:/delaware/de_growth_2015/qc/tva/rating_distribution/avg_teacher_rating_school.png", width = 800, height = 800)
ggplot(a_rating_avg_school, aes(avg_rating, fill = ..count..)) + geom_histogram()  + 
  scale_fill_gradient(colours = pal)
dev.off()

# histogram 2 example (with facet)
png("N:/delaware/de_growth_2015/qc/tva/rating_distribution/teacher_rating_category_school.png", width = 800, height = 800)
ggplot(a_freq_ratings_school, aes(school_percent, fill = ..count..)) + geom_histogram()  + 
  scale_fill_gradient(colours = pal) + 
  facet_wrap(~bta_rating)
dev.off()

# scattplot example
png("N:/delaware/de_growth_2015/qc/tva/bta_pmet_compare_school.png", width = 800, height = 800)
ggplot(merged_school_ratings, aes(x = percent_bta, y = percent_met_2014)) + geom_point(fill = "dodgerblue4", colour = "dodgerblue4", size = 2, alpha = .8) +
  theme(plot.background = element_rect(fill = "azure2"),
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        plot.title = element_text(vjust = 1, colour = "dodgerblue4", face = "bold", size = 15),
        axis.title.y = element_text(vjust = 1.4, size = 15, face = "bold"), 
        axis.title.x = element_text(size = 15, face = "bold"), 
        axis.text = element_text(colour = "black", size = 12)) +
  scale_y_continuous(labels = percent) + scale_x_continuous(labels = percent) +
  xlab("Percent Students BTA (14-15)") + ylab("Percent Students Met Target (13-14)") +
  ggtitle("Percent Students BTA (14-15) vs. Percent Students Met Target (13-14), by School")
dev.off()

