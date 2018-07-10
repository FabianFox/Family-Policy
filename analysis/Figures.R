# Analyze and visualize family policies
# Data created in "FamilyPolicyData.R"

# Packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rio, countrycode, ggrepel)

# Participation rate of 0-to2-year-olds in ECEC, EU28, 2004-2013
mean.enrolment <- ecec.df %>%
  filter(year >= 2004) %>%
  group_by(year) %>%
  summarise(mean.enrol = mean(enrolment, na.rm = TRUE)) %>%
  mutate(label = "Mean of EU28")

ecec.plot.df <- ecec.df %>%
  filter(year >= 2004)

enrol.fig <- ggplot(ecec.plot.df, aes(x = year, y = enrolment)) +
  geom_line(aes(group = Country), color = "gray70", alpha = 0.8) +
  scale_y_continuous(name = "", labels = function(x) paste0(x*1, "%")) +
  labs(x = "",
       caption = "Source: OECD Family Database (Table PF3.2)") +
  ggtitle(label = "Percentage of 0-to-2-year-olds enrolled in formal childcare (EU28, 2004-2013)") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(size = .5))

enrol.fig <- enrol.fig + geom_line(data = subset(ecec.plot.df, Country == "Germany"),
                   aes(x = year, y = enrolment, group = Country), color = "red2", size = 1)

enrol.fig <- enrol.fig + geom_line(data = mean.enrolment, aes(x = year, y = mean.enrol, group = 1), 
                                   color = "black", size = 1, linetype = 2)

enrol.fig <- enrol.fig + geom_text_repel(data = subset(ecec.plot.df,
                                                       Country %in% "Germany" & year == 2011),
                                         aes(label = Country), box.padding = 1)

enrol.fig <- enrol.fig + 
  annotate(geom = "text", x = "2007", y = 22, label = "Mean of EU28") +
  annotate(geom = "segment", x = "2008", xend = "2007", y = 29, yend = 23)
  
ggsave(filename = "./output/EnrolmentFigure.tiff", plot = enrol.fig, dpi = "print")

