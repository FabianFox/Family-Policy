# Analyze and visualize family policies
# Data created in "FamilyPolicyData.R"

# Packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rio, countrycode, ggrepel)

# Participation rate of 0-to2-year-olds in ECEC, EU28, 2004-2013
### ------------------------------------------------------------------------------------------------ ###

# Mean enrolment
mean.enrolment <- ecec.df %>%
  filter(year >= 2004) %>%
  group_by(year) %>%
  summarise(mean.enrol = mean(enrolment, na.rm = TRUE)) %>%
  mutate(label = "Mean of EU28")

# Base DF for the plot
enrol.plot.df <- ecec.df %>%
  filter(year >= 2004)

# Base plot
enrol.fig <- ggplot(enrol.plot.df, aes(x = year, y = enrolment)) +
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

# Add further layers
enrol.fig <- enrol.fig + geom_line(data = subset(enrol.plot.df, Country == "Germany"),
                   aes(x = year, y = enrolment, group = Country), color = "red2", size = 1)

enrol.fig <- enrol.fig + geom_line(data = mean.enrolment, aes(x = year, y = mean.enrol, group = 1), 
                                   color = "black", size = 1, linetype = 2)

enrol.fig <- enrol.fig + geom_text_repel(data = subset(enrol.plot.df,
                                                       Country %in% "Germany" & year == 2011),
                                         aes(label = Country), box.padding = 1)

enrol.fig <- enrol.fig + 
  annotate(geom = "text", x = 2007, y = 22, label = "Mean of EU28") +
  annotate(geom = "segment", x = 2008, xend = 2007, y = 29, yend = 23)

# Save the figure to disk
ggsave(filename = "./output/EnrolmentFigure.tiff", plot = enrol.fig, dpi = "print")

# Public expenditures for ECEC
### ------------------------------------------------------------------------------------------------ ###

# Mean expenditure
mean.expenditure <- ecec.df %>%
  filter(between(year, 2000, 2013)) %>%
  group_by(year) %>%
  summarise(mean.expenditure = mean(expenditure, na.rm = TRUE)) %>%
  mutate(label = "Mean of EU28")

# Base DF for plot
expenditure.plot.df <- ecec.df %>%
  filter(between(year, 2000, 2013))

# Base plot
expenditure.fig <- ggplot(expenditure.plot.df, aes(x = year, y = expenditure)) +
  geom_line(aes(group = Country), color = "gray70", alpha = 0.8) +
  scale_y_continuous(name = "", labels = function(x) paste0(x*1, "%")) +
  scale_x_continuous(breaks = seq(2000, 2012, 2)) +
  labs(x = "",
       caption = "Source: OECD Family Database (Table PF3.1)") +
  ggtitle(label = "Public expenditures on ECEC in percent of GDP (EU28, 2000-2013)") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(size = .5))

# Add further layers
expenditure.fig <- expenditure.fig + geom_line(data = subset(expenditure.plot.df, Country == "Germany"),
                                   aes(x = year, y = expenditure, group = Country), color = "red2", size = 1)

expenditure.fig <- expenditure.fig + geom_line(data = mean.expenditure, aes(x = year, y = mean.expenditure, group = 1), 
                                   color = "black", size = 1, linetype = 2)

expenditure.fig <- expenditure.fig + geom_text_repel(data = subset(expenditure.plot.df,
                                                       Country %in% "Germany" & year == 2011),
                                         aes(label = Country), box.padding = 1)

expenditure.fig <- expenditure.fig + 
  annotate(geom = "text", x = 2006, y = 0.75, label = "Mean of EU28") +
  annotate(geom = "segment", x = 2005, xend = 2006, y = 0.55, yend = 0.72)

# Save the figure to disk
ggsave(filename = "./output/ExpenditureFigure.tiff", plot = expenditure.fig, dpi = "print")
