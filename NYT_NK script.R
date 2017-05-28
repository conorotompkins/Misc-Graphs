library(tidyverse)
library(ggrepel)
library(stringr)
library(scales)
library(cowplot)
library(gridExtra)

set.seed(1234)

theme_set(theme_bw())

#rm(list = ls())

#load data from NYT article https://nyti.ms/2rfMEBD
df <- tibble(proposed_action = c("Economic sanctions",
                                 "Increase pressure on China",
                                 "Cyberattacks against military targets",
                                 "Military action",
                                 "Conducting airstrikes",
                                 "Sending arms and supplies",
                                 "Sending ground troops",
                                 "Doing nothing"),
             could_find = c(59, 63, 37, 5, -1, -12, -34, -45),
             could_not_find = c(49, 48, 18, 9, 3, -13, -19, -22))

#adjust variables
df <- df %>% 
  mutate(could_find = could_find / 100,
         could_not_find = could_not_find / 100,
         diff = abs(could_find - could_not_find),
         proposed_action_formatted = str_wrap(proposed_action, width = 20)) #format text for labelling

#set up text for plots
nyt_url <- "https://nyti.ms/2rfMEBD"
my_subtitle <- paste0("NYT article: ", nyt_url)
my_caption <- "@conor_tompkins"
net_suport_label <- "(Net support is a measure showing the percent of respondents who supported a policy minus the percent who said they did not support it)"

#create and save scatter plot
scatter_plot <- df %>%
  ggplot(aes(could_find, could_not_find, 
             label = proposed_action_formatted)) +
  geom_abline() +
  geom_point(aes(size = diff),
             shape = 21) +
  geom_label_repel(force = 15,
                   size = 3,
                   max.iter = 3e3) +
  coord_equal() +
  scale_x_continuous(limits = c(-.65, .65),
                     breaks = seq(-.50, .50, 
                                  by = .25),
                     labels=percent) +
  scale_y_continuous(limits = c(-.65, .65),
                     breaks = seq(-.50, .50, 
                                  by = .25),
                     labels=percent) +
  scale_size_continuous(name = "Absolute difference\n(Could Find - Could Not Find)",
                        labels = percent) +
  labs(x = paste0("Could Find North Korea\n", net_suport_label),
       y = "Could Not Find North Korea",
       title = "Comparing net support for policies towards North Korea",
       subtitle = my_subtitle,
       caption = my_caption) +
  theme(plot.caption = element_text(hjust = 1))
scatter_plot
ggsave("NYT_scatter_plot.png", width = 12, height = 12)



#create and save dot plot
dot_plot <- df %>% 
  gather(metric, measure, -c(proposed_action, proposed_action_formatted, diff)) %>% 
  mutate(metric = factor(metric, levels = c("could_not_find", "could_find"))) %>% 
  arrange(metric) %>% 
  ggplot(aes(measure, metric, color = metric)) +
  geom_vline(xintercept = 0) +
  geom_point(size = 5,
             shape = 21, 
             stroke = 2,
             show.legend = FALSE) +
  scale_x_continuous(labels=percent) +
  scale_y_discrete(labels = c("Could not find North Korea", "Could find North Korea")) +
  scale_color_discrete(breaks = c("could_not_find", "could_find")) +
  labs(x = paste0("Net support\n", net_suport_label),
       y = NULL,
       title = "Comparing distributions of net support",
       subtitle = paste("Each circle represents a proposed action.               ", my_subtitle),
       caption = my_caption)
dot_plot
ggsave("NYT_dot_plot.png", width = 12, height = 12)

#attempt to arrange both plots in one image
#gridExtra
grid.arrange(scatter_plot, dot_plot, 
             ncol = 2)
             
             ,
             widths = 1:2, 
             heights=unit(c(1,10), c("in", "mm")))

gA <- ggplotGrob(scatter_plot)
gB <- ggplotGrob(dot_plot)
grid::grid.newpage()
grid::grid.draw(rbind(gA, gB))

#cowplot
plot_grid(scatter_plot, dot_plot, ncol=2, align="v")
