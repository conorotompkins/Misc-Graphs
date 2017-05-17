library(tidyverse)
library(ggrepel)
library(stringr)
library(scales)
install.packages("cowplot")
library(cowplot)
library(gridExtra)
set.seed(1234)

theme_set(theme_bw())

rm(list = ls())

df <- tibble(proposed_action = c("Economic sanctions",
                                 "Increase pressure on China",
                                 "Cyberatacks against military targets",
                                 "Military action",
                                 "Conducting airstrikes",
                                 "Sending arms and supplies",
                                 "Sending ground troops",
                                 "Doing nothing"),
             could_find = c(59, 63, 37, 5, -1, -12, -34, -45),
             could_not_find = c(49, 48, 18, 9, 3, -13, -19, -22))

df <- df %>% 
  mutate(could_find = could_find / 100,
         could_not_find = could_not_find / 100,
         diff = abs(could_find - could_not_find),
         proposed_action_formatted = str_wrap(proposed_action, width = 20))

nyt_url <- "https://nyti.ms/2rfMEBD"
my_subtitle <- paste0("NYT article: ", nyt_url)
my_caption <- "@conor_tompkins"

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
  scale_size_continuous(name = "Absolute Differential\n(Could Find - Could Not Find)",
                        labels = percent) +
  labs(x = "Could Find North Korea",
       y = "Could Not Find North Korea",
       title = "Comparing net support for policies toward North Korea",
       subtitle = my_subtitle) +
  theme(plot.caption = element_text(hjust = 1))
scatter_plot
ggsave("NYT_scatter_plot.png", width = 12, height = 12)


net_suport_label <- "(Net support is a measure showing the percent of respondents who supported a policy minus the percent who said they did not support it)"

dot_plot <- df %>% 
  gather(metric, measure, -c(proposed_action, proposed_action_formatted, diff)) %>% 
  ggplot(aes(measure, metric,
             color = metric)) +
  geom_point(size = 5,
             shape = 21, 
             stroke = 2,
             show.legend = FALSE) +
  scale_x_continuous(labels=percent) +
  scale_y_discrete(labels = c("Could find North Korea", "Could not find North Korea")) +
  labs(x = paste0("Net support\n", net_suport_label),
       y = NULL,
       title = "Comparing distributions of net support",
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
