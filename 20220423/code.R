## R语言ggplot2学习 每周一图 第一周
## 欢迎关注公众号 小明的数据分析笔记本 


df_trade <- readr::read_csv("trade.csv")

library(tidyverse)
library(ggthemes)
library(cowplot)
library(ggplot2)
p1 <- df_trade %>% 
  mutate(
    trade_deficit = trade_deficit / 10^9, 
    manufacture_employment = manufacture_employment / 10^5,
  ) %>% 
  gather(cat, value, -year) %>% 
  ggplot(aes(year, value, fill = cat)) + 
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0, color = "red1", size = 0.7) +
  scale_x_continuous(breaks = 1995:2016, 
                     labels = c("1995", "", "", "", "", "", "", "", "", "", 
                                "2005", "", "", "", "", "", "", "", "", "", "", "2016")) +
  scale_y_continuous(limits = c(-380, 210), breaks = seq(-300, 200, by = 100)) +
  scale_fill_manual(name = "", values = c("dodgerblue3", "firebrick4"), 
                    labels = c("Manufacturing employment (100K)",
                               "Trade deficit with China in goods ($B)")) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(x = NULL, y = NULL, 
       title = "",
       subtitle = "\n", 
       caption = "\nVisualization by Cédric Scherer  |  Sources: US Census Bureau; BLS") +
  theme_economist() +
  theme(text = element_text(family = "serif"), 
        axis.text = element_text(size = 12),
        axis.text.y = element_text(hjust = 1),
        axis.ticks.length = unit(5, "pt"),
        legend.text = element_text(size = 14),
        legend.position = "top",
        legend.justification = c(0, 1),
        plot.caption = element_text(color = "grey40"),
        plot.background = element_rect(fill = "#dcf0f7"),
        panel.grid.major.y = element_line(color = "grey70", size = 0.4),
        panel.background = element_rect(fill = "#dcf0f7"))
p1
p_trade <- ggdraw(p1) + 
  draw_text("Free markets and free workers", 
            x = 0.01, y = 0.98, 
            hjust = 0, vjust = 1, 
            size = 20, family = "serif") +
  draw_text("United States",
            x = 0.01, y = 0.91, 
            hjust = 0, 
            vjust = 1, 
            size = 14, 
            family = "serif")
p_trade
