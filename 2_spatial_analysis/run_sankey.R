# libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggsankey)

# Distance
load('Isolation_HPC/dist_highsev_v2.RData')

sankey.dist <- df.dist %>%
  mutate(row = row_number()) %>%
  dplyr::select(row, d.1, d.2, d.3, d.4) %>%
  group_by(row) %>%
  gather(key='year', value='dist', d.1:d.4) %>%
  ungroup() %>%
  mutate(year = str_replace(year, "d.1", '2020')) %>%
  mutate(year = str_replace(year, "d.2", '2021')) %>%
  mutate(year = str_replace(year, "d.3", '2022')) %>%
  mutate(year = str_replace(year, "d.4", '2023')) %>%
  mutate(g = cut(dist, breaks=c(-1,200,400,Inf), labels=c('0-200','200-400','>400'))) %>%
  pivot_wider(id_cols=row, names_from=year, values_from=g) %>%
  dplyr::select('2020','2021','2022', '2023') %>%
  make_long('2020','2021', '2022', '2023') %>%
  mutate(node = factor(node, levels=c('0-200','200-400','>400'))) %>%
  mutate(next_node = factor(next_node, levels=c('0-200','200-400','>400'))) %>%
  ggplot(aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = "black", show.legend = FALSE, width=0.1) +
  scale_fill_manual(values=c('#0084A8','#CFA252','#543005')) +
  theme_minimal() + 
  theme(legend.position = "bottom") +
  theme(axis.title = element_blank()
        , axis.text.y = element_blank()
        , axis.ticks = element_blank()  
        , panel.grid = element_blank(),
        axis.text.x = element_text( size = 6))

ggsave('sankey_dist_thin_highsev_ver2.png', sankey.dist, width = 1.5, height = 3, dpi=500)

remove(df.dist)
gc()


# D2WD
load('Isolation_HPC/d2wd_highsev_v2.RData')

sankey.d2wd <- df.d2wd %>%
    mutate(row = row_number()) %>%
    dplyr::select(row, d2wd.1, d2wd.2, d2wd.3, d2wd.4) %>%
    group_by(row) %>%
    gather(key='year', value='D2WD', d2wd.1:d2wd.4) %>%
    ungroup() %>%
    mutate(year = str_replace(year, "d2wd.1", '2020')) %>%
    mutate(year = str_replace(year, "d2wd.2", '2021')) %>%
    mutate(year = str_replace(year, "d2wd.3", '2022')) %>%
    mutate(year = str_replace(year, "d2wd.4", '2023')) %>%
    mutate(g = cut(D2WD, breaks=c(-1,0.082,0.271,1), labels=c('0-0.082','0.082-0.271','0.271-1'))) %>%
    pivot_wider(id_cols=row, names_from=year, values_from=g) %>%
    dplyr::select('2020', '2021', '2022', '2023') %>%
    make_long('2020', '2021', '2022', '2023') %>%
    mutate(node = factor(node, levels=c('0-0.082','0.082-0.271','0.271-1'))) %>%
    mutate(next_node = factor(next_node, levels=c('0-0.082','0.082-0.271','0.271-1'))) %>%
    ggplot(aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
    geom_sankey(flow.alpha = 0.5, node.color = "black", show.legend = FALSE, width=0.1) +
    scale_fill_manual(values=c('#d7191c','#fdae61','#2c7bb6')) +
    theme_minimal() +
    theme(legend.position = "none") +
    theme(axis.title = element_blank()
          , axis.text.y = element_blank()
          , axis.ticks = element_blank()
          , panel.grid = element_blank(),
          axis.text.x = element_text( size = 6)
    )

ggsave('sankey_d2wd_thin_highsev_ver2.png', sankey.d2wd, width = 1.5, height = 3, dpi=500)

remove(df.d2wd)
gc()




