
# [00] Setup ----

libraries = c("readxl",                     
              "openxlsx",                   
              "tidyverse",
              "ggridges",
              "ggpubr",
              "plyr", 
              "arrow", 
              "scales")
lapply(libraries, require, character.only = TRUE)                        

pathmrio = ("../../ADB/MRIO/")
pathindicators = ("../../GVC 2023/Indicators/Trade Accounting/")

source("./src/R/adb colors.R")

# [01] Data ----

  # trade accounting 
ta <- read_parquet(paste0(pathindicators, "ta62.parquet")) |> 
          filter(as.integer(t) < 2017) |> 
          rbind((read_parquet(paste0(pathindicators, "/ta.parquet"))))

kaz_ta <- ta |> filter(s == 49) |> 
          select(t, Exports:PDC2) |> 
          group_by(t) |> 
          dplyr::summarise(across(everything(), sum)) |> 
          rowwise() |> 
          dplyr::mutate(DAVAX = sum(c_across(matches("DAVAX"))),
                        REX = sum(c_across(matches("REX"))),
                        REF = sum(c_across(matches("REF"))),
                        PDC = sum(c_across(matches("PDC")))) |> 
          mutate(t_new = ifelse(t == "2000", "2000",
                         ifelse(t == "2007", "2007", 
                         str_sub(as.character(t), -2)))) |>
          add_row(t = " ", t_new = " ") |>
          mutate(t = fct_relevel(t, "2000", " ", "2007"),
                 t_new = fct_relevel(t_new, "2000", " ", "2007")) |> 
          ungroup() |> 
          pivot_longer(Exports:PDC, names_to = "term")

  # pta database
pta <- read_parquet("./data/01_merged data full.parquet")
kaz_pta <- read_parquet("./data/02_merged data kazakhstan.parquet")

# [02] Chapter 2 ----

## 1. Exports decomposition ----

decomp_terms <- c("DAVAX", "REX", "REF", "FVA", "PDC")

fig2.1 <- ggplot(kaz_ta |> filter(term %in% decomp_terms), aes(x = t_new, y = value)) +
          geom_bar(aes(fill = fct_relevel(term, "DAVAX", "REX", "REF", "FVA", "PDC")), 
                   width = 0.6, stat = "identity", position = position_stack(reverse = TRUE)) + 
          charts.theme + 
          xlab("") +
          ylab("$ billion") +
          theme(axis.text.x = element_text(size = 10, angle = 0, vjust = 0.5)) + 
          scale_fill_manual(values = c(main1, main4, accent6, accent2, accent3)) + 
          scale_y_continuous(labels = unit_format(unit = "", scale = 1/1000))
fig2.1      

ggsave(filename="fig2.1.png", plot=fig2.1, device="png", path="./charts", 
       width = 8, height = 5)

## 2. Depth ----

pta_year <- pta |> filter(`_merge` == "both", !grepl("Enlargement", agreement)) |> 
            distinct(agreement, .keep_all=TRUE) |> 
            select(entry_force, depth_wto_plus:depth_nonecon) |> 
            group_by(entry_force) |> 
            dplyr::summarise(across(everything(), mean)) |> 
            mutate(year_new = ifelse(entry_force == "1991", "1991",
                           str_sub(as.character(entry_force), -2))) |> 
            mutate(year_new = fct_reorder(year_new, entry_force)) |> 
            pivot_longer(depth_wto_plus:depth_nonecon, names_to = "depth")


decade_means <- pta_year |> 
  mutate(decade = ifelse(entry_force %in% c(1991:2000), "1",
                  ifelse(entry_force %in% c(2001:2010), "2",
                  ifelse(entry_force %in% c(2011:2015), "3", "Before 1990")))) |> 
  filter(depth == 'depth_total') |> 
  group_by(decade) |> 
  dplyr::summarise(decade_mean = mean(value))

fig2.2 <- ggplot(pta_year |> filter(depth %in% c("depth_wto_plus", "depth_wto_x"),
                                    entry_force > 1990), aes(x = entry_force, y = value)) +
          geom_bar(aes(fill = depth), 
                   width = 0.6, stat = "identity", position = position_stack(reverse = TRUE)) +
          geom_segment(data = decade_means |> filter(decade==1), 
                       aes(x = 1991, xend = 2000, y = decade_mean, yend = decade_mean),
                       linewidth = 0.3) + 
          geom_segment(data = decade_means |> filter(decade==2), 
               aes(x = 2001, xend = 2010, y = decade_mean, yend = decade_mean),
               linewidth = 0.3) + 
          geom_segment(data = decade_means |> filter(decade==3), 
               aes(x = 2011, xend = 2015, y = decade_mean, yend = decade_mean),
               linewidth = 0.3) + 
          geom_segment(aes(x = 1998, xend = 1998, y = 15, yend = 10.6), size = 0.008) + 
          annotate("text",x=1996 + 0.3,y=15.5, hjust=0,vjust=0,label="Decade average", size = 3.5) + 
          charts.theme + 
          xlab("") +
          ylab("number of provisions") +
          theme(axis.text.x = element_text(size = 10, angle = 0, vjust = 0.5)) + 
          scale_fill_manual(values = c(main1, accent6), labels = c("WTO-plus", "WTO-extra"))
fig2.2

ggsave(filename="fig2.2.png", plot=fig2.2, device="png", path="./charts", 
       width = 8, height = 4)


# [03] Chapter 3 ----
## 1. Country comparison ----

countries <- c("KAZ", "RUS", "GEO", "TUR",
               "SAU", "KGZ", "ARM", "ARE",
               "BRN")

pta_comp <- pta |> filter(source_iso %in% countries, !is.na(agreement)) |> 
            select(source_iso:source_name, agreement, entry_force, depth_wto_plus:depth_nonecon) |> 
            mutate(year_group = ifelse(entry_force %in% c(1991:2000), '1991-2000',
                                ifelse(entry_force %in% c(2001:2010), '2001-2010',
                                ifelse(entry_force %in% c(2011:2015), '2011-2015', 'Before 1990')))) |> 
            distinct() |> 
            group_by(source_iso, source_name, year_group) |> 
            dplyr::summarise_at(vars(depth_wto_plus:depth_nonecon), mean)

fig3.1a <- ggplot(pta_comp, aes(x = fct_rev(source_name), y = depth_wto_plus)) + 
  geom_segment(data = pta_comp |> select(year_group, depth_wto_plus) |> 
                      pivot_wider(names_from = year_group, values_from = depth_wto_plus) |> 
                      rowwise() |> 
                      mutate(min = pmin(`1991-2000`, `2001-2010`, `2011-2015`, na.rm = TRUE),
                             max = pmax(`1991-2000`, `2001-2010`, `2011-2015`, na.rm = TRUE)),
               aes(x = fct_rev(source_name), xend = fct_rev(source_name), 
                   y = min, yend = max),
               color = accent6, linewidth = 1.3, alpha = 0.8) + 
  annotate('rect', size = 3, 
           xmin = 5.6, xmax = 6.4,
           ymin = -Inf, ymax = Inf,
           alpha = 0.3, fill = '#c7c3c3') +
  geom_point(aes(color = year_group), size = 3) +
  charts.theme + 
  xlab("") +
  ylab("") +
  coord_flip() + 
  labs(subtitle = "WTO-plus") + 
  theme(axis.text.x = element_text(size = 10, angle = 0, vjust = 0.5),
        panel.grid.major.x = element_line(color = "#d4d4d4", linetype = 2, size = 0.3),
        panel.grid.major.y = element_line(color = "transparent"),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, margin = margin(b = 5))) + 
  scale_color_manual(values = c(accent3, main4, main1))
fig3.1a

fig3.1b <- ggplot(pta_comp, aes(x = fct_rev(source_name), y = depth_wto_x)) + 
  geom_segment(data = pta_comp |> select(year_group, depth_wto_x) |> 
                 pivot_wider(names_from = year_group, values_from = depth_wto_x) |> 
                 rowwise() |> 
                 mutate(min = pmin(`1991-2000`, `2001-2010`, `2011-2015`, na.rm = TRUE),
                        max = pmax(`1991-2000`, `2001-2010`, `2011-2015`, na.rm = TRUE)),
               aes(x = fct_rev(source_name), xend = fct_rev(source_name), 
                   y = min, yend = max),
               color = accent6, linewidth = 1.3, alpha = 0.8) + 
  annotate('rect', size = 3, 
           xmin = 5.6, xmax = 6.4,
           ymin = -Inf, ymax = Inf,
           alpha = 0.3, fill = '#c7c3c3') +
  geom_point(aes(color = year_group), size = 3) +
  charts.theme + 
  xlab("") +
  ylab("") +
  coord_flip() + 
  labs(subtitle = "WTO-extra") + 
  theme(axis.text.x = element_text(size = 10, angle = 0, vjust = 0.5),
        panel.grid.major.x = element_line(color = "#d4d4d4", linetype = 2, size = 0.3),
        panel.grid.major.y = element_line(color = "transparent"),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, margin = margin(b = 5))) + 
  scale_color_manual(values = c(accent3, main4, main1)) + 
  scale_y_continuous(limits = c(0, 12), breaks = c(5, 10))
fig3.1b

fig3.1 <- ggarrange(fig3.1a, fig3.1b, common.legend = TRUE, legend="bottom")
fig3.1

ggsave(filename="fig3.1.png", plot=fig3.1, device="png", path="./charts", 
       width = 8, height = 3)

## 2. PTA comparison (WTO-x, WTO+) ----

sample = c("EU (28) Enlargement", "CEZ", "EAEC",
           "European Free Trade Association (EFTA)", 
           "ASEAN-Australia-New Zealand", "SAFTA", 
           "Trans-Pacific Strategic Economic Partnership",
           "NAFTA", "Eurasian Economic Union (EAEU)", 
           "CIS", "ASEAN free trade area", "GCC")

pta_sample <- pta |> filter(agreement %in% sample, !is.na(agreement))

fig3.2a <- ggplot(pta_sample,
                 aes(x = fct_reorder(agreement, depth_wto_plus), 
                     y = depth_wto_plus)) +
  geom_point(color = ifelse(pta_sample$agreement %in% c("Eurasian Economic Union (EAEU)", "CIS", "CEZ", "EAEC"), 
                            "#007db7", "#cfcfcf"), 
             size = 3) +
  geom_segment(aes(y = 0,
                   x = agreement,
                   yend = depth_wto_plus,
                   xend = agreement), 
               color = ifelse(pta_sample$agreement %in% 
                                c("Eurasian Economic Union (EAEU)", "CIS", "CEZ", "EAEC"), 
                              "#007db7", "#cfcfcf"), 
               linewidth = 1.3) +
  charts.theme + 
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(size = 10, angle = 0, vjust = 0.5),
        panel.grid.major.x = element_line(color = "#d4d4d4", linetype = 2, size = 0.3),
        panel.grid.major.y = element_line(color = "transparent"),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, margin = margin(b = 5))) + 
  scale_color_manual(values = c("#63ccec", "#007db7")) + 
  scale_y_continuous(breaks = c(0, 5, 10)) +
  scale_x_discrete(labels = c("ASEAN FTA", "SAFTA", "EAEC", "GCC",
                              "AANZFTA", "CIS", "P4", "CEZ",
                              "EAEU", "EFTA", "EU 28", "NAFTA")) + 
  guides(fill=guide_legend(title="", reverse = TRUE)) +
  annotate("rect", xmin = 9.5, xmax = 8.5, 
           ymin = -Inf, ymax = Inf, 
           alpha = 0.15, fill = "#63ccec") + 
  labs(subtitle = "WTO-plus") + 
  coord_flip()
fig3.2a

fig3.2b <- ggplot(pta_sample,
                  aes(x = fct_reorder(agreement, depth_wto_plus), 
                      y = depth_wto_x)) +
  geom_point(color = ifelse(pta_sample$agreement %in% c("Eurasian Economic Union (EAEU)", "CIS", "CEZ", "EAEC"), 
                            "#007db7", "#cfcfcf"), 
             size = 3) +
  geom_segment(aes(y = 0,
                   x = agreement,
                   yend = depth_wto_x,
                   xend = agreement), 
               color = ifelse(pta_sample$agreement %in% 
                                c("Eurasian Economic Union (EAEU)", "CIS", "CEZ", "EAEC"), 
                              "#007db7", "#cfcfcf"), 
               linewidth = 1.3) +
  charts.theme + 
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(size = 10, angle = 0, vjust = 0.5),
        panel.grid.major.x = element_line(color = "#d4d4d4", linetype = 2, size = 0.3),
        panel.grid.major.y = element_line(color = "transparent"),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, margin = margin(b = 5))) + 
  scale_color_manual(values = c("#63ccec", "#007db7")) + 
  scale_y_continuous(breaks = c(0, 10, 20)) +
  scale_x_discrete(labels = c("ASEAN FTA", "SAFTA", "EAEC", "GCC",
                              "AANZFTA", "CIS", "P4", "CEZ",
                              "EAEU", "EFTA", "EU 28", "NAFTA")) + 
  guides(fill=guide_legend(title="", reverse = TRUE)) +
  annotate("rect", xmin = 9.5, xmax = 8.5, 
           ymin = -Inf, ymax = Inf, 
           alpha = 0.15, fill = "#63ccec") + 
  labs(subtitle = "WTO-extra") + 
  coord_flip()
fig3.2b

fig3.2 <- ggarrange(fig3.2a, fig3.2b, common.legend = TRUE, legend="bottom")
fig3.2

ggsave(filename="fig3.2.png", plot=fig3.2, device="png", path="./charts", 
       width = 8, height = 3)


## 3. PTA comparison (core, nonecon) ----

fig3.3a <- ggplot(pta_sample,
                  aes(x = fct_reorder(agreement, depth_wto_plus), 
                      y = depth_core)) +
  geom_point(color = ifelse(pta_sample$agreement %in% c("Eurasian Economic Union (EAEU)", "CIS", "CEZ", "EAEC"), 
                            "#007db7", "#cfcfcf"), 
             stat = "identity", size = 3) +
  geom_segment(aes(y = 0,
                   x = agreement,
                   yend = depth_core,
                   xend = agreement), 
               color = ifelse(pta_sample$agreement %in% 
                                c("Eurasian Economic Union (EAEU)", "CIS", "CEZ", "EAEC"), 
                              "#007db7", "#cfcfcf"), 
               linewidth = 1.3) +
  charts.theme + 
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(size = 10, angle = 0, vjust = 0.5),
        panel.grid.major.x = element_line(color = "#d4d4d4", linetype = 2, size = 0.3),
        panel.grid.major.y = element_line(color = "transparent"),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, margin = margin(b = 5))) + 
  scale_color_manual(values = c("#63ccec", "#007db7")) + 
  scale_y_continuous(breaks = c(0, 5, 10, 15)) +
  scale_x_discrete(labels = c("ASEAN FTA", "SAFTA", "EAEC", "GCC",
                              "AANZFTA", "CIS", "P4", "CEZ",
                              "EAEU", "EFTA", "EU 28", "NAFTA")) + 
  guides(fill=guide_legend(title="", reverse = TRUE)) +
  annotate("rect", xmin = 9.5, xmax = 8.5, 
           ymin = -Inf, ymax = Inf, 
           alpha = 0.15, fill = "#63ccec") + 
  labs(subtitle = "core") + 
  coord_flip()
fig3.3a

fig3.3b <- ggplot(pta_sample,
                  aes(x = fct_reorder(agreement, depth_wto_plus), 
                      y = depth_econ)) +
  geom_point(color = ifelse(pta_sample$agreement %in% c("Eurasian Economic Union (EAEU)", "CIS", "CEZ", "EAEC"), 
                            "#007db7", "#cfcfcf"), 
             stat = "identity", size = 3) +
  geom_segment(aes(y = 0,
                   x = agreement,
                   yend = depth_econ,
                   xend = agreement), 
               color = ifelse(pta_sample$agreement %in% 
                                c("Eurasian Economic Union (EAEU)", "CIS", "CEZ", "EAEC"), 
                              "#007db7", "#cfcfcf"), 
               linewidth = 1.3) +
  charts.theme + 
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(size = 10, angle = 0, vjust = 0.5),
        panel.grid.major.x = element_line(color = "#d4d4d4", linetype = 2, size = 0.3),
        panel.grid.major.y = element_line(color = "transparent"),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, margin = margin(b = 5))) + 
  scale_color_manual(values = c("#63ccec", "#007db7")) + 
  scale_y_continuous(limits = c(0, 8.5), breaks = c(0, 4, 8)) +
  scale_x_discrete(labels = c("ASEAN FTA", "SAFTA", "EAEC", "GCC",
                              "AANZFTA", "CIS", "P4", "CEZ",
                              "EAEU", "EFTA", "EU 28", "NAFTA")) + 
  guides(fill=guide_legend(title="", reverse = TRUE)) +
  annotate("rect", xmin = 9.5, xmax = 8.5, 
           ymin = -Inf, ymax = Inf, 
           alpha = 0.15, fill = "#63ccec") + 
  labs(subtitle = "other economic") + 
  coord_flip()
fig3.3b

fig3.3 <- ggarrange(fig3.3a, fig3.3b, common.legend = TRUE, legend="bottom")
fig3.3

ggsave(filename="fig3.3.png", plot=fig3.3, device="png", path="./charts", 
       width = 8, height = 3)



# [04] Chapter 4 ----
## 1. Correlation: DVA & GDP ----

pta_corr <- kaz_pta |> mutate(highlight = ifelse(receive_iso %in% c("KOR", "NLD", "FRA", "TUR", "USA"), "yes", 
                                           ifelse(receive_iso %in% c("RUS", "KGZ", "ARM"), "highlight", "no")),
                              labels = ifelse(receive_iso %in% c("KGZ", "ARM", "NLD",
                                                                 "FRA", "TUR"), receive_name, 
                                        ifelse(receive_iso == "USA", "USA",
                                         ifelse(receive_iso == "RUS", "Russia",
                                          ifelse(receive_iso == "KOR", "S. Korea", " ")))))

fig4.1 <- ggplot(pta_corr |>  filter(DVA >= 0, t == 2019) |>  distinct(receive_iso, receive_name, .keep_all = TRUE), 
                 aes(x = log(DVA), y = log(gdpcap_d), label = labels)) +
  geom_jitter(aes(color = highlight, size = highlight)) + 
  geom_smooth(method = lm, color = "#424242", size = 0.5, alpha = 0.15) +
  geom_text(vjust = 0.05, nudge_y = 0.3, size = 3.5) + 
  charts.theme + 
  xlab("(log) DVA") +
  ylab("(log) GDP") + 
  scale_color_manual(values = c(accent3, '#cfcfcf', main1)) + 
  scale_size_manual(values = c(4, 3, 4)) +
  scale_y_continuous(limits = c(0,5), breaks = c(0, 2, 4)) +
  theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5),
        panel.grid.major.x = element_line(color = "#d4d4d4", linetype = 2, size = 0.3),
        panel.grid.major.y = element_line(color = "#d4d4d4", linetype = 2, size = 0.3)) +
  guides(color = "none", size = "none") 
fig4.1

ggsave(filename="fig4.1.png", plot=fig4.1, device="png", path="./charts", 
       width = 7, height = 3)

## 2. Correlation: DVA & distance ----

fig4.2 <- ggplot(pta_corr |>  filter(DVA >= 0, t == 2019) |>  distinct(receive_iso, receive_name, .keep_all = TRUE), 
                 aes(x = log(DVA), y = log(distcap), label = labels)) +
  geom_jitter(aes(color = highlight, size = highlight)) + 
  geom_smooth(method = lm, color = "#424242", size = 0.5, alpha = 0.15) +
  geom_text(hjust = 1.05, nudge_y = 0.28, size = 3.5) + 
  charts.theme + 
  xlab("(log) DVA") +
  ylab("(log) Distance") + 
  scale_color_manual(values = c(accent3, '#cfcfcf', main1)) + 
  scale_size_manual(values = c(4, 3, 4)) +
  scale_y_continuous(limits = c(5.5,10.5), breaks = c(6, 8, 10)) +
  theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5),
        panel.grid.major.x = element_line(color = "#d4d4d4", linetype = 2, size = 0.3),
        panel.grid.major.y = element_line(color = "#d4d4d4", linetype = 2, size = 0.3)) +
  guides(color = "none", size = "none") 
fig4.2

ggsave(filename="fig4.2.png", plot=fig4.2, device="png", path="./charts", 
       width = 8, height = 3)


# [05] Old Charts
## 1. Correlation: DVA & depth ----
B1 <- df.agg %>% filter(pta == 1) %>% 
      distinct(., agreement, .keep_all = TRUE) %>% 
      select(MRIO.Partner:Economy, DVA:entry_force, depth:pta, WB_classification.x, 
             WB_classification.y, Continent.x, Continent.y, cover)


  # correlation plot: DVA_INT and Depth 
C0 <- ggplot(B1, aes(x = depth, y = log(DVA_inter), label = agreement)) + 
  geom_jitter(aes(color = fct_rev(cover)), size = 6, stat = "identity", alpha = 0.9) +
  geom_smooth(method='lm', formula=y~x, level = 0.90, 
              colour = "black", size = 0.3, linetype = "dashed", fill = "#bdbdbd", alpha = 0.3) + 
  #geom_hline(yintercept = 1, size = 0.5, color = "#787878", linetype = 2) +
  #geom_vline(xintercept = 1, size = 0.5, color = "#787878", linetype = 2) +
  #geom_text(aes(color = cover), nudge_x = 0.09, fontface = "bold") +
  charts.theme + 
  scale_color_manual(values = c("#007db7", "#f57f29")) + 
  xlab("PTA depth") +
  ylab("Value added in intermediates") +
  theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5)) +
  scale_y_continuous(limits = c(0,11), breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_x_continuous(limits = c(0,40), breaks = c(0, 10, 20, 30 ,40)) +
  #labs(title = "Domestic Value Added and PTA Depth" ,
  #subtitle = "Selected Countries, 2000, 2007-2015",
  #caption = "Source: ADB Multiregional Input-Output Tables; World Bank Content of Trade Agreements Database") +
  guides(fill=guide_legend(title=""))
C0      

ggsave(filename="C0.png", plot=C0, device="png", path=pathcharts, 
       width = 10, height = 6)

  # correlation plot: DVA_INT and Depth 
C1 <- ggplot(B1, aes(x = depth, y = log(DVA_INTt), label = agreement)) + 
  geom_point(aes(color = fct_rev(cover)), size = 7, stat = "identity") +
  geom_smooth(method='lm', formula=y~x, level = 0.90, 
              colour = "black", size = 1, linetype = 3, fill = "#bdbdbd", alpha = 0.2) + 
  #geom_hline(yintercept = 1, size = 0.5, color = "#787878", linetype = 2) +
  #geom_vline(xintercept = 1, size = 0.5, color = "#787878", linetype = 2) +
  #geom_text(aes(color = cover), nudge_x = 0.09, fontface = "bold") +
  charts.theme + 
  scale_color_manual(values = c("#007db7", "#f57f29")) + 
  xlab("PTA depth") +
  ylab("log(DVA_INT)") +
  theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5)) +
  scale_y_continuous(limits = c(0,11), breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_x_continuous(limits = c(0,40), breaks = c(0, 10, 20, 30 ,40)) +
  #labs(title = "Domestic Value Added and PTA Depth" ,
  #subtitle = "Selected Countries, 2000, 2007-2015",
  #caption = "Source: ADB Multiregional Input-Output Tables; World Bank Content of Trade Agreements Database") +
  guides(fill=guide_legend(title="")) + 
  annotate("rect", xmin = 32, xmax = 40, ymin = 5, ymax = 8.7, 
           alpha = 0, color = "#007db7", size = 1.2) + 
  annotate("text", x = 36, y = 9, label = "EU Enlargement",
           color = "#007db7", fontface = "bold", size = 4) + 
  annotate("text", x = 24, y = 4.8, label = "EAEU",
           color = "#007db7", fontface = "bold", size = 4)
C1      

ggsave(filename="C1.png", plot=C1, device="png", path=pathcharts, 
       width = 12, height = 10)


  # correlation plot: FVA and Depth   
C2 <- ggplot(B1, aes(x = depth, y = log(FVA), label = agreement)) + 
  geom_point(aes(color = fct_rev(cover)), size = 7, stat = "identity") +
  geom_smooth(method='lm', formula=y~x, level = 0.90, 
              colour = "black", size = 1, linetype = 3, fill = "#bdbdbd", alpha = 0.2) + 
  charts.theme + 
  scale_color_manual(values = c("#007db7", "#f57f29")) + 
  xlab("PTA depth") +
  ylab("log(FVA)") +
  theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5)) +
  scale_y_continuous(limits = c(0,11), breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_x_continuous(limits = c(0,40), breaks = c(0, 10, 20, 30 ,40)) +
  #labs(title = "Foreign Value Added and PTA Depth" ,
  #subtitle = "Selected Countries, 2000, 2007-2015",
  #caption = "Source: ADB Multiregional Input-Output Tables; World Bank Content of Trade Agreements Database") +
  guides(fill=guide_legend(title=""))
C2      

ggsave(filename="C2.png", plot=C2, device="png", path=pathcharts, 
       width = 12, height = 10)


## 3. Core depth ----

C4 <- ggplot(B2, aes(x = fct_reorder(agreement, core), y = core)) +
  geom_point(color = ifelse(B2$agreement %in% c("Eurasian Economic Union (EAEU)", "CIS", "CEZ", "EAEC"), "#007db7", "#cfcfcf"), 
             stat = "identity", size = 6) +
  geom_segment(aes(y = 0,
                   x = agreement,
                   yend = core,
                   xend = agreement), 
               color = ifelse(B2$agreement %in% c("Eurasian Economic Union (EAEU)", "CIS", "CEZ", "EAEC"), "#007db7", "#cfcfcf"), 
               size = 1.5) +
  #geom_bar(aes(fill = depth), width = 0.5, stat = "identity", position = "dodge") +
  charts.theme + 
  xlab("") +
  ylab("Core depth") +
  theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5),
        panel.grid.major.x = element_line(color = "#d4d4d4", linetype = 2),
        panel.grid.major.y = element_line(color = "transparent")) +
  scale_color_manual(values = c("#63ccec", "#007db7")) + 
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18)) +
  #labs(title = "Depth of Selected \n Preferential Trade Agreements",
  #caption = "World Bank Content of Trade Agreements Database") +
  guides(fill=guide_legend(title="", reverse = TRUE)) +
  annotate("rect", xmin = 12.5, 
           xmax = 11.5, ymin = 0, ymax = 18, 
           alpha = 0.15, fill = "#63ccec") + 
  coord_flip()
C4      

ggsave(filename="C4.png", plot=C4, device="png", path=pathcharts, 
       width = 12, height = 6)

## 4. WTO+, WTOx ----

eaeu <- c("Eurasian Economic Union (EAEU)", "CIS", "CEZ", "EAEC")

B4 <- B2 %>% pivot_longer(., c(wtoplus, wtox), 
                          values_to = "DepthScore", names_to = "Provision") %>% 
             mutate(Provision = ifelse(Provision == "wtoplus", "WTO-plus", "WTO-X")) %>% 
             mutate(group = ifelse(agreement %in% eaeu, "A", "B"))

C5 <- ggplot(B4, aes(x = fct_reorder(agreement, depth), y = DepthScore)) +
  geom_bar(aes(fill = fct_rev(Provision), alpha = group), width = 0.5, stat = "identity") +
  charts.theme + 
  xlab("") +
  ylab("PTA depth") +
  theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5),
        panel.grid.major.x = element_line(color = "#d4d4d4", linetype = 2),
        panel.grid.major.y = element_line(color = "transparent")) +
  scale_fill_manual(values = c("#63ccec", "#007db7")) + 
  scale_alpha_manual(values = c(1, 0.5)) + 
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50)) +
  #labs(title = "Depth of Selected \n Preferential Trade Agreements",
  #caption = "World Bank Content of Trade Agreements Database") +
  guides(fill=guide_legend(title="", reverse = TRUE), alpha = "none") + 
  coord_flip()
C5      

ggsave(filename="C5.png", plot=C5, device="png", path=pathcharts, 
       width = 12, height = 6)

## 5. Density ----

B5 <- B1 %>% 
  mutate(group = ifelse(MRIO.Partner == "KAZ", "Kazakhstan",
                        Continent.x))
B5$group <- factor(B5$group, 
                   levels = c("Kazakhstan", "Asia", "North America", "Europe"))

mu <- B5 %>% 
  group_by(group) %>% 
  summarise(mean = mean(depth))

C6 <- ggplot(B5, aes(x = depth, y = fct_rev(group), group = group)) +
  geom_density_ridges(scale = 0.9, rel_min_height = 0.001,
                      quantile_lines = TRUE,
                      quantile_fun=function(x,...)mean(x),
                      aes(fill = group), alpha = 0.9) + 
  charts.theme + 
  xlab("Total Depth") +
  ylab("") + 
  scale_fill_manual(values = c("#007db7", "#8dc63f", "#f2e600", "#e9532b")) + 
  scale_x_continuous(limits = c(0,40), breaks = c(0, 10, 20, 30 , 40, 50)) +
  theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5)) +
  guides(fill = "none") 
C6

ggsave(filename="C6.png", plot=C6, device="png", path=pathcharts, 
       width = 12, height = 6)

