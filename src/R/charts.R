
# [00] Setup ----

libraries = c("readxl",                     
              "openxlsx",                   
              "tidyverse",
              "ggridges", 
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

## 2. Correlation ----

pta_corr <- pta |> filter(`_merge` == "both") |> 
            select(t:partners, agreement, depth_wto_plus:depth_nonecon) |> 
            pivot_longer(depth_wto_plus:depth_nonecon, names_to = "depth", values_to = "depth_val")
  
fig2.3 <- ggplot(pta_corr, aes(x = depth_val, y = log(DVA_int))) + 
  geom_jitter(size = 6, stat = "identity", alpha = 0.9) +
  geom_smooth(method='lm', formula=y~x, level = 0.90, 
              colour = "black", size = 0.3, linetype = "dashed", fill = "#bdbdbd", alpha = 0.3) + 
  facet_grid(~depth) + 
  #geom_hline(yintercept = 1, size = 0.5, color = "#787878", linetype = 2) +
  #geom_vline(xintercept = 1, size = 0.5, color = "#787878", linetype = 2) +
  #geom_text(aes(color = cover), nudge_x = 0.09, fontface = "bold") +
  charts.theme 
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
fig2.3
  

## 1. Correlation ----
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

## 2. PTA depth total ----

sample = c("EEA", "EU (28) Enlargement", "EC Treaty", "CEZ", "EAEC",
           "European Free Trade Association (EFTA)", "Japan-ASEAN", "ASEAN-Korea",
           "ASEAN-Australia-New Zealand", "SAFTA", "CEFTA", "Trans-Pacific Strategic Economic Partnership",
           "NAFTA", "Eurasian Economic Union (EAEU)", "CIS", "ASEAN free trade area")

B2 <- B1 %>% filter(agreement %in% sample) 
      #gather(., "depth", "value", depth:core)

C3 <- ggplot(B2, aes(x = fct_reorder(agreement, depth), y = depth)) +
  geom_point(color = ifelse(B2$agreement %in% c("Eurasian Economic Union (EAEU)", "CIS", "CEZ", "EAEC"), "#007db7", "#cfcfcf"), 
             stat = "identity", size = 6) +
  geom_segment(aes(y = 0,
                   x = agreement,
                   yend = depth,
                   xend = agreement), 
                   color = ifelse(B2$agreement %in% c("Eurasian Economic Union (EAEU)", "CIS", "CEZ", "EAEC"), "#007db7", "#cfcfcf"), 
                   size = 1.5) +
  #geom_bar(aes(fill = depth), width = 0.5, stat = "identity", position = "dodge") +
  charts.theme + 
  xlab("") +
  ylab("PTA depth") +
  theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5),
        panel.grid.major.x = element_line(color = "#d4d4d4", linetype = 2)) +
  scale_color_manual(values = c("#63ccec", "#007db7")) + 
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40)) +
  #labs(title = "Depth of Selected \n Preferential Trade Agreements",
       #caption = "World Bank Content of Trade Agreements Database") +
  guides(fill=guide_legend(title="", reverse = TRUE)) +
  annotate("rect", xmin = 13.5, 
           xmax = 12.5, ymin = 0, ymax = 40, 
           alpha = 0.15, fill = "#63ccec") + 
  coord_flip()
C3      

ggsave(filename="C3.png", plot=C3, device="png", path=pathcharts, 
       width = 12, height = 6)

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



## 6. Correlation: DVA & GDP ----

B7 <- df.agg %>% filter(MRIO.Economy == "KAZ") %>% 
  mutate(Continent = ifelse(Continent.y %in% c("Asia", "Australia"), "Asia and the Pacific",
                      ifelse(Continent.y == "Europe", "Europe", "Americas")))

C7 <- ggplot(B7 %>% filter(DVA >= 0), aes(x = log(DVA), y = log(gdp_d))) +
  geom_jitter(aes(color = Continent), size = 3, alpha = 0.7) +
  geom_smooth(method = lm, color = "black", size = 0.3, linetype = "dashed") +
  charts.theme + 
  xlab("(log) Bilateral Value Added Trade") +
  ylab("(log) GDP of Destination") + 
  scale_color_manual(values = c("#e9532b","#007db7","#8dc63f")) + 
  #scale_x_continuous(limits = c(0,40), breaks = c(0, 10, 20, 30 , 40, 50)) +
  theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5)) +
  guides(fill = "none") 
C7

ggsave(filename="C7.png", plot=C7, device="png", path=pathcharts, 
       width = 12, height = 6)

## 7. Correlation: DVA & Distance ----

C8 <- ggplot(B7 %>% filter(year == 2015), aes(x = log(DVA), y = distcap)) +
  geom_jitter(color = "#007db7", size = 2.5, alpha = 0.7) +
  geom_smooth(method = lm, color = "black", size = 0.3, linetype = "dashed") +
  charts.theme + 
  xlab("(log) Bilateral Value Added Trade") +
  ylab("Distance between Origin and Destination") + 
  #scale_color_manual(values = c("#e9532b","#007db7","#8dc63f")) + 
  scale_y_continuous(limits = c(0,15000), 
                     breaks = c(0, 2500, 5000, 7500 , 10000, 12500, 15000)) +
  theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5)) +
  guides(fill = "none") 
C8

ggsave(filename="C8.png", plot=C8, device="png", path=pathcharts, 
       width = 12, height = 6)

