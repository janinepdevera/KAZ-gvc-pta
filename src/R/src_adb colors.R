
# MAIN PALETTE ----
main1 <- "#007db7"   # main 1
main2 <- "#0088c7"   # main 2
main3 <- "#0099d8"   # main 3
main4 <- "#00a1cb"   # main 4
main5 <- "#41bee8"   # main 6 
main6 <- "#68c5ea"   # main 7
main7 <- "#6dbcf6"   # main 8
main8 <- "#6dcff6"   # main 9


# ACCENT PALETTE ----
accent1 <- "#00a5d2"   # accent 1 - blue
accent2 <- "#8dc63f"   # accent 2 - green 
accent3 <- "#e9532b"   # accent 3 - red
accent4 <- "#c8da2b"   # accent 5 - yellow green
accent5 <- "#f57f29"   # accent 6 - orange 
accent6 <- "#63ccec"   # accent 7 - light blue
accent7 <- "#f2e600"   # accent 8 - yellow
accent8 <- "#fdb515"   # accent 9 - light orange


# CHARTS THEME ----
charts.theme <- theme(axis.title.y.left = element_text(size = 10, margin = margin(r = 12)),
                      axis.title.y.right = element_text(size = 10, margin = margin(l = 12)),
                      axis.title.x = element_text(size = 10, margin = margin(t = 15)),
                      #axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5),
                      axis.text.x = element_text(size = 10),
                      axis.text.y = element_text(size = 10),
                      axis.ticks = element_blank(),
                      #axis.line.x = element_line("black", size = 0.5), 
                      #axis.line.y = element_line("black", size = 0.5),
                      axis.line.x = element_line("transparent", size = 0.5), 
                      axis.line.y = element_line("transparent", size = 0.5),
                      panel.border = element_rect(color = "#a3a3a3", fill = "transparent"),
                      panel.background = element_rect(fill = "white", color = "white"),
                      #panel.grid.major = element_line(color = "white"),
                      #panel.grid.minor = element_line(color = "white"),
                      panel.grid.major.y = element_line(color = "#d4d4d4", linetype = 2, size = 0.3),
                      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
                      plot.subtitle = element_text(size = 11, face = "italic", hjust = 0.5, margin = margin(b = 15)),
                      legend.position = "bottom",
                      legend.box = "vertical",
                      legend.box.margin = margin(b = 10),
                      legend.margin = margin(r = 10),
                      legend.background = element_rect(fill = "transparent"),
                      legend.spacing.x = unit(0.4, "cm"),
                      legend.box.spacing = unit(0, "pt"),
                      legend.key = element_blank(),
                      legend.title = element_blank(),
                      legend.text = element_text(size = 10),
                      plot.caption = element_text(size = 10, hjust = 0),
                      strip.background = element_rect(fill = "transparent"),
                      strip.text = element_text(size = 10))

  

# ACCENT PALETTE 15 ----
accentplus1  <-  "#c8da2b" 
accentplus2  <-  "#8dc63f"
accentplus3  <-  "#68c5ea" 
accentplus4  <-  "#00a1cb"
accentplus5  <-  "#0088c7"
accentplus6  <-  "#007db7"
accentplus7  <-  "#e33000"
accentplus8  <-  "#e9532b"
accentplus9  <-  "#f57f29"
accentplus10 <-  "#fdb515"
accentplus11 <-  "#ffc547"
accentplus12 <-  "#fcd06f" 
accentplus13 <-  "#ccc200"
accentplus14 <-  "#e8df38"
accentplus15 <-  "#f2e600"



