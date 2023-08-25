### TRADE AGREEMENTS NETWORK 

# I. Preliminaries --------------------------------------------------------

# packages
pacman::p_load(tidyverse, readxl, openxlsx, RDS, scales, knitr, kableExtra, grid, 
               gridExtra, patchwork, ggpubr, lubridate, plyr, ggridges, vtable,
               readr, stringr, quanteda, quanteda.textstats, rvest, tibble, xml2,
               igraph, statnet, intergraph, modelsummary, magick)

# directories
wd = getwd()
pathdata = paste0(wd, "/data")
pathcharts = paste0(wd, "/charts")

# charts themes
source(paste0(wd, "/code/src_charts.R"))


# II. Prepare data --------------------------------------------------------

  # trade agreements dataframe 
trade_df <- read_excel(paste0(pathdata, "/03_trade_networks_clean.xlsx"))
trade_df <- trade_df %>% 
  dplyr::rename(., "enforce" = "wto_x_le", "nonecon" = "wto_x_nonecon", "econ" = "wto_x_econ") %>% 
  select(., -starts_with("wto_X"), -starts_with("wto_plus"))

  # add political freedom attribute
fiw_dict <- read_excel(paste0(pathdata, "/FIW_dictionary.xlsx"))

  # attributes dataframe
countries <- rbind(trade_df %>% select(iso1) %>% dplyr::rename(iso=iso1), 
                   trade_df %>% select(iso2) %>% dplyr::rename(iso=iso2)) %>% distinct()
    ## regions
regions <- read_excel(paste0(pathdata, "/WB_regions.xls"), skip=4)
regions <- regions %>% select(Economy, Code, Region_Clean, `Income group`) %>% 
  filter(!is.na(Region_Clean)) %>% dplyr::rename(income_group=`Income group`)

    ## economic size
gdp <- read_excel(paste0(pathdata, "/WB_economy.xlsx")) %>% 
  filter(!is.na(`Series Code`)) %>% select(c(4, 14))
colnames(gdp) <- c("country", "gdp")
gdp <- gdp %>% mutate(log_gdp = log(as.numeric(gdp))) %>% 
  mutate(log_gdp = replace_na(log_gdp, min(log_gdp, na.rm = TRUE)))

    ## attributes to numeric
attr <- left_join(countries, regions, by = c("iso"="Code"))
attr <- left_join(attr, gdp, by = c("iso"="country"))
attr <- left_join(attr, fiw_dict, by = c("iso"="iso1")) %>% 
  select(!c(country, gdp))

attr$region_code <- as.character(as.integer(factor(attr$Region_Clean)))
attr$income_code <- as.character(as.integer(factor(attr$income_group)))
attr$fiw_code <- as.character(as.integer(factor(attr$fiw)))

attr <- attr |> mutate(fiw_code = ifelse(fiw_code == 1, 4, fiw_code)) # for serbia


# III. Construct networks -------------------------------------------------

### Full ----

  # trade agreements matrix (unweighted)
trade_uw <- trade_df %>% select(iso1, iso2) %>% 
  as.matrix()
  
  # nodes names as character
trade_uw[,1] <- as.character(trade_uw[,1])
trade_uw[,2] <- as.character(trade_uw[,2])

  # trade network
trade_net <- graph.edgelist(trade_uw, directed = F) # plot edges
linked_ids <- match(V(trade_net)$name, attr$iso) # define nodes 

  # node attributes 
V(trade_net)$region <- attr$region_code[linked_ids]
V(trade_net)$gdp <- attr$log_gdp[linked_ids]
V(trade_net)$income <- attr$income_code[linked_ids]
V(trade_net)$political <- attr$fiw_code[linked_ids]
V(trade_net)$transitivity <- transitivity(trade_net, type="local", isolates = "zero")

trade_net <- igraph::delete.vertices(trade_net, which(is.na(V(trade_net)$region)))
trade_net <- igraph::delete.vertices(trade_net, which(is.na(V(trade_net)$gdp)))
trade_net <- igraph::delete.vertices(trade_net, which(is.na(V(trade_net)$income)))
trade_net <- igraph::delete.vertices(trade_net, which(is.na(V(trade_net)$political)))  

### Deep ----
trade_nonecon <- trade_df %>% filter(nonecon == 1)
trade_econ <- trade_df %>% filter(econ == 1)

# trade agreements matrix (unweighted)
trade_ne <- trade_nonecon %>% select(iso1, iso2) %>% 
  as.matrix()

trade_oe <- trade_econ %>% select(iso1, iso2) %>% 
  as.matrix()

# trade network
trade_net_ne <- graph.edgelist(trade_ne, directed = F) # plot edges
linked_ids_ne <- match(V(trade_net_ne)$name, attr$iso) # define nodes 

# trade network
trade_net_oe <- graph.edgelist(trade_oe, directed = F) # plot edges
linked_ids_oe <- match(V(trade_net_oe)$name, attr$iso) # define nodes 

# node attributes 
V(trade_net_oe)$region <- attr$region_code[linked_ids_oe]
V(trade_net_oe)$gdp <- attr$log_gdp[linked_ids_oe]
V(trade_net_oe)$income <- attr$income_code[linked_ids_oe]
V(trade_net_oe)$political <- attr$fiw_code[linked_ids_oe]
V(trade_net_oe)$transitivity <- transitivity(trade_net_oe, type="local", isolates = "zero")

trade_net_oe <- igraph::delete.vertices(trade_net_oe, which(is.na(V(trade_net_oe)$region)))
trade_net_oe <- igraph::delete.vertices(trade_net_oe, which(is.na(V(trade_net_oe)$gdp)))
trade_net_oe <- igraph::delete.vertices(trade_net_oe, which(is.na(V(trade_net_oe)$income)))
trade_net_oe <- igraph::delete.vertices(trade_net_oe, which(is.na(V(trade_net_oe)$political)))  

# III. Descriptives -------------------------------------------------------

region = assortativity_nominal(trade_net, types = as.numeric(V(trade_net)$region) + 1, directed=FALSE) # region 
income = assortativity_nominal(trade_net, types = as.numeric(V(trade_net)$income) + 1, directed=FALSE) # income class
fiw = assortativity_nominal(trade_net, types = as.numeric(V(trade_net)$political) + 1, directed=FALSE) # fiw 
gdp = assortativity(trade_net, types1 = as.numeric(V(trade_net)$gdp), directed=FALSE) # gdp

region_oe = assortativity_nominal(trade_net_oe, types = as.numeric(V(trade_net_oe)$region) + 1, directed=FALSE) # region 
income_oe = assortativity_nominal(trade_net_oe, types = as.numeric(V(trade_net_oe)$income) + 1, directed=FALSE) # income class
fiw_oe = assortativity_nominal(trade_net_oe, types = as.numeric(V(trade_net_oe)$political) + 1, directed=FALSE) # fiw 
gdp_oe = assortativity(trade_net_oe, types1 = as.numeric(V(trade_net_oe)$gdp), directed=FALSE) # gdp

assort <- data.frame(matrix(ncol=3, nrow=0))
colnames(assort) <- c("attribute", "assortativity_score", "network")

assort <- rbind(assort, data.frame(attribute = "region", assortativity_score = region, network = "full"))
assort <- rbind(assort, data.frame(attribute = "income", assortativity_score = income, network = "full"))
assort <- rbind(assort, data.frame(attribute = "political", assortativity_score = fiw, network = "full"))
assort <- rbind(assort, data.frame(attribute = "gdp", assortativity_score = gdp, network = "full"))

assort <- rbind(assort, data.frame(attribute = "region", assortativity_score = region_oe, network = "deep"))
assort <- rbind(assort, data.frame(attribute = "income", assortativity_score = income_oe, network = "deep"))
assort <- rbind(assort, data.frame(attribute = "political", assortativity_score = fiw_oe, network = "deep"))
assort <- rbind(assort, data.frame(attribute = "gdp", assortativity_score = gdp_oe, network = "deep"))

## Plot assortativity ----
assort <- assort %>% 
  group_by(network) %>% 
  mutate(score = ifelse(network == "deep", -1*assortativity_score, assortativity_score)) %>% 
  mutate(score = ifelse(network == "deep" & attribute == "gdp", -1*score, score))

plot_assort <-  ggplot(assort, aes(x=fct_rev(attribute), y=assortativity_score, fill=fct_rev(network))) + 
  geom_bar(stat="identity", position=position_dodge(width=0.9), width=0.8) + 
  labs(title="Assortativity Scores with Select Node Attributes", 
       subtitle="Full vs Deep Trade Networs") + 
  xlab("attribute") + 
  ylab("assortativity score") + 
  #scale_y_continuous(limits = c(-40, 40), breaks = c(-40, -20, 0, 20, 40), labels = abs) + 
  scale_fill_manual(values = c("#2A769E", "#C85B89")) + 
  #scale_alpha_manual(values = c(0.5, 1)) + 
  charts.theme +
  coord_flip()
plot_assort

ggsave(paste0(pathcharts, "/grouped_bar_chart.png"), 
       plot=plot_assort, width=8, height=6, dpi=300)

# IV. ERG ---------------------------------------------------------------------

## Full network ----

trade_ergm <- asNetwork(trade_net)
trade_ergm

  # sample
random_graph <- ergm(trade_ergm ~ edges, control = control.ergm(seed = 1234))

inv.logit <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

theta <- coef(random_graph)
inv.logit(theta)

summary(random_graph)

model1 <- ergm(trade_ergm ~ edges + 
                 #nodefactor("region") + 
                 nodematch("region") +
                 #nodefactor("income") + 
                 nodematch("income") + 
                 #nodefactor("political") + 
                 nodematch("political", diff = T) + 
                 #nodematch("gdp") + 
                 nodematch("transitivity"))
summary(model1)

broom::tidy(model1, statistic = TRUE, conf.int = TRUE, conf.level = 0.95) %>% 
  kable(caption = "Table 1: Regression Results - Model 1", digits = 3) %>% 
  kable_classic(full_width = F, html_font = "Cambria")

indepvars = c("edges", 
              'nodematch("region")', 
              'nodematch("income")', 
              'nodematch("gdp")', 
              'nodematch("political")',
              'nodematch("transitivity")')

predict_iter <- function(indep_var) {
  ergm(as.formula(paste("trade_ergm ~ ", paste(indep_var), collapse = "+")))
}

predict <- indepvars %>% 
  map(predict_iter)  

# add results for regression including all independent variables
predict[[7]] <- model1

modelsummary(predict, 
             estimate = c("{estimate}{stars}"),
             output = "kableExtra") %>% 
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  kable_styling(font_size = 16)


  # plot coefficients
modelplot(list(model1), coef_omit = "Interc") + 
  charts.theme + 
  scale_color_manual(values = c("#007db7")) + 
  labs(title = "Predictors of Political Unfriending",
       subtitle = "Regression Results, Model 1 vs Model 2")
  #scale_x_continuous(limits = c(-0.3,0.3),
                     #breaks = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3))


inv.logit <- function(model1){
  odds <- exp(model1)
  prob <- odds / (1 + odds)
  return(prob)
}

theta <- coef(model1)
inv.logit(theta)

## Non-econ agreements ----
trade_ergm_oe <- asNetwork(trade_net_oe)
trade_ergm_oe

model2 <- ergm(trade_ergm_oe ~ edges + 
                 #nodefactor("region") + 
                 nodematch("region") +
                 #nodefactor("income") + 
                 nodematch("income") + 
                 #nodefactor("political") + 
                 nodematch("political", diff = T) + 
                 #nodematch("gdp") + 
                 nodematch("transitivity"))

summary(model2)

broom::tidy(model2, statistic = TRUE, conf.int = TRUE, conf.level = 0.95) %>% 
  kable(caption = "Table 1: Regression Results - Model 1", digits = 3) %>% 
  kable_classic(full_width = F, html_font = "Cambria")


predict_iter_ne <- function(indep_var) {
  ergm(as.formula(paste("trade_ergm_ne ~ ", paste(indep_var), collapse = "+")))
}

predict_ne <- indepvars %>% 
  map(predict_iter_ne)  

# add results for regression including all independent variables
predict_ne[[7]] <- model2

modelsummary(predict_ne, 
             estimate = c("{estimate}{stars}"),
             output = "kableExtra") %>% 
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  kable_styling(font_size = 16)

inv.logit <- function(model2){
  odds <- exp(model2)
  prob <- odds / (1 + odds)
  return(prob)
}

theta <- coef(model2)
inv.logit(theta)



# plot coefficients
plot_coeff <- modelplot(list(model1, model2), 
                        coef_omit = "Interc|edges",
                        coef_map = c( "nodematch.transitivity" = "transitivity",
                                      "nodematch.political.4" = "political: partially free",
                                      "nodematch.political.3" = "political: not free",
                                      "nodematch.political.2" = "political: free",
                                      "nodematch.income" = "income class",
                                      "nodematch.region" = "region"),
                        linewidth = 0.6,
                        size = 0.8) + 
  geom_vline(xintercept = 0, size = 0.3, color = "black") + 
  charts.theme + 
  scale_color_manual(values = c("#007db7", "#e9532b"),
                     labels=c("conventional", "deep")) + 
  xlab("coefficients and 95% confidence intervals") + 
  #labs(title = "Determinants of Trade Agreement Formation",
       #subtitle = "Exponential Random Graph Results, Deep vs Full PTA Networks") + 
  theme(legend.background = element_rect(fill = "transparent", color = "transparent"),
        axis.text.x = element_text(size = 10, angle = 0, vjust = 0.5),
        panel.grid.major.x = element_line(color = "#d4d4d4", linetype = 2, linewidth = 0.3),
        panel.grid.minor.x = element_line(color = "transparent"),
        panel.grid.major.y = element_line(color = "transparent"),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, margin = margin(b = 5))) 
plot_coeff

ggsave(filename="fig5.3.png", plot=plot_coeff, device="png", 
       path="/Users/janinedevera/Documents/ADB/WTO KAZ/KAZ-gvc-pta/charts", 
       width = 7, height = 4)

