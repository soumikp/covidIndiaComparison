require(tidyverse)
require(lubridate)
require(ggsci)
require(glue)
require(latex2exp)
require(cowplot)
require(ggridges)

address <- "~/Box/COVID India Comparisons/Revisions/covidIndiaComparison/revised_output/clean_revision/"
save.address <- "/Users/soumikp/Box/COVID India Comparisons/Revisions/covidIndiaComparison/revised_figures/"


obs <- read_csv(paste0(address, "observed.csv")) %>% 
  filter(Date >= "2020-10-18" & Date <= "2020-12-31") %>% 
  rename(date = Date) 
bl <- read_csv(paste0(address, "baseline.csv")) %>% 
  filter(date >= "2020-10-18" & date <= "2020-12-31")
es <- read_csv(paste0(address, "esir.csv")) %>% 
  filter(date >= "2020-10-18" & date <= "2020-12-31")
sp <- read_csv(paste0(address, "saphire.csv")) %>% 
  filter(date >= "2020-10-18" & date <= "2020-12-31")
sf <- read_csv(paste0(address, "seirfansy.csv")) %>% 
  filter(date >= "2020-10-18" & date <= "2020-12-31")
icm <- read_csv(paste0(address, "icm.csv")) %>% 
  filter(date >= "2020-10-18" & date <= "2020-12-31")

#### figure for active reported cases ####
arc.estim <- full_join(full_join(obs %>% 
                                   select(date, active.case), 
                                 es %>% 
                                   select(date, esir.arc.estim)),
                       sf %>% 
                         select(date, seirf.arc.estim))
arc <- arc.estim %>% 
  pivot_longer(cols = -date) %>% 
  mutate(date = as_date(date)) %>% 
  mutate(name = ifelse(name == "active.case", "Observed", 
                       ifelse(name == "esir.arc.estim", "eSIR", "SEIR-fansy"))) %>% 
  mutate(value = value/100000) %>%  
  ggplot(aes(x = date, y = value, color = name)) + 
  geom_line(size = 1) + 
  geom_point(color = "black", size = 0.6) + 
  scale_color_nejm() + 
  labs(
    title    = "Time series plot of projected and observed active reported cases from October 16 to December 31, 2020.",
    subtitle = glue("Projections are based on training data for India from March 18 to October 18, 2020.\n",
                    "Supplementary Table S1 describes parameter values used to generate these projections in detail."
    ), 
    x        = "Date",
    y        = TeX("Cases $\\left(\\times 10^6\\right)$"),
    color = "Model",
    caption  = glue(
      #"**\uA9 COV-IND-19 Study Group**<br>",
      "**Data Source:** covid19india.org<br>",
      "**Note:**<br>",
      " - We do not include projections from the baseline and SAPHIRE models as they do not yield active case count projections.<br>", 
      " - We do not include projections from the ICM model it yields only total (reported + unreported) case counts."
    )
  ) +
  theme_bw() +
  theme(
    #text               = element_text(family = "Helvetica Neue"),
    plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 16, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(hjust = 0,size = 16, lineheight = 1.1), 
    axis.text          = element_text(size = 16, color = "#36454f"),
    axis.title         = element_text(size = 16),
    legend.title = ggtext::element_markdown(size = 16),
    legend.text = ggtext::element_markdown(size = 16),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 60, vjust = 0.5)
  ) + 
  scale_x_date(date_breaks = "1 week" , date_labels = "%b %d")

ggsave(paste0(save.address, "arc.pdf"), 
       device = cairo_pdf(), 
       width = 16, 
       height = 2*16/3, 
       units = "in", 
       dpi = 300)
dev.off()



#### figure for cumulative reported cases ####
crc.estim <- full_join(full_join(full_join(full_join(obs %>% select(date, total.case),
                                                     bl %>% select(date, bl.crc.estim)),
                                           es %>% select(date, esir.crc.estim)),
                                 sp %>% select(date, sap.crc.estim)), 
                       sf %>% select(date, seirf.crc.estim))
crc.estim %>% 
  pivot_longer(cols = -date) %>% 
  mutate(date = as_date(date)) %>% 
  mutate(name = ifelse(name == "total.case", "Observed", 
                       ifelse(name == "bl.crc.estim", "Baseline", 
                              ifelse(name == "esir.crc.estim", "eSIR", 
                                     ifelse(name == "sap.crc.estim", "SAPHIRE", "SEIR-fansy"))))) %>% 
  mutate(value = value/100000) %>%  
  ggplot(aes(x = date, y = value, color = name)) + 
  geom_line(size = 1) + 
  geom_point(color = "black", size = 0.8) + 
  scale_color_nejm() + 
  labs(
    title    = "Time series plot of projected and observed cumulative reported cases from October 16 to December 31, 2020.",
    subtitle = glue("Projections are based on training data for India from March 18 to October 18, 2020.\n",
                    "Supplementary Table S1 describes parameter values used to generate these projections in detail."
    ), 
    x        = "Date",
    y        = TeX("Cases $\\left(\\times 10^6\\right)$"),
    color = "Model",
    caption  = glue(
      #"**\uA9 COV-IND-19 Study Group**<br>",
      "**Data Source:** covid19india.org<br>",
      "**Note:**<br>",
      " - We do not include projections from the ICM model it yields only total (reported + unreported) case counts."
    )
  ) +
  theme_bw() +
  theme(
    #text               = element_text(family = "Helvetica Neue"),
    plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 16, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(hjust = 0,size = 16, lineheight = 1.1), 
    axis.text          = element_text(size = 16, color = "#36454f"),
    axis.title         = element_text(size = 16),
    legend.title = ggtext::element_markdown(size = 16),
    legend.text = ggtext::element_markdown(size = 16),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 60, vjust = 0.5)
  ) + 
  scale_x_date(date_breaks = "1 week" , date_labels = "%b %d")

ggsave(paste0(save.address, "crc.pdf"), 
       device = cairo_pdf(), 
       width = 16, 
       height = 2*16/3, 
       units = "in", 
       dpi = 300)



#### figure for cumulative reported deaths ####
crd.estim <- full_join(full_join(full_join(obs %>% select(date, total.death) %>% rename(date = date),
                                 es %>% select(date, esir.crd.estim)),
                       sf %>% select(date, seirf.crd.estim)), 
                       icm %>% select(date, icm.ctd.estim))
crd <- crd.estim %>% 
  pivot_longer(cols = -date) %>% 
  mutate(date = as_date(date)) %>% 
  mutate(name = ifelse(name == "total.death", "Observed", 
                       ifelse(name == "esir.crd.estim", "eSIR",
                              ifelse(name == "icm.ctd.estim", "ICM", "SEIR-fansy")))) %>% 
  mutate(value = value/1000) %>%  
  ggplot(aes(x = date, y = value, color = name)) + 
  geom_line(size = 1) + 
  geom_point(color = "black", size = 0.8) + 
  scale_color_nejm() + 
  labs(
    title    = "Time series plot of projected and observed cumulative reported deaths from October 16 to December 31, 2020.",
    subtitle = glue("Projections are based on training data for India from March 18 to October 18, 2020.\n",
                    "Supplementary Table S1 describes parameter values used to generate these projections in detail."
    ), 
    x        = "Date",
    y        = TeX("Deaths $\\left(\\times 10^3\\right)$"),
    color = "Model",
    caption  = glue(
      #"**\uA9 COV-IND-19 Study Group**<br>",
      "**Data Source:** covid19india.org<br>",
      "**Note:**<br>",
      " - We do not include projections from the baseline and SAPHIRE models as they do not yield cumulative death counts.<br>"
    )
  ) +
  theme_bw() +
  theme(
    #text               = element_text(family = "Helvetica Neue"),
    plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 16, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(hjust = 0,size = 16, lineheight = 1.1), 
    axis.text          = element_text(size = 16, color = "#36454f"),
    axis.title         = element_text(size = 16),
    legend.title = ggtext::element_markdown(size = 16),
    legend.text = ggtext::element_markdown(size = 16),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 60, vjust = 0.5)
  ) + 
  scale_x_date(date_breaks = "1 week" , date_labels = "%b %d")

ggsave(paste0(save.address, "crd.pdf"), 
       device = cairo_pdf(), 
       width = 16, 
       height = 2*16/3, 
       units = "in", 
       dpi = 300)
dev.off()




#### scatter and density for active reported ####
scatter <- arc.estim %>% 
  pivot_longer(cols = -c(date, active.case)) %>% 
  mutate(date = as_date(date)) %>% 
  mutate(name = ifelse(name == "esir.arc.estim", "eSIR", "SEIR-fansy")) %>% 
  rename(Observed = active.case, 
         Projected = value) %>% 
  mutate(Observed = Observed/100000, 
         Projected = Projected/100000) %>% 
  ggplot(aes(x = Observed, y = Projected, color = name)) + 
  geom_point() + 
  scale_color_nejm() +
  xlab(TeX("Observed cases $\\left( \\times 10^6 \\right)$")) + 
  ylab(TeX("Projected cases $\\left( \\times 10^6 \\right)$")) + 
  theme_bw() + 
  labs(color = "Model") + 
  geom_abline(slope = 1, intercept = 0) +
  theme(
    #text               = element_text(family = "Helvetica Neue"),
    plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 16, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(hjust = 0,size = 16, lineheight = 1.1),
    axis.text          = element_text(size = 16, color = "#36454f"),
    axis.title         = element_text(size = 16),
    legend.title = ggtext::element_markdown(size = 16),
    legend.text = ggtext::element_markdown(size = 16),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 60, vjust = 0.5)
  )

density <- arc.estim %>% 
  pivot_longer(cols = -c(date)) %>% 
  mutate(date = as_date(date)) %>% 
  mutate(name = ifelse(name == "esir.arc.estim", "eSIR",
                       ifelse(name == "active.case", "Observed", "SEIR-fansy"))) %>% 
  rename(Model= name, 
         Cases = value) %>% 
  mutate(Cases = Cases/100000) %>% 
  ggplot(aes(x = Cases, y = Model, fill = Model, height = ..density..)) + 
  geom_density_ridges(alpha = 0.75) + 
  scale_fill_nejm() +
  xlab(TeX("Cases $\\left( \\times 10^6 \\right)$")) + 
  theme_bw() +
  theme(
    #text               = element_text(family = "Helvetica Neue"),
    plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 16, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(hjust = 0,size = 16, lineheight = 1.1),
    axis.text          = element_text(size = 16, color = "#36454f"),
    axis.title         = element_text(size = 16),
    legend.title = ggtext::element_markdown(size = 16),
    legend.text = ggtext::element_markdown(size = 16),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 60, vjust = 0.5)
  )

arc.scatDens <- ggpubr::ggarrange(density, scatter, ncol = 2,  
                                  legend = "bottom")  +
  labs(title = "Densities (L) and scatterplot (R) of projected and observed active reported cases from October 16 to December 31, 2020.",
       subtitle = glue("Projections are based on training data for India from March 18 to October 18, 2020.\n",
                       "Supplementary Table S1 describes parameter values used to generate these projections in detail."
       ),
       color = "Model",
       caption  = glue(
         #"**\uA9 COV-IND-19 Study Group**<br>",
         "**Data Source:** covid19india.org<br>",
         "**Note:**<br>",
         " - Solid black line on scatterplot (R) indicates y = x line.<br>",
         " - We do not include projections from the baseline and SAPHIRE models as they do not yield active case counts.<br>",
         " - We do not include projections from the ICM model it yields only total (reported + unreported) case counts."
       )
  ) +
  theme_bw() +
  theme(
    #text               = element_text(family = "Helvetica Neue"),
    plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 16, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(hjust = 0,size = 16, lineheight = 1.1),
    axis.text          = element_text(size = 16, color = "#36454f"),
    axis.title         = element_text(size = 16),
    legend.title = ggtext::element_markdown(size = 16),
    legend.text = ggtext::element_markdown(size = 16),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 60, vjust = 0.5)
  )

ggsave(paste0(save.address, "arcScatDens.pdf"), 
       plot = arc.scatDens,
       device = cairo_pdf(), 
       width = 16, 
       height = 2*16/3, 
       units = "in", 
       dpi = 300)
dev.off()




#### scatter and density for cumulative ####
scatter <- crc.estim %>% 
  pivot_longer(cols = -c(date, total.case)) %>% 
  mutate(date = as_date(date)) %>% 
  mutate(name = ifelse(name == "esir.crc.estim", "eSIR", 
                       ifelse(name == "bl.crc.estim", "Baseline", 
                              ifelse(name == "sap.crc.estim", "SAPHIRE", "SEIR-fansy")))) %>% 
  rename(Observed = total.case, 
         Projected = value) %>% 
  mutate(Observed = Observed/100000, 
         Projected = Projected/100000) %>% 
  ggplot(aes(x = Observed, y = Projected, color = name)) + 
  geom_point() + 
  scale_color_nejm() +
  xlab(TeX("Observed cases $\\left( \\times 10^6 \\right)$")) + 
  ylab(TeX("Projected cases $\\left( \\times 10^6 \\right)$")) + 
  theme_bw() + 
  labs(color = "Model") + 
  geom_abline(slope = 1, intercept = 0) +
  theme(
    #text               = element_text(family = "Helvetica Neue"),
    plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 16, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(hjust = 0,size = 16, lineheight = 1.1),
    axis.text          = element_text(size = 16, color = "#36454f"),
    axis.title         = element_text(size = 16),
    legend.title = ggtext::element_markdown(size = 16),
    legend.text = ggtext::element_markdown(size = 16),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 60, vjust = 0.5)
  )


density <- crc.estim %>% 
  pivot_longer(cols = -c(date)) %>% 
  mutate(date = as_date(date)) %>% 
  mutate(name = ifelse(name == "esir.crc.estim", "eSIR", 
                       ifelse(name == "bl.crc.estim", "Baseline", 
                              ifelse(name == "sap.crc.estim", "SAPHIRE",
                                     ifelse(name == "total.case", "Observed", "SEIR-fansy"))))) %>%
  rename(Model= name, 
         Cases = value) %>% 
  mutate(Cases = Cases/100000) %>% 
  ggplot(aes(x = Cases, y = Model, fill = Model, height = ..density..)) + 
  geom_density_ridges(alpha = 0.75) + 
  scale_fill_nejm() +
  xlab(TeX("Cases $\\left( \\times 10^6 \\right)$")) + 
  theme_bw() +
  theme(
    #text               = element_text(family = "Helvetica Neue"),
    plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 16, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(hjust = 0,size = 16, lineheight = 1.1),
    axis.text          = element_text(size = 16, color = "#36454f"),
    axis.title         = element_text(size = 16),
    legend.title = ggtext::element_markdown(size = 16),
    legend.text = ggtext::element_markdown(size = 16),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 60, vjust = 0.5)
  )


crc.scatDens <- ggpubr::ggarrange(density, scatter, ncol = 2,  
                                  legend = "bottom")  +
  labs(title = "Densities (L) and scatterplot (R) of projected and observed cumulative reported cases from October 16 to December 31, 2020.",
       subtitle = glue("Projections are based on training data for India from March 18 to October 18, 2020.\n",
                       "Supplementary Table S1 describes parameter values used to generate these projections in detail."
       ),
       color = "Model",
       caption  = glue(
         #"**\uA9 COV-IND-19 Study Group**<br>",
         "**Data Source:** covid19india.org<br>",
         "**Note:**<br>",
         " - Solid black line on scatterplot (R) indicates y = x line.<br>", 
         #" - We do not include projections from the baseline and SAPHIRE models as they do not yield active case counts.<br>",
         " - We do not include projections from the ICM model it yields only total (reported + unreported) case counts."
       )
  ) +
  theme_bw() +
  theme(
    #text               = element_text(family = "Helvetica Neue"),
    plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 16, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(hjust = 0,size = 16, lineheight = 1.1),
    axis.text          = element_text(size = 10, color = "#36454f"),
    axis.title         = element_text(size = 16),
    legend.title = ggtext::element_markdown(size = 16),
    legend.text = ggtext::element_markdown(size = 16),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 60, vjust = 0.5)
  )

ggsave(paste0(save.address, "crcScatDens.pdf"), 
       plot = crc.scatDens,
       device = cairo_pdf(), 
       width = 16, 
       height = 2*16/3, 
       units = "in", 
       dpi = 300)
dev.off()






#### scatter and density for active reported ####
scatter <- crd.estim %>% 
  pivot_longer(cols = -c(date, total.death)) %>% 
  mutate(date = as_date(date)) %>% 
  mutate(name = ifelse(name == "esir.crd.estim", "eSIR", 
                       ifelse(name == "icm.ctd.estim", "ICM", "SEIR-fansy"))) %>% 
  rename(Observed = total.death, 
         Projected = value) %>% 
  mutate(Observed = Observed/1000, 
         Projected = Projected/1000) %>% 
  ggplot(aes(x = Observed, y = Projected, color = name)) + 
  geom_point() + 
  scale_color_nejm() +
  xlab(TeX("Observed deaths $\\left( \\times 10^3 \\right)$")) + 
  ylab(TeX("Projected deaths $\\left( \\times 10^3 \\right)$")) + 
  theme_bw() + 
  labs(color = "Model") + 
  geom_abline(slope = 1, intercept = 0)+
  theme(
    #text               = element_text(family = "Helvetica Neue"),
    plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 16, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(hjust = 0,size = 16, lineheight = 1.1),
    axis.text          = element_text(size = 10, color = "#36454f"),
    axis.title         = element_text(size = 16),
    legend.title = ggtext::element_markdown(size = 16),
    legend.text = ggtext::element_markdown(size = 16),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 60, vjust = 0.5)
  )

density <- crd.estim %>% 
  pivot_longer(cols = -c(date, total.death)) %>% 
  mutate(date = as_date(date)) %>% 
  mutate(name = ifelse(name == "esir.crd.estim", "eSIR", 
                       ifelse(name == "icm.ctd.estim", "ICM", "SEIR-fansy"))) %>% 
  rename(Model= name, 
         Cases = value) %>% 
  mutate(Cases = Cases/1000) %>% 
  ggplot(aes(x = Cases, y = Model, fill = Model, height = ..density..)) + 
  geom_density_ridges(alpha = 0.75) + 
  scale_fill_nejm() +
  xlab(TeX("Deaths $\\left( \\times 10^3 \\right)$")) + 
  theme_bw()+
  theme(
    #text               = element_text(family = "Helvetica Neue"),
    plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 16, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(hjust = 0,size = 16, lineheight = 1.1),
    axis.text          = element_text(size = 10, color = "#36454f"),
    axis.title         = element_text(size = 16),
    legend.title = ggtext::element_markdown(size = 16),
    legend.text = ggtext::element_markdown(size = 16),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 60, vjust = 0.5)
  )

crd.scatDens <- ggpubr::ggarrange(density, scatter, ncol = 2,  
                                  legend = "bottom")  +
  labs(title = "Densities (L) and scatterplot (R) of projected and observed cumulative reported deaths from October 16 to December 31, 2020.",
       subtitle = glue("Projections are based on training data for India from March 18 to October 18, 2020.\n",
                       "Supplementary Table S1 describes parameter values used to generate these projections in detail."
       ),
       color = "Model",
       caption  = glue(
         #"**\uA9 COV-IND-19 Study Group**<br>",
         "**Data Source:** covid19india.org<br>",
         "**Note:**<br>",
         " - Solid black line on scatterplot (R) indicates y = x line.<br>",
         " - We do not include projections from the baseline and SAPHIRE models as they do not yield cumulative death counts.<br>"
       )
  ) +
  theme_bw() +
  theme(
    #text               = element_text(family = "Helvetica Neue"),
    plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 16, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(hjust = 0,size = 16, lineheight = 1.1),
    axis.text          = element_text(size = 10, color = "#36454f"),
    axis.title         = element_text(size = 16),
    legend.title = ggtext::element_markdown(size = 16),
    legend.text = ggtext::element_markdown(size = 16),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 60, vjust = 0.5)
  )

ggsave(paste0(save.address, "crdcScatDens.pdf"), 
       plot = crd.scatDens,
       device = cairo_pdf(), 
       width = 16, 
       height = 2*16/3, 
       units = "in", 
       dpi = 300)
dev.off()








arc <- es %>% 
  select(date, esir.arc.width) %>% 
  add_column(sf %>% 
               mutate(seirf.arc.width = seirf.arc.high - seirf.arc.low) %>% 
               select(seirf.arc.width)) %>% 
  rename(eSIR = esir.arc.width, 
         `SEIR-fansy` = seirf.arc.width) %>% 
  pivot_longer(cols = -date) %>% 
  ggplot(aes(x = value/(10^6), y = name)) + 
  geom_boxplot(aes(fill = name)) + 
  ggsci::scale_fill_nejm() +
  xlab(TeX("Width of confidence interval for active case projections $\\left(\\times 10^6 \\right)$")) + 
  ylab(TeX("Model")) + 
  labs(
    title    = "Boxplots showing width of confidence interval associated with projected active cases from October 16 to December 31, 2020.",
    subtitle = glue("Projections are based on training data for India from March 18 to October 18, 2020.\n",
                    "Supplementary Table S1 describes parameter values used to generate these projections in detail."
    ), 
    fill = "Model",
    caption  = glue(
      #"**\uA9 COV-IND-19 Study Group**<br>",
      "**Data Source:** covid19india.org<br>",
      "**Note:**<br>",
      " - We do not include projections from the baseline and SAPHIRE models as they do not yield active case count projections.<br>",
      " - We do not include projections from the ICM model it yields only total (reported + unreported) case counts."
    )
  ) +
  theme_bw() +
  theme(
    #text               = element_text(family = "Helvetica Neue"),
    plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 16, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(hjust = 0,size = 16, lineheight = 1.1), 
    axis.text          = element_text(size = 16, color = "#36454f"),
    axis.title         = element_text(size = 16),
    legend.title = ggtext::element_markdown(size = 16),
    legend.text = ggtext::element_markdown(size = 16),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  )

ggsave(paste0(save.address, "arc_box.pdf"), 
       plot = arc,
       device = cairo_pdf(), 
       width = 16, 
       height = 2*16/3, 
       units = "in", 
       dpi = 300)
dev.off()
























crc <- bl %>% 
  select(date, bl.crc.width) %>% 
  add_column(es %>% 
               select(esir.crc.width)) %>% 
  add_column(sp %>% 
               mutate(sap.crc.width = sap.crc.high - sap.crc.low) %>% 
               select(sap.crc.width)) %>% 
  add_column(sf %>% 
               select(seirf.crc.width)) %>%
  rename(Baseline = bl.crc.width, 
         eSIR = esir.crc.width, 
         SAPHIRE = sap.crc.width,
         `SEIR-fansy` = seirf.crc.width) %>% 
  pivot_longer(cols = -date) %>% 
  ggplot(aes(x = value/(10^6), y = name)) + 
  geom_boxplot(aes(fill = name)) + 
  ggsci::scale_fill_nejm() +
  xlab(TeX("Width of confidence interval for cumulative case projections $\\left(\\times 10^6 \\right)$")) + 
  ylab(TeX("Model")) + 
  labs(
    title    = "Boxplots showing width of confidence interval associated with projected cumulative cases from October 16 to December 31, 2020.",
    subtitle = glue("Projections are based on training data for India from March 18 to October 18, 2020.\n",
                    "Supplementary Table S1 describes parameter values used to generate these projections in detail."
    ), 
    fill = "Model",
    caption  = glue(
      #"**\uA9 COV-IND-19 Study Group**<br>",
      "**Data Source:** covid19india.org<br>",
      "**Note:**<br>",
      " - We do not include projections from the ICM model it yields only total (reported + unreported) case counts."
    )
  ) +
  theme_bw() +
  theme(
    #text               = element_text(family = "Helvetica Neue"),
    plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 16, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(hjust = 0,size = 16, lineheight = 1.1), 
    axis.text          = element_text(size = 10, color = "#36454f"),
    axis.title         = element_text(size = 16),
    legend.title = ggtext::element_markdown(size = 16),
    legend.text = ggtext::element_markdown(size = 16),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  )

ggsave(paste0(save.address, "crc_box.pdf"), 
       plot = crc,
       device = cairo_pdf(), 
       width = 16, 
       height = 2*16/3, 
       units = "in", 
       dpi = 300)
dev.off()












crd <-  es %>% 
  select(date, esir.crd.width) %>% 
  add_column(sf %>% 
               mutate(seirf.crd.width = seirf.crd.high - seirf.crd.low) %>% 
               select(seirf.crd.width)) %>% 
  add_column(icm %>% select(icm.ctd.width)) %>% 
  rename(eSIR = esir.crd.width, 
         `SEIR-fansy` = seirf.crd.width, 
         ICM = icm.ctd.width) %>% 
  pivot_longer(cols = -date) %>% 
  ggplot(aes(x = value/(10^3), y = name)) + 
  geom_boxplot(aes(fill = name)) + 
  ggsci::scale_fill_nejm() +
  xlab(TeX("Width of confidence interval for cumulative death projections $\\left(\\times 10^3 \\right)$")) + 
  ylab(TeX("Model")) + 
  labs(
    title    = "Boxplots showing width of confidence interval associated with projected cumulative deaths from October 16 to December 31, 2020.",
    subtitle = glue("Projections are based on training data for India from March 18 to October 18, 2020.\n",
                    "Supplementary Table S1 describes parameter values used to generate these projections in detail."
    ), 
    fill = "Model",
    caption  = glue(
      #"**\uA9 COV-IND-19 Study Group**<br>",
      "**Data Source:** covid19india.org<br>",
      "**Note:**<br>",
      " - We do not include projections from the baseline and SAPHIRE models as they do not yield cumulative death counts.<br>",
      " - We do not include projections from the ICM model it yields only total (reported + unreported) case counts."
    )
  ) +
  theme_bw() +
  theme(
    #text               = element_text(family = "Helvetica Neue"),
    plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 16, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(hjust = 0,size = 16, lineheight = 1.1), 
    axis.text          = element_text(size = 10, color = "#36454f"),
    axis.title         = element_text(size = 16),
    legend.title = ggtext::element_markdown(size = 16),
    legend.text = ggtext::element_markdown(size = 16),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  )

ggsave(paste0(save.address, "crd_box.pdf"), 
       plot = crc,
       device = cairo_pdf(), 
       width = 16, 
       height = 2*16/3, 
       units = "in", 
       dpi = 300)
dev.off()



arc <- es %>% 
  select(date, esir.arc.width) %>% 
  add_column(sf %>% 
               mutate(seirf.arc.width = seirf.arc.high - seirf.arc.low) %>% 
               select(seirf.arc.width)) %>% 
  rename(eSIR = esir.arc.width, 
         `SEIR-fansy` = seirf.arc.width) %>% 
  pivot_longer(cols = -date) %>% 
  ggplot(aes(y = value/(10^6), x = name)) + 
  geom_boxplot(aes(fill = name)) + 
  ggsci::scale_fill_nejm() +
  ylab(TeX("Width of 95% credible interval for active case projections $\\left(\\times 10^6 \\right)$")) + 
  xlab(TeX("Model")) + 
  labs(fill = "") + 
  theme_bw() +
  theme(
    #text               = element_text(family = "Helvetica Neue"),
    plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 16, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(hjust = 0,size = 16, lineheight = 1.1), 
    axis.text          = element_text(size = 10, color = "#36454f"),
    axis.title         = element_text(size = 16),
    legend.title = ggtext::element_markdown(size = 16),
    legend.text = ggtext::element_markdown(size = 12),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  )


crc <- bl %>% 
  select(date, bl.crc.width) %>% 
  add_column(es %>% 
               select(esir.crc.width)) %>% 
  add_column(sp %>% 
               mutate(sap.crc.width = sap.crc.high - sap.crc.low) %>% 
               select(sap.crc.width)) %>% 
  add_column(sf %>% 
               select(seirf.crc.width)) %>%
  rename(Baseline = bl.crc.width, 
         eSIR = esir.crc.width, 
         SAPHIRE = sap.crc.width,
         `SEIR-fansy` = seirf.crc.width) %>% 
  pivot_longer(cols = -date) %>% 
  ggplot(aes(y = value/(10^6), x = name)) + 
  geom_boxplot(aes(fill = name)) + 
  ggsci::scale_fill_nejm() +
  ylab(TeX("Width of 95% credible interval for cumulative case projections $\\left(\\times 10^6 \\right)$")) + 
  xlab(TeX("Model")) + 
  labs(fill = "") +
  theme_bw() +
  theme(
    #text               = element_text(family = "Helvetica Neue"),
    plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 16, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(hjust = 0,size = 16, lineheight = 1.1), 
    axis.text          = element_text(size = 10, color = "#36454f"),
    axis.title         = element_text(size = 16),
    legend.title = ggtext::element_markdown(size = 16),
    legend.text = ggtext::element_markdown(size = 12),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  )


crd <- es %>% 
  select(date, esir.crd.width) %>% 
  add_column(sf %>% 
               mutate(seirf.crd.width = seirf.crd.high - seirf.crd.low) %>% 
               select(seirf.crd.width)) %>% 
  add_column(icm %>% select(icm.ctd.width)) %>% 
  rename(eSIR = esir.crd.width, 
         `SEIR-fansy` = seirf.crd.width, 
         ICM = icm.ctd.width) %>% 
  pivot_longer(cols = -date) %>% 
  ggplot(aes(y = value/(10^3), x = name)) + 
  geom_boxplot(aes(fill = name)) + 
  ggsci::scale_fill_nejm() +
  ylab(TeX("Width of 95% credible interval for cumulative death projections $\\left(\\times 10^3 \\right)$")) + 
  xlab(TeX("Model")) + 
  labs(fill = "") +
  theme_bw() +
  theme(
    #text               = element_text(family = "Helvetica Neue"),
    plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 16, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(hjust = 0,size = 16, lineheight = 1.1), 
    axis.text          = element_text(size = 10, color = "#36454f"),
    axis.title         = element_text(size = 16),
    legend.title = ggtext::element_markdown(size = 16),
    legend.text = ggtext::element_markdown(size = 12),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  )


bp <- ggpubr::ggarrange(arc, crc, crd, ncol = 3,  
                        legend = "bottom")  +
  ggtitle("Boxplots of  width of 95% credible intervals associated with projected active  cases (L), cumulative cases (C) and cumulative deaths (R).")  + 
  labs(subtitle = glue("Projections are from October 16 to December 31, 2020, based on training data for India from March 18 to October 18, 2020.\n",
                       "Supplementary Table S1 describes parameter values used to generate these projections in detail."),
       color = "Model",
       caption  = glue(
         #"**\uA9 COV-IND-19 Study Group**<br>",
         "**Data Source:** covid19india.org<br>",
         "**Note:**<br>",
         " - We do not include active case or cumulative death projections from the baseline or SAPHIRE as those projections are not available.<br>",
         " - We do not include active or cumulative case projections from the ICM model it yields only total (reported + unreported) case counts.")
  ) +
  theme_bw() +
  theme(
    #text               = element_text(family = "Helvetica Neue"),
    plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 16, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(hjust = 0,size = 16, lineheight = 1.1),
    axis.text          = element_text(size = 10, color = "#36454f"),
    axis.title         = element_text(size = 16),
    legend.title = ggtext::element_markdown(size = 16),
    legend.text = ggtext::element_markdown(size = 12),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 60, vjust = 0.5)
  )



ggsave(paste0(save.address, "box.pdf"), 
       plot = bp,
       device = cairo_pdf(), 
       width = 16, 
       height = 2*16/3, 
       units = "in", 
       dpi = 300)
dev.off()



