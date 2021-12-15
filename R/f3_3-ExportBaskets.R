library(dplyr)
library(tidyr)
library(countrycode)
library(ggplot2)
library(here)
library(data.table)
library(ggpubr)
library(ggrepel)
library(scales)
library(ggthemr)
library(haven)
ggthemr('greyscale')

# Data source:
# https://atlas.cid.harvard.edu/explore/stack?country=90&year=2019&startYear=1995&productClass=HS&product=undefined&target=Product&partner=undefined
# https://atlas.cid.harvard.edu/explore/stack?country=61&year=2019&startYear=1995&productClass=HS&product=undefined&target=Product&partner=undefined

# Read data------------------------------------------------
raw_data_de <- fread(here("data/exp_germany.csv"), 
                     select = c("Year"="double", "Share"="character", 
                                "Sector"="character")) %>%
  dplyr::mutate(Country="DE")

grc_fill <- tibble(Year=1999, Share="0.00%", Sector="Other", Country="GR")
raw_data_gr <- fread(here("data/exp_greece.csv"), 
                     select = c("Year"="double", "Share"="character", 
                                "Sector"="character")) %>%
  dplyr::mutate(Country="GR") %>%
  rbind(., grc_fill)

raw_data <- rbind(raw_data_de, raw_data_gr) %>%
  dplyr::mutate(Share=as.double(gsub("%", "", Share)))

seq_sectors <- c(
  'Textiles', 'Agriculture', 'Stone', 'Minerals', 'Metals', 
  'Chemicals', 'Vehicles', 'Machinery', 'Electronics', 'Other'
  )

# Greek exports------------------------
plot_grc_data <- raw_data %>%
  dplyr::filter(Country=="GR") %>%
  dplyr::mutate(
    Sector=factor(Sector, levels = rev(seq_sectors)))

plot_grc_data_labels <- plot_grc_data %>%
  dplyr::filter(Year==2019) %>%
  dplyr::mutate(CumShare = cumsum(Share),
                CumShare = (CumShare - 0.5*Share)) 
plot_grc <- ggplot(
  data = plot_grc_data, aes(x=Year, y=Share, fill=Sector)
  ) +
  geom_area() +
  geom_text_repel(data = plot_grc_data_labels, 
                  mapping = aes(x=Year, y=CumShare, label=Sector), 
                  nudge_x = 10, hjust = 1, seed = 1) +
  scale_fill_grey() + 
  scale_color_grey() + 
  labs(title = "Greek export basket", y = "Export share") +
  scale_x_continuous(expand = expansion(add = c(0, 8)),
                     breaks = c(seq(1995, 2015, 5), 2019)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 100, 10),
                     expand = expansion(add = c(0, 4)))+
  theme(legend.position = "none", panel.grid = element_blank(),
        axis.title.x = element_blank(), 
        axis.ticks = element_blank())
plot_grc

# German exports-----------------------
plot_de_data <- raw_data %>%
  dplyr::filter(Country=="DE") %>%
  dplyr::mutate(
    Sector=factor(Sector, levels = rev(seq_sectors)))

plot_de_data_labels <- plot_de_data %>%
  dplyr::filter(Year==2019) %>%
  dplyr::mutate(CumShare = cumsum(Share),
                CumShare = (CumShare - 0.5*Share)) 
plot_de <- ggplot(
  data = plot_de_data, aes(x=Year, y=Share, fill=Sector)
) +
  geom_area() +
  geom_text_repel(data = plot_de_data_labels, 
                  mapping = aes(x=Year, y=CumShare, label=Sector), 
                  nudge_x = 10, hjust = 1, seed = 1) +
  scale_fill_grey() + 
  scale_color_grey() + 
  labs(title = "German export basket", y = "Export share") +
  scale_x_continuous(expand = expansion(add = c(0, 8)),
                     breaks = c(seq(1995, 2015, 5), 2019)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 100, 10),
                     expand = expansion(add = c(0, 4)))+
  theme(legend.position = "none", panel.grid = element_blank(),
        axis.title.x = element_blank(), 
        axis.ticks = element_blank())
plot_de


plot_full <- ggpubr::ggarrange(plot_de, plot_grc, ncol = 2,
                               labels = c("a)", "b)"), font.label = list(size=15))
plot_full <- annotate_figure(
  plot_full, bottom = text_grob(
    "Data: CID Atlas of Economic Complexity.", 
    hjust = -0.3))

ggsave(plot = plot_full, 
       filename = here("figures/Figure 3.3.pdf"), 
       width = 8, height = 4)

