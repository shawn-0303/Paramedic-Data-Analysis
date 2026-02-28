library(here)
library(lubridate)
library(ggsankey)

load(here("data", "HPS.rda"))

# Filter the Data
# Remove transfer patients
HPS_filtered <- HPS[!(HPS$PickupLocationDescription %in% "Hospital (Acute & Non-Acute)"), ]

# Remove pick-up locations with only one call
HPS_filtered <- HPS_filtered[HPS_filtered$PickupLocationDescription >= 1,]

# Sankey Diagram
HPS_sankey_filtered <- HPS_filtered |>
  make_long(PickupLocationDescription, `Receiving Facility/Destination`)

ggplot(HPS_sankey_filtered, aes(x = x,
                                next_x = next_x,
                                node = node,
                                next_node = next_node,
                                fill = factor(node),
                                label = node)) +
  geom_sankey(flow.alpha = 0.6,
              width = 0.1) +
  geom_sankey_label(size = 3,
                    color = "black",
                    fill = "white")+
  scale_fill_viridis_d(option = "viridis",
                       drop = FALSE) +
  theme_sankey(base_size = 15) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5)) +
  ggtitle("HPS Patient Flow") +
  scale_x_discrete(labels = c("Pick-Up Location", "Recieving Facility / Destination"))

ggsave(file = here("scripts", "4_sankey_diagrams",  "4_ENLS_Conference_sankeys", "HPS_location_sankey_filtered.png"),
       width = 8,
       height = 5)
