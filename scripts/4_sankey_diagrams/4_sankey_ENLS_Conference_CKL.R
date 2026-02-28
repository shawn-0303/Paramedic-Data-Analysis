library(here)
library(lubridate)
library(ggsankey)

load(here("data", "CKL.rda"))

# Filter the Data
## Remove transfer patients
CKL_filtered <- CKL[!(CKL$PickupLocationDescription %in% "Hospital (Acute & Non-Acute)"), ]

## Remove pick-up and drop-off locations with only one call
CKL_filtered <- CKL_filtered |>
  group_by(PickupLocationDescription) |>
  filter(n() > 1) |>
  ungroup() |>
  group_by(`Receiving Facility/Destination`) |>
  filter(n() > 1) |>
  ungroup()

## Group Sankey Data to Minimize categories
CKL_sankey_grouped <- CKL_filtered |>
  select(PickupLocationDescription, `Receiving Facility/Destination`) |>
  mutate(Grouped_pickups = case_when(
    PickupLocationDescription == "House/Town House" ~ PickupLocationDescription,
    PickupLocationDescription == "Apartment/Condo. Building" ~ PickupLocationDescription,
    PickupLocationDescription == "Street/Highway/Road" ~ PickupLocationDescription,
    PickupLocationDescription == "Long-Term Care Home" ~ PickupLocationDescription,
    TRUE ~ "Other"))

# Sankey Diagram
CKL_sankey_grouped <- CKL_sankey_grouped |>
  make_long(Grouped_pickups, `Receiving Facility/Destination`)

ggplot(CKL_sankey_grouped, aes(x = x,
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
  ggtitle("CKL Patient Flow") +
  scale_x_discrete(labels = c("Pick-Up Location", "Recieving Facility / Destination"))

ggsave(file = here("scripts", "4_sankey_diagrams", "4_ENLS_Conference_sankeys", "CKL_location_sankey_grouped_filtered.png"),
       width = 8,
       height = 6)










