library(here)
library(tidyverse)
library(lubridate)
library(ggsankey)
library(circlize)

load(here("data", "HPS.rda"))

# Sankey - HPS

HPS_sankey <- HPS |>
  make_long(PickupLocationDescription, `Receiving Facility/Destination`)

ggplot(CKL_sankey, aes(x = x,
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
  theme_sankey(base_size = 15) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5)) +
  ggtitle("HPS Patient Flow")

ggsave(file = here("scripts", "4_sankey_diagrams", "4_sankeys", "HPS_location_sankey.png"),
       width = 8,
       height = 10.5)

# Radial Sankey - HPS
png(filename = here("scripts", "4_sankey_diagrams", "4_sankeys", "HPS_location_radial_sankey.png"),
    width = 8,
    height = 5,
    units = "in",
    res = 300)

HPS_sankey_radial <- HPS |>
  select(PickupLocationDescription, `Receiving Facility/Destination`)

sectors <- unique(c(HPS_sankey_radial$PickupLocationDescription,
                    HPS_sankey_radial$`Receiving Facility/Destination`))

wrap_width <- setNames(rep(10, length(sectors)),
                       sectors)

wrap_width["QHC North Hastings - Bancroft"] <- 40
wrap_width["Peterborough Regional Health Centre"] <- 25
wrap_width["Huntsville District Memorial Hosp"] <- 40

wrapped_labels <- setNames(
  mapply(function(s, w) str_wrap(s, width = w), sectors, wrap_width),
  sectors)

grid.col = c("HHHS Minden Hospital" = "darkgrey",
             "Ross Memorial Hospital" = "darkgrey",
             "Haliburton Hospital" = "darkgrey",
             "Not Transported" = "darkgrey",
             "QHC North Hastings - Bancroft" = "darkgrey",
             "Peterborough Regional Health Centre"= "darkgrey",
             "Huntsville District Memorial Hosp" = "darkgrey",
             "Long-Term Care Home" = "red",
             "Apartment/Condo. Building" = "orange",
             "House/Town House" = "green",
             "Hospital (Acute & Non-Acute)" = "blue",
             "Other" = "purple",
             "Street/Highway/Road" = "pink")

chordDiagram(HPS_sankey_radial, annotationTrack = "grid",
             grid.col = grid.col,
             preAllocateTracks = list(track.height = 0.18))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter,
              CELL_META$ylim[1],
              wrapped_labels[CELL_META$sector.index],
              facing = "clockwise",
              niceFacing = TRUE,
              adj = c(0, 0.5),
              cex = 0.6)
}, bg.border = NA)

dev.off()
circos.clear()










































