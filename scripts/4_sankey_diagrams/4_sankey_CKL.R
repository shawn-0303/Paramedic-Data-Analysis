library(here)
library(lubridate)
library(ggsankey)
library(circlize)

load(here("data", "CKL.rda"))

# Sankey - CKL

CKL_sankey <- CKL |>
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
  ggtitle("CKL Patient Flow")

ggsave(file = here("scripts", "4_sankey_diagrams", "4_sankeys", "CKL_location_sankey.png"),
       width = 8,
       height = 10.5)

# Radial Sankey - CKL
png(filename = here("scripts", "4_sankey_diagrams", "4_sankeys", "CKL_location_radial_sankey.png"),
    width = 12,
    height = 10,
    units = "in",
    res = 300)

CKL_sankey_radial <- CKL |>
  select(PickupLocationDescription, `Receiving Facility/Destination`) |>
  na.omit()

grid.col = c("Coroner-East Region, Kingston Office" = "darkgrey",
             "Haliburton Hospital" = "darkgrey",
             "HOSPITAL FOR SICK CHILDREN" = "darkgrey",
             "Lakeridge Health Bowmanville" = "darkgrey",
             "Lakeridge Health Port Perry" = "darkgrey",
             "Minden Hospital"= "darkgrey",
             "Not Transported"= "darkgrey",
             "Orillia Soldiers' Memorial Hospital" = "darkgrey",
             "Other" = "darkgrey",
             "Peterborough Regional Health Centre" = "darkgrey",
             "Ross Memorial Hospital" = "darkgrey")

chordDiagram(CKL_sankey_radial, annotationTrack = "grid",
             grid.col = grid.col,
             preAllocateTracks = list(track.height = 0.18))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter,
              CELL_META$ylim[1],
              CELL_META$sector.index,
              facing = "clockwise",
              niceFacing = TRUE,
              adj = c(0, 0.5),
              cex = 0.6)
}, bg.border = NA)

dev.off()
circos.clear()










































