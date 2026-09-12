# =============================================================================
# Figure 1B-F
# Geographic distribution of individual pathogens in China
# =============================================================================

# Load required packages
library(tidyverse)
library(sf)
library(readxl)
library(ggspatial)
library(ggplot2)
library(grid)
library(patchwork)
library(ggplotify)

# Set working directory and input files
setwd("F:/BaiduSyncdisk/桌面文档/R/maps")
prov_geo_path <- "json/全量数据province.geojson"
city_geo_path <- "json/全量数据city.geojson"
border_geo_path <- "json/guo.json"
prov_excel <- "provinfection.xlsx"
city_excel <- "rickettsiacityExport_Output_5.xlsx"

# Load input data
prov_df <- read_excel(prov_excel)
city_df <- read_excel(city_excel)

prov_df <- prov_df %>%
  mutate(DZM = format(DZM, scientific = FALSE),
         DZM = trimws(DZM),
         DZM = as.character(DZM))

city_df <- city_df %>%
  mutate(市代码 = format(市代码, scientific = FALSE),
         市代码 = trimws(市代码),
         市代码 = as.character(市代码)) %>%
  rename(cases = `恙虫病东方体`) %>%
  mutate(cases = as.numeric(cases))

# Load geographic layers
china_prov <- read_sf(prov_geo_path) %>%
  { if (is.na(st_crs(.))) st_set_crs(., 4326) else . } %>%
  st_make_valid() %>%
  mutate(province_adcode = as.character(province_adcode))

china_city <- read_sf(city_geo_path) %>%
  { if (is.na(st_crs(.))) st_set_crs(., 4326) else . } %>%
  st_make_valid() %>%
  mutate(city_adcode = as.character(city_adcode))

china_line <- read_sf(border_geo_path) %>%
  { if (is.na(st_crs(.))) st_set_crs(., 4326) else . } %>%
  st_make_valid()

# Join provincial data
china_prov_joined <- china_prov %>%
  left_join(prov_df, join_by(province_adcode == DZM)) %>%
  mutate(
    infection_level = cut(
      `恙虫病东方体`,
      breaks = c(0, 1, 5, 10, 20, 50, 100),
      labels = c("1", "2-5", "6-10", "11-20", "21-50", "51-100"),
      include.lowest = FALSE, right = TRUE
    )
  )

# Join city data
china_city_joined <- china_city %>%
  left_join(city_df, join_by(name == 市))

# Define map projection
laea_cn <- st_crs("+proj=laea +lat_0=40 +lon_0=104 +datum=WGS84 +units=m +no_defs")

prov_laea <- st_transform(china_prov_joined, laea_cn)
city_laea <- st_transform(china_city_joined, laea_cn)
line_laea <- st_transform(china_line, laea_cn)

city_pts_laea <- city_laea %>%
  mutate(point = st_point_on_surface(geometry)) %>%
  st_as_sf() %>%
  st_set_geometry("point")

city_pts_nonzero <- city_pts_laea %>% filter(!is.na(cases), cases > 0)

# Define color palettes
palqf <- c("#fff0ea", "#fdd7c5", "#fbb490", "#f47a55", "#d94832", "#7f1d1d")
palrt <- c("#f3e8f9", "#e2c9f2", "#c6a1e3", "#9c6cc4", "#6a51a3", "#3f007d")
palor <- c("#f2fadc", "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850", "#00441b")
#pal <- c("#e8f1fa", "#d0e0f0", "#a6c8e0", "#5a9ccf", "#1f77b4", "#08306b")
#pal <- c("#e6f5f4", "#c7eae5", "#80cdc1", "#4eb3b9", "#238b8d", "#045a68")
#pal <- c("#edf8f0", "#d4edda", "#a6dba0", "#5aae61", "#2b8c3e", "#00441b")
palrj <- c("#f9f7e9", "#e6e3b9", "#d1cd8a", "#a9a65e", "#7a8042", "#4d5326")
#pal <- c("#fef6e4", "#fce3b0", "#f9c97a", "#e49e4a", "#b66a2c", "#733b17")
palrf <- c("#fff7e0", "#fee8b0", "#fdd382", "#fdae42", "#e07b1c", "#7f3b08")
#pal <- c("#fef5e5", "#fdd9b5", "#fcb16e", "#f9823a", "#d7530e", "#7c2605")


# Calculate north arrow rotation
center_lonlat <- st_coordinates(st_transform(st_centroid(st_union(prov_laea)), 4326))
center_lon <- center_lonlat[1]; center_lat <- center_lonlat[2]
north_line <- st_sfc(
  st_linestring(matrix(c(center_lon, center_lat, center_lon, center_lat + 1),
                       ncol = 2, byrow = TRUE)), crs = 4326
)
target_crs <- "+proj=laea +lat_0=40 +lon_0=104 +datum=WGS84 +units=m +no_defs"
north_line_proj <- st_transform(north_line, target_crs)
coords <- st_coordinates(north_line_proj)
dx <- coords[2,1] - coords[1,1]; dy <- coords[2,2] - coords[1,2]
angle_deg <- atan2(dx, dy) * 180 / pi





# Create Taiwan hatching
taiwan_laea <- prov_laea %>% filter(grepl("台湾", name))

angle <- 45      # 斜线角度
spacing <- 20000 # 间距（米）

grid_polygons <- st_make_grid(taiwan_laea, cellsize = spacing, what = "polygons")
grid_lines <- st_cast(st_boundary(grid_polygons), "LINESTRING")

theta <- angle * pi / 180
rotate_mat <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol = 2)
center <- st_coordinates(st_centroid(st_union(taiwan_laea)))

rotate_geom <- function(geom, center, mat) {
  coords <- st_coordinates(geom)
  rotated <- t(mat %*% t(coords[, 1:2] - matrix(center, nrow(coords), 2, byrow = TRUE))) +
    matrix(center, nrow(coords), 2, byrow = TRUE)
  st_linestring(rotated)
}

grid_rotated <- st_sfc(lapply(1:length(grid_lines), function(i) {
  rotate_geom(grid_lines[i], center, rotate_mat)
}), crs = st_crs(taiwan_laea))

taiwan_stripes <- suppressWarnings(st_intersection(grid_rotated, st_union(taiwan_laea)))







# Create Taiwan legend
taiwan_legend <- ggplot() +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
            fill = "white", color = "grey30", linewidth = 0.4) +
  # Add 45-degree hatching
  geom_abline(slope = 1, intercept = seq(-1, 1, by = 0.2),
              color = "grey30", linewidth = 0.4, alpha = 0.8) +
  annotate("text", x = 0.5, y = -0.15, label = "台湾省\n（数据暂缺）",
           size = 2.5, hjust = 0.5, lineheight = 0.8) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
  theme_void() +
  theme(plot.margin = margin(0.5, 0, 0.5, 0, "cm"))

# Generate main map
main_plot <- ggplot() +
  geom_sf(data = prov_laea, aes(fill = infection_level), color = "grey60", linewidth = 0.15) +
  {if(length(taiwan_stripes) > 0) geom_sf(data = taiwan_stripes, color = "grey30", linewidth = 0.4, alpha = 0.8)} +
  {if(nrow(taiwan_laea) > 0) geom_sf(data = taiwan_laea, fill = NA, color = "grey30", linewidth = 0.4)} +
  geom_sf(data = line_laea, color = "black", linewidth = 0.4, fill = NA) +
  geom_sf(data = city_pts_nonzero, aes(size = cases),
          shape = 21, fill = "#45B7D1", color = "black", stroke = 0.25, alpha = 0.75) +
  scale_fill_manual(values = palor, na.value = "grey90", name = "Cases in provinces") +
  scale_size_area(
    name = "Cases in cities",
    max_size = 16, limits = c(1, 110), breaks = c(1, 10, 30, 60, 100),
    guide = guide_legend(title.position = "top", direction = "horizontal")
  ) +
  geom_sf_text(data = prov_laea, 
               aes(label = name_en),  # Province labels
               size = 2.5, 
               color = "black",
               check_overlap = TRUE) +  # Avoid label overlap
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering(text_angle = 0),
                         height = unit(2, "cm"), width  = unit(2, "cm"),
                         rotation = angle_deg) +
  annotation_scale(
    location = "bl",
    width_hint = 0.3,
    style = "bar",
    line_width = 0.5,
    text_cex = 0.8
  ) +
  coord_sf(
    ylim = c(-2387082, 1654989),
    crs = laea_cn
  ) +
  labs(title = "scrub typhus") +
  theme_minimal() +
  theme(
    axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
    panel.grid = element_blank(), panel.background = element_blank(),
    legend.position = c(0.02, 0.05),
    legend.justification = c(0, 0),
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.background = element_rect(fill = "white", color = "white", size = 0.5),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.background = element_rect(fill = "white", color = "white")
  )

# Create South China Sea inset
nine_map <- ggplot() +
  geom_sf(data = line_laea, color = 'black', size = 0.5) +
  geom_sf(data = china_prov, color = "black", size = 0.5) +
  coord_sf(
    ylim = c(-4028017, -1477844),
    xlim = c(200000, 1900000),
    crs = laea_cn
  ) +
  theme_void() +
  theme(panel.border = element_rect(fill = NA, color = "grey10", size = 0.5))

# Combine maps
combined_plot <- main_plot +
  inset_element(
    nine_map,
    left = 0.88,
    bottom = 0.15,
    right = 1.00,
    top = 0.30
  )

# Export figure
print(combined_plot)
ggsave("恙虫病东方体.pdf", plot = combined_plot, width = 16, height = 10, dpi = 300)
