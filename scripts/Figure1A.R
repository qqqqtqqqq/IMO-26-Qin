# =============================================================================
# Figure 1A
# Geographic distribution of rickettsial pathogen detections in China
# =============================================================================

setwd("F:/BaiduSyncdisk/桌面文档/R")
# Load required packages
library(ggplot2)
library(readxl)

# Load input data
data_long <- read_excel("rickettsia_hotmap_long_format_with_group.xlsx")

# Inspect data
str(data_long)
head(data_long)

# Rename columns
colnames(data_long) <- c("pathogens", "Province", "Value", "Region", "Diseasestyle")

# Define region labels
region_order_chinese <- c(
  "Northeast" = "东北",
  "North China" = "华北", 
  "Northwest" = "西北",
  "East China" = "华东",
  "Central China" = "华中",
  "Southwest" = "西南",
  "South China" = "华南",
  "No data" = "无数据"
)

# Set region order
data_long$Region <- factor(data_long$Region, levels = names(region_order_chinese))

# Define province order
manual_province_order <- c(
  # Northeast China
   "Liaoning", "Jilin", "Heilongjiang", 
  # North China
  "Beijing", "Tianjin", "Hebei", "Shanxi", "Neimeng",
  # East China
  "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", "Jiangxi", "Shandong",
  # Central China
  "Henan", "Hubei", "Hunan",
  # South China
  "Guangdong", "Guangxi", "Hainan",
  # Southwest China
  "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Xizang",
  # Northwest China
  "Shaanxi", "Gansu", "Qinghai", "Ningxia", "Xinjiang",
  # Others
  "Hongkong", "Macao", "Tanwan"
)

# Keep provinces present in the dataset
manual_province_order <- manual_province_order[manual_province_order %in% unique(data_long$Province)]

# Append remaining provinces
missing_provinces <- setdiff(unique(data_long$Province), manual_province_order)
final_province_order <- c(manual_province_order, missing_provinces)

# Apply province order
data_long$Province <- factor(data_long$Province, levels = final_province_order)

# Define pathogen order
# Specify pathogen display order
manual_pathogen_order <- c(
  # Major pathogen groups
  "Rickettsia", "Orientia", "Coxiella", "Anaplasma phagocytophilum",
  # Rickettsia species
  "R. typhi", "R. prowazekii", "R. felis", "R. bellii","R. japonica", "R. sibirica", "R. monacensis",  "unidentified Rickettsia"
)

# Keep pathogens present in the dataset
manual_pathogen_order <- manual_pathogen_order[manual_pathogen_order %in% unique(data_long$pathogens)]

# Append remaining pathogens
missing_pathogens <- setdiff(unique(data_long$pathogens), manual_pathogen_order)
final_pathogen_order <- c(manual_pathogen_order, missing_pathogens)

# Apply pathogen order
data_long$pathogens <- factor(data_long$pathogens, levels = final_pathogen_order)

# Create labels
data_long$label_text <- ifelse(
  data_long$Region == "No data", 
  NA,  # Hide labels
  ifelse(data_long$Value == 0, NA, as.character(data_long$Value))
)



# Generate heatmap
p <- ggplot(data_long, aes(x = pathogens, y = Province)) +
  # Draw tiles for regions with data
  geom_tile(
    data = subset(data_long, Region != "No data"),
    aes(fill = Value),
    color = "black", width = 1, height = 1
  ) +
  # Draw tiles for regions without data
  geom_tile(
    data = subset(data_long, Region == "No data"),
    fill = "#D3D3D3",  # Light gray
    color = "black", width = 1, height = 1
  ) +
  # Define color scale
  scale_fill_gradient(
    low = "#FFFFE0", high = "#CC0000", 
    #low = "#FFF5B7", high = "#D7301F",
    na.value = "#F5F5F5",
    limits = c(1, 220),
    name = "检测值"
  ) +
  # Create faceted heatmap
  facet_grid(Region ~ Diseasestyle, 
             scales = "free", 
             space = "free",
             switch = "y",
             labeller = labeller(Region = region_order_chinese)) +
  # Add value labels
  geom_text(aes(label = label_text), 
            color = "black", size = 4, na.rm = TRUE) +
  theme_minimal() +
  labs(
    title = "立克次体检测热图", 
    x = "病原体", 
    y = "省份"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    strip.background = element_rect(fill = "lightblue", color = "black"),
    strip.text.x = element_text(size = 10, face = "bold"),
    strip.text.y = element_text(size = 10, face = "bold", angle = 0),
    strip.placement = "outside"
  )

# Display figure
print(p)

# Export figure
ggsave("rickettsia_heatmap_faceted.pdf", p, 
       width = 7.5,    # Figure width
       height = 16,   # Figure height
       dpi = 300)     # Resolution
