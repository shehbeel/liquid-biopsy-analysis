# Script to make Circos plot for liquid biopsy dataset
# Author: Shehbeel Arif
# Children's Hospital of Philadelphia

# Load libraries
library(tidyverse)
library(ComplexHeatmap)
library(circlize)

# SETTING UP DIRECTORIES & LOADING DATA

## Set directories
root_dir <- rprojroot::find_root(rprojroot::has_dir(".git"))
# Without git, simply do this:
# root_dir <- file.path("directory...")
data_dir <- file.path(root_dir, "data")
plots_dir <- file.path(root_dir, "plots")

# Create the plots folder if it doesn't exist
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir)
}

# Load GeneLab dataset descriptions
data_file <- file.path(data_dir, "circos_plot_data.csv")
df <- readr::read_csv(data_file)

# Columns of interest: StudySubject_ID,	SDG_ID,	Diagnosis,	Specimen_Type	Gender,	Case_type,	AgeGroup_at_Collection
df <- df %>%
  select(SDG_ID,	Diagnosis,	Specimen_Type,	Gender,	Case_type,	AgeGroup_at_Collection) %>%
  column_to_rownames('SDG_ID') %>%
  arrange(Diagnosis, Case_type, AgeGroup_at_Collection, Gender, Specimen_Type)

# Split the Circos plot by diagnosis (where to add the gaps)
split <- factor(df$Diagnosis)

# Specify colors for each variable
color_palette <- list(# Diagnosis
                      "Medulloblastoma"="#FE71CF",
                      "Atypical Teratoid Rhabdoid Tumor (ATRT)" = "#5C5BAE",
                      "Ependymoma" = "#06FFA1", 
                      "Germ Cell Tumor" = "#FBFC7A",
                      "High-grade glioma" = "#7BFDFF",
                      "Low-grade glioma" = "#B868FF",
  
                      # Case_type
                      "Initial CNS Tumor" = "#6AB3B9",
                      "Recurrence" = "#F2BAAD",
                      "Progressive" = "#9A605F",
  
                      # AgeGroup_at_Collection
                      "0-2yrs" ="#B1F5FD", 
                      "3-5yrs" = '#6AB3B9',
                      "6-12yrs" = "#FFCEE4",
                      "13-18yrs" = "#BB7FBB",
                      "Over 18yrs" = "#A24D75", 
                      
                      # Gender
                      "Male" = "#BEB8DA", 
                      "Female" = "#B2E063",
  
                      # Specimen_Type
                      "Plasma" = "#FD7F6F",
                      "CSF" = "#4D75A2",
                      "Both" = "#4DA27A"
)

# MAKE CIRCOS PLOT!

# First clear any previously saved circos plot data
circos.clear()

# Specify some of the characteristics of the circos plot
circos.par(start.degree = 30, # Rotate the circos plot
           gap.degree = 1, # Gap between splits
           points.overflow.warning = FALSE) # Ignore annoying warning messages


circos.heatmap(df,
               split=split,
               col = unlist(color_palette),
               show.sector.labels = T, # Label Diagnosis around the plot
               cell.border = "black", # Color of cell borders
               bg.border = "black",
               bg.lwd = 3,
               track.height = 0.5, # Width of plot (imagine width of donut)
)



# ADDING LEGEND
lgd_diagnosis = Legend(title = "Diagnosis", 
                  at = c("Medulloblastoma",
                         "Atypical Teratoid Rhabdoid Tumor (ATRT)",
                         "Ependymoma", 
                         "Germ Cell Tumor",
                         "High-Grade Glioma",
                         "Low-Grade Glioma"), 
                  legend_gp = gpar(fill = c("#FE71CF","#5C5BAE",
                                            "#06FFA1","#FBFC7A",
                                            "#7BFDFF","#B868FF")))
 
lgd_case_type = Legend(title = "Case type", 
                       at = c("Initial CNS Tumor", "Recurrence", "Progressive"), 
                       legend_gp = gpar(fill = c("#6AB3B9", "#F2BAAD", "#9A605F")))

lgd_age_group = Legend(title="Age Group",
                    at = c("0-2yrs", "3-5yrs","6-12yrs", 
                           "13-18yrs", "Over 18yrs"),
                    legend_gp = gpar(fill = c("#B1F5FD", '#6AB3B9', 
                                              "#FFCEE4", "#BB7FBB", 
                                              "#A24D75")))

lgd_gender = Legend(title="Gender",
                      at = c("Male", "Female"),
                      legend_gp = gpar(fill = c("#BEB8DA", "#B2E063")))


lgd_specimen_type = Legend(title="Specimen Type",
                             at = c("Plasma", "CSF", 
                                    "Both"),
                             legend_gp = gpar(fill = c("#FD7F6F","#4D75A2","#4DA27A")))


h = dev.size()[2]

lgd_list1 = packLegend(lgd_diagnosis, max_height = unit(0.9*h, "inch"), direction = "horizontal")
lgd_list2 = packLegend(lgd_age_group, lgd_case_type, max_height = unit(0.9*h, "inch"), direction = "horizontal")
lgd_list3 = packLegend(lgd_specimen_type, lgd_gender, max_height = unit(0.9*h, "inch"), direction = "horizontal")

dev.off()
draw(lgd_list1, x = unit(50, "mm"), y = unit(100, "mm"))
draw(lgd_list2, x = unit(45, "mm"), y = unit(70, "mm"))
draw(lgd_list3, x = unit(39, "mm"), y = unit(46, "mm"))


