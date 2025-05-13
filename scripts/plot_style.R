#------------------------------------------------------------------------------#
# PLOT STYLE
#------------------------------------------------------------------------------#
# This file contains objects for plotting and label ordering

#------------------------------------------------------------------------------#
# Create label mapping for plotting
#------------------------------------------------------------------------------#
x_lab_ori <- c("arch_index","n_authors_log","n_countries_log","n_samples_log",
               "year_publication","n_authors","n_countries","n_samples")
x_lab_disp <- c("Archaeology Index","log(Number of authors)",
                "log(Number of countries)","log(Number of samples)",
                "Year of publication","Number of authors",
                "Number of countries","Number of samples")
x_lab_lookup <- cbind(ori = x_lab_ori, disp = x_lab_disp)
y_lab_ori <- c("Anthropological_age","Anthropological_sex","Body_arrangement",
               "Body_positioning","C14","Culture","Grave","Grave_goods","Images",
               "IndividualID","Geolocation","Multiple_names","Overall_chronology",
               "References","Sample_chronology","Site_description")
y_lab_disp <- c("Anthropological age","Anthropological sex","Body arrangement",
                "Body positioning","C14","Culture","Grave","Grave goods","Images",
                "Individual ID","Geolocation","Multiple names","Overall chronology",
                "References","Sample chronology","Site description")
y_lab_ori2 <- c("Anthropological_age.Extended","Anthropological_sex.Extended","Body_arrangement.Extended",
               "Body_positioning.Extended","C14.Extended","Culture.Extended","Grave.Extended","Grave_goods.Extended","Images.Extended",
               "IndividualID.Core","Geolocation.Core","Multiple_names.Core","Overall_chronology.Core",
               "References.Core","Sample_chronology.Core","Site_description.Core")
y_lab_disp2 <- c("Anthropological age.Extended","Anthropological sex.Extended","Body arrangement.Extended",
                "Body positioning.Extended","C14.Extended","Culture.Extended","Grave.Extended","Grave goods.Extended","Images.Extended",
                "Individual ID.Core","Geolocation.Core","Multiple names.Core","Overall chronology.Core",
                "References.Core","Sample chronology.Core","Site description.Core")
y_lab_lookup <- cbind(ori = y_lab_ori, disp = y_lab_disp)
y_lab_lookup2 <- cbind(ori = y_lab_ori2, disp = y_lab_disp2)
cat_lab_ori <- c("Core", "Extended")
cat_lab_disp <- c("Core", "Extended") # Otherwise plotting wouldn't work
cat_lab_lookup <- cbind(ori = cat_lab_ori, disp = cat_lab_disp)
pc_lab_ori <- paste0("Dim.",seq(1:16))
pc_lab_disp <- paste0("PC",seq(1:16))
pc_lab_lookup <- cbind(ori = pc_lab_ori, disp = pc_lab_disp)

lab_lookup <- as.data.frame(rbind(x_lab_lookup, y_lab_lookup, y_lab_lookup2, cat_lab_lookup, pc_lab_lookup))
lab_mapping <- setNames(lab_lookup$disp, lab_lookup$ori)

y_lab_order <- c("Geolocation","IndividualID","Multiple_names","Overall_chronology",
                 "References","Sample_chronology","Site_description","Anthropological_age","Anthropological_sex","Body_arrangement",
                 "Body_positioning","C14","Culture","Grave","Grave_goods","Images")

#------------------------------------------------------------------------------#
# For patchwork
#------------------------------------------------------------------------------#
remove_x <- theme(
  axis.title.x = element_blank()
)

remove_y <- theme(
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.title.y = element_blank()
)

#------------------------------------------------------------------------------#
# For patchwork
#------------------------------------------------------------------------------#
theme_pca <-  theme(legend.position = "none",                     
                    panel.grid.minor = element_blank(),
                    axis.title = element_text(size = 10, hjust = 1))

#------------------------------------------------------------------------------#
# Colours for each y or x if applicable
#------------------------------------------------------------------------------#
y_cols <- c(MetBrewer::met.brewer("Hiroshige", n = 18)[c(10:15,1,16:18,2:7)])
x_cols <- MetBrewer::met.brewer("VanGogh1",7)[1:5]
pc_cols1 <- rev(RColorBrewer::brewer.pal(6, "Blues"))
