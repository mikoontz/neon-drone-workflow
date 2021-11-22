# https://github.com/weecology/NeonTreeEvaluation_package

library(devtools)
devtools::install_github("Weecology/NeonTreeEvaluation_package")
library(NeonTreeEvaluation)

NeonTreeEvaluation::download()

df <- submission %>% filter(plot_name=="NIWO_017_2018")
results <- evaluate_field_stems(predictions = df, project = F, show = T, summarize = T)

NeonTreeEvaluation::boxes_to_spatial_polygons()

rgb_path <- get_data(plot_name = "NIWO_017_2019", type="rgb")
rgb <- terra::rast(rgb_path)

crowns <- 
  sf::st_read("data/drone/L3a/geometric/niwo_017/2019-10-09/niwo_017_2019-10-09_crowns.gpkg") %>% 
  sf::st_transform(local_utm) %>% 
  dplyr::filter(sf::st_intersects(x = ., y = interior_plot, sparse = FALSE)) %>% 
  dplyr::rename(geometry = geom) %>% 
  dplyr::mutate(xmin_proj = NA, ymin_proj = NA, xmax_proj = NA, ymax_proj = NA)

for(i in 1:nrow(crowns)) {
  bbox <- st_bbox(crowns[i, "geometry"])
  crowns[i, "xmin_proj"] <- bbox$xmin
  crowns[i, "ymin_proj"] <- bbox$ymin
  crowns[i, "xmax_proj"] <- bbox$xmax
  crowns[i, "ymax_proj"] <- bbox$ymax
}

res <- terra::res(rgb)
e <- sf::st_bbox(terra::ext(rgb))

crowns <-
  crowns %>% 
  dplyr::mutate(xmin = (xmin_proj - e$xmin) / (e$xmax - e$xmin) * ncol(rgb),
                ymin = (ymin_proj - e$ymin) / (e$ymax - e$ymin) * nrow(rgb),
                xmax = (xmax_proj - e$xmin) / (e$xmax - e$xmin) * ncol(rgb),
                ymax = (ymax_proj - e$ymin) / (e$ymax - e$ymin) * nrow(rgb),
                label = "Tree",
                plot_name = "NIWO_017_2018") %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(xmin, ymin, xmax, ymax, label, plot_name)

crowns
df

results3 <- evaluate_field_stems(predictions = crowns, project = FALSE, show = TRUE, summarize = TRUE)
results <- evaluate_image_crowns(predictions = df, project = FALSE, show = F, summarize = TRUE)


df<-submission %>% filter(plot_name %in% c("NIWO_017_2018"))
#Compute total recall and precision for the overlap data
results<-evaluate_image_crowns(predictions = df,project = T, show=F, summarize = T)
drone_results<-evaluate_image_crowns(predictions = crowns,project = T, show=F, summarize = T)


results<-evaluate_image_crowns(predictions = df,project = T, show=F, summarize = T)
?NeonTreeEvaluation::compute_precision_recall()
?evaluate_field_stems
