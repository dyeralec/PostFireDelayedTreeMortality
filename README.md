# PostFireDelayedTreeMortality
This repository stores the source code and select spatial data utilized in Dyer et al. (in review) for data processing and analysis.

Dyer AS, Busby S, Evers C, Reilly M, Zuspan A, Holz A (2025). Ecological implications of post-fire delayed tree mortality following high severity wildfires in temperate rainforests.

Please note that imagery data and derivative products are not included in this repository due to source licensing and large data sizes. Refer to Dyer et al. (in review) for information regarding imagery sources and indices.

# Repository Layout

- `0_image_processing/` - Scripts to calculate remote-sensing indices using annual fire imagery including boundary and psuedo-invariant feature (PIF) point shapefiles.
- `1_forest_classification/` - Scripts to classify forest cover anually and psuedo-invariant feature (PIF) point shapefiles.
- `2_spatial_analysis/` - Scripts to calculate patch- and landscape-scale metrics and statistics.
- `3_fire_sensitivity/` - Scripts and table utilized to calcualte fire sensitivity.