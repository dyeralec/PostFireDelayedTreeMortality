# PostFireDelayedTreeMortality
This repository stores the source code and select spatial data utilized to assess the spatiotemporal patterns of post-fire delayed tree mortality in temperate rainforests (Dyer et al. 2025).

Dyer, A.S., Busby, S., Evers, C. et al. Post-fire delayed tree mortality in mesic coniferous forests reduces fire refugia and seed sources. Landsc Ecol 40, 101 (2025). https://doi.org/10.1007/s10980-025-02111-2

Please note that imagery data and derivative products are not included in this repository due to source licensing and large data sizes. Refer to Dyer et al. (in review) for information regarding imagery sources and indices.

# Repository Layout

- `0_image_processing/` - Scripts to calculate remote-sensing indices using annual fire imagery including boundary and pseudo-invariant feature (PIF) point shapefiles.
- `1_forest_classification/` - Scripts to classify forest cover annually and pseudo-invariant feature (PIF) point shapefiles.
- `2_spatial_analysis/` - Scripts to calculate patch- and landscape-scale metrics and statistics.
- `3_fire_sensitivity/` - Scripts and table utilized to calculate fire sensitivity.
