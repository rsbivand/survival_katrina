# survival_katrina

Runnable R script derived from submission manuscript August 2020. Run time 9-12 hours, 16GB RAM required. Very recent INLA version required to extract DIC in these cases.

```
R version 4.0.2 (2020-06-22)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Fedora 32 (Workstation Edition)

Matrix products: default
BLAS:   /home/rsb/topics/R/R402-share/lib64/R/lib/libRblas.so
LAPACK: /home/rsb/topics/R/R402-share/lib64/R/lib/libRlapack.so

locale:
 [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8    
 [5] LC_MONETARY=en_GB.UTF-8    LC_MESSAGES=en_GB.UTF-8   
 [7] LC_PAPER=en_GB.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] parallel  stats4    stats     graphics  grDevices utils     datasets 
[8] methods   base     

other attached packages:
 [1] ProbitSpatial_1.0    speedglm_0.3-2       MASS_7.3-52         
 [4] Rcpp_1.0.5           spatialreg_1.1-6     spBayesSurv_1.1.4   
 [7] INLA_20.08.11-1      foreach_1.5.0        ggfortify_0.4.10    
[10] ggplot2_3.3.2        zoo_1.8-8            survival_3.2-3      
[13] car_3.0-9            carData_3.0-4        mapview_2.9.1       
[16] xtable_1.8-4         spatialprobit_0.9-11 tmvtnorm_1.4-10     
[19] gmm_1.6-5            sandwich_2.5-1       mvtnorm_1.1-1       
[22] spdep_1.1-5          sf_0.9-6             spData_0.3.8        
[25] sp_1.4-4             Matrix_1.2-18       

loaded via a namespace (and not attached):
 [1] nlme_3.1-148            satellite_1.0.2         webshot_0.5.2          
 [4] gmodels_2.18.1          numDeriv_2016.8-1.1     Deriv_4.0              
 [7] tools_4.0.2             R6_2.4.1                KernSmooth_2.23-17     
[10] DBI_1.1.0               colorspace_1.4-1        raster_3.3-13          
[13] withr_2.2.0             tidyselect_1.1.0        gridExtra_2.3          
[16] leaflet_2.0.3           curl_4.3                compiler_4.0.2         
[19] leafem_0.1.3            expm_0.999-5            labeling_0.3           
[22] scales_1.1.1            classInt_0.4-3          systemfonts_0.2.3      
[25] stringr_1.4.0           digest_0.6.25           foreign_0.8-80         
[28] svglite_1.2.3.2         rio_0.5.16              base64enc_0.1-3        
[31] pkgconfig_2.0.3         htmltools_0.5.0         htmlwidgets_1.5.1      
[34] rlang_0.4.7             readxl_1.3.1            farver_2.0.3           
[37] generics_0.0.2          jsonlite_1.7.0          crosstalk_1.1.0.1      
[40] gtools_3.8.2            dplyr_1.0.2             zip_2.1.0              
[43] magrittr_1.5            munsell_0.5.0           abind_1.4-5            
[46] gdtools_0.2.2           lifecycle_0.2.0         stringi_1.4.6          
[49] yaml_2.2.1              grid_4.0.2              gdata_2.18.0           
[52] forcats_0.5.0           crayon_1.3.4            deldir_0.1-28          
[55] lattice_0.20-41         haven_2.3.1             splines_4.0.2          
[58] hms_0.5.3               leafpop_0.0.5           pillar_1.4.6           
[61] uuid_0.1-4              boot_1.3-25             codetools_0.2-16       
[64] LearnBayes_2.15.1       glue_1.4.1              leaflet.providers_1.9.0
[67] data.table_1.13.0       png_0.1-7               vctrs_0.3.2            
[70] MatrixModels_0.4-1      cellranger_1.1.0        RANN_2.6.1             
[73] gtable_0.3.0            purrr_0.3.4             tidyr_1.1.1            
[76] openxlsx_4.1.5          e1071_1.7-3             coda_0.19-3            
[79] class_7.3-17            tibble_3.0.3            iterators_1.0.12       
[82] units_0.6-7             ellipsis_0.3.1          brew_1.0-6             
[85] spDataLarge_0.4.1      
```
