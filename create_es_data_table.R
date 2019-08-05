
library(tidyverse)


### Create vectors for columns
study_names <- c("ariga_2007_exp2", 
                 "beanland_pammer_2010_exp1A_fixating", 
                 "beanland_pammer_2010_exp1A_moving", 
                 "beanland_pammer_2010_exp2_slow_us", 
                 "beanland_pammer_2010_exp2_fast_us", 
                 "gabay_2012_exp1",
                 "gabay_2012_exp2", 
                 "lo_yeh_2008_exp1_200ms", 
                 "lo_yeh_2008_exp1_500ms", 
                 "lo_yeh_2008_exp2_200ms",
                 "lo_yeh_2008_exp2_500ms",
                 "mack_and_rock_2000_exp1",
                 "mack_and_rock_2000_exp2",
                 "mack_and_rock_2000_exp3",
                 "mack_and_rock_2000_exp4",
                 "mack_and_rock_2000_exp5",
                 "moore_egeth_1997_exp1", 
                 "moore_egeth_1997_exp3", 
                 "moore_2003_exp3",
                 "moore_2004", 
                 "most_2005_exp1to7pooled", 
                 "razpurker_pratt_2008_columns_rows_rt", 
                 "razpurker_pratt_2008_columns_rows_acc", 
                 "razpurker_pratt_2008_triangle_arrow_rt",
                 "razpurker_pratt_2008_triangle_arrow_acc", 
                 "richards_2012_tracking", 
                 "russell_driver_2005_exp1_acc", 
                 "russell_driver_2005_exp1_rt", 
                 "russell_driver_2005_exp2_acc",
                 "russell_driver_2005_exp2_rt", 
                 "russell_driver_2005_exp3_acc", 
                 "russell_driver_2005_exp3_rt",
                 "russell_driver_2005_exp4a_acc", 
                 "russell_driver_2005_exp4b_acc", 
                 "russell_driver_2005_exp5_acc",
                 "russell_driver_2005_exp5_rt", 
                 "shafto_pitts_2015", 
                 "schnuerch_2016_exp1",
                 "schnuerch_2016_exp2"
                 )

N_per_group <- c(20, #ariga_2007_exp2  
                 27, #beanland_pammer_2010_exp1A_fixating
                 31, #beanland_pammer_2010_exp1A_moving
                 41, #beanland_pammer_2010_exp2_slow_us
                 41, #beanland_pammer_2010_exp2_fast_us
                 18, #gabay_2012_exp1
                 10, #gabay_2012_exp2
                 43, #lo_yeh_2008_exp1_200ms
                 41, #lo_yeh_2008_exp1_500ms
                 23, #lo_yeh_2008_exp2_200ms
                 25, #lo_yeh_2008_exp2_500ms
                 80, #mack_and_rock_2000_exp1,
                 75, #mack_and_rock_2000_exp2,
                 30, #mack_and_rock_2000_exp3,
                 40, #mack_and_rock_2000_exp4,
                 30, #mack_and_rock_2000_exp5,
                 20, #moore_egeth_1997_exp1
                 20, #moore_egeth_1997_exp3
                 16, #moore_2003_exp3
                 25, #moore_2004
                 186, #most_2005_exp1to7pooled
                 14, #razpurker_pratt_2008_columns_rows_rt
                 14, #razpurker_pratt_2008_columns_rows_acc
                 14, #razpurker_pratt_2008_triangle_arrow_rt
                 14, #razpurker_pratt_2008_triangle_arrow_acc
                 119, #richards_2012_tracking
                 25, #russell_driver_2005_exp1_acc
                 25, #russell_driver_2005_exp1_rt
                 28, #russell_driver_2005_exp2_acc
                 28, #russell_driver_2005_exp2_rt
                 24, #russell_driver_2005_exp3_acc
                 24, #russell_driver_2005_exp3_rt
                 20, #russell_driver_2005_exp4a_acc
                 21, #russell_driver_2005_exp4b_acc
                 24, #russell_driver_2005_exp5_acc
                 24, #russell_driver_2005_exp5_rt
                 15, #shafto_pitts_2015
                 61, #schnuerch_2016_exp1
                 58 #schnuerch_2016_exp2
                 )



us_relevance <- c(0, #ariga_2007_exp2  
                  0, #beanland_pammer_2010_exp1A_fixating
                  0, #beanland_pammer_2010_exp1A_moving
                  0, #beanland_pammer_2010_exp2_slow_us
                  0, #beanland_pammer_2010_exp2_fast_us
                  1, #gabay_2012_exp1
                  1, #gabay_2012_exp2
                  1, #lo_yeh_2008_exp1_200ms
                  1, #lo_yeh_2008_exp1_500ms
                  0, #lo_yeh_2008_exp2_200ms
                  0, #lo_yeh_2008_exp2_500ms
                  0, #mack_and_rock_2000_exp1,
                  0, #mack_and_rock_2000_exp2,
                  0, #mack_and_rock_2000_exp3,
                  0, #mack_and_rock_2000_exp4,
                  0, #mack_and_rock_2000_exp5,
                  1, #moore_egeth_1997_exp1
                  1, #moore_egeth_1997_exp3
                  1, #moore_2003_exp3
                  0, #moore_2004
                  0, #most_2005_exp1to7pooled
                  0, #razpurker_pratt_2008_columns_rows_rt
                  0, #razpurker_pratt_2008_columns_rows_acc
                  0, #razpurker_pratt_2008_triangle_arrow_rt
                  0, #razpurker_pratt_2008_triangle_arrow_acc
                  0, #richards_2012_tracking
                  0, #russell_driver_2005_exp1_acc
                  0, #russell_driver_2005_exp1_rt
                  0, #russell_driver_2005_exp2_acc
                  0, #russell_driver_2005_exp2_rt
                  0, #russell_driver_2005_exp3_acc
                  0, #russell_driver_2005_exp3_rt
                  0, #russell_driver_2005_exp4a_acc
                  0, #russell_driver_2005_exp4b_acc
                  0, #russell_driver_2005_exp5_acc
                  0, #russell_driver_2005_exp5_rt
                  0, #shafto_pitts_2015
                  0, #schnuerch_2016_exp1
                  0 #schnuerch_2016_exp2
                  )


implicit_type <- c(1, #ariga_2007_exp2  
                   0, #beanland_pammer_2010_exp1A_fixating
                   0, #beanland_pammer_2010_exp1A_moving
                   0, #beanland_pammer_2010_exp2_slow_us
                   0, #beanland_pammer_2010_exp2_fast_us
                   1, #gabay_2012_exp1
                   1, #gabay_2012_exp2
                   1, #lo_yeh_2008_exp1_200ms
                   1, #lo_yeh_2008_exp1_500ms
                   1, #lo_yeh_2008_exp2_200ms
                   1, #lo_yeh_2008_exp2_500ms
                   NA, #mack_and_rock_2000_exp1,
                   NA, #mack_and_rock_2000_exp2,
                   NA, #mack_and_rock_2000_exp3,
                   NA, #mack_and_rock_2000_exp4,
                   NA, #mack_and_rock_2000_exp5,
                   1, #moore_egeth_1997_exp1
                   1, #moore_egeth_1997_exp3
                   1, #moore_2003_exp3
                   0, #moore_2004
                   0, #most_2005_exp1to7pooled
                   0, #razpurker_pratt_2008_columns_rows_rt
                   1, #razpurker_pratt_2008_columns_rows_acc
                   1, #razpurker_pratt_2008_triangle_arrow_rt
                   0, #razpurker_pratt_2008_triangle_arrow_acc
                   0, #richards_2012_tracking
                   0, #russell_driver_2005_exp1_acc
                   0, #russell_driver_2005_exp1_rt
                   0, #russell_driver_2005_exp2_acc
                   0, #russell_driver_2005_exp2_rt
                   0, #russell_driver_2005_exp3_acc
                   0, #russell_driver_2005_exp3_rt
                   0, #russell_driver_2005_exp4a_acc
                   0, #russell_driver_2005_exp4b_acc
                   0, #russell_driver_2005_exp5_acc
                   0, #russell_driver_2005_exp5_rt
                   0, #shafto_pitts_2015
                   0, #schnuerch_2016_exp1
                   0 #schnuerch_2016_exp2
                   )


implicit_measure <- c(0, #ariga_2007_exp2  
                      1, #beanland_pammer_2010_exp1A_fixating
                      1, #beanland_pammer_2010_exp1A_moving
                      1, #beanland_pammer_2010_exp2_slow_us
                      1, #beanland_pammer_2010_exp2_fast_us
                      0, #gabay_2012_exp1
                      0, #gabay_2012_exp2
                      1, #lo_yeh_2008_exp1_200ms
                      1, #lo_yeh_2008_exp1_500ms
                      0, #lo_yeh_2008_exp2_200ms
                      0, #lo_yeh_2008_exp2_500ms
                      NA, #mack_and_rock_2000_exp1,
                      NA, #mack_and_rock_2000_exp2,
                      NA, #mack_and_rock_2000_exp3,
                      NA, #mack_and_rock_2000_exp4,
                      NA, #mack_and_rock_2000_exp5,
                      1, #moore_egeth_1997_exp1
                      1, #moore_egeth_1997_exp3
                      0, #moore_2003_exp3
                      0, #moore_2004
                      1, #most_2005_exp1to7pooled
                      NA, #razpurker_pratt_2008_columns_rows_rt
                      NA, #razpurker_pratt_2008_columns_rows_acc
                      NA, #razpurker_pratt_2008_triangle_arrow_rt
                      NA, #razpurker_pratt_2008_triangle_arrow_acc
                      NA, #richards_2012_tracking
                      1, #russell_driver_2005_exp1_acc
                      0, #russell_driver_2005_exp1_rt
                      1, #russell_driver_2005_exp2_acc
                      0, #russell_driver_2005_exp2_rt
                      1, #russell_driver_2005_exp3_acc
                      0, #russell_driver_2005_exp3_rt
                      1, #russell_driver_2005_exp4a_acc
                      1, #russell_driver_2005_exp4b_acc
                      1, #russell_driver_2005_exp5_acc
                      0, #russell_driver_2005_exp5_rt
                      NA, #shafto_pitts_2015
                      0, #schnuerch_2016_exp1
                      0 #schnuerch_2016_exp2
                      )



N_trials_implicit <- c(1, #ariga_2007_exp2  
                       2, #beanland_pammer_2010_exp1A_fixating
                       2, #beanland_pammer_2010_exp1A_moving
                       2, #beanland_pammer_2010_exp2_slow_us
                       2, #beanland_pammer_2010_exp2_fast_us
                       40, #gabay_2012_exp1
                       40, #gabay_2012_exp2
                       16, #lo_yeh_2008_exp1_200ms
                       16, #lo_yeh_2008_exp1_500ms
                       64, #lo_yeh_2008_exp2_200ms
                       64, #lo_yeh_2008_exp2_500ms
                       1, #mack_and_rock_2000_exp1,
                       1, #mack_and_rock_2000_exp2,
                       1, #mack_and_rock_2000_exp3,
                       1, #mack_and_rock_2000_exp4,
                       1, #mack_and_rock_2000_exp5,
                       17, #moore_egeth_1997_exp1
                       17, #moore_egeth_1997_exp3
                       1, #moore_2003_exp3
                       1, #moore_2004
                       1, #most_2005_exp1to7pooled
                       320, #razpurker_pratt_2008_columns_rows_rt
                       320, #razpurker_pratt_2008_columns_rows_acc
                       320, #razpurker_pratt_2008_triangle_arrow_rt
                       320, #razpurker_pratt_2008_triangle_arrow_acc
                       1, #richards_2012_tracking
                       480, #russell_driver_2005_exp1_acc
                       480, #russell_driver_2005_exp1_rt
                       480, #russell_driver_2005_exp2_acc
                       480, #russell_driver_2005_exp2_rt
                       600, #russell_driver_2005_exp3_acc
                       600, #russell_driver_2005_exp3_rt
                       600, #russell_driver_2005_exp4a_acc
                       600, #russell_driver_2005_exp4b_acc
                       600, #russell_driver_2005_exp5_acc
                       600, #russell_driver_2005_exp5_rt
                       216, #shafto_pitts_2015
                       150, #schnuerch_2016_exp1
                       192 #schnuerch_2016_exp2
                       )

N_participants_implicit <- c(20, #ariga_2007_exp2  
                    27, #beanland_pammer_2010_exp1A_fixating
                    31, #beanland_pammer_2010_exp1A_moving
                    41, #beanland_pammer_2010_exp2_slow_us
                    41, #beanland_pammer_2010_exp2_fast_us
                    18, #gabay_2012_exp1
                    10, #gabay_2012_exp2
                    43, #lo_yeh_2008_exp1_200ms
                    41, #lo_yeh_2008_exp1_500ms
                    23, #lo_yeh_2008_exp2_200ms
                    25, #lo_yeh_2008_exp2_500ms
                    NA, #mack_and_rock_2000_exp1,
                    NA, #mack_and_rock_2000_exp2,
                    NA, #mack_and_rock_2000_exp3,
                    NA, #mack_and_rock_2000_exp4,
                    NA, #mack_and_rock_2000_exp5,
                    20, #moore_egeth_1997_exp1
                    20, #moore_egeth_1997_exp3
                    16, #moore_2003_exp3
                    25, #moore_2004
                    186, #most_2005_exp1to7pooled
                    14, #razpurker_pratt_2008_columns_rows_rt
                    14, #razpurker_pratt_2008_columns_rows_acc
                    14, #razpurker_pratt_2008_triangle_arrow_rt
                    14, #razpurker_pratt_2008_triangle_arrow_acc
                    131, #richards_2012_tracking
                    25, #russell_driver_2005_exp1_acc
                    25, #russell_driver_2005_exp1_rt
                    28, #russell_driver_2005_exp2_acc
                    28, #russell_driver_2005_exp2_rt
                    24, #russell_driver_2005_exp3_acc
                    24, #russell_driver_2005_exp3_rt
                    20, #russell_driver_2005_exp4a_acc
                    20, #russell_driver_2005_exp4b_acc
                    24, #russell_driver_2005_exp5_acc
                    24, #russell_driver_2005_exp5_rt
                    15, #shafto_pitts_2015
                    61, #schnuerch_2016_exp1
                    58 #schnuerch_2016_exp2
                    )


awareness_objective <- c(0, #ariga_2007_exp2  
                         0, #beanland_pammer_2010_exp1A_fixating
                         0, #beanland_pammer_2010_exp1A_moving
                         0, #beanland_pammer_2010_exp2_slow_us
                         0, #beanland_pammer_2010_exp2_fast_us
                         1, #gabay_2012_exp1
                         1, #gabay_2012_exp2
                         0, #lo_yeh_2008_exp1_200ms
                         0, #lo_yeh_2008_exp1_500ms
                         0, #lo_yeh_2008_exp2_200ms
                         0, #lo_yeh_2008_exp2_500ms
                         1, #mack_and_rock_2000_exp1,
                         1, #mack_and_rock_2000_exp2,
                         1, #mack_and_rock_2000_exp3,
                         1, #mack_and_rock_2000_exp4,
                         1, #mack_and_rock_2000_exp5,
                         0, #moore_egeth_1997_exp1
                         0, #moore_egeth_1997_exp3
                         0, #moore_2003_exp3
                         0, #moore_2004
                         1, #most_2005_exp1to7pooled
                         1, #razpurker_pratt_2008_columns_rows_rt
                         1, #razpurker_pratt_2008_columns_rows_acc
                         1, #razpurker_pratt_2008_triangle_arrow_rt
                         1, #razpurker_pratt_2008_triangle_arrow_acc
                         0, #richards_2012_tracking
                         0, #russell_driver_2005_exp1_acc
                         0, #russell_driver_2005_exp1_rt
                         0, #russell_driver_2005_exp2_acc
                         0, #russell_driver_2005_exp2_rt
                         0, #russell_driver_2005_exp3_acc
                         0, #russell_driver_2005_exp3_rt
                         0, #russell_driver_2005_exp4a_acc
                         0, #russell_driver_2005_exp4b_acc
                         0, #russell_driver_2005_exp5_acc
                         0, #russell_driver_2005_exp5_rt
                         1, #shafto_pitts_2015
                         1, #schnuerch_2016_exp1
                         1 #schnuerch_2016_exp2
                         )



us_presentation <- c(0, #ariga_2007_exp2  
                     0, #beanland_pammer_2010_exp1A_fixating
                     0, #beanland_pammer_2010_exp1A_moving
                     0, #beanland_pammer_2010_exp2_slow_us
                     0, #beanland_pammer_2010_exp2_fast_us
                     1, #gabay_2012_exp1
                     1, #gabay_2012_exp2
                     0, #lo_yeh_2008_exp1_200ms
                     0, #lo_yeh_2008_exp1_500ms
                     0, #lo_yeh_2008_exp2_200ms
                     0, #lo_yeh_2008_exp2_500ms
                     1, #mack_and_rock_2000_exp1,
                     1, #mack_and_rock_2000_exp2,
                     1, #mack_and_rock_2000_exp3,
                     1, #mack_and_rock_2000_exp4,
                     1, #mack_and_rock_2000_exp5,
                     0, #moore_egeth_1997_exp1
                     0, #moore_egeth_1997_exp3
                     0, #moore_2003_exp3
                     0, #moore_2004
                     0, #most_2005_exp1to7pooled
                     1, #razpurker_pratt_2008_columns_rows_rt
                     1, #razpurker_pratt_2008_columns_rows_acc
                     1, #razpurker_pratt_2008_triangle_arrow_rt
                     1, #razpurker_pratt_2008_triangle_arrow_acc
                     NA, #richards_2012_tracking
                     0, #russell_driver_2005_exp1_acc
                     0, #russell_driver_2005_exp1_rt
                     0, #russell_driver_2005_exp2_acc
                     0, #russell_driver_2005_exp2_rt
                     0, #russell_driver_2005_exp3_acc
                     0, #russell_driver_2005_exp3_rt
                     0, #russell_driver_2005_exp4a_acc
                     0, #russell_driver_2005_exp4b_acc
                     0, #russell_driver_2005_exp5_acc
                     0, #russell_driver_2005_exp5_rt
                     0, #shafto_pitts_2015
                     0, #schnuerch_2016_exp1
                     0 #schnuerch_2016_exp2
                     )



us_delay_type <- c(0, #ariga_2007_exp2  
                   0, #beanland_pammer_2010_exp1A_fixating
                   0, #beanland_pammer_2010_exp1A_moving
                   0, #beanland_pammer_2010_exp2_slow_us
                   0, #beanland_pammer_2010_exp2_fast_us
                   1, #gabay_2012_exp1
                   1, #gabay_2012_exp2
                   0, #lo_yeh_2008_exp1_200ms
                   0, #lo_yeh_2008_exp1_500ms
                   0, #lo_yeh_2008_exp2_200ms
                   0, #lo_yeh_2008_exp2_500ms
                   NA, #mack_and_rock_2000_exp1,
                   NA, #mack_and_rock_2000_exp2,
                   NA, #mack_and_rock_2000_exp3,
                   NA, #mack_and_rock_2000_exp4,
                   NA, #mack_and_rock_2000_exp5,
                   1, #moore_egeth_1997_exp1
                   1, #moore_egeth_1997_exp3
                   0, #moore_2003_exp3
                   0, #moore_2004
                   0, #most_2005_exp1to7pooled
                   1, #razpurker_pratt_2008_columns_rows_rt
                   1, #razpurker_pratt_2008_columns_rows_acc
                   1, #razpurker_pratt_2008_triangle_arrow_rt
                   1, #razpurker_pratt_2008_triangle_arrow_acc
                   1, #richards_2012_tracking
                   1, #russell_driver_2005_exp1_acc
                   1, #russell_driver_2005_exp1_rt
                   1, #russell_driver_2005_exp2_acc
                   1, #russell_driver_2005_exp2_rt
                   1, #russell_driver_2005_exp3_acc
                   1, #russell_driver_2005_exp3_rt
                   1, #russell_driver_2005_exp4a_acc
                   1, #russell_driver_2005_exp4b_acc
                   1, #russell_driver_2005_exp5_acc
                   1, #russell_driver_2005_exp5_rt
                   1, #shafto_pitts_2015
                   1, #schnuerch_2016_exp1
                   1 #schnuerch_2016_exp2
                   )

us_assessment <- c(0, #ariga_2007_exp2  
                   1, #beanland_pammer_2010_exp1A_fixating
                   1, #beanland_pammer_2010_exp1A_moving
                   1, #beanland_pammer_2010_exp2_slow_us
                   1, #beanland_pammer_2010_exp2_fast_us
                   1, #gabay_2012_exp1
                   1, #gabay_2012_exp2
                   0, #lo_yeh_2008_exp1_200ms
                   0, #lo_yeh_2008_exp1_500ms
                   0, #lo_yeh_2008_exp2_200ms
                   0, #lo_yeh_2008_exp2_500ms
                   NA, #mack_and_rock_2000_exp1,
                   NA, #mack_and_rock_2000_exp2,
                   NA, #mack_and_rock_2000_exp3,
                   NA, #mack_and_rock_2000_exp4,
                   NA, #mack_and_rock_2000_exp5,
                   0, #moore_egeth_1997_exp1
                   0, #moore_egeth_1997_exp3
                   0, #moore_2003_exp3
                   0, #moore_2004
                   0, #most_2005_exp1to7pooled
                   1, #razpurker_pratt_2008_columns_rows_rt
                   1, #razpurker_pratt_2008_columns_rows_acc
                   1, #razpurker_pratt_2008_triangle_arrow_rt
                   1, #razpurker_pratt_2008_triangle_arrow_acc
                   1, #richards_2012_tracking
                   0, #russell_driver_2005_exp1_acc
                   0, #russell_driver_2005_exp1_rt
                   0, #russell_driver_2005_exp2_acc
                   0, #russell_driver_2005_exp2_rt
                   0, #russell_driver_2005_exp3_acc
                   0, #russell_driver_2005_exp3_rt
                   0, #russell_driver_2005_exp4a_acc
                   0, #russell_driver_2005_exp4b_acc
                   0, #russell_driver_2005_exp5_acc
                   0, #russell_driver_2005_exp5_rt
                   1, #shafto_pitts_2015
                   1, #schnuerch_2016_exp1
                   1 #schnuerch_2016_exp2
                   )

N_trials_awareness <- c(0, #ariga_2007_exp2  
                        1, #beanland_pammer_2010_exp1A_fixating
                        1, #beanland_pammer_2010_exp1A_moving
                        1, #beanland_pammer_2010_exp2_slow_us
                        1, #beanland_pammer_2010_exp2_fast_us
                        40, #gabay_2012_exp1
                        40, #gabay_2012_exp2
                        0, #lo_yeh_2008_exp1_200ms
                        0, #lo_yeh_2008_exp1_500ms
                        0, #lo_yeh_2008_exp2_200ms
                        0, #lo_yeh_2008_exp2_500ms
                        1, #mack_and_rock_2000_exp1,
                        1, #mack_and_rock_2000_exp2,
                        1, #mack_and_rock_2000_exp3,
                        1, #mack_and_rock_2000_exp4,
                        1, #mack_and_rock_2000_exp5,
                        0, #moore_egeth_1997_exp1
                        0, #moore_egeth_1997_exp3
                        0, #moore_2003_exp3
                        0, #moore_2004
                        0, #most_2005_exp1to7pooled
                        1, #razpurker_pratt_2008_columns_rows_rt
                        1, #razpurker_pratt_2008_columns_rows_acc
                        1, #razpurker_pratt_2008_triangle_arrow_rt
                        1, #razpurker_pratt_2008_triangle_arrow_acc
                        1, #richards_2012_tracking
                        0, #russell_driver_2005_exp1_acc
                        0, #russell_driver_2005_exp1_rt
                        0, #russell_driver_2005_exp2_acc
                        0, #russell_driver_2005_exp2_rt
                        0, #russell_driver_2005_exp3_acc
                        0, #russell_driver_2005_exp3_rt
                        0, #russell_driver_2005_exp4a_acc
                        0, #russell_driver_2005_exp4b_acc
                        0, #russell_driver_2005_exp5_acc
                        0, #russell_driver_2005_exp5_rt
                        1, #shafto_pitts_2015
                        1, #schnuerch_2016_exp1
                        1 #schnuerch_2016_exp2
                        )





N_participants_awareness <- c(20, #ariga_2007_exp2  
                              72, #beanland_pammer_2010_exp1A_fixating
                              72, #beanland_pammer_2010_exp1A_moving
                              50, #beanland_pammer_2010_exp2_slow_us
                              50, #beanland_pammer_2010_exp2_fast_us
                              30, #gabay_2012_exp1
                              23, #gabay_2012_exp2
                              43, #lo_yeh_2008_exp1_200ms
                              41, #lo_yeh_2008_exp1_500ms
                              23, #lo_yeh_2008_exp2_200ms
                              25, #lo_yeh_2008_exp2_500ms
                              NA, #mack_and_rock_2000_exp1,
                              NA, #mack_and_rock_2000_exp2,
                              NA, #mack_and_rock_2000_exp3,
                              NA, #mack_and_rock_2000_exp4,
                              NA, #mack_and_rock_2000_exp5,
                              20, #moore_egeth_1997_exp1
                              20, #moore_egeth_1997_exp3
                              16, #moore_2003_exp3
                              25, #moore_2004
                              370, #most_2005_exp1to7pooled
                              14, #razpurker_pratt_2008_columns_rows_rt
                              14, #razpurker_pratt_2008_columns_rows_acc
                              14, #razpurker_pratt_2008_triangle_arrow_rt
                              14, #razpurker_pratt_2008_triangle_arrow_acc
                              131, #richards_2012_tracking
                              25, #russell_driver_2005_exp1_acc
                              25, #russell_driver_2005_exp1_rt
                              28, #russell_driver_2005_exp2_acc
                              28, #russell_driver_2005_exp2_rt
                              24, #russell_driver_2005_exp3_acc
                              24, #russell_driver_2005_exp3_rt
                              20, #russell_driver_2005_exp4a_acc
                              21, #russell_driver_2005_exp4b_acc
                              24, #russell_driver_2005_exp5_acc
                              24, #russell_driver_2005_exp5_rt
                              30, #shafto_pitts_2015
                              61, #schnuerch_2016_exp1
                              61 #schnuerch_2016_exp2
                              )


significance <- c(0, #ariga_2007_exp2  
                  1, #beanland_pammer_2010_exp1A_fixating
                  1, #beanland_pammer_2010_exp1A_moving
                  0, #beanland_pammer_2010_exp2_slow_us
                  0, #beanland_pammer_2010_exp2_fast_us
                  1, #gabay_2012_exp1
                  1, #gabay_2012_exp2
                  0, #lo_yeh_2008_exp1_200ms
                  1, #lo_yeh_2008_exp1_500ms
                  0, #lo_yeh_2008_exp2_200ms
                  0, #lo_yeh_2008_exp2_500ms
                  1, #mack_and_rock_2000_exp1,
                  1, #mack_and_rock_2000_exp2,
                  1, #mack_and_rock_2000_exp3,
                  1, #mack_and_rock_2000_exp4,
                  1, #mack_and_rock_2000_exp5,
                  1, #moore_egeth_1997_exp1
                  1, #moore_egeth_1997_exp3
                  1, #moore_2003_exp3
                  0, #moore_2004
                  1, #most_2005_exp1to7pooled
                  1, #razpurker_pratt_2008_columns_rows_rt
                  1, #razpurker_pratt_2008_columns_rows_acc
                  1, #razpurker_pratt_2008_triangle_arrow_rt
                  1, #razpurker_pratt_2008_triangle_arrow_acc
                  NA, #richards_2012_tracking
                  1, #russell_driver_2005_exp1_acc
                  0, #russell_driver_2005_exp1_rt
                  1, #russell_driver_2005_exp2_acc
                  0, #russell_driver_2005_exp2_rt
                  1, #russell_driver_2005_exp3_acc
                  0, #russell_driver_2005_exp3_rt
                  1, #russell_driver_2005_exp4a_acc
                  1, #russell_driver_2005_exp4b_acc
                  0, #russell_driver_2005_exp5_acc
                  0, #russell_driver_2005_exp5_rt
                  1, #shafto_pitts_2015
                  1, #schnuerch_2016_exp1
                  1 #schnuerch_2016_exp2
                  )


es_table <- as_tibble(data.frame("study"=study_names,
                                 "N_per_group"=N_per_group,
                                 "us_relevance"=us_relevance,
                                 "implicit_type"=implicit_type,
                                 "implicit_measure"=implicit_measure,
                                 "N_trials_implicit"=N_trials_implicit,
                                 "N_participants_implicit"=N_participants_implicit,
                                 "awareness_objective"=awareness_objective,
                                 "us_presentation"=us_presentation,
                                 "us_delay_type"=us_delay_type,
                                 "us_assessment"=us_assessment,
                                 "N_trials_awareness"=N_trials_awareness,
                                 "N_participants_awareness"=N_participants_awareness,
                                 "significance"=significance))
