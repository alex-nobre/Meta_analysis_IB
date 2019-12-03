
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
                        "razpurker_pratt_2008_columns_rows", 
                        "razpurker_pratt_2008_columns_rows", 
                        "razpurker_pratt_2008_triangle_arrow",
                        "razpurker_pratt_2008_triangle_arrow", 
                        #"richards_2012_tracking", 
                        "russell_driver_2005_exp1", 
                        "russell_driver_2005_exp1", 
                        "russell_driver_2005_exp2",
                        "russell_driver_2005_exp2", 
                        "russell_driver_2005_exp3", 
                        "russell_driver_2005_exp3",
                        "russell_driver_2005_exp4a", 
                        "russell_driver_2005_exp4b", 
                        "russell_driver_2005_exp5",
                        "russell_driver_2005_exp5", 
                        #"shafto_pitts_2015",
                        "schnuerch_2016_exp1",
                        "schnuerch_2016_exp2",
                        "wood_simons_2019_exp1",
                        "wood_simons_2019_exp2",
                        "rashal_2017_exp1",
                        "rashal_2017_exp2",
                        "rashal_2017_exp3",
                        "rashal_2017_exp4",
                        "rashal_2017_exp4",
                        "rashal_2017_exp5",
                        "rashal_2017_exp5",
                        "rashal_2017_exp6",
                        "rashal_2017_exp6",
                        "kimchi_2004_exp_1_column_row_color",
                        "kimchi_2004_exp_1_triangle_arrow_color",
                        "kimchi_2004_exp_1_triangle_arrow",
                        "kimchi_2004_exp_2_square_cross_color",
                        "kimchi_2004_exp_2_square_cross",
                        "kimchi_2004_exp_2_square_cross",
                        "kimchi_2008_exp_1",
                        "kimchi_2008_exp_1",
                        "lamy_2006_exp2",
                        "lamy_2006_exp2",
                        "lamy_2006_exp3",
                        "lamy_2006_exp4",
                        "lamy_2006_exp5"
)

studies_per_outcome <- c("ariga_2007_exp2", 
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
                 #"richards_2012_tracking", 
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
                 #"shafto_pitts_2015",
                 "schnuerch_2016_exp1",
                 "schnuerch_2016_exp2",
                 "wood_simons_2019_exp1",
                 "wood_simons_2019_exp2",
                 "rashal_2017_exp1_RT",
                 "rashal_2017_exp2_acc",
                 "rashal_2017_exp3_RT",
                 "rashal_2017_exp4_acc",
                 "rashal_2017_exp4_RT",
                 "rashal_2017_exp5_acc",
                 "rashal_2017_exp5_RT",
                 "rashal_2017_exp6_acc",
                 "rashal_2017_exp6_RT",
                 "kimchi_2004_exp_1_column_row_color_RT",
                 "kimchi_2004_exp_1_triangle_arrow_color_acc",
                 "kimchi_2004_exp_1_triangle_arrow_acc",
                 "kimchi_2004_exp_2_square_cross_color_acc",
                 "kimchi_2004_exp_2_square_cross_RT",
                 "kimchi_2004_exp_2_square_cross_acc",
                 "kimchi_2008_exp_1_RT",
                 "kimchi_2008_exp_1_acc",
                 "lamy_2006_exp2_same",
                 "lamy_2006_exp2_different",
                 "lamy_2006_exp3_RT",
                 "lamy_2006_exp4_RT",
                 "lamy_2006_exp5"
)


#====== Did the us interact with the target stimulus? ======
us_relevance <- c("relevant", #ariga_2007_exp2  
                  "irrelevant", #beanland_pammer_2010_exp1A_fixating
                  "irrelevant", #beanland_pammer_2010_exp1A_moving
                  "irrelevant", #beanland_pammer_2010_exp2_slow_us
                  "irrelevant", #beanland_pammer_2010_exp2_fast_us
                  "relevant", #gabay_2012_exp1
                  "relevant", #gabay_2012_exp2
                  "relevant", #lo_yeh_2008_exp1_200ms
                  "relevant", #lo_yeh_2008_exp1_500ms
                  "irrelevant", #lo_yeh_2008_exp2_200ms
                  "irrelevant", #lo_yeh_2008_exp2_500ms
                  "irrelevant", #mack_and_rock_2000_exp1,
                  "irrelevant", #mack_and_rock_2000_exp2,
                  "irrelevant", #mack_and_rock_2000_exp3,
                  "irrelevant", #mack_and_rock_2000_exp4,
                  "irrelevant", #mack_and_rock_2000_exp5,
                  "relevant", #moore_egeth_1997_exp1
                  "relevant", #moore_egeth_1997_exp3
                  "relevant", #moore_2003_exp3
                  "irrelevant", #moore_2004
                  "irrelevant", #most_2005_exp1to7pooled
                  "irrelevant", #razpurker_pratt_2008_columns_rows_rt
                  "irrelevant", #razpurker_pratt_2008_columns_rows_acc
                  "irrelevant", #razpurker_pratt_2008_triangle_arrow_rt
                  "irrelevant", #razpurker_pratt_2008_triangle_arrow_acc
                  #"irrelevant", #richards_2012_tracking
                  "irrelevant", #russell_driver_2005_exp1_acc
                  "irrelevant", #russell_driver_2005_exp1_rt
                  "irrelevant", #russell_driver_2005_exp2_acc
                  "irrelevant", #russell_driver_2005_exp2_rt
                  "irrelevant", #russell_driver_2005_exp3_acc
                  "irrelevant", #russell_driver_2005_exp3_rt
                  "irrelevant", #russell_driver_2005_exp4a_acc
                  "irrelevant", #russell_driver_2005_exp4b_acc
                  "irrelevant", #russell_driver_2005_exp5_acc
                  "irrelevant", #russell_driver_2005_exp5_rt
                  #"irrelevant", #shafto_pitts_2015
                  "irrelevant", #schnuerch_2016_exp1
                  "irrelevant", #schnuerch_2016_exp2,
                  "relevant", #"wood_simons_2019_exp1"
                  "irrelevant", #"wood_simons_2019_exp2"
                  "irrelevant", #"rashal_2017_exp1_RT"
                  "irrelevant",  #"rashal_2017_exp2_acc"
                  "irrelevant",   #"rashal_2017_exp3_RT"
                  "irrelevant",  #"rashal_2017_exp4_acc"
                  "irrelevant",  #"rashal_2017_exp4_RT",
                  "irrelevant", #"rashal_2017_exp5_acc"
                  "irrelevant", #"rashal_2017_exp5_RT"
                  "irrelevant", #"rashal_2017_exp6_acc"
                  "irrelevant", #"rashal_2017_exp6_RT"
                  "irrelevant", #"kimchi_2004_exp_1_column_row_color_RT"
                  "irrelevant", #"kimchi_2004_exp_1_triangle_arrow_color_acc"
                  "irrelevant", #"kimchi_2004_exp_1_triangle_arrow_acc"
                  "irrelevant", #"kimchi_2004_exp_2_square_cross_color_acc"
                  "irrelevant", #"kimchi_2004_exp_2_square_cross_RT"
                  "irrelevant", #"kimchi_2004_exp_2_square_cross_acc"
                  "irrelevant", #"kimchi_2008_exp_1_RT"
                  "irrelevant", #"kimchi_2008_exp_1_acc"
                  "relevant", #"lamy_2006_exp2_same"
                  "relevant", #"lamy_2006_exp2_different"
                  "irrelevant", #"lamy_2006_exp3_RT"
                  "irrelevant", #"lamy_2006_exp4_RT"
                  "irrelevant" #"lamy_2006_exp5"
                  )


#======= Is implicit processing measured by RT or acc? ======
implicit_measure <- c("RT", #ariga_2007_exp2  
                      "accuracy", #beanland_pammer_2010_exp1A_fixating
                      "accuracy", #beanland_pammer_2010_exp1A_moving
                      "accuracy", #beanland_pammer_2010_exp2_slow_us
                      "accuracy", #beanland_pammer_2010_exp2_fast_us
                      "RT", #gabay_2012_exp1
                      "RT", #gabay_2012_exp2
                      "accuracy", #lo_yeh_2008_exp1_200ms
                      "accuracy", #lo_yeh_2008_exp1_500ms
                      "RT", #lo_yeh_2008_exp2_200ms
                      "RT", #lo_yeh_2008_exp2_500ms
                      "accuracy", #mack_and_rock_2000_exp1,
                      "accuracy", #mack_and_rock_2000_exp2,
                      "accuracy", #mack_and_rock_2000_exp3,
                      "accuracy", #mack_and_rock_2000_exp4,
                      "accuracy", #mack_and_rock_2000_exp5,
                      "accuracy", #moore_egeth_1997_exp1
                      "accuracy", #moore_egeth_1997_exp3
                      "RT", #moore_2003_exp3
                      "RT", #moore_2004
                      "accuracy", #most_2005_exp1to7pooled
                      "RT", #razpurker_pratt_2008_columns_rows_rt
                      "accuracy", #razpurker_pratt_2008_columns_rows_acc
                      "RT", #razpurker_pratt_2008_triangle_arrow_rt
                      "accuracy", #razpurker_pratt_2008_triangle_arrow_acc
                      #"accuracy", #richards_2012_tracking
                      "accuracy", #russell_driver_2005_exp1_acc
                      "RT", #russell_driver_2005_exp1_rt
                      "accuracy", #russell_driver_2005_exp2_acc
                      "RT", #russell_driver_2005_exp2_rt
                      "accuracy", #russell_driver_2005_exp3_acc
                      "RT", #russell_driver_2005_exp3_rt
                      "accuracy", #russell_driver_2005_exp4a_acc
                      "accuracy", #russell_driver_2005_exp4b_acc
                      "accuracy", #russell_driver_2005_exp5_acc
                      "RT", #russell_driver_2005_exp5_rt
                      #NA, #shafto_pitts_2015
                      "RT", #schnuerch_2016_exp1
                      "RT", #schnuerch_2016_exp2,
                      "accuracy", #"wood_simons_2019_exp1"
                      "accuracy", #"wood_simons_2019_exp2"
                      "RT", #"rashal_2017_exp1_RT"
                      "accuracy", #"rashal_2017_exp2_acc"
                      "RT",   #"rashal_2017_exp3_RT"
                      "accuracy",  #"rashal_2017_exp4_acc"
                      "RT",  #"rashal_2017_exp4_RT",
                      "accuracy", #"rashal_2017_exp5_acc"
                      "RT", #"rashal_2017_exp5_RT"
                      "accuracy", #"rashal_2017_exp6_acc"
                      "RT", #"rashal_2017_exp6_RT"
                      "RT", #"kimchi_2004_exp_1_column_row_color_RT"
                      "accuracy", #"kimchi_2004_exp_1_triangle_arrow_color_acc"
                      "accuracy", #"kimchi_2004_exp_1_triangle_arrow_acc"
                      "accuracy", #"kimchi_2004_exp_2_square_cross_color_acc"
                      "RT", #"kimchi_2004_exp_2_square_cross_RT"
                      "accuracy", #"kimchi_2004_exp_2_square_cross_acc"
                      "RT", #"kimchi_2008_exp_1_RT"
                      "accuracy", #"kimchi_2008_exp_1_acc"
                      "accuracy", #"lamy_2006_exp2_same"
                      "accuracy", #"lamy_2006_exp2_different"
                      "RT", #"lamy_2006_exp3_RT"
                      "RT", #"lamy_2006_exp4_RT"
                      "RT" #"lamy_2006_exp5"
                      )

#=========== How many trials were used to assess implicit processing? =============
N_trials_implicit <- c(1, #ariga_2007_exp2  
                       2, #beanland_pammer_2010_exp1A_fixating
                       2, #beanland_pammer_2010_exp1A_moving
                       2, #beanland_pammer_2010_exp2_slow_us
                       2, #beanland_pammer_2010_exp2_fast_us
                       72, #gabay_2012_exp1
                       72, #gabay_2012_exp2
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
                       #1, #richards_2012_tracking
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
                       #216, #shafto_pitts_2015
                       150, #schnuerch_2016_exp1
                       192, #schnuerch_2016_exp2
                       1, #"wood_simons_2019_exp1"
                       1, #"wood_simons_2019_exp2"
                       160, #"rashal_2017_exp1_RT"
                       160,  #"rashal_2017_exp2_acc"
                       160,   #"rashal_2017_exp3_RT"
                       160,  #"rashal_2017_exp4_acc"
                       160,  #"rashal_2017_exp4_RT",
                       160, #"rashal_2017_exp5_acc"
                       160, #"rashal_2017_exp5_RT"
                       160, #"rashal_2017_exp6_acc"
                       160, #"rashal_2017_exp6_RT"
                       160, #"kimchi_2004_exp_1_column_row_color_RT"
                       160, #"kimchi_2004_exp_1_triangle_arrow_color_acc"
                       160, #"kimchi_2004_exp_1_triangle_arrow_acc"
                       160, #"kimchi_2004_exp_2_square_cross_color_acc"
                       160, #"kimchi_2004_exp_2_square_cross_RT"
                       160, #"kimchi_2004_exp_2_square_cross_acc"
                       160, #"kimchi_2008_exp_1_RT"
                       160, #"kimchi_2008_exp_1_acc"
                       16, #"lamy_2006_exp2_same"
                       16, #"lamy_2006_exp2_different"
                       180, #"lamy_2006_exp3_RT"
                       180, #"lamy_2006_exp4_RT"
                       180 #"lamy_2006_exp5"
                       )

#======== How many subjects in the sample for implicit processing testing? ===========
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
                            50, #mack_and_rock_2000_exp1,
                            41, #mack_and_rock_2000_exp2,
                            21, #mack_and_rock_2000_exp3,
                            29, #mack_and_rock_2000_exp4,
                            19, #mack_and_rock_2000_exp5,
                            20, #moore_egeth_1997_exp1
                            20, #moore_egeth_1997_exp3
                            16, #moore_2003_exp3
                            25, #moore_2004
                            186, #most_2005_exp1to7pooled
                            14, #razpurker_pratt_2008_columns_rows_rt
                            14, #razpurker_pratt_2008_columns_rows_acc
                            14, #razpurker_pratt_2008_triangle_arrow_rt
                            14, #razpurker_pratt_2008_triangle_arrow_acc
                            #131, #richards_2012_tracking
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
                            #15, #shafto_pitts_2015
                            61, #schnuerch_2016_exp1
                            58, #schnuerch_2016_exp2
                            175, #"wood_simons_2019_exp1"
                            216, #"wood_simons_2019_exp2"
                            20, #"rashal_2017_exp1_RT"
                            28,  #"rashal_2017_exp2_acc"
                            18,  #"rashal_2017_exp3_RT"
                            15,  #"rashal_2017_exp4_acc"
                            15,  #"rashal_2017_exp4_RT",
                            18, #"rashal_2017_exp5_acc"
                            18, #"rashal_2017_exp5_RT"
                            18, #"rashal_2017_exp6_acc"
                            18, #"rashal_2017_exp6_RT"
                            14, #"kimchi_2004_exp_1_column_row_color_RT"
                            14, #"kimchi_2004_exp_1_triangle_arrow_color_acc"
                            14, #"kimchi_2004_exp_1_triangle_arrow_acc"
                            12, #"kimchi_2004_exp_2_square_cross_color_acc"
                            12, #"kimchi_2004_exp_2_square_cross_RT"
                            12, #"kimchi_2004_exp_2_square_cross_acc"
                            46, #"kimchi_2008_exp_1_RT"
                            46, #"kimchi_2008_exp_1_acc"
                            8, #"lamy_2006_exp2_same"
                            8, #"lamy_2006_exp2_different"
                            9, #"lamy_2006_exp3_RT"
                            9, #"lamy_2006_exp4_RT"
                            11 #"lamy_2006_exp5"
                            )




#======= In how many trials was awareness assessed (how many assessments)? ========
N_trials_awareness <- c(1, #ariga_2007_exp2  
                        1, #beanland_pammer_2010_exp1A_fixating
                        1, #beanland_pammer_2010_exp1A_moving
                        1, #beanland_pammer_2010_exp2_slow_us
                        1, #beanland_pammer_2010_exp2_fast_us
                        40, #gabay_2012_exp1
                        40, #gabay_2012_exp2
                        1, #lo_yeh_2008_exp1_200ms
                        1, #lo_yeh_2008_exp1_500ms
                        1, #lo_yeh_2008_exp2_200ms
                        1, #lo_yeh_2008_exp2_500ms
                        1, #mack_and_rock_2000_exp1,
                        1, #mack_and_rock_2000_exp2,
                        1, #mack_and_rock_2000_exp3,
                        1, #mack_and_rock_2000_exp4,
                        1, #mack_and_rock_2000_exp5,
                        1, #moore_egeth_1997_exp1
                        1, #moore_egeth_1997_exp3
                        1, #moore_2003_exp3
                        1, #moore_2004
                        1, #most_2005_exp1to7pooled
                        1, #razpurker_pratt_2008_columns_rows_rt
                        1, #razpurker_pratt_2008_columns_rows_acc
                        1, #razpurker_pratt_2008_triangle_arrow_rt
                        1, #razpurker_pratt_2008_triangle_arrow_acc
                        #1, #richards_2012_tracking
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
                        #1, #shafto_pitts_2015
                        1, #schnuerch_2016_exp1
                        1, #schnuerch_2016_exp2
                        1, #"wood_simons_2019_exp1"
                        1, #"wood_simons_2019_exp2"
                        1, #"rashal_2017_exp1_RT"
                        1, #"rashal_2017_exp2_acc"
                        1, #"rashal_2017_exp3_RT"
                        1,  #"rashal_2017_exp4_acc"
                        1,  #"rashal_2017_exp4_RT",
                        1, #"rashal_2017_exp5_acc"
                        1, #"rashal_2017_exp5_RT"
                        1, #"rashal_2017_exp6_acc"
                        1, #"rashal_2017_exp6_RT"
                        1, #"kimchi_2004_exp_1_column_row_color_RT"
                        1, #"kimchi_2004_exp_1_triangle_arrow_color_acc"
                        1, #"kimchi_2004_exp_1_triangle_arrow_acc
                        1, #"kimchi_2004_exp_2_square_cross_color_acc"
                        1, #"kimchi_2004_exp_2_square_cross_RT"
                        1, #"kimchi_2004_exp_2_square_cross_acc"
                        1, #"kimchi_2008_exp_1_RT"
                        1, #"kimchi_2008_exp_1_acc"
                        1, #"lamy_2006_exp2_same"
                        1, #"lamy_2006_exp2_different"
                        1, #"lamy_2006_exp3_RT"
                        1, #"lamy_2006_exp4_RT"
                        1 #"lamy_2006_exp5"
                        )

#===== How many subjects in the sample for awareness testing? ======
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
                              50, #mack_and_rock_2000_exp1,
                              41, #mack_and_rock_2000_exp2,
                              21, #mack_and_rock_2000_exp3,
                              29, #mack_and_rock_2000_exp4,
                              19, #mack_and_rock_2000_exp5,
                              20, #moore_egeth_1997_exp1
                              20, #moore_egeth_1997_exp3
                              16, #moore_2003_exp3
                              25, #moore_2004
                              370, #most_2005_exp1to7pooled
                              14, #razpurker_pratt_2008_columns_rows_rt
                              14, #razpurker_pratt_2008_columns_rows_acc
                              14, #razpurker_pratt_2008_triangle_arrow_rt
                              14, #razpurker_pratt_2008_triangle_arrow_acc
                              #131, #richards_2012_tracking
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
                              #30, #shafto_pitts_2015
                              61, #schnuerch_2016_exp1
                              61, #schnuerch_2016_exp2
                              175, #"wood_simons_2019_exp1"
                              216, #"wood_simons_2019_exp2"
                              20,  #"rashal_2017_exp1_RT"
                              28,  #"rashal_2017_exp2_acc"
                              18, #"rashal_2017_exp3_RT"
                              15,  #"rashal_2017_exp4_acc"
                              15,  #"rashal_2017_exp4_RT",
                              18,  #"rashal_2017_exp5_acc"
                              18, #"rashal_2017_exp5_RT"
                              18, #"rashal_2017_exp6_acc"
                              18, #"rashal_2017_exp6_RT"
                              14, #"kimchi_2004_exp_1_column_row_color_RT"
                              14, #"kimchi_2004_exp_1_triangle_arrow_color_acc"
                              14, #"kimchi_2004_exp_1_triangle_arrow_acc
                              12, #"kimchi_2004_exp_2_square_cross_color_acc"
                              12, #"kimchi_2004_exp_2_square_cross_RT"
                              12, #"kimchi_2004_exp_2_square_cross_acc"
                              46, #"kimchi_2008_exp_1_RT"
                              46, #"kimchi_2008_exp_1_acc"
                              8, #"lamy_2006_exp2_same"
                              8, #"lamy_2006_exp2_different"
                              9, #"lamy_2006_exp3_RT"
                              9, #"lamy_2006_exp4_RT"
                              11 #"lamy_2006_exp5"
                              )

#====== Was the result for the implicit test significant? =======
implicit_significance <- c("no", #ariga_2007_exp2  
                  "yes", #beanland_pammer_2010_exp1A_fixating
                  "yes", #beanland_pammer_2010_exp1A_moving
                  "no", #beanland_pammer_2010_exp2_slow_us
                  "no", #beanland_pammer_2010_exp2_fast_us
                  "yes", #gabay_2012_exp1
                  "yes", #gabay_2012_exp2
                  "no", #lo_yeh_2008_exp1_200ms
                  "yes", #lo_yeh_2008_exp1_500ms
                  "no", #lo_yeh_2008_exp2_200ms
                  "no", #lo_yeh_2008_exp2_500ms
                  "yes", #mack_and_rock_2000_exp1,
                  "yes", #mack_and_rock_2000_exp2,
                  "yes", #mack_and_rock_2000_exp3,
                  "yes", #mack_and_rock_2000_exp4,
                  "yes", #mack_and_rock_2000_exp5,
                  "yes", #moore_egeth_1997_exp1
                  "yes", #moore_egeth_1997_exp3
                  "yes", #moore_2003_exp3
                  "no", #moore_2004
                  "yes", #most_2005_exp1to7pooled
                  "yes", #razpurker_pratt_2008_columns_rows_rt
                  "yes", #razpurker_pratt_2008_columns_rows_acc
                  "yes", #razpurker_pratt_2008_triangle_arrow_rt
                  "yes", #razpurker_pratt_2008_triangle_arrow_acc
                  #"no", #richards_2012_tracking
                  "yes", #russell_driver_2005_exp1_acc
                  "no", #russell_driver_2005_exp1_rt
                  "yes", #russell_driver_2005_exp2_acc
                  "no", #russell_driver_2005_exp2_rt
                  "yes", #russell_driver_2005_exp3_acc
                  "no", #russell_driver_2005_exp3_rt
                  "yes", #russell_driver_2005_exp4a_acc
                  "yes", #russell_driver_2005_exp4b_acc
                  "no", #russell_driver_2005_exp5_acc
                  "no", #russell_driver_2005_exp5_rt
                  #"yes", #shafto_pitts_2015
                  "yes", #schnuerch_2016_exp1
                  "yes", #schnuerch_2016_exp2
                  "yes", #"wood_simons_2019_exp1"
                  "no",  #"wood_simons_2019_exp2"
                  "yes", #"rashal_2017_exp1_RT"
                  "no",  #"rashal_2017_exp2_acc"
                  "no",   #"rashal_2017_exp3_RT"
                  "yes",  #"rashal_2017_exp4_acc"
                  "yes",  #"rashal_2017_exp4_RT",
                  "yes",  #"rashal_2017_exp5_acc"
                  "no", #"rashal_2017_exp5_RT"
                  "no", #"rashal_2017_exp6_acc"
                  "no", #"rashal_2017_exp6_RT"
                  "yes", #"kimchi_2004_exp_1_column_row_color_RT"
                  "no", #"kimchi_2004_exp_1_triangle_arrow_color_acc"
                  "no", #"kimchi_2004_exp_1_triangle_arrow_acc"
                  "no", #"kimchi_2004_exp_2_square_cross_color_acc"
                  "no", #"kimchi_2004_exp_2_square_cross_RT"
                  "no", #"kimchi_2004_exp_2_square_cross_acc"
                  "yes", #"kimchi_2008_exp_1_RT"
                  "yes", #"kimchi_2008_exp_1_acc"
                  "yes", #"lamy_2006_exp2_same"
                  "yes", #"lamy_2006_exp2_different"
                  "yes", #"lamy_2006_exp3_RT"
                  "yes", #"lamy_2006_exp4_RT"
                  "yes" #"lamy_2006_exp5"
                  )

#====== Gray literature =======
gray_literature <- c("no", #ariga_2007_exp2  
                     "no", #beanland_pammer_2010_exp1A_fixating
                     "no", #beanland_pammer_2010_exp1A_moving
                     "no", #beanland_pammer_2010_exp2_slow_us
                     "no", #beanland_pammer_2010_exp2_fast_us
                     "no", #gabay_2012_exp1
                     "no", #gabay_2012_exp2
                     "no", #lo_yeh_2008_exp1_200ms
                     "no", #lo_yeh_2008_exp1_500ms
                     "no", #lo_yeh_2008_exp2_200ms
                     "no", #lo_yeh_2008_exp2_500ms
                     "yes", #mack_and_rock_2000_exp1,
                     "yes", #mack_and_rock_2000_exp2,
                     "yes", #mack_and_rock_2000_exp3,
                     "yes", #mack_and_rock_2000_exp4,
                     "yes", #mack_and_rock_2000_exp5,
                     "no", #moore_egeth_1997_exp1
                     "no", #moore_egeth_1997_exp3
                     "no", #moore_2003_exp3
                     "no", #moore_2004
                     "no", #most_2005_exp1to7pooled
                     "no", #razpurker_pratt_2008_columns_rows_rt
                     "no", #razpurker_pratt_2008_columns_rows_acc
                     "no", #razpurker_pratt_2008_triangle_arrow_rt
                     "no", #razpurker_pratt_2008_triangle_arrow_acc
                     #"no", #richards_2012_tracking
                     "no", #russell_driver_2005_exp1_acc
                     "no", #russell_driver_2005_exp1_rt
                     "no", #russell_driver_2005_exp2_acc
                     "no", #russell_driver_2005_exp2_rt
                     "no", #russell_driver_2005_exp3_acc
                     "no", #russell_driver_2005_exp3_rt
                     "no", #russell_driver_2005_exp4a_acc
                     "no", #russell_driver_2005_exp4b_acc
                     "no", #russell_driver_2005_exp5_acc
                     "no", #russell_driver_2005_exp5_rt
                     #"no", #shafto_pitts_2015
                     "no", #schnuerch_2016_exp1
                     "no", #schnuerch_2016_exp2
                     "no", #"wood_simons_2019_exp1"
                     "no",  #"wood_simons_2019_exp2"
                     "no",  #"rashal_2017_exp1_RT"
                     "no",  #"rashal_2017_exp2_acc"
                     "no",   #"rashal_2017_exp3_RT"
                     "no",  #"rashal_2017_exp4_acc"
                     "no",  #"rashal_2017_exp4_RT",
                     "no",  #"rashal_2017_exp5_acc"
                     "no", #"rashal_2017_exp5_RT"
                     "no", #"rashal_2017_exp6_acc"
                     "no", #"rashal_2017_exp6_RT"
                     "no", #"kimchi_2004_exp_1_column_row_color_RT"
                     "no", #"kimchi_2004_exp_1_triangle_arrow_color_acc"
                     "no", #"kimchi_2004_exp_1_triangle_arrow_acc"
                     "no", #"kimchi_2004_exp_2_square_cross_color_acc"
                     "no", #"kimchi_2004_exp_2_square_cross_RT"
                     "no", #"kimchi_2004_exp_2_square_cross_acc"
                     "no", #"kimchi_2008_exp_1_RT"
                     "no", #"kimchi_2008_exp_1_acc"
                     "no", #"lamy_2006_exp2_same"
                     "no", #"lamy_2006_exp2_different"
                     "no", #"lamy_2006_exp3_RT"
                     "no", #"lamy_2006_exp4_RT"
                     "no" #"lamy_2006_exp5"
                     )

#======== Categorize experiments as inattention paradigms or not; 0 = no, 1 = yes ========
inattention_paradigm <- c(rep("no", 21), #ariga_2007_exp2 to most_2005_exp1to7pooled
                                   rep("yes",4), #razpurker-apfeld and pratt, 2008
                                   #0, #richards_2012_tracking
                                   rep("yes", 10), #russel_driver_2005
                                   rep("no",2), #schunerch_2016
                                   rep("no", 2), #wood_simons_2019
                                   rep("yes", 17), #rashal_2017, kimchi_2008
                          rep("no", 5) #"lamy_2006"
                          )

#======== Categorize experiments as group assessment of awareness or not ========
group_aware_assess <- c("yes", #ariga_2007_exp2
                                 rep("no", 6), #beanland_pammer_2010_exp1A_fixating to gabay_2012_exp2
                                 rep("yes",4), #lo_yeh_2008_exp1_200ms
                                 rep("no", 5), #mack_and_rock_2000
                                 rep("yes",4), # moore_egeth_1997_exp1 to moore_2004  
                                 rep("no",1), #most_2005_exp1to7pooled
                                 rep("yes", 4), #razpurker_pratt_2008
                                 #0, #richards_2012_tracking
                                 rep("yes", 10), #russsel_driver_2005
                                 rep("no",2), #schunerch_2016
                                 rep("no", 2), #wood_simons_2019
                                 rep("yes", 22) #rashal_2017, kimchi_2004, kimchi_2008,lamy_2006 
                        )

#============= Create table by merging columns ============
es_table <- as_tibble(data.frame("study"=study_names,
                                 "studies_per_outcome"=studies_per_outcome,
                                 "us_relevance"=us_relevance,
                                 "implicit_measure"=implicit_measure,
                                 "N_trials_implicit"=N_trials_implicit,
                                 "N_participants_implicit"=N_participants_implicit,
                                 "N_trials_awareness"=N_trials_awareness,
                                 "N_participants_awareness"=N_participants_awareness,
                                 "implicit_significance"=implicit_significance,
                                 "gray_literature"=gray_literature,
                                 "inattention"=inattention_paradigm,
                                 "group_awareness"=group_aware_assess))
es_table$study <- as.character(es_table$study)
