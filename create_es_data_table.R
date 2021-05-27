
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
                 "kimchi_2004_exp_1_connected_triangle_arrow",
                 "kimchi_2004_exp_1_connected_triangle_arrow",
                 "kimchi_2004_exp_2_square_cross_color",
                 "kimchi_2004_exp_2_square_cross",
                 "kimchi_2004_exp_2_square_cross",
                 "kimchi_2004_exp_2_disconnected_square_cross",
                 "kimchi_2004_exp_2_disconnected_square_cross",
                 "kimchi_2008_exp_1",
                 "kimchi_2008_exp_1",
                 "lamy_2006_exp2",
                 "lamy_2006_exp2",
                 "lamy_2006_exp3",
                 "lamy_2006_exp4",
                 "lamy_2006_exp5",
                 "pugnaghi_2020_exp1",
                 "pugnaghi_2020_exp1",
                 "pugnaghi_2020_exp2",
                 "pugnaghi_2020_exp2",
                 "nobre_2020",
                 "pugnaghi_2019",
                 "kreitz_2020_dataset1",
                 #"kreitz_2020_dataset2",
                 "kreitz_2020_dataset3",
                 "kreitz_2020_dataset4",
                 "kreitz_2020_dataset5",
                 "kreitz_2020_dataset6",
                 "kreitz_2020_dataset7",
                 "kreitz_2020_dataset8",
                 "kreitz_2020_dataset9",
                 "kreitz_2020_dataset10",
                 "kreitz_2020_dataset13",
                 "kreitz_2020_dataset14",
                 "kreitz_2020_dataset15",
                 "kreitz_2020_dataset16",
                 "kreitz_2020_dataset2",
                 "kreitz_2020_dataset2"
)


studies_outcomes <- c("ariga_2007_exp2", 
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
                 "russell_driver_2005_exp1_acc", 
                 "russell_driver_2005_exp1_rt", 
                 "russell_driver_2005_exp2_acc",
                 "russell_driver_2005_exp2_rt", 
                 "russell_driver_2005_exp3_acc", 
                 "russell_driver_2005_exp3_rt",
                 "russell_driver_2005_exp4a", 
                 "russell_driver_2005_exp4b", 
                 "russell_driver_2005_exp5_acc",
                 "russell_driver_2005_exp5_rt", 
                 "schnuerch_2016_exp1",
                 "schnuerch_2016_exp2",
                 "wood_simons_2019_exp1",
                 "wood_simons_2019_exp2",
                 "rashal_2017_exp1",
                 "rashal_2017_exp2",
                 "rashal_2017_exp3",
                 "rashal_2017_exp4_acc",
                 "rashal_2017_exp4_RT",
                 "rashal_2017_exp5_acc",
                 "rashal_2017_exp5_RT",
                 "rashal_2017_exp6_acc",
                 "rashal_2017_exp6_RT",
                 "kimchi_2004_exp_1_column_row_color",
                 "kimchi_2004_exp_1_triangle_arrow_color",
                 "kimchi_2004_exp_1_triangle_arrow",
                 "kimchi_2004_exp_1_connected_triangle_arrow_RT",
                 "kimchi_2004_exp_1_connected_triangle_arrow_acc",
                 "kimchi_2004_exp_2_square_cross_color",
                 "kimchi_2004_exp_2_square_cross_RT",
                 "kimchi_2004_exp_2_square_cross_acc",
                 "kimchi_2004_exp_2_disconnected_square_cross_RT",
                 "kimchi_2004_exp_2_disconnected_square_cross_acc",
                 "kimchi_2008_exp_1_RT",
                 "kimchi_2008_exp_1_acc",
                 "lamy_2006_exp2_same",
                 "lamy_2006_exp2_different",
                 "lamy_2006_exp3",
                 "lamy_2006_exp4",
                 "lamy_2006_exp5",
                 "pugnaghi_2020_exp1_RT",
                 "pugnaghi_2020_exp1_acc",
                 "pugnaghi_2020_exp2_RT",
                 "pugnaghi_2020_exp2_acc",
                 "nobre_2020",
                 "pugnaghi_2019",
                 "kreitz_2020_dataset1",
                 #"kreitz_2020_dataset2",
                 "kreitz_2020_dataset3",
                 "kreitz_2020_dataset4",
                 "kreitz_2020_dataset5",
                 "kreitz_2020_dataset6",
                 "kreitz_2020_dataset7",
                 "kreitz_2020_dataset8",
                 "kreitz_2020_dataset9",
                 "kreitz_2020_dataset10",
                 "kreitz_2020_dataset13",
                 "kreitz_2020_dataset14",
                 "kreitz_2020_dataset15",
                 "kreitz_2020_dataset16",
                 "kreitz_2020_dataset2.1",
                 "kreitz_2020_dataset2.2"
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
                  "irrelevant", #"kimchi_2004_exp_1_connected_triangle_arrow_RT",
                  "irrelevant", #"kimchi_2004_exp_1_connected_triangle_arrow_acc",
                  "irrelevant", #"kimchi_2004_exp_2_square_cross_color_acc"
                  "irrelevant", #"kimchi_2004_exp_2_square_cross_RT"
                  "irrelevant", #"kimchi_2004_exp_2_square_cross_acc"
                  "irrelevant", #"kimchi_2004_exp_2_disconnected_square_cross_RT",
                  "irrelevant", #"kimchi_2004_exp_2_disconnected_square_cross_acc",
                  "irrelevant", #"kimchi_2008_exp_1_RT"
                  "irrelevant", #"kimchi_2008_exp_1_acc"
                  "relevant", #"lamy_2006_exp2_same"
                  "relevant", #"lamy_2006_exp2_different"
                  "irrelevant", #"lamy_2006_exp3_RT"
                  "irrelevant", #"lamy_2006_exp4_RT"
                  "irrelevant", #"lamy_2006_exp5"
                  "irrelevant", #"pugnaghi_2020_exp1_RT"
                  "irrelevant", #"pugnaghi_2020_exp1_acc"
                  "irrelevant", #"pugnaghi_2020_exp2_RT"
                  "irrelevant", #"pugnaghi_2020_exp2_acc"
                  "irrelevant", #"nobre_2020"
                  "irrelevant", #"pugnaghi_2019"
                  "irrelevant", #"kreitz_2020_dataset1"
                  #"irrelevant", #"kreitz_2020_dataset2"
                  "irrelevant", #"kreitz_2020_dataset3"
                  "irrelevant", #"kreitz_2020_dataset4"
                  "irrelevant", #"kreitz_2020_dataset5"
                  "irrelevant", #"kreitz_2020_dataset6"
                  "irrelevant", #"kreitz_2020_dataset7"
                  "irrelevant", #"kreitz_2020_dataset8"
                  "irrelevant", #"kreitz_2020_dataset9"
                  "irrelevant", #"kreitz_2020_dataset10"
                  "irrelevant", #"kreitz_2020_dataset13"
                  "irrelevant", #"kreitz_2020_dataset14"
                  "irrelevant", #"kreitz_2020_dataset15"
                  "irrelevant", #"kreitz_2020_dataset16",
                  "irrelevant", #"kreitz_2020_dataset2.1",
                  "irrelevant" #"kreitz_2020_dataset2.2"
                  )


#==== Gestalt study ====
gestalt <- c("yes", #"ariga_2007_exp2"
             "no", #"beanland_pammer_2010_exp1A_fixating"
             "no", #"beanland_pammer_2010_exp1A_moving"
             "no", #"beanland_pammer_2010_exp2_slow_us" 
             "no", #"beanland_pammer_2010_exp2_fast_us"
             "no", #"gabay_2012_exp1"
             "no", #"gabay_2012_exp2"
             "yes", #"lo_yeh_2008_exp1_200ms"
             "yes", #"lo_yeh_2008_exp1_500ms"
             "no", #"lo_yeh_2008_exp2_200ms"
             "no", #"lo_yeh_2008_exp2_500ms"
             "no", #"mack_and_rock_2000_exp1"
             "no", #"mack_and_rock_2000_exp2"
             "no", #"mack_and_rock_2000_exp3"
             "no", #"mack_and_rock_2000_exp4"
             "no", #"mack_and_rock_2000_exp5"
             "yes", #"moore_egeth_1997_exp1"
             "yes", #"moore_egeth_1997_exp3"
             "yes", #moore_2003_exp3
             "no", #moore_2004
             "no", #most_2005_exp1to7pooled
             "yes", #razpurker_pratt_2008_columns_rows_rt
             "yes", #razpurker_pratt_2008_columns_rows_acc
             "yes", #razpurker_pratt_2008_triangle_arrow_rt
             "yes", #razpurker_pratt_2008_triangle_arrow_acc
             "yes", #russell_driver_2005_exp1_acc
             "yes", #russell_driver_2005_exp1_rt
             "yes", #russell_driver_2005_exp2_acc
             "yes", #russell_driver_2005_exp2_rt
             "yes", #russell_driver_2005_exp3_acc
             "yes", #russell_driver_2005_exp3_rt
             "yes", #russell_driver_2005_exp4a_acc
             "yes", #russell_driver_2005_exp4b_acc
             "yes", #russell_driver_2005_exp5_acc
             "yes", #russell_driver_2005_exp5_rt
             "no", #schnuerch_2016_exp1
             "no", #schnuerch_2016_exp2,
             "yes", #"wood_simons_2019_exp1"
             "no", #"wood_simons_2019_exp2"
             "yes", #"rashal_2017_exp1_RT"
             "yes",  #"rashal_2017_exp2_acc"
             "yes",   #"rashal_2017_exp3_RT"
             "yes",  #"rashal_2017_exp4_acc"
             "yes",  #"rashal_2017_exp4_RT",
             "yes", #"rashal_2017_exp5_acc"
             "yes", #"rashal_2017_exp5_RT"
             "yes", #"rashal_2017_exp6_acc"
             "yes", #"rashal_2017_exp6_RT"
             "yes", #"kimchi_2004_exp_1_column_row_color_RT"
             "yes", #"kimchi_2004_exp_1_triangle_arrow_color_acc"
             "yes", #"kimchi_2004_exp_1_triangle_arrow_acc"
             "no", #"kimchi_2004_exp_1_connected_triangle_arrow_RT",
             "no", #"kimchi_2004_exp_1_connected_triangle_arrow_acc",
             "yes", #"kimchi_2004_exp_2_square_cross_color_acc"
             "yes", #"kimchi_2004_exp_2_square_cross_RT"
             "yes", #"kimchi_2004_exp_2_square_cross_acc"
             "yes", #"kimchi_2004_exp_2_disconnected_square_cross_RT",
             "yes", #"kimchi_2004_exp_2_disconnected_square_cross_acc",
             "yes", #"kimchi_2008_exp_1_RT"
             "yes", #"kimchi_2008_exp_1_acc"
             "yes", #"lamy_2006_exp2_same"
             "yes", #"lamy_2006_exp2_different"
             "yes", #"lamy_2006_exp3_RT"
             "yes", #"lamy_2006_exp4_RT"
             "no", #"lamy_2006_exp5"
             "no", #"pugnaghi_2020_exp1_RT",
             "no", #"pugnaghi_2020_exp1_acc",
             "no", #"pugnaghi_2020_exp2_RT",
             "no", #"pugnaghi_2020_exp2_acc",
             "yes", #"nobre_2020",
             "no", #"pugnaghi_2019",
             "no", #"kreitz_2020_dataset1",
             #"no", #"kreitz_2020_dataset2",
             "no", #"kreitz_2020_dataset3",
             "no", #"kreitz_2020_dataset4",
             "no", #"kreitz_2020_dataset5",
             "no", #"kreitz_2020_dataset6",
             "no", #"kreitz_2020_dataset7",
             "no", #"kreitz_2020_dataset8",
             "no", #"kreitz_2020_dataset9",
             "no", #"kreitz_2020_dataset10",
             "no", #"kreitz_2020_dataset13",
             "no", #"kreitz_2020_dataset14",
             "no", #"kreitz_2020_dataset15",
             "no", #"kreitz_2020_dataset16"
             "no", #"kreitz_2020_dataset2.1"
             "no" #"kreitz_2020_dataset2.2"
)

#======= Implicit measure ======
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
                      "RT", #"kimchi_2004_exp_1_connected_triangle_arrow_RT",
                      "accuracy", #"kimchi_2004_exp_1_connected_triangle_arrow_acc",
                      "accuracy", #"kimchi_2004_exp_2_square_cross_color_acc"
                      "RT", #"kimchi_2004_exp_2_square_cross_RT"
                      "accuracy", #"kimchi_2004_exp_2_square_cross_acc"
                      "RT", #"kimchi_2004_exp_2_disconnected_square_cross_RT",
                      "accuracy", #"kimchi_2004_exp_2_disconnected_square_cross_acc",
                      "RT", #"kimchi_2008_exp_1_RT"
                      "accuracy", #"kimchi_2008_exp_1_acc"
                      "accuracy", #"lamy_2006_exp2_same"
                      "accuracy", #"lamy_2006_exp2_different"
                      "RT", #"lamy_2006_exp3_RT"
                      "RT", #"lamy_2006_exp4_RT"
                      "RT", #"lamy_2006_exp5"
                      "RT", #"pugnaghi_2020_exp1_RT"
                      "accuracy", #"pugnaghi_2020_exp1_acc"
                      "RT", #"pugnaghi_2020_exp2_RT"
                      "accuracy", #"pugnaghi_2020_exp2_acc"
                      "RT", #"nobre_2020"
                      "RT", #"pugnaghi_2019"
                      "accuracy", #"kreitz_2020_dataset1"
                      #"accuracy", #"kreitz_2020_dataset2"
                      "accuracy", #"kreitz_2020_dataset3"
                      "accuracy", #"kreitz_2020_dataset4"
                      "accuracy", #"kreitz_2020_dataset5"
                      "accuracy", #"kreitz_2020_dataset6"
                      "accuracy", #"kreitz_2020_dataset7"
                      "accuracy", #"kreitz_2020_dataset8"
                      "accuracy", #"kreitz_2020_dataset9"
                      "accuracy", #"kreitz_2020_dataset10"
                      "accuracy", #"kreitz_2020_dataset13"
                      "accuracy", #"kreitz_2020_dataset14"
                      "accuracy", #"kreitz_2020_dataset15"
                      "accuracy", #"kreitz_2020_dataset16"
                      "accuracy", #"kreitz_2020_dataset2.1"
                      "accuracy" #"kreitz_2020_dataset2.2"
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
                       160, #"kimchi_2004_exp_1_connected_triangle_arrow_RT",
                       160, #"kimchi_2004_exp_1_connected_triangle_arrow_acc",
                       160, #"kimchi_2004_exp_2_square_cross_color_acc"
                       160, #"kimchi_2004_exp_2_square_cross_RT"
                       160, #"kimchi_2004_exp_2_square_cross_acc"
                       160, #"kimchi_2004_exp_2_disconnected_square_cross_RT",
                       160, #"kimchi_2004_exp_2_disconnected_square_cross_acc",
                       160, #"kimchi_2008_exp_1_RT"
                       160, #"kimchi_2008_exp_1_acc"
                       16, #"lamy_2006_exp2_same"
                       16, #"lamy_2006_exp2_different"
                       180, #"lamy_2006_exp3_RT"
                       180, #"lamy_2006_exp4_RT"
                       180, #"lamy_2006_exp5"
                       160, #"pugnaghi_2020_exp1_RT"
                       160, #"pugnaghi_2020_exp1_acc"
                       160, #"pugnaghi_2020_exp2_RT"
                       160, #"pugnaghi_2020_exp2_acc"
                       240, #"nobre_2020"
                       220, #"pugnaghi_2019"
                       1, #"kreitz_2020_dataset1"
                       #1, #"kreitz_2020_dataset2"
                       1, #"kreitz_2020_dataset3"
                       1, #"kreitz_2020_dataset4"
                       1, #"kreitz_2020_dataset5"
                       1, #"kreitz_2020_dataset6"
                       1, #"kreitz_2020_dataset7"
                       1, #"kreitz_2020_dataset8"
                       1, #"kreitz_2020_dataset9"
                       1, #"kreitz_2020_dataset10"
                       1, #"kreitz_2020_dataset13"
                       1, #"kreitz_2020_dataset14"
                       1, #"kreitz_2020_dataset15"
                       1, #"kreitz_2020_dataset16"
                       1, #"kreitz_2020_dataset2.1"
                       1 #"kreitz_2020_dataset2.2"
                       )

#======== N participants implicit =========
# How many subjects in the sample for implicit processing testing?
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
                            14, #"kimchi_2004_exp_1_connected_triangle_arrow_RT",
                            14, #"kimchi_2004_exp_1_connected_triangle_arrow_acc",
                            12, #"kimchi_2004_exp_2_square_cross_color_acc"
                            12, #"kimchi_2004_exp_2_square_cross_RT"
                            12, #"kimchi_2004_exp_2_square_cross_acc"
                            12, #"kimchi_2004_exp_2_disconnected_square_cross_RT",
                            12, #"kimchi_2004_exp_2_disconnected_square_cross_acc",
                            46, #"kimchi_2008_exp_1_RT"
                            46, #"kimchi_2008_exp_1_acc"
                            8, #"lamy_2006_exp2_same"
                            8, #"lamy_2006_exp2_different"
                            9, #"lamy_2006_exp3_RT"
                            9, #"lamy_2006_exp4_RT"
                            11, #"lamy_2006_exp5"
                            65, #"pugnaghi_2020_exp1_RT"
                            65, #"pugnaghi_2020_exp1_acc"
                            102, #"pugnaghi_2020_exp2_RT"
                            102, #"pugnaghi_2020_exp2_acc"
                            13, #"nobre_2020"
                            201, #"pugnaghi_2019"
                            69, #"kreitz_2020_dataset1"
                            #86, #"kreitz_2020_dataset2"
                            62, #"kreitz_2020_dataset3"
                            64, #"kreitz_2020_dataset4"
                            38, #"kreitz_2020_dataset5"
                            290, #"kreitz_2020_dataset6"
                            42, #"kreitz_2020_dataset7"
                            33, #"kreitz_2020_dataset8"
                            21, #"kreitz_2020_dataset9"
                            34, #"kreitz_2020_dataset10"
                            57, #"kreitz_2020_dataset13"
                            64, #"kreitz_2020_dataset14"
                            68, #"kreitz_2020_dataset15"
                            178, #"kreitz_2020_dataset16"
                            86, #"kreitz_2020_dataset2.1"
                            86 #"kreitz_2020_dataset2.2"
                            )


#======= Fow many assessments of awareness? ========
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
                        1, #"kimchi_2004_exp_1_connected_triangle_arrow_RT",
                        1, #"kimchi_2004_exp_1_connected_triangle_arrow_acc",
                        1, #"kimchi_2004_exp_2_square_cross_color_acc"
                        1, #"kimchi_2004_exp_2_square_cross_RT"
                        1, #"kimchi_2004_exp_2_square_cross_acc"
                        1, #"kimchi_2004_exp_2_disconnected_square_cross_RT",
                        1, #"kimchi_2004_exp_2_disconnected_square_cross_acc",
                        1, #"kimchi_2008_exp_1_RT"
                        1, #"kimchi_2008_exp_1_acc"
                        1, #"lamy_2006_exp2_same"
                        1, #"lamy_2006_exp2_different"
                        1, #"lamy_2006_exp3_RT"
                        1, #"lamy_2006_exp4_RT"
                        1, #"lamy_2006_exp5"
                        1, #"pugnaghi_2020_exp1_RT"
                        1, #"pugnaghi_2020_exp1_acc"
                        1, #"pugnaghi_2020_exp2_RT"
                        1, #"pugnaghi_2020_exp2_acc"
                        1, #"nobre_2020"
                        1, #"pugnaghi_2019"
                        1, #"kreitz_2020_dataset1"
                        #1, #"kreitz_2020_dataset2"
                        1, #"kreitz_2020_dataset3"
                        1, #"kreitz_2020_dataset4"
                        1, #"kreitz_2020_dataset5"
                        1, #"kreitz_2020_dataset6"
                        1, #"kreitz_2020_dataset7"
                        1, #"kreitz_2020_dataset8"
                        1, #"kreitz_2020_dataset9"
                        1, #"kreitz_2020_dataset10"
                        1, #"kreitz_2020_dataset13"
                        1, #"kreitz_2020_dataset14"
                        1, #"kreitz_2020_dataset15"
                        1, #"kreitz_2020_dataset16"
                        1, #"kreitz_2020_dataset2.1"
                        1 #"kreitz_2020_dataset2.2"
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
                              14, #"kimchi_2004_exp_1_connected_triangle_arrow_RT",
                              14, #"kimchi_2004_exp_1_connected_triangle_arrow_acc",
                              12, #"kimchi_2004_exp_2_square_cross_color_acc"
                              12, #"kimchi_2004_exp_2_square_cross_RT"
                              12, #"kimchi_2004_exp_2_square_cross_acc"
                              12, #"kimchi_2004_exp_2_disconnected_square_cross_RT",
                              12, #"kimchi_2004_exp_2_disconnected_square_cross_acc",
                              46, #"kimchi_2008_exp_1_RT"
                              46, #"kimchi_2008_exp_1_acc"
                              8, #"lamy_2006_exp2_same"
                              8, #"lamy_2006_exp2_different"
                              9, #"lamy_2006_exp3_RT"
                              9, #"lamy_2006_exp4_RT"
                              11, #"lamy_2006_exp5"
                              75, #"pugnaghi_2020_exp1_RT"
                              75, #"pugnaghi_2020_exp1_acc"
                              106, #"pugnaghi_2020_exp2_RT"
                              106, #"pugnaghi_2020_exp2_acc"
                              30, #"nobre_2020"
                              212, #"pugnaghi_2019"
                              116, #"kreitz_2020_dataset1"
                              #172, #"kreitz_2020_dataset2"
                              111, #"kreitz_2020_dataset3"
                              110, #"kreitz_2020_dataset4"
                              106, #"kreitz_2020_dataset5"
                              554, #"kreitz_2020_dataset6"
                              95, #"kreitz_2020_dataset7"
                              90, #"kreitz_2020_dataset8"
                              89, #"kreitz_2020_dataset9"
                              115, #"kreitz_2020_dataset10"
                              188, #"kreitz_2020_dataset13"
                              184, #"kreitz_2020_dataset14"
                              277, #"kreitz_2020_dataset15"
                              260, #"kreitz_2020_dataset16"
                              172, #"kreitz_2020_dataset2.1"
                              172 #"kreitz_2020_dataset2.2"
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
                  "yes", #"kimchi_2004_exp_1_connected_triangle_arrow_RT",
                  "yes", #"kimchi_2004_exp_1_connected_triangle_arrow_acc",
                  "no", #"kimchi_2004_exp_2_square_cross_color_acc"
                  "no", #"kimchi_2004_exp_2_square_cross_RT"
                  "no", #"kimchi_2004_exp_2_square_cross_acc"
                  "yes", #"kimchi_2004_exp_2_disconnected_square_cross_RT",
                  "yes", #"kimchi_2004_exp_2_disconnected_square_cross_acc",
                  "yes", #"kimchi_2008_exp_1_RT"
                  "yes", #"kimchi_2008_exp_1_acc"
                  "yes", #"lamy_2006_exp2_same"
                  "yes", #"lamy_2006_exp2_different"
                  "yes", #"lamy_2006_exp3_RT"
                  "yes", #"lamy_2006_exp4_RT"
                  "yes", #"lamy_2006_exp5"
                  "yes", #"pugnaghi_2020_exp1_RT"
                  "yes", #"pugnaghi_2020_exp1_acc"
                  "no", #"pugnaghi_2020_exp2_RT"
                  "yes", #"pugnaghi_2020_exp2_acc"
                  "no", #"nobre_2020"
                  "yes", #"pugnaghi_2019"
                  "yes", #"kreitz_2020_dataset1"
                  #"yes", #"kreitz_2020_dataset2"
                  "yes", #"kreitz_2020_dataset3"
                  "no", #"kreitz_2020_dataset4"
                  "no", #"kreitz_2020_dataset5"
                  "yes", #"kreitz_2020_dataset6"
                  "yes", #"kreitz_2020_dataset7"
                  "yes", #"kreitz_2020_dataset8"
                  "yes", #"kreitz_2020_dataset9"
                  "no", #"kreitz_2020_dataset10"
                  "no", #"kreitz_2020_dataset13"
                  "no", #"kreitz_2020_dataset14"
                  "no", #"kreitz_2020_dataset15"
                  "no", #"kreitz_2020_dataset16"
                  "yes", #"kreitz_2020_dataset2.1"
                  "yes" #"kreitz_2020_dataset2.2"
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
                     "no", #"kimchi_2004_exp_1_connected_triangle_arrow_RT",
                     "no", #"kimchi_2004_exp_1_connected_triangle_arrow_acc",
                     "no", #"kimchi_2004_exp_2_square_cross_color_acc"
                     "no", #"kimchi_2004_exp_2_square_cross_RT"
                     "no", #"kimchi_2004_exp_2_square_cross_acc"
                     "no", #"kimchi_2004_exp_2_disconnected_square_cross_RT",
                     "no", #"kimchi_2004_exp_2_disconnected_square_cross_acc",
                     "no", #"kimchi_2008_exp_1_RT"
                     "no", #"kimchi_2008_exp_1_acc"
                     "no", #"lamy_2006_exp2_same"
                     "no", #"lamy_2006_exp2_different"
                     "no", #"lamy_2006_exp3_RT"
                     "no", #"lamy_2006_exp4_RT"
                     "no", #"lamy_2006_exp5"
                     "no", #"pugnaghi_2020_exp1_RT"
                     "no", #"pugnaghi_2020_exp1_acc"
                     "no", #"pugnaghi_2020_exp2_RT"
                     "no", #"pugnaghi_2020_exp2_acc"
                     "no", #"nobre_2020"
                     "no", #"pugnaghi_2019"
                     "no", #"kreitz_2020_dataset1"
                     #"no", #"kreitz_2020_dataset2"
                     "no", #"kreitz_2020_dataset3"
                     "no", #"kreitz_2020_dataset4"
                     "no", #"kreitz_2020_dataset5"
                     "no", #"kreitz_2020_dataset6"
                     "no", #"kreitz_2020_dataset7"
                     "no", #"kreitz_2020_dataset8"
                     "no", #"kreitz_2020_dataset9"
                     "no", #"kreitz_2020_dataset10"
                     "no", #"kreitz_2020_dataset13"
                     "no", #"kreitz_2020_dataset14"
                     "no", #"kreitz_2020_dataset15"
                     "no", #"kreitz_2020_dataset16"
                     "no", #"kreitz_2020_dataset2.1"
                     "no" #"kreitz_2020_dataset2.2"
                     )

#====== Static vs dynamic =======
static_dynamic <- c("static", #ariga_2007_exp2  
                     "dynamic", #beanland_pammer_2010_exp1A_fixating
                     "dynamic", #beanland_pammer_2010_exp1A_moving
                     "dynamic", #beanland_pammer_2010_exp2_slow_us
                     "dynamic", #beanland_pammer_2010_exp2_fast_us
                     "static", #gabay_2012_exp1
                     "static", #gabay_2012_exp2
                     "static", #lo_yeh_2008_exp1_200ms
                     "static", #lo_yeh_2008_exp1_500ms
                     "static", #lo_yeh_2008_exp2_200ms
                     "static", #lo_yeh_2008_exp2_500ms
                     "static", #mack_and_rock_2000_exp1,
                     "static", #mack_and_rock_2000_exp2,
                     "static", #mack_and_rock_2000_exp3,
                     "static", #mack_and_rock_2000_exp4,
                     "static", #mack_and_rock_2000_exp5,
                     "static", #moore_egeth_1997_exp1
                     "static", #moore_egeth_1997_exp3
                     "static", #moore_2003_exp3
                     "static", #moore_2004
                     "dynamic", #most_2005_exp1to7pooled
                     "static", #razpurker_pratt_2008_columns_rows_rt
                     "static", #razpurker_pratt_2008_columns_rows_acc
                     "static", #razpurker_pratt_2008_triangle_arrow_rt
                     "static", #razpurker_pratt_2008_triangle_arrow_acc
                     "static", #russell_driver_2005_exp1_acc
                     "static", #russell_driver_2005_exp1_rt
                     "static", #russell_driver_2005_exp2_acc
                     "static", #russell_driver_2005_exp2_rt
                     "static", #russell_driver_2005_exp3_acc
                     "static", #russell_driver_2005_exp3_rt
                     "static", #russell_driver_2005_exp4a_acc
                     "static", #russell_driver_2005_exp4b_acc
                     "static", #russell_driver_2005_exp5_acc
                     "static", #russell_driver_2005_exp5_rt
                     "static", #schnuerch_2016_exp1
                     "static", #schnuerch_2016_exp2
                     "static", #"wood_simons_2019_exp1"
                     "static",  #"wood_simons_2019_exp2"
                     "static",  #"rashal_2017_exp1_RT"
                     "static",  #"rashal_2017_exp2_acc"
                     "static",   #"rashal_2017_exp3_RT"
                     "static",  #"rashal_2017_exp4_acc"
                     "static",  #"rashal_2017_exp4_RT",
                     "static",  #"rashal_2017_exp5_acc"
                     "static", #"rashal_2017_exp5_RT"
                     "static", #"rashal_2017_exp6_acc"
                     "static", #"rashal_2017_exp6_RT"
                     "static", #"kimchi_2004_exp_1_column_row_color_RT"
                     "static", #"kimchi_2004_exp_1_triangle_arrow_color_acc"
                     "static", #"kimchi_2004_exp_1_triangle_arrow_acc"
                     "static", #"kimchi_2004_exp_1_connected_triangle_arrow_RT",
                     "static", #"kimchi_2004_exp_1_connected_triangle_arrow_acc",
                     "static", #"kimchi_2004_exp_2_square_cross_color_acc"
                     "static", #"kimchi_2004_exp_2_square_cross_RT"
                     "static", #"kimchi_2004_exp_2_square_cross_acc"
                     "static", #"kimchi_2004_exp_2_disconnected_square_cross_RT",
                     "static", #"kimchi_2004_exp_2_disconnected_square_cross_acc",
                     "static", #"kimchi_2008_exp_1_RT"
                     "static", #"kimchi_2008_exp_1_acc"
                     "static", #"lamy_2006_exp2_same"
                     "static", #"lamy_2006_exp2_different"
                     "static", #"lamy_2006_exp3_RT"
                     "static", #"lamy_2006_exp4_RT"
                     "static", #"lamy_2006_exp5"
                    "static", #"pugnaghi_2020_exp1_RT"
                    "static", #"pugnaghi_2020_exp1_acc"
                    "static", #"pugnaghi_2020_exp2_RT"
                    "static", #"pugnaghi_2020_exp2_acc"
                    "static", #"nobre_2020"
                    "static", #"pugnaghi_2019"
                    "static", #"kreitz_2020_dataset1"
                    #NA, #"kreitz_2020_dataset2" - will be replaced by separate entries for each experiment
                    "static", #"kreitz_2020_dataset3"
                    "static", #"kreitz_2020_dataset4"
                    "static", #"kreitz_2020_dataset5"
                    "static", #"kreitz_2020_dataset6"
                    "dynamic", #"kreitz_2020_dataset7"
                    "dynamic", #"kreitz_2020_dataset8"
                    "dynamic", #"kreitz_2020_dataset9"
                    "dynamic", #"kreitz_2020_dataset10"
                    "static", #"kreitz_2020_dataset13"
                    "static", #"kreitz_2020_dataset14"
                    "static", #"kreitz_2020_dataset15"
                    "dynamic", #"kreitz_2020_dataset16"
                    "static", #"kreitz_2020_dataset2.1"
                    "dynamic" #"kreitz_2020_dataset2.2"
)

#======== Categorize experiments as inattention paradigms or not; 0 = no, 1 = yes ========
inattention_paradigm <- c(rep("no", 21), #ariga_2007_exp2 to most_2005_exp1to7pooled
                                   rep("yes",4), #razpurker-apfeld and pratt, 2008
                                   #0, #richards_2012_tracking
                                   rep("yes", 10), #russel_driver_2005
                                   rep("no",2), #schunerch_2016
                                   rep("no", 2), #wood_simons_2019
                                   rep("yes", 21), #rashal_2017, kimchi_2004, kimchi_2008
                          rep("no", 26) #"lamy_2006", pughnaghi_2020, nobre_2020, pugnaghi_2019, kreitz_2020
                          )

#======== Categorize experiments as group assessment of awareness or not ========
group_aware_assess <- c("yes", #ariga_2007_exp2
                                 rep("no", 6), #beanland_pammer_2010_exp1A_fixating to gabay_2012_exp2
                                 rep("yes",4), #lo_yeh_2008_exp1_200ms
                                 rep("no", 5), #mack_and_rock_2000
                                 rep("yes",4), # moore_egeth_1997_exp1 to moore_2004  
                                 rep("no",1), #most_2005_exp1to7pooled
                                 rep("yes", 4), #razpurker_pratt_2008
                                 rep("yes", 10), #russsel_driver_2005
                                 rep("no",2), #schunerch_2016
                                 rep("no", 2), #wood_simons_2019
                                 rep("yes", 26), #rashal_2017, kimchi_2004, kimchi_2008,lamy_2006
                                 rep("no", 21) #pughnaghi_2020, nobre_2020, pugnaghi_2019, kreitz_2020
                        )

#============= Create table by merging columns ============
es_table <- as_tibble(data.frame("study"=study_names,
                                 "studies_outcomes"=studies_outcomes,
                                 "us_relevance"=us_relevance,
                                 "implicit_measure"=implicit_measure,
                                 "N_trials_implicit"=N_trials_implicit,
                                 "N_participants_implicit"=N_participants_implicit,
                                 "N_trials_awareness"=N_trials_awareness,
                                 "N_participants_awareness"=N_participants_awareness,
                                 "implicit_significance"=implicit_significance,
                                 "gray_literature"=gray_literature,
                                 "inattention"=inattention_paradigm,
                                 "group_awareness"=group_aware_assess,
                                 "gestalt_study"=gestalt,
                                 "static_dynamic"=static_dynamic))

es_table$individual_studies <- as.character(es_table$study)

str(es_table)
