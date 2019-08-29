
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
                 #"shafto_pitts_2015",
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
                 #15, #shafto_pitts_2015
                 61, #schnuerch_2016_exp1
                 58 #schnuerch_2016_exp2
                 )


us_relevance <- c("irrelevant", #ariga_2007_exp2  
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
                  "irrelevant", #richards_2012_tracking
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
                  "irrelevant" #schnuerch_2016_exp2
                  )


implicit_type <- c("response", #ariga_2007_exp2  
                   "attentional", #beanland_pammer_2010_exp1A_fixating
                   "attentional", #beanland_pammer_2010_exp1A_moving
                   "attentional", #beanland_pammer_2010_exp2_slow_us
                   "attentional", #beanland_pammer_2010_exp2_fast_us
                   "response", #gabay_2012_exp1
                   "response", #gabay_2012_exp2
                   "response", #lo_yeh_2008_exp1_200ms
                   "response", #lo_yeh_2008_exp1_500ms
                   "response", #lo_yeh_2008_exp2_200ms
                   "response", #lo_yeh_2008_exp2_500ms
                   "attentional", #mack_and_rock_2000_exp1,
                   "attentional", #mack_and_rock_2000_exp2,
                   "attentional", #mack_and_rock_2000_exp3,
                   "attentional", #mack_and_rock_2000_exp4,
                   "attentional", #mack_and_rock_2000_exp5,
                   "response", #moore_egeth_1997_exp1
                   "response", #moore_egeth_1997_exp3
                   "response", #moore_2003_exp3
                   "attentional", #moore_2004
                   "attentional", #most_2005_exp1to7pooled
                   "attentional", #razpurker_pratt_2008_columns_rows_rt
                   "response", #razpurker_pratt_2008_columns_rows_acc
                   "response", #razpurker_pratt_2008_triangle_arrow_rt
                   "attentional", #razpurker_pratt_2008_triangle_arrow_acc
                   "attentional", #richards_2012_tracking
                   "attentional", #russell_driver_2005_exp1_acc
                   "attentional", #russell_driver_2005_exp1_rt
                   "attentional", #russell_driver_2005_exp2_acc
                   "attentional", #russell_driver_2005_exp2_rt
                   "attentional", #russell_driver_2005_exp3_acc
                   "attentional", #russell_driver_2005_exp3_rt
                   "attentional", #russell_driver_2005_exp4a_acc
                   "attentional", #russell_driver_2005_exp4b_acc
                   "attentional", #russell_driver_2005_exp5_acc
                   "attentional", #russell_driver_2005_exp5_rt
                   #"attentional", #shafto_pitts_2015
                   "attentional", #schnuerch_2016_exp1
                   "attentional" #schnuerch_2016_exp2
                   )


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
                      "accuracy", #richards_2012_tracking
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
                      #0, #shafto_pitts_2015
                      "RT", #schnuerch_2016_exp1
                      "RT" #schnuerch_2016_exp2
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
                       #216, #shafto_pitts_2015
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
                            #15, #shafto_pitts_2015
                            61, #schnuerch_2016_exp1
                            58 #schnuerch_2016_exp2
                            )


awareness_objective <- c("objective", #ariga_2007_exp2  
                         "objective", #beanland_pammer_2010_exp1A_fixating
                         "objective", #beanland_pammer_2010_exp1A_moving
                         "objective", #beanland_pammer_2010_exp2_slow_us
                         "objective", #beanland_pammer_2010_exp2_fast_us
                         "subjective", #gabay_2012_exp1
                         "subjective", #gabay_2012_exp2
                         "objective", #lo_yeh_2008_exp1_200ms
                         "objective", #lo_yeh_2008_exp1_500ms
                         "objective", #lo_yeh_2008_exp2_200ms
                         "objective", #lo_yeh_2008_exp2_500ms
                         "objective", #mack_and_rock_2000_exp1,
                         "objective", #mack_and_rock_2000_exp2,
                         "objective", #mack_and_rock_2000_exp3,
                         "objective", #mack_and_rock_2000_exp4,
                         "objective", #mack_and_rock_2000_exp5,
                         "objective", #moore_egeth_1997_exp1
                         "objective", #moore_egeth_1997_exp3
                         "objective", #moore_2003_exp3
                         "objective", #moore_2004
                         "subjective", #most_2005_exp1to7pooled
                         "subjective", #razpurker_pratt_2008_columns_rows_rt
                         "subjective", #razpurker_pratt_2008_columns_rows_acc
                         "subjective", #razpurker_pratt_2008_triangle_arrow_rt
                         "subjective", #razpurker_pratt_2008_triangle_arrow_acc
                         "objective", #richards_2012_tracking
                         "objective", #russell_driver_2005_exp1_acc
                         "objective", #russell_driver_2005_exp1_rt
                         "objective", #russell_driver_2005_exp2_acc
                         "objective", #russell_driver_2005_exp2_rt
                         "objective", #russell_driver_2005_exp3_acc
                         "objective", #russell_driver_2005_exp3_rt
                         "objective", #russell_driver_2005_exp4a_acc
                         "objective", #russell_driver_2005_exp4b_acc
                         "objective", #russell_driver_2005_exp5_acc
                         "objective", #russell_driver_2005_exp5_rt
                         #"subjective", #shafto_pitts_2015
                         "subjective", #schnuerch_2016_exp1
                         "subjective" #schnuerch_2016_exp2
                         )


us_presentation <- c("block_or_phase", #ariga_2007_exp2  
                     "block_or_phase", #beanland_pammer_2010_exp1A_fixating
                     "block_or_phase", #beanland_pammer_2010_exp1A_moving
                     "block_or_phase", #beanland_pammer_2010_exp2_slow_us
                     "block_or_phase", #beanland_pammer_2010_exp2_fast_us
                     "interleaved", #gabay_2012_exp1
                     "interleaved", #gabay_2012_exp2
                     "block_or_phase", #lo_yeh_2008_exp1_200ms
                     "block_or_phase", #lo_yeh_2008_exp1_500ms
                     "block_or_phase", #lo_yeh_2008_exp2_200ms
                     "block_or_phase", #lo_yeh_2008_exp2_500ms
                     "interleaved", #mack_and_rock_2000_exp1,
                     "interleaved", #mack_and_rock_2000_exp2,
                     "interleaved", #mack_and_rock_2000_exp3,
                     "interleaved", #mack_and_rock_2000_exp4,
                     "interleaved", #mack_and_rock_2000_exp5,
                     "block_or_phase", #moore_egeth_1997_exp1
                     "block_or_phase", #moore_egeth_1997_exp3
                     "block_or_phase", #moore_2003_exp3
                     "block_or_phase", #moore_2004
                     "block_or_phase", #most_2005_exp1to7pooled
                     "interleaved", #razpurker_pratt_2008_columns_rows_rt
                     "interleaved", #razpurker_pratt_2008_columns_rows_acc
                     "interleaved", #razpurker_pratt_2008_triangle_arrow_rt
                     "interleaved", #razpurker_pratt_2008_triangle_arrow_acc
                     0, #richards_2012_tracking
                     "block_or_phase", #russell_driver_2005_exp1_acc
                     "block_or_phase", #russell_driver_2005_exp1_rt
                     "block_or_phase", #russell_driver_2005_exp2_acc
                     "block_or_phase", #russell_driver_2005_exp2_rt
                     "block_or_phase", #russell_driver_2005_exp3_acc
                     "block_or_phase", #russell_driver_2005_exp3_rt
                     "block_or_phase", #russell_driver_2005_exp4a_acc
                     "block_or_phase", #russell_driver_2005_exp4b_acc
                     "block_or_phase", #russell_driver_2005_exp5_acc
                     "block_or_phase", #russell_driver_2005_exp5_rt
                     #"block_or_phase", #shafto_pitts_2015
                     "block_or_phase", #schnuerch_2016_exp1
                     "block_or_phase" #schnuerch_2016_exp2
                     )


us_delay_type <- c("fixed", #ariga_2007_exp2  
                   "fixed", #beanland_pammer_2010_exp1A_fixating
                   "fixed", #beanland_pammer_2010_exp1A_moving
                   "fixed", #beanland_pammer_2010_exp2_slow_us
                   "fixed", #beanland_pammer_2010_exp2_fast_us
                   "variable", #gabay_2012_exp1
                   "variable", #gabay_2012_exp2
                   "fixed", #lo_yeh_2008_exp1_200ms
                   "fixed", #lo_yeh_2008_exp1_500ms
                   "fixed", #lo_yeh_2008_exp2_200ms
                   "fixed", #lo_yeh_2008_exp2_500ms
                   "variable", #mack_and_rock_2000_exp1,
                   "variable", #mack_and_rock_2000_exp2,
                   "variable", #mack_and_rock_2000_exp3,
                   "variable", #mack_and_rock_2000_exp4,
                   "variable", #mack_and_rock_2000_exp5,
                   "variable", #moore_egeth_1997_exp1
                   "variable", #moore_egeth_1997_exp3
                   "fixed", #moore_2003_exp3
                   "fixed", #moore_2004
                   "fixed", #most_2005_exp1to7pooled
                   "variable", #razpurker_pratt_2008_columns_rows_rt
                   "variable", #razpurker_pratt_2008_columns_rows_acc
                   "variable", #razpurker_pratt_2008_triangle_arrow_rt
                   "variable", #razpurker_pratt_2008_triangle_arrow_acc
                   "variable", #richards_2012_tracking
                   "variable", #russell_driver_2005_exp1_acc
                   "variable", #russell_driver_2005_exp1_rt
                   "variable", #russell_driver_2005_exp2_acc
                   "variable", #russell_driver_2005_exp2_rt
                   "variable", #russell_driver_2005_exp3_acc
                   "variable", #russell_driver_2005_exp3_rt
                   "variable", #russell_driver_2005_exp4a_acc
                   "variable", #russell_driver_2005_exp4b_acc
                   "variable", #russell_driver_2005_exp5_acc
                   "variable", #russell_driver_2005_exp5_rt
                   #"variable", #shafto_pitts_2015
                   "variable", #schnuerch_2016_exp1
                   "variable" #schnuerch_2016_exp2
                   )


us_assessment <- c("trial", #ariga_2007_exp2  
                   "block_or_phase", #beanland_pammer_2010_exp1A_fixating
                   "block_or_phase", #beanland_pammer_2010_exp1A_moving
                   "block_or_phase", #beanland_pammer_2010_exp2_slow_us
                   "block_or_phase", #beanland_pammer_2010_exp2_fast_us
                   "block_or_phase", #gabay_2012_exp1
                   "block_or_phase", #gabay_2012_exp2
                   "trial", #lo_yeh_2008_exp1_200ms
                   "trial", #lo_yeh_2008_exp1_500ms
                   "trial", #lo_yeh_2008_exp2_200ms
                   "trial", #lo_yeh_2008_exp2_500ms
                   "trial", #mack_and_rock_2000_exp1,
                   "trial", #mack_and_rock_2000_exp2,
                   "trial", #mack_and_rock_2000_exp3,
                   "trial", #mack_and_rock_2000_exp4,
                   "trial", #mack_and_rock_2000_exp5,
                   "trial", #moore_egeth_1997_exp1
                   "trial", #moore_egeth_1997_exp3
                   "trial", #moore_2003_exp3
                   "trial", #moore_2004
                   "trial", #most_2005_exp1to7pooled
                   "block_or_phase", #razpurker_pratt_2008_columns_rows_rt
                   "block_or_phase", #razpurker_pratt_2008_columns_rows_acc
                   "block_or_phase", #razpurker_pratt_2008_triangle_arrow_rt
                   "block_or_phase", #razpurker_pratt_2008_triangle_arrow_acc
                   "block_or_phase", #richards_2012_tracking
                   "trial", #russell_driver_2005_exp1_acc
                   "trial", #russell_driver_2005_exp1_rt
                   "trial", #russell_driver_2005_exp2_acc
                   "trial", #russell_driver_2005_exp2_rt
                   "trial", #russell_driver_2005_exp3_acc
                   "trial", #russell_driver_2005_exp3_rt
                   "trial", #russell_driver_2005_exp4a_acc
                   "trial", #russell_driver_2005_exp4b_acc
                   "trial", #russell_driver_2005_exp5_acc
                   "trial", #russell_driver_2005_exp5_rt
                   #"block_or_phase", #shafto_pitts_2015
                   "block_or_phase", #schnuerch_2016_exp1
                   "block_or_phase" #schnuerch_2016_exp2
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
                        #1, #shafto_pitts_2015
                        1, #schnuerch_2016_exp1
                        1 #schnuerch_2016_exp2
                        )

## add controls?
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
                              #30, #shafto_pitts_2015
                              61, #schnuerch_2016_exp1
                              61 #schnuerch_2016_exp2
                              )


significance <- c("no", #ariga_2007_exp2  
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
                  "no", #richards_2012_tracking
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
                  "yes" #schnuerch_2016_exp2
                  )

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
                     "no", #richards_2012_tracking
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
                     "no" #schnuerch_2016_exp2
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
                                 "significance"=significance,
                                 "gray_literature"=gray_literature))
