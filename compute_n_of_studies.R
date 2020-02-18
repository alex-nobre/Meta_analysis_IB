
# N of papers in meta-analysis
unique_studies <- c("ariga_2007",
                    "beanland_pammer_2010",
                    "gabay_2012",
                    "lo_yeh_2008",
                    "mack_and_rock_2000",
                    "moore_egeth_1997",
                    "moore_2003",
                    "moore_2004",
                    "most_2005",
                    "razpurker_pratt_2008",
                    "russell_driver_2005",
                    "schnuerch_2016",
                    "wood_simons_2019",
                    "rashal_2017",
                    "kimchi_2004",
                    "kimchi_2008",
                    "lamy_2006")

length(unique(unique_studies))


# N of experiments in meta-analysis
source("create_es_data_table.R")

length(unique(study_names))

       