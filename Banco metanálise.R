Study <- c("Ariga et al. (2007)", "Beanland e Pammer (2010)", "Beanland e Pammer (2010)", 
           "Bressan e Pizzighello (2008)", "Bressan e Pizzighello (2008)", "Bressan e Pizzighello (2008)", 
           "Bressan e Pizzighello (2008)", "Bressan e Pizzighello (2008)", "Gabay et al. (2012)", 
           "Gabay et al. (2012)", "Lathrop et al. (2011)", "Lathrop et al. (2011)", "Lo e Yeh (2008)", 
           "Lo e Yeh (2008)", "Lo e Yeh (2011)", "Lo e Yeh (2011)", "Memmert (2006)", "Memmert (2006)",
           "Moore et al. (2003)", "Moore et al. (2004)", "Most et al. (2005)", "Pitts et al. (2012)", 
           "Richards et al. (2012)", "Rock et al. (1992)", "Rock et al. (1992)", "Rock et al. (1992)", 
           "Rock et al. (1992)", "Rock et al. (1992)", "Scholte et al. (2006)", "Scholte et al. (2006)", 
           "Thakral (2011)", "Vanderbroucke et al. (2014)", "Waywand et al. (2005)", "Wiemer et al. (2013)")
Experiment <- c("2", "1A", "2", "1", "2", "3", "4", "5", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "3", "single", "8", "single", "single", "4, pt. 1", "4, pt. 2", "4, pt. 3", "4, pt. 4", "4, pt. 5", "1", "2", "single", "single", "1", "single")

tableOfStudies <- data.frame(Study = Study, Experiment = Experiment)

tableOfStudies$VD <- c("Same object-advantage", "Bounce counting", "Bounce counting", "Bounce counting", 
                       "Bounce counting", "Bounce counting", "Bounce counting", "Bounce counting", 
                       "Orienting of attention", "Orienting of attention", "Roelofs Effect", "Roelofs Effect", 
                       "Shape discrimination/texture segregation", "Simon effect", "Simon effect/compatibility", 
                       "Simon effect/compatibility", "eye movements", "task performance", "surface completion", 
                       "Simon effect", "Task performance", "ERPs for patterns", "eye movements", "shape perception", 
                       "shape perception", "shape perception", "shape perception", "shape perception",
                       "MEG Signal", "fMRI activation", "fMRI activation", "perceptual integration", "processing of noxious stimuli", 
                       "skin conductance/eye movements")

tableOfStudies$n <- c(20, 72, 50, 59, 34, 64, 34, 38, 30, 23, 57, 53, 84, 48, 66, 64, 20, 101, 44, 25, 84, 32, 131, 18, 18, 12, 18, 13, 14, 10, 9, 42, 165, 120)

tableOfStudies$effectSize

