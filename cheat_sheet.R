# Ways to check that you're doing it right 


library(skimr)
cps_labor %>% group_by(LABFORCE) %>% skim(ASECWT)
q2b %>% group_by(LABFORCE) %>% skim(ASECWT)
table(cps_labor$SEX)
table(cps_labor$SEX)
table(q2b$LABFORCE)
population = sum()
