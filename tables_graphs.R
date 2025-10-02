
load("regression_outputs.Rdata")

library(ordinal)
library(tidyr)
library(texreg)

#how to use
texreg::texreg(m1, 
       booktabs = TRUE,   # nicer LaTeX tables
       use.packages = FALSE,
       caption = "CLMM results",
       label = "tab:clmm")

m1

# Save to file
texreg(m1, 
       file = "m1_table.tex", 
       booktabs = TRUE, 
       use.packages = FALSE)
#replicate table 3

# Save to file
texreg(list(m1, m2, m3, m4),
       file = "r1_table.tex", 
       booktabs = TRUE, 
       use.packages = FALSE,
       file = "r1_table.tex")

###other regressions

texreg(list(m1_agg, m2_agg),
       booktabs = TRUE,   # nicer LaTeX tables
       use.packages = FALSE,
       caption = "CLMM results",
       label = "tab:clmm")

texreg(list(m1_agg, m2_agg),
       booktabs = TRUE,   # nicer LaTeX tables
       use.packages = FALSE,
       caption = "CLMM results",
       label = "tab:clmm",
       file = "r2_table.tex"   # <-- saves table as TeX file
)

texreg(list(m1_dis, m2_dis, m3_dis),
       booktabs = TRUE,   # nicer LaTeX tables
       use.packages = FALSE,
       label = "tab:clmm",
       file = "r3_table.tex"   # <-- saves table as TeX file
)

texreg(list(m1_no_ego, m2_no_ego),
       booktabs = TRUE,   # nicer LaTeX tables
       use.packages = FALSE,
       caption = "CLMM results",
       label = "tab:clmm",
       file = "r4_table.tex")

texreg(list(m1_socio, m2_socio),
       booktabs = TRUE,   # nicer LaTeX tables
       use.packages = FALSE,
       caption = "CLMM results",
       label = "tab:clmm",
       file = "r5_table.tex")
