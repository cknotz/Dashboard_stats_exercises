
#############
# Tables in R
#############

# If you haven't installed haven (to read .dta & .sav) or essurvey
if(!require(haven)){
  install.packages("haven")
}
if(!require(essurvey)){
  install.packages("essurvey")
}

library(bst290)
library(essurvey)

# mtcars
########

data(mtcars)

# Summary table
###############
vars <- c("mpg","disp","hp")

# Basic example
oppsumtabell(dataset = mtcars,
             variables = c("cyl"))

# Example with factor variable (plus warning message)
mtcars$cyl_f <- factor(mtcars$cyl)

oppsumtabell(dataset = mtcars,
       variables = c("cyl_f"))

# Multiple variables
oppsumtabell(dataset = mtcars, 
             variables = c("disp","mpg"))

# Illustrates export function
oppsumtabell(dataset = mtcars,
       variables = "mpg",
       export = T)

# Illustrates to.data.frame() functionality
result <- oppsumtabell(dataset = mtcars,
       variables = vars)
result

# Catches error when students want to summarize a text variable
mtcars$cyl_c <- as.character(mtcars$cyl)

oppsumtabell(dataset = mtcars,
             variables = "text")

# Catches error when students use non-existing variables
oppsumtabell(dataset = mtcars,
             variables = "hadley")


# Crosstable
############

# Basic example
krysstabell(dataset = mtcars,
            rowvar = "gear", colvar = "cyl")

# with a factor variable
krysstabell(dataset = mtcars,
            rowvar = "gear", colvar = "cyl_f")

# with a character variable
krysstabell(dataset = mtcars,
            rowvar = "gear", colvar = "cyl_c")


# Export function
krysstabell(dataset = mtcars,
            rowvar = "carb", colvar = "gear",
            export = T)

# Catches error when students tabulate a variable by itself
krysstabell(dataset = mtcars,
            rowvar = "gear", colvar = "gear")

# Catches error when variable(s) don't exist
krysstabell(dataset = mtcars,
            rowvar = "gear", colvar = "hadley")

krysstabell(dataset = mtcars,
            rowvar = "hadley", colvar = "gear")



# Tabulate by
#############

# Basic
oppsum_grupp(dataset = mtcars,
          variable = "drat",
          by.var = "gear")

# to.data.frame(), with factor
result <- oppsum_grupp(dataset = mtcars,
                    variable = "mpg",
                    by.var = "cyl_f",
                    export = F)
result

# Catches error when variable does not exist
oppsum_grupp(dataset = mtcars,
             variable = "hadley",
             by.var = "gear")

oppsum_grupp(dataset = mtcars,
             variable = "drat",
             by.var = "hadley")

# Catches error when students use character variable
oppsum_grupp(dataset = mtcars,
             variable = "cyl_c",
             by.var = "gear")

# This works, however!
oppsum_grupp(dataset = mtcars,
             variable = "drat",
             by.var = "cyl_c")


# ESS
##### 

essurvey::set_email("carlo.knotz@gmail.com")

ess <- import_rounds(1)
ess <- recode_missings(ess)
ess <- ess[which(ess$cntry=="NO"),]

ess <- sample_n(ess, size = 187)

vars <- c("netuse","ppltrst")

# Basic examples
oppsumtabell(dataset = ess,
             variables = vars)

oppsumtabell(dataset = ess,
             variables = c("netuse"))


krysstabell(dataset = ess,
            rowvar = "vote",
            colvar = "gndr")


oppsum_grupp(dataset = ess,
          variable = "netuse",
          by.var = "nwsptot")

oppsum_grupp(dataset = ess,
             variable = "netuse",
             by.var = "gndr")



# CPDS
######

cpds <- haven::read_dta("https://www.cpds-data.org/images/Update2021/CPDS_1960-2019_Update_2021.dta")

# Basic examples
krysstabell(dataset = cpds,
            rowvar = "eu",
            colvar = "emu")

oppsumtabell(dataset = cpds,
             variables = c("gov_left1","gov_right1","womenpar"))

# Asks to confirm when categories of by.var>=10
oppsum_grupp(dataset = cpds,
          variable = "womenpar",
          by.var = "country")

# With factor
cpds$poco <- as.factor(cpds$poco)

oppsum_grupp(dataset = cpds,
             variable = "womenpar",
             by.var = "poco")

oppsumtabell(dataset = cpds,
             variables = "poco")


