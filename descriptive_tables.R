
#############
# Tables in R
#############

# If you haven't installed haven (to read .dta & .sav)
if(!require(haven)){
  install.packages("haven")
}

library(bst290)

# mtcars
########

data(mtcars)

mtcars$cyl_c <- as.character(mtcars$cyl)
mtcars$cyl_f <- factor(mtcars$cyl)

# Summary table
###############
vars <- c("mpg","disp","hp")

oppsumtabell(dataset = mtcars,
       variables = c("cyl_f"))

# does not work, illustrates function checks
oppsumtabell(dataset = mtcars, 
             variables = c("disp","mpg"),
             export = F)

# Illustrates export function
oppsumtabell(dataset = mtcars,
       variables = "mpg",
       export = T)

# Illustrates to.data.frame() functionality
result <- oppsumtabell(dataset = mtcars,
       variables = vars)
result

# Crosstable
############

# with a factor variable
krysstabell(dataset = mtcars,
            rowvar = "gear", colvar = "cyl_c",
            export = F)


# Export
krysstabell(dataset = mtcars,
            rowvar = "carb", colvar = "gear",
            export = T)


# Tabulate by
#############

# Simple
oppsum_grupp(dataset = mtcars,
          variable = "drat",
          by.var = "gear")

# to.data.frame(), with factor
result <- oppsum_grupp(dataset = mtcars,
                    variable = "mpg",
                    by.var = "cyl_f",
                    export = F)
result


# ESS
#####

library(essurvey)

essurvey::set_email("carlo.knotz@gmail.com")

ess <- import_rounds(1)
ess <- recode_missings(ess)
ess <- ess[which(ess$cntry=="NO"),]

ess <- sample_n(ess, size = 187)

vars <- c("netuse","ppltrst")

oppsumtabell(dataset = ess,
             variables = vars,
             export = F)

oppsumtabell(dataset = ess,
             variables = c("netuse","gndr"),
             export = F)


krysstabell(dataset = ess,
            rowvar = "vote",
            colvar = "gndr",
            export = F)


oppsum_grupp(dataset = ess,
          variable = "netuse",
          by.var = "nwsptot",
          export = F)

oppsum_grupp(dataset = ess,
             variable = "netuse",
             by.var = "gndr",
             export = F)



# CPDS
######

cpds <- haven::read_dta("https://www.cpds-data.org/images/Update2021/CPDS_1960-2019_Update_2021.dta")

krysstabell(dataset = cpds,
            rowvar = "eu",
            colvar = "emu")

oppsumtabell(dataset = cpds,
             variables = c("gov_left1","gov_right1","womenpar"))


oppsum_grupp(dataset = cpds,
          variable = "womenpar",
          by.var = "country")

# Test with factor
cpds$poco <- as.factor(cpds$poco)

oppsum_grupp(dataset = cpds,
             variable = "womenpar",
             by.var = "poco")

oppsumtabell(dataset = cpds,
             variables = "poco")

