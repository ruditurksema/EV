# EV

library(readxl)
library(tidyverse)

options(scipen = 999)

Elektrische_voertuigen <- read_excel("Elektrische voertuigen.xlsx", col_types = c("text", "text", "numeric"))

ggplot(Elektrische_voertuigen, aes(Jaar, Aantal, fill = Type)) + geom_col(position = "dodge") + theme_minimal() + 
  ggtitle("Registratie elektrische voertuigen i.v.t. totaal aantal voertuigen, 2014-2017 (nov.)") +
  theme(legend.position = "bottom")




                                     