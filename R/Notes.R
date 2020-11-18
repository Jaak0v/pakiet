library(devtools)
library(tidyverse)

use_git()

#use_r("fun_pack")

# piszemy funkcje

load_all()

fun_pack()

exists("fun_pack", where = globalenv(), inherits = FALSE)

fun_pack(a = 1, b = 500, d = 23)
