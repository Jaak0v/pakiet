library(devtools)
library(tidyverse)

use_git()

use_r("fun_pack")

# piszemy funkcje

load_all()

fun_pack()

exists("fun_pack", where = globalenv(), inherits = FALSE)

