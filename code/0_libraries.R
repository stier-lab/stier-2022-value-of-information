#####
#Libraries
#####

library(ggplot2)
library(reshape)
library(gridExtra)
library(here)
library(ggpubr)
library(ggthemes)
library(tidyverse)
library(cowplot)
library(gdata)
library(wesanderson)
library(stringr)

pal <- wes_palette("Zissou1", 100, type = "continuous")

gc <- guide_colorbar(
  frame.colour = "black",
  barheight = 8,
  frame.linewidth = 2,
  ticks.colour = "black",
  ticks.linewidth = 2
)