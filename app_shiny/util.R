

library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)

dados <- read.csv("dados_shiny_2022081822.csv")
options(warn = -1)

theme_set(theme_bw())

