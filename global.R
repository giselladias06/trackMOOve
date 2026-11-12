# Loaded once by Shiny, before ui.R and server.R.

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)
library(epinemo)
library(Matrix)
library(igraph)
library(DT)
library(RColorBrewer)
library(data.table)
library(EpiContactTrace)

# Simulated sample for Sentara (see data/ and its metadata file).
# Replace with the real file when it is available; the column names must match.
data <- fread("sentara_movements_sample_2024_h1.csv", encoding = "UTF-8")

global_palette <- brewer.pal(8, "Accent")
