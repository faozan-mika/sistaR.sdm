# inst/app/global.R
# This file runs before server.R and ui.R

# Load all required libraries
library(shiny)
library(shinydashboard)
library(terra)
library(RSQLite)
library(dplyr)
library(readxl)
library(geodata)
library(ggplot2)
library(dismo)
library(sf)
library(randomForest)
library(corrplot)
library(tidyr)
library(pROC)
library(geosphere)
library(psych)
library(e1071)
library(maxnet)
library(shinycssloaders)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(htmltools)

# You can also define any global variables or functions here
options(shiny.maxRequestSize = 1000*1024^2)  # Increase file upload limit to 1GB
