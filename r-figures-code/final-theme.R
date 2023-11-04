
# Final ggplot theme ------------------------------------------------------
# -------------------------------------------------------------------------

# Import libraries
library(tidyverse) # or install.packages()
library(RColorBrewer)
library(showtext)
library(sysfonts)
library(patchwork)
# -------------------------------------------------------------------------
# Download and import the font into R using the lines below
font_add(family = "CMU-bright", regular = "cmunbmr.ttf")
font_add_google('EB Garamond')
showtext_auto()
#Check if font was added
font_families()

# Colors ------------------------------------------------------------------

# Important objects
colors <- RColorBrewer::brewer.pal(8, 'Dark2')[c(3,4,7,8)]
ncolors <- c("#332288", "#882255", "#DDCC77", "#666666", #1-4
             "#44AA99", "#CC6677",                       #5,6
             "#117733", "#88CCEE", "#AA4499" )          #7,8,9
cfont <- "CMU-bright"
# -------------------------------------------------------------------------

# Theme -------------------------------------------------------------------
# Values to test ----------------------------------------------------------

## These are the values you can play around to check the output (V1-V8)
V1 <- 26 #Text size in facets. orig-10.    didn't change anything in these plots.
V2 <- 26 # Plot title size. orig-15.       didn't change anything in these plots.
V3 <- .05 # grid line width
V4 <- 22 # Axes text size. orig-13 --OK
V5 <- 26 # Axes title size. orig-20
V6 <- .25 #Axes linewidth
V7 <- 22 # Legend title size. orig-14
V8 <- 16 # Legend text size. orig-11

## RUN it, NOT CHANGE IT
theme1 <- theme(# GENERAL
  plot.background = element_rect(fill = "white", color = NA), #background color
  text = element_text(family = cfont, color = "grey10"), # color of all text in the plot 
  strip.text = element_text(colour = "grey10", size = V1), # specs of the text inside plot
  plot.title = element_text(hjust = 0.5, color = "grey10", size = V2, face = "bold"), # specs of the title
  # GRID
  panel.grid.major.x = element_line(linewidth = V3), # change the grid layout
  panel.grid.major.y = element_line(linewidth = V3), # change the grid layout
  panel.grid.minor.x = element_blank(), # remove the grid layout
  panel.grid.minor.y = element_blank(), # remove the grid layout
  # AXES
  axis.text=element_text(size=V4, color = "grey10"), # specs of the text in axis
  axis.title.x = element_text(size = V5, family = cfont), # Font family for x-axis label
  axis.title.y = element_text(size = V5,family = cfont), # Font family for y-axis label
  axis.line = element_line(linewidth = V6, colour = "grey10"),
  # LEGEND
  legend.title = element_text(size = V7,family = cfont), # Font family for legend title
  legend.position = "bottom", 
  #legend.title = element_blank(), # remove legend title,
  legend.text = element_text(size = V8),
  legend.key.size = unit(0.1, 'in'),
  # MARGINS
  plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
  #plot.tag.position = 'right',
  #plot.tag = element_text(
  #  size = 13,                     # Font size
  #  hjust = 1,                     # Horizontal adjustment
  #  vjust = 1,                     # Vertical adjustment
  #  angle = -90,                   # Font angle
  #  margin = margin(0, 0, 0, 10)), # Margins (t, r, b, l)
  #plot.subtitle =   element_text(
  #  size = 13,                     # Font size
  #  hjust = .5,                     # Horizontal adjustment
  #  vjust = 1,                     # Vertical adjustment
  #  margin = margin(0, 0, 5, 0)) # Margins (t, r, b, l)
)

