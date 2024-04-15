# Hierarchical Modelling of Species Communities and bird frugivory in *Celtis tala*
This repository contains data and code to reproduce analyses reported in the manuscript *Palacio FX, M Ordano, M Jones, and O Ovaskainen. Seed dispersal scaling from bird species to communities is driven by fruit and bird traits, rather than by spatial context and species interactions*

# Repository structure

**analyses**: contains code to reproduce analyses and figures.

**data**: includes the datasets analysed for this manuscript:
- SPECIES DATA (Y.csv)
  
The species data consists of fruit consumption events of 14 bird species.

- STUDY DESIGN (S.csv)
  
The study design contains frugivore visits to trees.

The tree id is in the column "Tree" and its coordinates are in columns "Tree_x" (longitude) and "Tree_y" (latitude).

The year of visit is in the column "Year".

- COVARIATES (X)
  
Fruit_crop (discrete): number of ripe fruits.

Sugar_concentration (continuous): mean sugar concentration (°Brix).

Fruit_diameter (continuous): mean fruit diameter (mm).

Effort (categorical): observation hours.

- TRAITS (Tr)
  
Gape_width (continuous): gape width (mm).

Handling (factor): fruit handling behavior (gulper or pulp consumer).

**stored results**: includes the fitted models since HMSC takes a considerable amount of time to run.

The names of all other files should be self-explanatory. If they are not, please open an issue in this repository.
