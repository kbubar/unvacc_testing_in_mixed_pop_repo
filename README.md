# Transmission and unvaccinated-only screening in populations of mixed vaccination status
Github repo associated with the manuscript "SARS-CoV-2 Transmission and Impacts of Unvaccinated-Only Screening in Populations of Mixed Vaccination Status"

## Dependencies:
R version 4.1.0 or higher is required. All package dependents can be found in **setup.R**. For initial font install, see line 13. 
To install packages, use command 'install.packages("package_name")'. Installation of all dependencies will take approximately 1.5 hours.

## Code layout:

**setup.R** loads packages, model and plotting functions, and initializes all the default parameter settings.

**helpers.R** defines functions to run model, compute necessary data for figures and plotting related functions. 

**Demo.R** example code for how to run the model and plot infection and hospitalization curves. 

**Fig.R and SuppFig.R** scripts to recreate manuscript figures. Includes code to generate relevant data frames or use pre-generated dataframes saved in the dataframes folder.

## Notes:

- If you do not have fonts installed in R, see line 22 in set_up.R to configure this correctly. Otherwise any line that specifies family="Arial" will return an error.

- Exporting a cairo_pdf embeds the text in figures correctly on Windows. But, if you are a Mac user, this file format may not be compatible. We recommend exporting a png instead.

- We recommend using the document outline (on the right-hand side) to navigate each file. 

- Finely detailed heatmap data (e.g., Figure 5) will take approximately 2 hours with a fine grid (psi and phi by 0.01) to generate on a standard machine. To improve runtime, increase the step size when initializating the dataframe to 'by=0.05'.


# License

Copyright (C) 2021, Kate Bubar

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program (see LICENSE.md).  If not, see <https://www.gnu.org/licenses/>.
