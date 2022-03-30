# Transmission and unvaccinated-only testing in populations of mixed vaccination status
Github repo associated with the manuscript "SARS-CoV-2 Transmission and Impacts of Unvaccinated-Only Screening in Populations of Mixed Vaccination Status"

## Dependencies:
R version 4.1.0 or higher is required. All package dependents can be found in **setup.R**. For initial font install, see line 13. 
To install packages, use command 'install.packages("package_name")'. Installation of all dependencies will take approximately 1.5 hours.

## Code layout:

**setup.R** loads packages, plotting functions, and initializes all the default parameter settings.

**helpers.R** defines functions to run model and compute necessary data for figures. 

**main.R** runs the model and generates the main text line plots and table.

**contourplot.R** runs the model and generates the heatmap figures. 

**suppfig.R** runs the model and generates the supplemental figures.

## Demo:
Dataframes for each figure are included in the _dataframes_ folder of the repository. To replicate manuscript figures, load dataframes using the 'readRDS(filename.RData)' command, then execute the figure code in the appropriate file. 
Finely detailed heatmap data (in countourplot.R) will take approximately 12 hours to generate on a standard machine. To improve runtime, increase the step size on lines 31 and 32 in **countourplot.R** with 'by=0.05'.


## Notes:

- If you do not have fonts installed in R, see line 22 in set_up.R to configure this correctly. Otherwise any line that specifies family="Arial" will return an error.

- Exporting a cairo_pdf embeds the text in figures correctly on Windows. But, if you are a Mac user, this file format may not be compatible. We recommend exporting a png instead.

- We recommend using the document outline (on the right-hand side) to navigate each file. 


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
