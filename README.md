# Transmission and unvaccinated-only testing in populations of mixed vaccination status
Github repo associated with the manuscript "Transmission and Unvaccinated-Only Testing in Populations of Mixed Vaccination Status"

## Code layout:

setup.R - loads packages, plotting functions and initializes all the default parameter settings.

main.R - calls set_up.R to initialize everything. Runs the model and generates the main text figures and table.

suppfig.R - runs the model and generates the supplemental figures.

helper_functions.R - defines functions used in main.R and suppfig.R.



## Notes:

-If you do not have fonts installed in R, see line 13 in set_up.R to configure this correctly. Else any line that specifies family="Arial" will return an error.

-Exporting a cairo_pdf embeds the text in figures correctly on Windows. But, if you are a Mac user, this file format may not be compatible. We recommend exporting a png instead.

-We recommend using the document outline to navigate


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
