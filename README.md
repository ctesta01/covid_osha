
# OSHA Complaints and COVID-19

This repository stores the code which was used to render figures and analyses
for the *COVID-19: US Federal accountability for entry, spread, and
inequities—lessons for the future* manuscript. 

**Authors:** William P. Hanage<sup>1</sup>, Christian Testa<sup>2</sup>, Jarvis T. Chen<sup>2</sup>, Letitia Davis<sup>3</sup>, Elise Pechter<sup>3</sup>, Peg Seminario<sup>4</sup> Mauricio Santillana<sup>5</sup> and Nancy Krieger<sup>2</sup>

1. Center for Communicable Disease Dynamics, Department of Epidemiology, Harvard T. H. Chan School of Public Health, Boston, MA USA.

2. Department of Social and Behavioral Sciences, Harvard T.H. Chan School of Public Health, Boston, MA USA.

3. Consultant, Boston, MA USA.

4. AFL-CIO (retired), Washington DC USA.

5. Computational Health Informatics Program, Boston Children’s Hospital, Boston, MA USA.

---

The first two figures are produced by the scripts in `inst/figures/`.  

The R package contained herein standardizes the methods to load the OSHA complaints
and clean the data.


### Figure 1. National OSHA complaints and COVID—19 deaths per million (7 Day Average), January 16-September 18, 2020 

![Figure 1 shows the OSHA complaints and death rate time series for the United States at large and each of the major US regions (west, midwest, south, northeast). The figure shows how the COVID-19 death rates increased following an increase in OSHA complaints. Each respective region's correlation after accounting for the lag between complaints and deaths is presented.](inst/figures/regional_complaints_and_deaths/full_plot.png)


### Figure 2. Heatmaps showing the lagged correlations between OSHA complaint volume and COVID-19 cases and COVID-19 deaths, nationally and by US region, January 16, 2020 – September 30, 2020.

![Figure 2 shows the correlations between COVID-19 cases and deaths with OSHA complaints volume after accounting for different lag times in days between the two ranging from -28 to 28 days](inst/figures/osha_covid_correlations_regional/five_panel_correlations.png)


