This folder contains the data and R scripts (code) needed to replicate the calculations and analyses performed in Heumann et al. 2024 "Rates and controls of nitrogen fixation in post-fire lodgepole pine forests". 

SCRIPTS:
There are three scripts contained in this repository:
1. Heumann_et.al.2024_field_flux_calcs contains the code needed to reproduce all the N fixation calculations used in this paper starting with raw ethylene production data and mass/area estimates of each cryptic niche studied. 

2. Heumann_et.al.2024_field_flux_figs_analyses contains the script needed to reproduce all the figures and statistical analyses used in this study with the data calculated in Heumann_et.al.2024_field_flux_calcs. 

3. 3_Heumann_et.al.2024_Climate_exp_analyses contains the script need to reproduce all the figures and analyses used for the climate experiment portion of the study starting from raw ethylene production data from those experiments. 

DATA:
There are 9 data files in this repository located in the Data folder. Those are:
1. revised_2022_season_fluxes_05_24 contains the raw ethylene production data from field measurements throughout each season to calculate N fixation rates by niche. This dataset is in CSV format with columns representing the following:
	season: the season during which the measurements were made (fall = October measurements, spring = May measurements, summer = August measurements, 		summer_wet = August after a brief rain event measurements)	
	site: The site (50 x 50 m plot) in which samples were collected and measurements made	
	niche: cryptic niche	
	quad: quadrant in which sample was collected within the site (this essentially is a unique identifier as multiple samples were collected from each 		quadrant in some cases). This is not an important variable in scaling up the measurements. 	
	blank_corrected_ppm: this is the blank corrected ppm concentration of the sample measured. Blank corrected meaning the concentration of 			abiotically produce ethylene from samples incubated without acetylene and from samples of the acetylene gas used were subtracted from the 		ethylene concentration of the sample measured. 	
	dry_mass: dry mass of the sample in grams	
	sample_area_cm2: the area of our sample collection tubes
	sample_depth_cm: the depth the sample was collected to (only applies to soil)
	moist: the gravimetric moisture content of the sample as measured as the % of the dry mass that is water. 

2. GYE22_UnderstoryMossLichen: contains the % cover within each plot of either moss or lichen crust based on transect surveys of moss and lichen cover in 0.25-m2 quadrats (see methods). This is used to scale up the moss and lichen N fixation data. 

3. GYE 2022 Litter mass: Contains the litter mass data for each plot used to scale up litter N fixation rates. These data are based on litter collected in 900cm2 quadrats in each plot and dried (see methods).  

4. GYE22_CoarseWoodyDebrisBiomass: Contains the coarse woody debris biomass data for each plot used to scale up wood N fixation data. These are based on modified browns transects of class 4 and 5 coarse woody debris in Megagrams from each plot (see methods).

5. c2h4_rates_6_2024 is a cleaned dataset of the raw ethylene production rates across season created in the Heumann_et.al.2024_field_flux_calcs script then used for further analysis in Heumann_et.al.2024_field_flux_figs_analyses

The following three files are created in Heumann_et.al.2024_field_flux_calcs to represent the N fixation rates of each niche under the three different R ratios from Soper et al. 2021
6. geomean_season_flux_calcs_6_2024.csv" - using the niche specific geometric mean
7. high_end_season_flux_calcs_6_2024.csv" - using the niche specific minimum R ratio (lower R ratio = bigger N fixation rate - high end of range)
8. low_end_season_flux_calcs_6_2024.csv" - using the niche specific maximum R ratio (bigger R ratio = lower N fixation rate - low end of range)

9. final_clim_exp_fluxes: Contains the raw ethylene production data for both rounds of climate experiments (1 and 2). Columns are described below:
	round: 1 or 2 - 1 all five niches, 2 focusing on heterotrophs	
	niche: cryptic niche of sample
	temp: Temperature treatment in C	
	moist_level: targeted moisture content	
	sample_no: sample number in that treatment	
	dry_mass: dry mass in grams	
	actual_moist_perc: the actual gravimetrically determined moisture content of sample
	flux_nmol_C2H4_g_hr: the measured flux of nanomols of ethylene per gram dry mass per hour

TO RUN THE SCRIPTS:
1. Clone the repository: git clone https://github.com/robbieheumann/Heumann-et-al.-2024-Rates-and-controls-of-nitrogen-fixation-in-post-fire-lodgepole-pine-forests.git

2. Open R studio and set working directory to the clone repository

3. Run scripts ensuring outputs consistent with those reported in the paper. 
	

 
