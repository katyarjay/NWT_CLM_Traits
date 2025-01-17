# NWT_CLM_Traits
This repository containts scripts that generate input data for running single-point Community Land Model (CLM) simulations at Niwot Ridge and analyze CLM outputs. 

The "input_data" folder contains two scripts that prepare input data needed to run CLM. tvan_L1_preprocess.R downloads and preprocesses data from the Tvan flux tower, and prepare_forcings_for_clm.R gapfills and collates data needed to run CLM. See the repository https://github.com/NWTlter/NWT_CLM for additional details and code updates.

The "analysis" folder contains scripts that access CLM output data and Niwot Ridge observations and create figures for the manuscript "Plant functional trait uncertainty outweighs climate change uncertainty in tundra ecosystems." NWT_hillslope_paper-traits-Figs.ipynb creates Figures 3-5, S1, and S2 and NWT_traits_AnnFluxes-Fig.ipynb creates Figure 6.
