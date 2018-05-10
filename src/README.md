### Documentation for `src` scripts

This readme file provides documentation of the scripts that are housed within the `src` directory. A full listing of the scripts is provided as well as a  brief overview as to what they do.

1. `get_data.R` -- A R script to download the necessary raw data files from stable URLs. The data files are downloaded to the `raw` folder and subsequently unzipped there.
2. `process_data.R` -- A R script to load, reproject to EPSG:102003, and process (clip) the counties and SoVI data. The processed data is used in the subsequent analyses. Data is input from `raw` and output to `data`.
3. `prep_container.R` -- A R script to install required R packages additional to those held within the `rocker/geospatial:3.4.2` image.
4. `helper_funcs.R` -- A library of functions to help process and analyze the SoVI data based on different methods.
    * `summarize_data()` -- A function to summarize data within a given buffer of a point. Used by both the `generate_aa()` and `generate_bi()` functions.
    * `sf_to_df()` -- A simple function that converts a `sf` object to a `df` object. Need to write geometry as NULL in order to do so, hence the function to remove additional step.
    * `generate_uhc()` -- A function to generate data based on the unit-hazard coincidence method
    * `generate_bi()` -- A function to generate data based on the buffered intersection method
    * `generate_aa()` -- A function to generate data based on the areal apportionment method
    * `m3_dat()` -- A second function that is employed in `generate_aa()` given necessity of for loop over sites (to avoid weird spatial intersections for NPL sites within the same census blocks.)
    * `generate_all()` -- A driver function that employs the three method-specific generate functions and joins data into a single data frame.
