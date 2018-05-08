### Documentation for `src` scripts

This readme file provides documentation of the scripts that are housed within the `src` directory. A full listing of the scripts is provided as well as a  brief overview as to what they do.

1. `get_data.R` -- A R script to download the necessary raw data files from stable URLs. The data files are downloaded to the `raw` folder and subsequently unzipped there.
2. `process_data.R` -- A R script to load, reproject to EPSG:102003, and process (clip) the counties and SoVI data. The processed data is used in the subsequent analyses. Data is input from `raw` and output to `data`.
