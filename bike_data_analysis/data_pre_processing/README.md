## DATA PRE-PROCESSING

__DIRECTORY STRUCTURE__

- osrm.R: script that computes the distances between bike stations.
- weather_data_processing.R: takes the weather data in the "raw_data" folder in the parent folder and polishes them.
- bike_sharing_data.R: downloads the bike sharing data from the website and performs all the necessary pre-processing steps. It calls "osrm.R" and "weather_data_processing.R" and creates a subfolder ("initial_data") in which it saves all the necessary intermediate data_structures.

__DIRECTORY USAGE__

Run the script "bike_sharing_data.R".
