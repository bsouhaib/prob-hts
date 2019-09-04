# Source code for the smart meters experiments of the paper *Hierarchical Probabilistic Forecasting of Electricity Demand with Smart Meter Data* by Ben Taieb, Souhaib, Taylor, James, and Hyndman, Rob.

The code computes all the results in Section 6, and produces all the figures in the paper.

# Usage

1. Downnload the following files: *edrp_elec.csv*, *edrp_metadata.xlsx* and *edrp_geography_data.xlsx* from https://discover.ukdataservice.ac.uk/catalogue/?sn=7591 (registration required)
2. Save these files in "../rawdata"
3. Apply a preprocessing step and save the results in new Rdata files (slow)
```
source("init_1_raw_meters.R")
source("init_2_myHierarchy.R")
source("init_3_bottom_meters.R")
source("init_4_aggregated_meters.R")
```
3. Specify the scenario for the bottom-level forecasts (see run_basef.sh) and run the following script (slow):
```
./run_basef.sh
```
3. Specify the scenario for the aggregate-level forecasts (see run_basef.sh) and run the script again (slow):
```
./run_basef.sh
```
4. Compute the covariance matrix W for the MinT method 
```
source("MinT_ecov.R")
```
5. Compute the permutations (empirical copulas)
```
source("permutations.R")
```
6. Merge the results by half hour
```
source("makebf_byhalfhour.R")
```
7. Compute the forecasts for the aggregated series (slow)
```
./run_aggregation.sh
```
8. Merge the aggregation results
```
source("aggregation_merge.R")
```
9. Produce the figures with all the accuracy measures
```
source("results.R")
```

To produce all the Figures, run the following scripts:
```
source("plot_calendar_effects.R")
source("plot_coherency.R")
source("plot_coverage.R")
source("plot_forecasts.R")
source("plot_parameters.R")
source("plot_series.R")
source("plot_tree.R")
```


