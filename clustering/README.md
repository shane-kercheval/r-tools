# clustering
Collection of methods to assist in performing/analyzing clustering techniques.

## Visualization

Methods to produce, for example:

Dendograms:
![Dendograms](../readme/dendogram.png)

Heatmaps that show scaled clustering averages:
![Heatmaps](../readme/kmeans_5_clusters.png)

# Note
NA values are automatically omitted with these clustering methods, so be conscious about cleaning your data;
- for example, pre-clustering your data such (e.g. could split up data between paid v. non-paying customers if you have a lot of data with NAs for non-paying customers)
- or converting columns with NAs (such as a time column `time_to_paid`) to a TRUE/FASLE column (e.g. `is_paying`)
