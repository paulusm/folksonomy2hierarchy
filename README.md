# Hierarchical Taxonomy from a Folksonomy - R project

Implementation of Tibely et als(2013) Algorithm A for inferring a hierarchy from a tag network. This project uses tag data from the [https://archive.org/details/stackexchange](Stack Exchange Data Dump)

Process:

1. Extract data (munge/01-extractdata.R)
2. Find tag cooccurances from post metadata (munge/01-extractdata.R)
2. Create two way directed network (munge/01-A.R)
3. Prune by local weights (src/algorithm.R)
4. Calculate z-scores and retain highest scoring ancestor (src/algorithm.R)
5. Select a global root and reconnect other roots (src/algorithm.R)

This project uses [ProjectTemplate](http://projecttemplate.net). The data is not included in the repository but must be downloaded, the XML extracted, then placed in the data folder. To inially load the data, set the "munge" option to TRUE in the global settings (config/global.dcf)

Set the options in config/global.dcf, then run:

	library('ProjectTemplate')
	load.project()

.. to set up the project.

## Reference

Tibeĺy, G., Pollner, P., Vicsek, T., & Palla, G. (2013). Extracting tag hierarchies. PLoS ONE, 8(12), 1–46. https://doi.org/10.1371/journal.pone.0084133

Stack Exchange data is cc-by-sa 4.0 constributed by the site communities