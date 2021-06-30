==========================================
## Blue Whiting Spatial Forecast
==========================================

TODO:
* Observational data. There is clearly an issue with the Jan 2020 EN4 product that needs to be resolved. This may be fixed when the final version is released, or maybe not at all. Consider expanding set of observational datasources to include reanalyses? And potentially merging them using either a neural network, a random forest, average weighted by uncertainties or similar
  * Am thinking that a variance weighted average may be the way forward, with variance determined by comparison with the data from the survey itself
* Do a comparison between the CTD profiles from the IBWSS survey and the observational products at hand.
* Try to bring the Ellot Line data into this as well
* Implement (in PredEng) the CDO based approach to vertical averaging used here

Next Actions
* Check EN4 data in months prior

2020.03.26
* General updates to code over the last few days. Codebase needs a near complete rethink.
* Implemented new version of RCMEMS v 1.0.0 that works with modules. This was an improvement
* Implemented interfacing to PredEng

