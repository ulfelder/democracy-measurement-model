# Democracy Measurement Model

This respository houses replication materials for "A measurement error model of democracy status" by Jay Ulfelder and Sean J. Taylor. The key task is to efficiently combine experts' annual assessments of countries' democratic status using a Bayesian model. The country-year data set produced by these models is:

`data.out/democracy.scores.merged.csv`.

This data set covers the period 1945-2013, but we recommend that researchers truncate it to 1955-2010, when we have "votes" (labels) from at least three of the five contributing data sets and therefore get more reliable estimates of the probability that a country's national political regime is a democracy. The columns `all`, `four`, and `three` can be used to subset the data to periods when labels are available from that many of the contributing sources.

We generate two versions of our probabilistic measure of democracy status and their 95% credible intervals:

* One that imposes very little structure on the inference problem: `nomod.p`, `nomod.lcl`, `nomod.ucl`; and
* Another that assumes some autocorrelation with country series while still allowing for sudden shifts: `auto.p`, `auto.lcl`, `auto.ucl`. 

The data set also includes the binary indicators of democracy status from the five source data sets:

* Cheibub, Gandhi, and Vreeland's Democracy and Dictatorship Revisited (`d.dd`)
* Boix, Miller, and Rosato's Complete Dataset of Political Regimes (`d.bmr`)
* Ulfelder's Democracy/Autocracy Dataset (`d.dad`)
* Freedom House's list of electoral democracies (`d.fh`)
* The Political Instability Task Force's Polity-based indicator (`d.pitf`)

Finally, the data set also includes Pemstein et al.'s Unified Democracy Scores (`uds.mean` and its related measures), to which we compare and contrast our results.

## Directory Structure

 * `data.in` - Input data and papers
 * `data.out` - Bayesian measurements of democracy and expert equality
 * `py` - Python version of model fitting using `pystan`
 * `R` - R scripts for fitting and plotting
 * `stan` - Stan model files using different generative models for democracy
 * `docs` - Some writing we've done so far.
