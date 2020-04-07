# Global estimates of the benefit from COVID-19 interventions

We combine global-level mortality predictions from [Walker et al. (2020)](https://doi.org/10.25561/77735) from the Imperial College COVID-19 Response Team with estimates of the country-specific VSL from [Viscusi and Masterman (2017)](https://doi.org/10.1017/bca.2017.12) to estimate the welfare value of COVID-19 mitigation and suppression policies.

# Datasets included

We include a number of relevant datasets:

* [VSL.csv](data/vsl.csv) includes the country-specific VSL extracted from [Viscusi and Masterman (2017)](https://doi.org/10.1017/bca.2017.12). The code used to extract these estimates is provided in [read-vsl](R/create-data/readl-vsl.R) for completeness, but the `R` library [`tabulizer`](https://github.com/ropensci/tabulizer) has many dependencies and may not run well on your machine.
* The [mortality predictions](data/Imperial-Cllege-COVID19-Imperial-College-COVID19-Global-unmitigated-mitigated-suppression-scenarios.xlsx) from the Walker et al. (2020) paper are also provided in their original Excel format. These were downloaded from [this url](https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-Global-unmitigated-mitigated-suppression-scenarios.xlsx) and are current as of April 2, 2020, but are likely to be revised.
* A [list of OECD countries](data/oecd-countries.RDS)

# Datasets created

* World Bank data is extracted using the file [read-wb.R](R/create-data/read-wb.R), which makes use of the `R` library `wbstats` and produces the file `wb_data.csv`.
* Mortality predictions from Walker et al. (2020) are extracted using [read-mortality.R](R/create-data/read-mortality.R). We combine them with the VSL estimates and the World Bank data. This produces an `R` data storage file `mortality-estimates` which is used to generate the figures

# Results

The [Walker et al. (2020)](https://doi.org/10.25561/77735) use a base R0 value of 3.0 for their mortality predictions, so we restrict our estimates to the use of this value.

## Demographic Data

The World Bank provides estimates of the [percent population above the age of 65](https://data.worldbank.org/indicator/sp.pop.65up.to.zs) and the percentage of the workforce that is is [self- or informally-employed](https://data.worldbank.org/indicator/sl.emp.vuln.zs) (defined by the ILO as "insecure"[^1]).

## Mortality estimates

Total deaths and total population are taken from Walker et al. (2020).

## VSL estimates

For income blocs we use the population-weighted average VSL.





[^1]: From the ILO: "A measure of what are deemed to be the more vulnerable statuses of employment, namely own-account workers and contributing family workers. The vulnerable employment rate is calculated as the sum of own account and contributing family workers as a proportion of total employment: 
Vulnerable employment rate = [(number of own-account workers + number of contributing family workers) \(\div \) total employment] \(\times\) 100



