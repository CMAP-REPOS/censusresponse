# Analysis of 2020 census self-response rates in CMAP region

This repo contains code used in the production of [this policy update], which analyzes current self-response rates to the US Census. The [script](script.R) was developed and run in the open-source programming language R. As part of the analysis, we also relied on the following sources:
- Current self-response rates from the US Census Bureau, which we pulled directly via the Census Bureau's API. These data are also presented in visual form, [here](https://2020census.gov/en/response-rates.html).
- Planned shapefiles for the 2020 Census, [here](https://data.world/uscensusbureau/2020-census-response-rates/workspace). The Census Bureau has reported self-response data for both 2010 and 2020 using these new tract definitions, and so we also relied on the Bureau's ["crosswalk"](Data\rr_tract_rel.txt) file to assign demographics based on 2010 tracts into these new geographies.
- A [list](Data\County_MSA_Crosswalk.csv) of counties included in each of the USA's Metropolitan Statistical Areas, which we obtained from the Bureau of Economic Analysis, [here](https://apps.bea.gov/regional/docs/msalist.cfm).

We have also provided supplemental [scripts](supplemental_script.R) used in the development of the piece. These include consistency checks and a script that allows a more targeted data pull for the Chicago region at the tract- and municipality-level.

In addition to the visualizations included in the published policy update, we are making the full outputs and results of our analysis available for those interested in leveraging or extending upon them. Those data, including response rates and demographic information, are provided at both the Census Tract and Incoporated Place level. All outputs are available for download [here](Output).
