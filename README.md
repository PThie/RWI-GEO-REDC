# RWI-GEO-REDC: Commercial Real Estate Data for Germany

This repository shows the detailed preparation and generation of the RWI-GEO-REDC dataset.

## Abstract Data Description (V3)

The FDZ Ruhr provides a dataset on commercial real estate advertisements in Germany, drawing from information obtained from the internet platform ImmoScout24. The dataset encompasses a wide range of real estate types, including but not limited to offices, retail, and hotels. It is important to note that the data are available for scientific research purposes only. The provided dataset offers detailed regional information and a comprehensive set of characteristics. This data report offers a concise overview of the data, its limitations, and specific details. The data report is intended for (potential) users of the data as support for their data preparation. 

The current version RWI-GEO-REDC V3, which covers data from 01/2010 to 12/2023. Note that the years 2021 and 2022 are currently not present in the data.

## Access

The data are available to researchers for non-commercial use. There are two versions of the RWI-GEO-REDC data. Firstly, the Scientific Use File (SUF) covers all information with the exception of exact geo-coordinates and other variables related to the address. Furthermore, some grid cells are anonymized due to the anonymization rules outlined in the [data report](https://www.rwi-essen.de/fileadmin/user_upload/RWI/FDZ/FDZ_Datensatzbeschreibung_REDC_v3.pdf). Secondly, the complete dataset is available in the Data Secure Room of the FDZ Ruhr in Essen (with on-site access). The data can be obtained in both .csv and .parquet file formats. It should be noted that data access to both versions requires a signed data use agreement. Both versions are restricted to non-commercial research; only researchers from scientific institutions are eligible to apply for data access. The SUF may be used at the workplace of the users.

Data access is provided by the Research Data Centre Ruhr at the RWI – Leibniz-Institute for Economic Research (FDZ Ruhr). The data can be accessed at [https://www.rwi-essen.de/en/research-advice/further/research-data-center-ruhr-fdz/data-access](https://www.rwi-essen.de/en/research-advice/further/research-data-center-ruhr-fdz/data-access). The application form includes a brief description and title of the project, potential cooperation, information on the applying department, expected duration of data usage as well as further participants in the project. 

Data users shall cite the datasets properly with the respective DOIs. The DOIs of the current version (V3) of the datesets are: 

**Scientific Use File (SUF):** [http://doi.org/10.7807/immo:redc:suf:v3](http://doi.org/10.7807/immo:redc:suf:v3)

**On-site dataset:** [http://doi.org/10.7807/immo:redc:v3](http://doi.org/10.7807/immo:redc:v3)

## More Information

- [General information on RWI-GEO-RED/C/X](https://www.rwi-essen.de/en/research-advice/further/research-data-center-ruhr-fdz/data-sets/rwi-geo-red/x-real-estate-data-and-price-indices)
- [Data report RWI-GEO-REDC V3](https://www.rwi-essen.de/fileadmin/user_upload/RWI/FDZ/FDZ_Datensatzbeschreibung_REDC_v3.pdf). Please cite the data report as: Thiel (2025), FDZ Data Description: Commercial Real-Estate Data for Germany (RWI-GEO-REDC V3) - Commercial advertisements on the Internet Platform ImmoScout24 01/2010-12/2023, RWI Projektberichte, Essen.

## DOI

- Repository for V3.0: [![DOI:10.5281/zenodo.15719891](http://img.shields.io/badge/DOI-10.5281/zenodo.15719891-048BC0.svg)](https://zenodo.org/account/settings/github/repository/PThie/RWI-GEO-REDC)
- RWI-GEO-REDC V3 (SUF): [http://doi.org/10.7807/immo:redc:suf:v3](http://doi.org/10.7807/immo:redc:suf:v3)
- RWI-GEO-REDC V3 (On-site): [http://doi.org/10.7807/immo:redc:v3](http://doi.org/10.7807/immo:redc:v3)

## Contact Person

Please contact [Dr. Patrick Thiel](https://www.rwi-essen.de/rwi/team/person/patrick-thiel) in case of questions.

## Disclaimer

All rights reserved to RWI and the author of the code, [Dr. Patrick Thiel](https://www.rwi-essen.de/rwi/team/person/patrick-thiel). In the case of used libraries, the main file ([_targets.R](https://github.com/PThie/RWI-GEO-REDC/blob/main/_targets.R)) should be consulted. For a comprehensive list, including direct dependencies, refer to the [renv.lock file](https://github.com/PThie/RWI-GEO-REDC/blob/main/renv.lock). Please note that the terms of conditions of each library apply.
