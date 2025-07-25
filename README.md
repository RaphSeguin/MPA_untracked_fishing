# Code for *Global patterns and drivers of untracked industrial fishing in coastal marine protected areas*

This repository contains the code and workflows used for the analyses and figures presented in the paper:
     
Raphael Seguin et al., Global patterns and drivers of untracked industrial fishing in coastal marine protected areas.Science389,396-401(2025).
DOI: 10.1126/science.ado9468

https://www.science.org/doi/10.1126/science.ado9468

---

## Overview

The code here reproduces:
- Cleaning and analysis of tracked and untracked industrial fishing vessels using AIS and SAR data from Global Fishing Watch. 
- Statistical models (spaMM) estimating drivers of industrial fishing vessel detections within MPAs. 
- Statistical models estimating untracked fishing effort within MPAs. 
- Figures and results included in the publication.

---

## Repository Structure

```
├── R/          # All functions and code used in the analysis
├── README.md   # Project description
└── make.R      # Main script from which all functions are called
```

## Data requirements

To download or find information about the data used in this study and its limitations: 

- detections of tracked and untracked industrial fishing vessels, as well as AIS-derived fishing effort, can be found and downloaded on Global fishing watch @ https://globalfishingwatch.org/
- Boundaries of self declared marine protected areas can be found and downloaded on the World Database on Protected Areas @ https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA
- All covariates used in the model can be downloaded following the links in the supplementary materials of the study
  
## Limitations of Sentinel‑1 SAR Vessel Dataset

- **Incomplete global coverage**  
  Data only includes areas **sampled by Sentinel‑1 images**,so only coastal areas, meaning we miss most remote MPAs. 

- **Minimum vessel size threshold**  
  Reliable detection starts at vessels around **15 m in length** due to the resolutions of Sentinel-1 images.
  
- **Scene edge and shoreline buffering**  
  Detection areas exclude a **500 m buffer from scene edges** and a **1 km buffer from shorelines**, leading to potential under-sampling near land.

- **AIS matching uncertainty**  
  Matching SAR detections to AIS tracks is probabilistic and **error-prone**—some vessel identities may be missed or mismatched.
  
- **Fishing classification errors**  
  SAR vessel detections which are untracked **cannot provide vessel ID, flag, gear type, or activity**. Global Fishing Watch classify vessel detections as probable fishing vessels but **false positives/negatives occur**, especially in areas with high vessel density (commercial routes) or marine infrastructure. We therefore use a conservative threshold of fishing vessel classifications, keeping only vessels with a probablity of being a fishing vessel of 90% or higher.

