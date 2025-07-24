# Code for *Global patterns and drivers of untracked industrial fishing in coastal marine protected areas*

This repository contains the code and workflows used for the analyses and figures presented in the paper:

**Seguin, R., Le Manach, F., Devillers, R., Velez, L., Mouillot, D. (2025).  
Global patterns and drivers of untracked industrial fishing in coastal marine protected areas. *Science*, July 24, 2025.**

---

## Overview

The code here reproduces:
- Cleaning and analysis of tracked and untracked industrial fishing vessels using AIS and SAR data from Global Fishing Watch. 
- Statistical models (spaMM) estimating drivers of industrial fishing vessel detections within MPAs. 
- Statistical models estimating untracked fishing effort within MPAs. 
- Figures and results included in the publication.

---

## Repository Structure

├── R/ #All functions and code used in the analysis
└── README.md
└── make.R #Main script from which all functions are called 

## Data requirements

To download or find information about the data used in this study and its limitations: 

- detections of tracked and untracked industrial fishing vessels, as well as AIS-derived fishing effort, can be found and downloaded on Global fishing watch @ https://globalfishingwatch.org/
- Boundaries of self declared marine protected areas can be found and downloaded on the World Database on Protected Areas @ https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA
- All covariates used in the model can be downloaded following the links in the supplementary materials of the study

## 
