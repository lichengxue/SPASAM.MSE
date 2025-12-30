# SPASAM.MSE  
**Spatial Processes and Stock Assessment Methods – Management Strategy Evaluation**

## Overview

**SPASAM.MSE** is a spatially explicit **Management Strategy Evaluation (MSE)** platform designed to evaluate fisheries management strategies under realistic spatial population structure, movement, and observation processes. The package provides an end-to-end, closed-loop MSE framework that links **operating models**, **data generation**, **estimation models**, **harvest control rules**, and **management implementation error**, enabling systematic evaluation of management performance and risk across spatially structured systems.

SPASAM.MSE is built on the **Woods Hole Assessment Model (WHAM)**, a **state-space, age-structured stock assessment model** developed and widely used at the NOAA Northeast Fisheries Science Center (NEFSC). By leveraging WHAM’s state-space formulation, SPASAM.MSE allows both **biological and fishery processes** to be treated as **random effects** in *both* the operating model and the estimation model. This design enables controlled testing of model misspecification, robustness, and spatial management performance under complex spatial dynamics.

---

## Key Capabilities

### Spatially Explicit Operating Models
- Multi-stock, multi-region population structure  
- Spatial heterogeneity in biological processes (recruitment, natural mortality, selectivity)  
- Spatial heterogeneity in fishing dynamics (region-specific exploitation, fleets-as-areas, selectivity, effort allocation)  
- Explicit **connectivity** linking regions  

### Movement and Connectivity Scenarios
- Bidirectional vs unidirectional movement  
- Natal homing and source–sink dynamics  
- Time-varying movement trends  
- Optional environmental covariate effects on movement  
- Movement specified by **age, year, season, stock, and region**, using fixed or random effects  

### State-Space Framework (Operating and Estimation Models)
- WHAM-based state-space design  
- Time- and/or age-varying random effects on:
  - Recruitment / numbers-at-age  
  - Selectivity  
  - Natural mortality  
  - Fishing mortality  
- Consistent process-error structures in both operating and estimation models  
- Natural propagation of uncertainty into projections and management advice  

### Closed-Loop Management Strategy Evaluation
- Full feedback loop:  
  **Operating model → data generation → estimation model → projections → harvest control rules → implementation error → feedback**
- Supports fishery-dependent and fishery-independent data streams  
- Designed for large-scale simulation experiments across scenarios and replicates  

### Performance Evaluation and Reporting
- Bias and precision of key quantities (SSB, F, recruitment, catch)  
- Probability-based status metrics (overfishing, overfished)  
- Spatial trade-offs in catch, biomass, and fishing mortality  
- Automated plotting and reporting tools (PNG / HTML / PDF)  

---

## Relationship to WHAM

### WHAM: Woods Hole Assessment Model

The **Woods Hole Assessment Model (WHAM)** is a general **state-space, age-structured stock assessment framework** designed to incorporate environmental effects and time-varying process error in population dynamics. WHAM is actively developed and used in assessment and research applications at **NOAA Northeast Fisheries Science Center (NEFSC)**.

WHAM can be configured as:
- Statistical catch-at-age (SCAA) models with recruitment as fixed effects  
- SCAA models with recruitment as random effects  
- Fully state-space models where abundance at all ages are random effects  

WHAM enables constrained random effects on:
- Recruitment / numbers-at-age  
- Selectivity  
- Natural mortality  
- Environmental effects on the above  

A key advantage of WHAM’s state-space formulation is that uncertainty in biological and environmental processes is **naturally propagated into projections and forecasts**.

**WHAM website and documentation:**  
https://timjmiller.github.io/wham/

**WHAM vignettes:**  
https://timjmiller.github.io/wham/articles

**Overview presentation (Jan 8, 2021):**  
https://www.youtube.com/watch?v=o8vJvbIaOdE

SPASAM.MSE extends WHAM by embedding it within a **spatially explicit MSE framework**, enabling systematic testing of spatial assessment and management strategies under controlled, reproducible simulation experiments.

---

## Documentation and Vignettes

We recommend starting with the SPASAM.MSE vignettes to understand the workflow, data structures, and scenario design:

https://lichengxue.github.io/SPASAM.MSE

---

## Installation

Install the development version of SPASAM.MSE from GitHub:

```r
remotes::install_github("lichengxue/SPASAM.MSE")
```

WHAM must be installed separately:

```r
install.packages("wham")
```

---

## Intended Use

SPASAM.MSE is designed primarily for **research, method development, and management strategy testing**, including:
- Evaluation of spatial stock assessment model misspecification  
- Comparison of spatial vs non-spatial assessment approaches  
- Testing robustness of harvest control rules under movement and connectivity  
- Exploration of source–sink and natal homing dynamics  
- Climate- and environment-driven scenario testing  

---

## Disclaimer

This package is a **research tool** intended for simulation and methodological evaluation. Results generated using SPASAM.MSE should not be interpreted as official stock assessments or management advice without appropriate review.

---

## Author

Chengxue Li  
Research Scientist, Stony Brook University  
Affiliated Scientist, NOAA Northeast Fisheries Science Center

---
