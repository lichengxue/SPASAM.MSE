# SPASAM.MSE  
**Spatial Processes and Stock Assessment Methods – Management Strategy Evaluation**

## Overview

**SPASAM.MSE** is a spatially explicit, closed-loop **Management Strategy Evaluation (MSE)** framework designed to evaluate fisheries management strategies under realistic spatial population structure, movement, and observation processes. The package provides an end-to-end workflow that links **operating models**, **data generation (observation models)**, **estimation models**, **projections**, **harvest control rules**, and **management implementation error**, enabling systematic evaluation of management performance and risk in spatially structured systems.

SPASAM.MSE is built on the **Woods Hole Assessment Model (WHAM)**, an **age-structured, state-space stock assessment model** developed and widely used at the NOAA Northeast Fisheries Science Center (NEFSC). By leveraging WHAM’s state-space formulation, SPASAM.MSE can treat key **biological and fishery processes** as **random effects** (as configured) in both the operating and estimation models, allowing controlled evaluation of model misspecification, robustness, and spatial management performance under complex spatial dynamics.

---

## Key Capabilities

### 1) Spatial Operating Models (OM)
- Multi-stock, multi-region population structure  
- Spatial heterogeneity in biological processes  
- Spatial heterogeneity in fishing dynamics  
- Explicit connectivity among regions via movement  

### 2) Movement and Connectivity Scenarios
- Bidirectional and unidirectional movement  
- Metapopulation and natal homing dynamics  
- Time- and/or age-varying movement  
- Monotonic movement patterns (e.g., increasing or decreasing connectivity over time)  
- Optional environmental covariate effects on movement  
- Movement parameterized using fixed or random effects across age, year, season, stock, and region  

### 3) Observation Models and Data Generation
- Fishery-dependent data generation (e.g., catch, effort, composition)  
- Fishery-independent data generation (e.g., survey indices and composition)  
- Configurable observation error for both fishery-dependent and fishery-independent data streams  
- Flexible sampling assumptions to explore data quality, coverage, and spatial sampling design  

### 4) Estimation Models (EM)
- Fit alternative estimation models to simulated data (spatial and non-spatial configurations)  
- Flexible model structure to test robustness under misspecification (e.g., movement, spatial structure, process-error assumptions)  
- Supports consistent or intentionally different configurations relative to the operating model  
- Optional data aggregation to match the spatial resolution of the estimation model (e.g., aggregating OM regional data to a coarser EM structure)  
- Generates assessment outputs used for management advice, projections, and performance evaluation  

### 5) Closed-Loop Management Strategy Evaluation (MSE)
- Full feedback loop:  

  **Operating model → data generation → estimation model → projections → harvest control rules → implementation error → feedback**

- Projection and management options:
  - User-defined projection lengths and assessment update frequencies  
  - Flexible averaging windows for biological and fishery inputs (e.g., weight-at-age, maturity, selectivity, natural mortality, movement)  
  - Harvest control rules based on biological reference points (e.g., F<sub>40%SPR</sub>, F<sub>MSY</sub>) or threshold-based (hockey-stick) control rules  
  - Flexible fishing mortality or catch specifications during projection  
  - Propagation of recruitment, process, and movement uncertainty into forecasts  
  - Optional continuation of random effects and environmental covariate processes  

- Management implementation error:
  - Implementation uncertainty in applying management advice (e.g., deviations between advised and realized catch or fishing mortality)  

- Realizations:
  - Supports large-scale simulation experiments across scenarios and stochastic replicates  

### 6) Performance Evaluation and Reporting
- Bias and precision of key quantities (SSB, F, recruitment, catch)  
- Probability-based status metrics (e.g., overfishing, overfished)  
- Spatial trade-offs in catch, biomass, and fishing mortality  
- Automated visualization and reporting tools (PNG / HTML / PDF)  

---

## State-Space and Process Error Framework (WHAM-based)

SPASAM.MSE leverages WHAM’s state-space structure, which can represent key processes as fixed effects or random effects (as configured) in both operating and estimation models. This enables realistic uncertainty propagation and controlled testing of misspecification.

- WHAM-based state-space design  
- Time- and/or age-varying random effects can be configured for:
  - Recruitment and numbers-at-age  
  - Fishing selectivity  
  - Natural mortality  
  - Fishing mortality  
  - Survey catchability
  - Movement
- Supports misspecified process-error structures between OM and EM  

---

## Relationship to WHAM

### WHAM: Woods Hole Assessment Model

The **Woods Hole Assessment Model (WHAM)** is a general **state-space, age-structured stock assessment framework** designed to incorporate environmental effects and time- and/or age-varying process error in population and fishery dynamics. WHAM is actively developed and applied in assessment and research contexts at the **NOAA Northeast Fisheries Science Center (NEFSC)**.

WHAM can be configured as:
- Statistical catch-at-age (SCAA) models with recruitment as fixed effects  
- SCAA models with recruitment as random effects  
- Fully state-space models in which abundance at all ages are treated as random effects  

WHAM supports constrained random effects on:
- Recruitment and numbers-at-age  
- Fishing selectivity  
- Natural mortality  
- Fishing mortality  
- Survey catchability
- Movement

WHAM also supports:
- Environmental covariate effects on the above processes such as recruitment, natural mortality, survey catchability, and movement.

**WHAM resources**  
- Website & documentation: https://timjmiller.github.io/wham/  
- Vignettes: https://timjmiller.github.io/wham/articles  
- Overview presentation (Jan 8, 2021): https://www.youtube.com/watch?v=o8vJvbIaOdE  

SPASAM.MSE extends WHAM by embedding it within a **spatially explicit MSE framework**, enabling systematic and reproducible testing of spatial stock assessment and management strategies under controlled simulation experiments.

---

## Documentation and Vignettes

Comprehensive documentation and worked examples are provided through SPASAM.MSE vignettes, which describe the workflow, data structures, and scenario design:

👉 https://lichengxue.github.io/SPASAM.MSE

---

## Installation

Install the development version of **SPASAM.MSE** from GitHub:

```r
remotes::install_github("lichengxue/SPASAM.MSE", dependencies = TRUE)
