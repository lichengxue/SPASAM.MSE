# SPASAM.MSE  
**Spatial Processes and Stock Assessment Methods – Management Strategy Evaluation**

<p align="center">
  <a href="https://lichengxue.github.io/SPASAM.MSE/"><b>🌐 SPASAM.MSE Website</b></a> &nbsp;•&nbsp;
  <a href="https://github.com/lichengxue/SPASAM.MSE"><b>💻 GitHub Repo</b></a> &nbsp;•&nbsp;
  <a href="https://timjmiller.github.io/wham/"><b>⚙️ WHAM</b></a> &nbsp;•&nbsp;
  <a href="mailto:chengxue.li@stonybrook.edu"><b>✉️ Contact</b></a>
</p>

---

## Why SPASAM.MSE?

**SPASAM.MSE** is a **spatially explicit, closed-loop Management Strategy Evaluation (MSE)** framework built on **WHAM** (Woods Hole Assessment Model). It is designed to test how **spatial heterogeneity**—driven by **population biology, fishing dynamics, and movement/connectivity**—can shape:

- **stock assessment performance** (bias, precision, robustness under misspecification)  
- **management outcomes** (catch, biomass, fishing mortality)  
- **risk** (overfishing / overfished probabilities)  
- **spatial trade-offs** across regions and stocks  

At its core, SPASAM.MSE links the full end-to-end pipeline:

> **Operating model → data generation → estimation model → projections → harvest control rules → implementation error → feedback**

By leveraging WHAM’s **state-space platform**, SPASAM.MSE can treat key **biological and fishery processes** as **random effects** (as configured) in both **operating** and **estimation** models—making it ideal for controlled experiments on **model misspecification**, **spatial structure**, and **management robustness**.

---

## What you can do with SPASAM.MSE

### 1) Spatial Operating Models (OM)
- **Multi-stock, multi-region** population structure  
- Spatial heterogeneity in **biology** and **fishing dynamics**  
- Explicit **connectivity among regions** via movement  

### 2) Movement & Connectivity Scenarios
- **Bidirectional** and **unidirectional** movement  
- **Metapopulation** and **natal homing** dynamics  
- **Age- and/or year-varying** movement  
- Monotonic movement patterns (e.g., increasing/decreasing connectivity through time)  
- Optional **environmental covariate** effects on movement  
- Movement parameterized with **fixed or random effects** across age, year, season, stock, and region  

### 3) Observation Models & Data Generation
- Fishery-dependent data (e.g., catch, effort, composition)  
- Fishery-independent data (e.g., survey indices and composition)  
- Configurable observation error and sampling design to explore:
  - data quality & coverage  
  - spatial sampling structure  
  - fleet / survey configurations  

### 4) Estimation Models (EM)
- Fit **spatial** or **non-spatial** estimation models to simulated data  
- Deliberately test robustness under **misspecification** (movement, spatial structure, process error assumptions)  
- Optional aggregation to match EM spatial resolution (e.g., OM regions → coarser EM)  
- Generates assessment outputs used for management advice, projections, and evaluation  

### 5) Closed-Loop MSE
- Full feedback loop with flexible controls for:
  - projection length & assessment update frequency  
  - averaging windows for biological/fishery inputs  
  - harvest control rules (e.g., **F₄₀%SPR**, **FMSY**, hockey-stick rules)  
  - fishing mortality or catch specification during projection  
  - propagation of recruitment/process/movement uncertainty  
  - optional continuation of random effects and environmental covariates  

**Implementation uncertainty** supported (e.g., deviations between advised and realized catch/F).

### 6) Performance Evaluation & Reporting
- Bias/precision for **SSB, F, recruitment, catch**  
- Probability-based metrics (overfishing / overfished)  
- Spatial trade-offs across regions/stocks  
- Automated reporting outputs (**PNG / HTML / PDF**)  

---

## State-Space & Process-Error Framework

SPASAM.MSE leverages WHAM’s state-space design to represent processes as fixed or random effects (as configured) in both OM and EM, enabling realistic uncertainty propagation and controlled tests of misspecification.

WHAM-based random effects can be configured for:  
- Recruitment and numbers-at-age  
- Fishing selectivity  
- Natural mortality  
- Fishing mortality  
- Survey catchability  
- Movement  

---

## Relationship to WHAM

### WHAM: Woods Hole Assessment Model
**WHAM** is a general **state-space, age-structured stock assessment framework** designed to incorporate environmental effects and time- and/or age-varying process error. WHAM is actively developed and applied in research and assessment contexts at **NOAA NEFSC**.

WHAM can be configured as:  
- SCAA with recruitment as fixed effects  
- SCAA with recruitment as random effects  
- Fully state-space models with abundance-at-age as random effects  

**WHAM resources**  
- Website & documentation: https://timjmiller.github.io/wham/  
- Vignettes: https://timjmiller.github.io/wham/articles  
- Overview presentation (Jan 8, 2021): https://www.youtube.com/watch?v=o8vJvbIaOdE  

SPASAM.MSE extends WHAM by embedding it within a **spatially explicit MSE framework**, enabling systematic and reproducible testing of spatial assessment and management strategies under controlled simulation experiments.

---

## Documentation & Examples

👉 **Project website (docs & vignettes):** https://lichengxue.github.io/SPASAM.MSE

---

## Installation

Install the development version from GitHub:

```r
remotes::install_github("lichengxue/SPASAM.MSE", dependencies = TRUE)
