# Mar.fleets

<!-- badges: start -->
![R Package](https://img.shields.io/badge/R-package-blue)
![GitHub last commit](https://img.shields.io/github/last-commit/Maritimes/Mar.fleets)
<!-- badges: end -->

**BREAKING CHANGES in Latest Version**

**Major changes that will impact all users:**

1. **Oracle connections required** - Pass an existing oracle connection (`cxn`) instead of username/password
2. **Data encryption** - Protected B data is now encrypted when extracted

**Why these changes?** See [NEWS.md](NEWS.md) for detailed explanation.

---

## Overview

An R package designed specifically for Maritimes fisheries assessment biologists to link commercial catch data (MARFIS) with detailed observer data (ISDB) for fleet-specific analysis.

**The Problem:** Linking commercial catch records with observer data is relatively complex.  "Fleets" are not always easily defined, the two databases are not inherently connected, and it is often imperative to know which commercial trips are associated with which observed trips to correctly prorate catch data.

**The Solution:** Mar.fleets automatically identifies fleet vessels and links their commercial activity with corresponding observer trips, providing assessment-ready datasets that combine the breadth of commercial data with the detail of observer data.

## Key Features

- **Pre-configured fleet definitions** - No need to define complex gear/license/area combinations
- **Automated data linking** - Matches MARFIS and ISDB records using multiple criteria (licenses, VRNs, dates, areas)
- **Comprehensive output** - Returns all associated data in a structured, analysis-ready format
- **Built-in QC** - Includes diagnostics and match details for validation

## Documentation

For detailed technical documentation and methodology, see: [Mar.fleets Technical Guide](https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/41019763.pdf)
 

## Installation

```r
# Install from GitHub
library(devtools)
install_github('Maritimes/Mar.fleets')
```

## Quick Start

```r
library(Mar.fleets)

# 1. Establish Oracle connection (NEW REQUIREMENT)
cxn <- ROracle::dbConnect(DBI::dbDriver("Oracle"), 
                         "oracle.username", "oracle.password", "PTRAN")

# 2. Extract fleet data for analysis period
redfish_data <- fleet_redfish(dateStart = "2020-01-01", 
                              dateEnd = "2020-12-31", 
                              cxn = cxn)

# 3. Explore the results
names(redfish_data)  # See available data components
summary(redfish_data$ISDB_CATCHES)  # Observer catch summary
summary(redfish_data$MARFIS_CATCHES)  # Commercial catch summary
```

## Available Fleet Wrappers

The package includes pre-configured wrappers for major Maritimes fleets:

| Function | Target Fleet | Key Gear Types |
|----------|--------------|----------------|
| `fleet_halibut()` | Atlantic Halibut | Longline |
| `fleet_redfish()` | Redfish | Bottom trawl |
| `fleet_haddock()` | Haddock | Bottom trawl |
| `fleet_cod()` | Atlantic Cod | Multiple gear types |
| `fleet_pollock()` | Pollock | Bottom trawl |
| `fleet_yellowtail()` | Yellowtail Flounder | Bottom trawl |
| `fleet_plaice()` | American Plaice | Bottom trawl |
| `fleet_witch()` | Witch Flounder | Bottom trawl |
| `fleet_winter()` | Winter Flounder | Bottom trawl |
| `fleet_mackerel()` | Atlantic Mackerel | Purse seine, gillnet |
| `fleet_herring()` | Atlantic Herring | Purse seine, weir |
| `fleet_scallop()` | Sea Scallop | Scallop dredge |
| `fleet_lobster()` | American Lobster | Lobster trap |
| `fleet_crab()` | Snow Crab | Crab trap |
| `fleet_shrimp()` | Northern Shrimp | Shrimp trawl |
| `fleet_monkfish()` | Monkfish | Gillnet, longline |
| `fleet_skate()` | Skate | Bottom trawl |
| `fleet_turbot()` | Greenland Halibut | Gillnet |
| `fleet_urchin()` | Sea Urchin | Diving |
| `fleet_whelk()` | Waved Whelk | Trap |
| `fishin_CHPS()` | Combined Halibut/Pollock/Skate | Mixed groundfish |

## What You Get Back

Each fleet wrapper returns a comprehensive nested list containing:

```r
# Example structure from fleet_redfish()
str(redfish_data, max.level = 1)
```

**Components include:**

- **`$params`** - Parameters used in the extraction
- **`$fleet_members`** - Identified fleet vessels and their activity
- **`$MARFIS_TRIPS`** - Commercial trip records
- **`$MARFIS_SETS`** - Commercial fishing set details  
- **`$MARFIS_CATCHES`** - Commercial catch records
- **`$MARF_MATCH`** - Additional MARFIS data used for matching
- **`$ISDB_TRIPS`** - Observer trip records
- **`$ISDB_SETS`** - Observer set details
- **`$ISDB_CATCHES`** - Observer catch records (with biological details)
- **`$area_summary`** - Spatial distribution summary by NAFO/custom areas
- **`$match_details`** - Documentation of how MARFIS and ISDB records were linked
- **`$debug`** - Diagnostic information about excluded records (optional)

## Basic Workflow

```r
# Extract halibut fleet data for 2023
cxn <- ROracle::dbConnect(DBI::dbDriver("Oracle"), 
                         "username", "password", "PTRAN")

halibut_2023 <- fleet_halibut(dateStart = "2023-01-01", 
                              dateEnd = "2023-12-31", 
                              cxn = cxn)

# Examine fleet composition
unique(halibut_2023$fleet_members$VR_NUMBER)  # Vessel numbers
table(halibut_2023$fleet_members$LICENCE_TYPE)  # License types

# Compare commercial vs observer catches
commercial_halibut <- sum(halibut_2023$MARFIS_CATCHES$RND_WEIGHT_KGS, na.rm = TRUE)
observed_halibut <- sum(halibut_2023$ISDB_CATCHES$EST_KEPT_WT, na.rm = TRUE)

# Analyze species composition from observer data
species_comp <- aggregate(EST_KEPT_WT ~ SPECIES, 
                         data = halibut_2023$ISDB_CATCHES, 
                         FUN = sum, na.rm = TRUE)

# Examine spatial distribution
table(halibut_2023$area_summary$NAFO_UNIT_AREA)
```

## Advanced Usage

### Custom Areas
```r
# Include custom spatial areas in analysis
fleet_cod(dateStart = "2023-01-01", 
          dateEnd = "2023-12-31",
          areas = c("4VsW", "4X"),  # Custom NAFO subdivisions
          cxn = cxn)
```

### Debugging and QC
```r
# Enable debug output to understand exclusions
redfish_debug <- fleet_redfish(dateStart = "2023-01-01", 
                               dateEnd = "2023-12-31",
                               debug = TRUE,
                               cxn = cxn)

# Examine why records were excluded
redfish_debug$debug$excluded_trips
redfish_debug$debug$excluded_vessels
```

### Working with Results
```r
# Combine commercial and observer data for analysis
all_sets <- rbind(
  transform(cod_data$MARFIS_SETS, source = "commercial"),
  transform(cod_data$ISDB_SETS, source = "observer")
)

# Calculate observer coverage rates
coverage <- nrow(cod_data$ISDB_SETS) / nrow(cod_data$MARFIS_SETS) * 100
cat("Observer coverage:", round(coverage, 1), "%\n")
```

## Migration Guide (Breaking Changes)

**Old way:**
```r
fleet_redfish(dateStart = "2023-01-01", dateEnd = "2023-12-31",
              username = "myuser", password = "mypass", dsn="PTRAN", usepkg="roracle", data.dir="c:/mydata")
```

**New way:**
```r
cxn <- ROracle::dbConnect(DBI::dbDriver("Oracle"), 
                         "myuser", "mypass", "PTRAN")
fleet_redfish(dateStart = "2023-01-01", dateEnd = "2023-12-31", cxn = cxn)
```

## Requirements

- R (tested with 3.5+)
- Oracle database access with appropriate permissions for MARFIS and ISDB
- ROracle package for database connectivity
- Valid DFO credentials for accessing fisheries databases

## Related Packages

- [Mar.datawrangling](https://github.com/Maritimes/Mar.datawrangling) - General database extraction
- [Mar.utils](https://github.com/Maritimes/Mar.utils) - Utility functions  

## Getting Help

- Check [NEWS.md](NEWS.md) for recent changes
- Contact: mike.mcmahon@dfo-mpo.gc.ca
- Report issues: [GitHub Issues](https://github.com/Maritimes/Mar.fleets/issues)

---

*Linking commercial and observer data for Maritimes fleet analysis*
