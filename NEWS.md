# Mar.fleets NEWS

## Mar.fleets 2025.06.05

### BREAKING CHANGES

This release contains significant breaking changes that will impact all users. Please read carefully and update your code accordingly.

#### Database Connection Changes

* **REMOVED**: `username`, `password`, `dsn`, `usepkg` and `data.dir` parameters from all fleet wrapper functions
* **NEW REQUIREMENT**: Pass an existing, valid Oracle connection via `cxn` parameter
* **WHY**: Improves troubleshooting by separating connection issues from package issues
* **MIGRATION**: 
  ```r
  # Old way
  fleet_redfish(dateStart = "2023-01-01", dateEnd = "2023-12-31",
                username = "myuser", password = "mypass", dsn = "PTRAN", 
                usepkg = "roracle", data.dir = "c:/mydata")
  
  # New way  
  cxn <- ROracle::dbConnect(DBI::dbDriver("Oracle"), "myuser", "mypass", "PTRAN")
  fleet_redfish(dateStart = "2023-01-01", dateEnd = "2023-12-31", cxn = cxn)
  ```

#### Data Storage Changes

* **REMOVED**: `data.dir` parameter - data now stored in standardized locations
* **NEW**: Protected B data is now encrypted when extracted
* **IMPACT**: Cannot easily share extraction files with other users
* **WHY**: 
  - Prevents accidental data duplication and storage waste
  - Ensures proper handling of Protected B data following DFO security protocols

#### What You Need to Do

1. **Update your scripts** to establish Oracle connections before calling fleet wrapper functions
2. **Remove** `username`, `password`, `dsn`, `usepkg`, and `data.dir` parameters from function calls
3. **Re-extract data** using the new system (old extractions may not be compatible)
4. **Do not share** raw extraction files with other users

### Technical Details

* Oracle connections must be established prior to calling wrapper functions, and submitted via the cxn parameter
* All fleet wrapper functions now require valid `cxn` parameter
* Data extractions are stored in user-specific encrypted format
* Fleet identification and data linking functionality remains unchanged

### Need Help?

* Check the updated README.md for migration examples
* See technical documentation: https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/41019763.pdf
* Contact mike.mcmahon@dfo-mpo.gc.ca for assistance
* Report issues at https://github.com/Maritimes/Mar.fleets/issues

---

## Mar.fleets 2019.08.22

### NEW FEATURES

* Package created with initial fleet wrapper functions
* Automated linking of MARFIS commercial data with ISDB observer data
* Pre-configured fleet definitions for major Maritimes fisheries
