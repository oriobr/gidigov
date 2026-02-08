# gidigov

An R package for accessing Israel's government open data portal
([data.gov.il](https://data.gov.il)) and the Central Bureau of Statistics
(CBS) API. It provides a type-safe, object-oriented interface built on
[S7](https://rconsortium.github.io/S7/) classes and
[httr2](https://httr2.r-lib.org/) for HTTP requests.

## Installation

```r
# install.packages("pak")
pak::pak("oriobr/gidigov")
```

## Quick Start

```r
library(gidigov)

# Search for organizations
orgs <- gidi_organization_list()

# Get packages published by a specific organization
paks <- gidi_paks_by_org("israel-police")

# Get resources within a package
resources <- gidi_resources_by_pak("police-open-data")

# Download datastore records as a data.table
dt <- gidi_datastore("resource-id-here")
```

## Core Functions

### Browsing the Portal

| Function | Description |
|---|---|
| `gidi_organization_list()` | List all organizations on data.gov.il |
| `gidi_paks_by_org(org_id)` | List packages published by an organization |
| `gidi_resources_by_pak(package_id)` | List resources within a package |
| `gidi_search_pak(q)` | Search packages by keyword |
| `gidi_search_org(q)` | Search organizations by keyword |

### Retrieving Data

| Function | Description |
|---|---|
| `gidi_datastore(resource_id)` | Download datastore records (main data retrieval function) |
| `gidi_city_code()` | Query city/locality codes from the CBS API |

### Resource Information

| Function | Description |
|---|---|
| `get_resource_info(resource_id)` | Detailed metadata for one or more resources |
| `get_resource_name(resource_id)` | Get a resource's display name |
| `get_resource_info_by_name(name)` | Look up resource metadata by name |
| `get_resource_id_by_name(name)` | Look up a resource ID by name |
| `get_package_info(package_id)` | List active resources in a package |

### Interactive Helpers

| Function | Description |
|---|---|
| `gidi_see(x)` | Open any gidi object's page on data.gov.il in the browser |

## S7 Object Model

gidigov uses [S7](https://rconsortium.github.io/S7/) classes to represent
portal entities. Every object inherits from the abstract base class
`gidi_obj` and can be converted to a data frame with `as.data.frame()`.

### Class Hierarchy

```
gidi_obj (abstract)
 |-- gidi_org           Organization (id, eng_name, heb_name)
 |    '-- gidi_org_full_info  + packages, resources counts
 |-- gidi_pak           Package (pak_id, pak_heb_name, pak_eng_name, org_*)
 '-- gidi_resource      Resource (resource_id, pak_*, size, created, last_modified)
```

### List Containers

Collections of objects are held in typed list classes that validate their
elements and support standard subsetting (`[`, `[[`):

| Class | Contains |
|---|---|
| `list_gidi_org` | `gidi_org` objects |
| `list_gidi_pak` | `gidi_pak` objects |
| `list_gidi_resource` | `gidi_resource` objects |

All list classes inherit from `gidi_objs_list` and can be converted to a
data frame with `as.data.frame()`.

## Usage Examples

### Browse organizations and drill down

```r
# List all organizations (returns list_gidi_org)
orgs <- gidi_organization_list()

# As a data frame instead
orgs_df <- gidi_organization_list(as_data.frame = TRUE)

# Get only organization names
org_names <- gidi_organization_list(names_only = TRUE)

# Get packages for a specific organization
paks <- gidi_paks_by_org("israel-police")

# Drill into a package's resources
resources <- gidi_resources_by_pak("police-open-data")

# Open a resource's page in the browser
gidi_see(resources[[1]])
```

### Download data from the datastore

```r
# Basic usage - returns a data.table
dt <- gidi_datastore("resource-id")

# Select specific fields
dt <- gidi_datastore("resource-id", fields = c("name", "date", "value"))

# Filter records
dt <- gidi_datastore("resource-id", filters = list(city = "Jerusalem"))

# Limit number of rows
dt <- gidi_datastore("resource-id", max_row = 1000)

# Keep original column names (don't convert to snake_case)
dt <- gidi_datastore("resource-id", fix_names = FALSE)

# Multiple resources at once, with custom names
dt <- gidi_datastore(c(crimes = "id1", traffic = "id2"))
```

### Search the portal

```r
# Search for packages
results <- gidi_search_pak("traffic accidents")

# Search for organizations
results <- gidi_search_org("police")

# Get search results as a data frame
results_df <- gidi_search_pak("education", as_data.frame = TRUE)
```

### Query CBS city codes

```r
# Search cities by name
cities <- gidi_city_code(q = "Tel")

# Look up a specific city by ID
city <- gidi_city_code(city_id = 5000)

# Search with different match types
cities <- gidi_city_code(q = "Jer", match_type = "BEGINS_WITH")
```

### Work with gidi objects

```r
# All gidi objects have pretty-print methods
org <- orgs[[1]]
print(org)
# <gidi_org>
# Organization:
# English Name: israel-police
# Hebrew Name: משטרת ישראל
# ID: abc123

# Convert to data frame
df <- as.data.frame(org)

# Convert a whole list
df <- as.data.frame(orgs)

# Pass objects directly to functions (no need to extract IDs manually)
paks <- gidi_paks_by_org(org)
resources <- gidi_resources_by_pak(paks[[1]])
dt <- gidi_datastore(resources[[1]])
```

## Flexible Input Types

Most functions accept multiple input types through S7 generic dispatch:

```r
# All of these work:
gidi_datastore("resource-id-string")
gidi_datastore(my_gidi_resource_object)
gidi_datastore(my_list_of_gidi_resources)
gidi_datastore("https://data.gov.il/dataset/pkg/resource/id")

gidi_paks_by_org("org-name")
gidi_paks_by_org(my_gidi_org_object)

gidi_resources_by_pak("package-name")
gidi_resources_by_pak(my_gidi_pak_object)
```

## Dependencies

- [httr2](https://httr2.r-lib.org/) -- HTTP requests and parallel execution
- [S7](https://rconsortium.github.io/S7/) -- Object-oriented class system
- [data.table](https://rdatatable.gitlab.io/data.table/) -- Fast data frames
- [collapse](https://sebkrantz.github.io/collapse/) -- Fast data operations
- [RcppSimdJson](https://github.com/eddelbuettel/rcppsimdjson) -- JSON parsing
- [arrow](https://arrow.apache.org/docs/r/) -- CSV parsing
- [cli](https://cli.r-lib.org/) -- Formatted terminal output

## License

See [DESCRIPTION](DESCRIPTION) for license information.
