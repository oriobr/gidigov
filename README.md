# gidigov <img src="man/figures/logo.png" align="right" height="139" />

> Get data from Israeli data.gov

## Installation

```r
# install.packages("pak")
pak::pak("oriobr/gidigov")
```

## What Does gidigov Do?

Israel's open data portal ([data.gov.il](https://data.gov.il)) hosts
thousands of datasets published by government organizations -- from police
crime statistics to education budgets to city planning data. gidigov lets
you **explore** this portal and **download** data directly into R, all in a
few lines of code.

The portal is organized in three levels:

```
Organization (ארגון)
 └── Package (חבילת נתונים)
      └── Resource (משאב / קובץ נתונים)
```

gidigov mirrors this hierarchy with R objects you can browse interactively.

> **Note:** Currently gidigov only supports resources available through the
> data.gov.il **API** (datastore). Resources that are only offered as file
> downloads (e.g. CSV/Excel links without an API endpoint) are not yet
> supported.

## The Discovery Pipeline

The core idea of gidigov is a **drill-down workflow**: start at the top,
explore with `$`, and work your way down to the data you want.

```r
library(gidigov)

# Step 1: List all government organizations
orgs <- gidi_organization_list()

# The result is a named list -- use $ to pick one by its Hebrew name
orgs$`משטרת ישראל`
#> <gidi_org>
#> Organization:
#> English Name: israel-police
#> Hebrew Name: משטרת ישראל
#> Packages: 12
#> Resources: 47
#> ID: 759a03c9-...

# Step 2: Get packages published by that organization
paks <- gidi_paks_by_org(orgs$`משטרת ישראל`)

# Again, a named list -- pick a package with $
paks$`עבירות פליליות`
#> <gidi_pak>
#> Organization Name:
#> English: israel-police
#> Hebrew: משטרת ישראל
#> Package Name:
#> English: criminal-offenses
#> Hebrew: עבירות פליליות
#> Package ID: 759a03c9-...

# Step 3: Get resources inside that package
resources <- gidi_resources_by_pak(paks$`עבירות פליליות`)

# Pick a specific resource with $
resources$`עבירות פליליות 2023`
#> <gidi_resource>
#> Package Name:
#> English: criminal-offenses
#> Hebrew: עבירות פליליות
#> Resource:
#> Name: עבירות פליליות 2023
#> Size: 2.1 MB
#> ID: abc123...

# Step 4: Download the data!
dt <- gidi_datastore(resources$`עבירות פליליות 2023`)
#> A data.table with thousands of rows, ready to analyze
```

### The Whole Pipeline in One Chain

Because every function accepts the output of the previous step, you can
chain the entire discovery process:

```r
# From organization → package → resources → data, step by step
org <- gidi_organization_list()$`משטרת ישראל`
pak <- gidi_paks_by_org(org)$`עבירות פליליות`
res <- gidi_resources_by_pak(pak)$`עבירות פליליות 2023`
dt  <- gidi_datastore(res)
```

Or with R's native pipe (`|>`):

```r
org <- gidi_organization_list()$`משטרת ישראל`

org |>
  gidi_paks_by_org() |>
  as.data.frame()
```

You can also skip the discovery and go straight to the data if you already
have an ID or a URL:

```r
# By resource ID
dt <- gidi_datastore("abc123-def456-...")

# By data.gov.il URL
dt <- gidi_datastore("https://data.gov.il/dataset/criminal-offenses/resource/abc123")
```

## Accessing Object Properties

gidigov objects use S7's `@` operator for accessing properties:

```r
org <- gidi_organization_list()$`משטרת ישראל`

org@eng_name
#> [1] "israel-police"

org@heb_name
#> [1] "משטרת ישראל"

org@packages
#> [1] 12
```

Each object type has different properties:

| Object | Properties |
|---|---|
| `gidi_org` | `@id`, `@eng_name`, `@heb_name` |
| `gidi_org_full_info` | all of the above + `@packages`, `@resources` |
| `gidi_pak` | `@pak_id`, `@pak_heb_name`, `@pak_eng_name`, `@org_heb_name`, `@org_eng_name` |
| `gidi_resource` | `@resource_id`, `@pak_heb_name`, `@pak_eng_name`, `@resource_heb_name`, `@size`, `@created`, `@last_modified`, `@see` |

The `gidi_resource` object has a special `@see` property -- call it as a
function to open the resource's page in your browser:

```r
res@see()
# Opens https://data.gov.il/dataset/.../resource/... in the browser
```

## Converting to Data Frames

Every gidi object and list can be converted to a flat data frame:

```r
# Single object → one-row data frame
as.data.frame(org)

# List of objects → multi-row data frame
orgs_df <- as.data.frame(orgs)
#>   id            eng_name     heb_name       packages resources
#> 1 759a03c9-...  israel-police  משטרת ישראל    12        47
#> 2 ...           ...            ...            ...       ...

# Or get a data frame directly from the API
gidi_organization_list(as_data.frame = TRUE)
gidi_paks_by_org("israel-police", as_data.frame = TRUE)
gidi_resources_by_pak("criminal-offenses", as_data.frame = TRUE)
```

## Downloading Data with `gidi_datastore()`

`gidi_datastore()` is the main data retrieval function. It handles
pagination automatically (the API returns at most 32,000 rows per request),
so you always get the full dataset.

```r
# Basic usage
dt <- gidi_datastore("resource-id")

# Select specific fields
dt <- gidi_datastore("resource-id", fields = c("city", "date", "count"))

# Server-side filtering (only matching rows are downloaded)
dt <- gidi_datastore("resource-id", filters = list(city = "Jerusalem"))

# Limit total rows (useful for previewing large datasets)
dt <- gidi_datastore("resource-id", max_row = 500)

# Keep original column names instead of converting to snake_case
dt <- gidi_datastore("resource-id", fix_names = FALSE)

# Download multiple resources at once, with custom names
dts <- gidi_datastore(c(crimes = "id1", traffic = "id2"))
# Returns a named list: dts$crimes, dts$traffic
```

## Searching the Portal

Don't know the exact organization or package name? Search for it:

```r
# Search for packages
gidi_search_pak("traffic accidents")

# Search for organizations
gidi_search_org("police")

# Get results as a data frame
gidi_search_pak("education", as_data.frame = TRUE)
```

## CBS City Codes

gidigov also connects to the Central Bureau of Statistics (CBS) API for
looking up Israeli city and locality codes:

```r
# Search cities by name
gidi_city_code(q = "Tel")
#>    city_code city_name_eng    city_name_heb
#> 1:      5000 Tel Aviv-Yafo    תל אביב-יפו
#> 2:       ...  ...              ...

# Look up a specific city by ID
gidi_city_code(city_id = 5000)

# Match type: CONTAINS (default), BEGINS_WITH, or EQUALS
gidi_city_code(q = "Jer", match_type = "BEGINS_WITH")
```

## Function Reference

| Function | What it does |
|---|---|
| `gidi_organization_list()` | List all organizations |
| `gidi_paks_by_org()` | Get packages for an organization |
| `gidi_resources_by_pak()` | Get resources in a package |
| `gidi_search_pak()` | Search packages by keyword |
| `gidi_search_org()` | Search organizations by keyword |
| `gidi_datastore()` | Download datastore records |
| `gidi_city_code()` | Query CBS city codes |
| `gidi_see()` | Open item in browser |
| `get_resource_info()` | Resource metadata |
| `get_resource_name()` | Resource display name |
| `get_resource_info_by_name()` | Look up resource by name |
| `get_resource_id_by_name()` | Look up resource ID by name |
| `get_package_info()` | Active resources in a package |

## Dependencies

- [httr2](https://httr2.r-lib.org/) -- HTTP requests with parallel execution
- [S7](https://rconsortium.github.io/S7/) -- Modern object-oriented class system
- [data.table](https://rdatatable.gitlab.io/data.table/) -- Fast data frames
- [collapse](https://sebkrantz.github.io/collapse/) -- Fast data operations
- [RcppSimdJson](https://github.com/eddelbuettel/rcppsimdjson) -- High-speed JSON parsing
- [arrow](https://arrow.apache.org/docs/r/) -- CSV parsing
- [cli](https://cli.r-lib.org/) -- Formatted terminal output

## License

See [DESCRIPTION](DESCRIPTION) for license information.
