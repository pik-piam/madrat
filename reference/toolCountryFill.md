# Tool: CountryFill

This function expects a MAgPIE object with ISO country codes in the
spatial dimension. These ISO codes are compared with the official ISO
code country list (stored as supplementary data in the madrat package).
If there is an ISO code in the data but not in the official list this
entry is removed, if an entry of the official list is missing in the
data this entry is added and set to the value of the argument fill.

## Usage

``` r
toolCountryFill(
  x,
  fill = NA,
  no_remove_warning = NULL,
  overwrite = FALSE,
  verbosity = 1,
  countrylist = NULL,
  ...
)
```

## Arguments

- x:

  MAgPIE object with ISO country codes in the spatial dimension

- fill:

  Number which should be used for filling the gaps of missing countries.

- no_remove_warning:

  A vector of non-ISO country codes that exist in the data and that
  should be removed by CountryFill but without creating a warning (they
  will be removed in any case). You should use that argument if you are
  certain that the given entries should be actually removed from the
  data.

- overwrite:

  logical deciding whether existing data should be overwritten, if there
  is a specific mapping provided for that country, or not

- verbosity:

  verbosity for information about filling important countries. 0 =
  warning will show up (recommended if filling of important countries is
  not expected), 1 = note will show up in reduced log file (default), 2
  = info will show up in extended log file (recommended if filling of
  important countries is not critical and desired).

- countrylist:

  character vector of official country names (if other than ISO)

- ...:

  Mappings between countries for which the data is missing and countries
  from which the data should be used instead for these countries (e.g.
  "HKG"="CHN" if Hong Kong should receive the value of China). This
  replacement usually only makes sense for intensive values. Can be also
  provided as a argument called "map" which contains a named vector of
  these mappings.

## Value

A MAgPIE object with spatial entries for each country of the official
ISO code country list.

## Author

Jan Philipp Dietrich

## Examples

``` r
library(magclass)
x <- new.magpie("DEU", 1994, "bla", 0)
y <- toolCountryFill(x, 99)
#> NOTE:  - toolCountryFill set missing values for IMPORTANT countries to 
#> 99:
#>  --- 
#> c("Afghanistan (AFG) ", "Angola (AGO) ", "Albania (ALB) ", "United Arab Emirates (ARE) ", "Argentina (ARG) ", "Armenia (ARM) ", "Australia (AUS) ", "Austria (AUT) ", "Azerbaijan (AZE) ", "Burundi (BDI) ", "Belgium (BEL) ", "Benin (BEN) ", "Burkina Faso (BFA) ", "Bangladesh (BGD) ", "Bulgaria (BGR) ", "Bahrain (BHR) ", "Bosnia and Herzegovina (BIH) ", "Belarus (BLR) ", "Bolivia, Plurinational State of (BOL) ", "Brazil (BRA) ", "Botswana (BWA) ", "Central African Republic (CAF) ", "Canada (CAN) ", 
#> "Switzerland (CHE) ", "Chile (CHL) ", "China (CHN) ", "Cote d Ivoire (CIV) ", "Cameroon (CMR) ", "Congo, the Democratic Republic of the (COD) ", "Congo (COG) ", "Colombia (COL) ", "Costa Rica (CRI) ", "Cuba (CUB) ", "Cyprus (CYP) ", "Czech Republic (CZE) ", "Denmark (DNK) ", "Dominican Republic (DOM) ", "Algeria (DZA) ", "Ecuador (ECU) ", "Egypt (EGY) ", "Eritrea (ERI) ", "Spain (ESP) ", "Estonia (EST) ", "Ethiopia (ETH) ", "Finland (FIN) ", "France (FRA) ", "Gabon (GAB) ", "United Kingdom (GBR) ", 
#> "Georgia (GEO) ", "Ghana (GHA) ", "Guinea (GIN) ", "Gambia (GMB) ", "Guinea-Bissau (GNB) ", "Equatorial Guinea (GNQ) ", "Greece (GRC) ", "Guatemala (GTM) ", "Hong Kong (HKG) ", "Honduras (HND) ", "Croatia (HRV) ", "Haiti (HTI) ", "Hungary (HUN) ", "Indonesia (IDN) ", "India (IND) ", "Ireland (IRL) ", "Iran, Islamic Republic of (IRN) ", "Iraq (IRQ) ", "Israel (ISR) ", "Italy (ITA) ", "Jamaica (JAM) ", "Jordan (JOR) ", "Japan (JPN) ", "Kazakhstan (KAZ) ", "Kenya (KEN) ", "Kyrgyzstan (KGZ) ", "Cambodia (KHM) ", 
#> "Korea, Republic of (KOR) ", "Kuwait (KWT) ", "Lao People's Democratic Republic (LAO) ", "Lebanon (LBN) ", "Liberia (LBR) ", "Libya (LBY) ", "Sri Lanka (LKA) ", "Lesotho (LSO) ", "Lithuania (LTU) ", "Latvia (LVA) ", "Morocco (MAR) ", "Moldova, Republic of (MDA) ", "Madagascar (MDG) ", "Mexico (MEX) ", "Macedonia, the former Yugoslav Republic of (MKD) ", "Mali (MLI) ", "Myanmar (MMR) ", "Mongolia (MNG) ", "Mozambique (MOZ) ", "Mauritania (MRT) ", "Mauritius (MUS) ", "Malawi (MWI) ", "Malaysia (MYS) ", 
#> "Namibia (NAM) ", "Niger (NER) ", "Nigeria (NGA) ", "Nicaragua (NIC) ", "Netherlands (NLD) ", "Norway (NOR) ", "Nepal (NPL) ", "New Zealand (NZL) ", "Oman (OMN) ", "Pakistan (PAK) ", "Panama (PAN) ", "Peru (PER) ", "Philippines (PHL) ", "Papua New Guinea (PNG) ", "Poland (POL) ", "Puerto Rico (PRI) ", "Korea, Democratic People's Republic of (PRK) ", "Portugal (PRT) ", "Paraguay (PRY) ", "Palestine, State of (PSE) ", "Qatar (QAT) ", "Romania (ROU) ", "Russian Federation (RUS) ", "Rwanda (RWA) ", "Saudi Arabia (SAU) ", 
#> "Sudan (SDN) ", "Senegal (SEN) ", "Singapore (SGP) ", "Sierra Leone (SLE) ", "El Salvador (SLV) ", "Somalia (SOM) ", "Serbia (SRB) ", "South Sudan (SSD) ", "Slovakia (SVK) ", "Slovenia (SVN) ", "Sweden (SWE) ", "Swaziland (SWZ) ", "Syrian Arab Republic (SYR) ", "Chad (TCD) ", "Togo (TGO) ", "Thailand (THA) ", "Tajikistan (TJK) ", "Turkmenistan (TKM) ", "Timor-Leste (TLS) ", "Trinidad and Tobago (TTO) ", "Tunisia (TUN) ", "Turkey (TUR) ", "Taiwan, Province of China (TWN) ", "Tanzania, United Republic of (TZA) ", 
#> "Uganda (UGA) ", "Ukraine (UKR) ", "Uruguay (URY) ", "United States (USA) ", "Uzbekistan (UZB) ", "Venezuela, Bolivarian Republic of (VEN) ", "Viet Nam (VNM) ", "Yemen (YEM) ", "South Africa (ZAF) ", "Zambia (ZMB) ", "Zimbabwe (ZWE) ")
```
