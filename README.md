
<!-- README.md is generated from README.Rmd. Please edit that file -->

## forestdata

`forestdata` is an R package for downloading forestry and land use data.
This package aims to provide users with easy access to various datasets
related to forestry and land use.

## Installation

You can install the development version of `forestdata` from GitHub
using the `pak` package:

``` r
pak::pak("Cidree/forestdata")
```

## Functions

In the following table, you can find a summary of the available
functions, a short description, and the object type of the output.


<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal" style>{forestdata} Package Functions</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Download and Process Forestry and Land Use Data</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Function">Function</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Description">Description</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Output">Output</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" scope="colgroup" id="Land Cover">Land Cover</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Land Cover  Function" class="gt_row gt_left">fd_landcover_copernicus</td>
<td headers="Land Cover  Description" class="gt_row gt_left"><div class='gt_from_md'><p>Download data from the Global Land Cover from the Copernicus Global Land Service.</p>
</div></td>
<td headers="Land Cover  Output" class="gt_row gt_left">SpatRaster</td></tr>
    <tr><td headers="Land Cover  Function" class="gt_row gt_left gt_striped">fd_landcover_esri</td>
<td headers="Land Cover  Description" class="gt_row gt_left gt_striped"><div class='gt_from_md'><p>Download data from the ESRI Land Cover Explorer.</p>
</div></td>
<td headers="Land Cover  Output" class="gt_row gt_left gt_striped">SpatRaster</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" scope="colgroup" id="Forest">Forest</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Forest  Function" class="gt_row gt_left">fd_canopy_height</td>
<td headers="Forest  Description" class="gt_row gt_left"><div class='gt_from_md'><p>Download data from the ETH Global Sentinel-2 canopy height from 2020.</p>
</div></td>
<td headers="Forest  Output" class="gt_row gt_left">SpatRaster</td></tr>
    <tr><td headers="Forest  Function" class="gt_row gt_left gt_striped">fd_forest_chorological</td>
<td headers="Forest  Description" class="gt_row gt_left gt_striped"><div class='gt_from_md'><p>Download data from the European Chorological Maps.</p>
</div></td>
<td headers="Forest  Output" class="gt_row gt_left gt_striped">sf</td></tr>
    <tr><td headers="Forest  Function" class="gt_row gt_left">fd_forest_eutrees4f</td>
<td headers="Forest  Description" class="gt_row gt_left"><div class='gt_from_md'><p>Download data from the EU-Trees4F Database.</p>
</div></td>
<td headers="Forest  Output" class="gt_row gt_left">SpatRaster</td></tr>
    <tr><td headers="Forest  Function" class="gt_row gt_left gt_striped">fd_forest_extent_glad</td>
<td headers="Forest  Description" class="gt_row gt_left gt_striped"><div class='gt_from_md'><p>Download data for the forest extension from the Global Land Analysis &amp; Discovery.</p>
</div></td>
<td headers="Forest  Output" class="gt_row gt_left gt_striped">SpatRaster</td></tr>
    <tr><td headers="Forest  Function" class="gt_row gt_left">fd_forest_spain_mfe50</td>
<td headers="Forest  Description" class="gt_row gt_left"><div class='gt_from_md'><p>Download the Spanish Forest Map (MFE50) for a province.</p>
</div></td>
<td headers="Forest  Output" class="gt_row gt_left">sf</td></tr>
    <tr><td headers="Forest  Function" class="gt_row gt_left gt_striped">fd_forest_france</td>
<td headers="Forest  Description" class="gt_row gt_left gt_striped"><div class='gt_from_md'><p>Download the BD Forêt for a French Department (French Forest Map).</p>
</div></td>
<td headers="Forest  Output" class="gt_row gt_left gt_striped">sf</td></tr>
    <tr><td headers="Forest  Function" class="gt_row gt_left">fd_inventory_spain</td>
<td headers="Forest  Description" class="gt_row gt_left"><div class='gt_from_md'><p>Download data for the IFN2, IFN3 or IFN4 for a Spanish province.</p>
</div></td>
<td headers="Forest  Output" class="gt_row gt_left">list</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" scope="colgroup" id="Forest Pathogens">Forest Pathogens</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Forest Pathogens  Function" class="gt_row gt_left gt_striped">fd_pathogens_defid2</td>
<td headers="Forest Pathogens  Description" class="gt_row gt_left gt_striped"><div class='gt_from_md'><p>Download the Database of European Forest Insect and Disease Disturbances (DEFID).</p>
</div></td>
<td headers="Forest Pathogens  Output" class="gt_row gt_left gt_striped">sf</td></tr>
  </tbody>
  &#10;  
</table>
</div>

## Note

Please read the function documentation carefully. Some datasets may
require proper citation when used.
