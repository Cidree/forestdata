
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

<div id="izlkimcdsr" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#izlkimcdsr table {
  font-family: Merriweather;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#izlkimcdsr thead, #izlkimcdsr tbody, #izlkimcdsr tfoot, #izlkimcdsr tr, #izlkimcdsr td, #izlkimcdsr th {
  border-style: none;
}
&#10;#izlkimcdsr p {
  margin: 0;
  padding: 0;
}
&#10;#izlkimcdsr .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 12px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #929292;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #929292;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#izlkimcdsr .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#izlkimcdsr .gt_title {
  color: #333333;
  font-size: 14px;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#izlkimcdsr .gt_subtitle {
  color: #333333;
  font-size: 12px;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#izlkimcdsr .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#izlkimcdsr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #929292;
}
&#10;#izlkimcdsr .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #929292;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #929292;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#izlkimcdsr .gt_col_heading {
  color: #FFFFFF;
  background-color: #016763;
  font-size: 14px;
  font-weight: bold;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#izlkimcdsr .gt_column_spanner_outer {
  color: #FFFFFF;
  background-color: #016763;
  font-size: 14px;
  font-weight: bold;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#izlkimcdsr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#izlkimcdsr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#izlkimcdsr .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #929292;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#izlkimcdsr .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#izlkimcdsr .gt_group_heading {
  padding-top: 10px;
  padding-bottom: 10px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 14px;
  font-weight: bold;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #929292;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #929292;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#izlkimcdsr .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 14px;
  font-weight: bold;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #929292;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #929292;
  vertical-align: middle;
}
&#10;#izlkimcdsr .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#izlkimcdsr .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#izlkimcdsr .gt_row {
  padding-top: 10px;
  padding-bottom: 10px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: dashed;
  border-top-width: 1px;
  border-top-color: #929292;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #929292;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #929292;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#izlkimcdsr .gt_stub {
  color: #333333;
  background-color: #D5D5D5;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #FFFFFF;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#izlkimcdsr .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#izlkimcdsr .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#izlkimcdsr .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#izlkimcdsr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#izlkimcdsr .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #929292;
}
&#10;#izlkimcdsr .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#izlkimcdsr .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #929292;
}
&#10;#izlkimcdsr .gt_grand_summary_row {
  color: #333333;
  background-color: #D5D5D5;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#izlkimcdsr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #929292;
}
&#10;#izlkimcdsr .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #929292;
}
&#10;#izlkimcdsr .gt_striped {
  background-color: #F4F4F4;
}
&#10;#izlkimcdsr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #929292;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #929292;
}
&#10;#izlkimcdsr .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#izlkimcdsr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#izlkimcdsr .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#izlkimcdsr .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#izlkimcdsr .gt_left {
  text-align: left;
}
&#10;#izlkimcdsr .gt_center {
  text-align: center;
}
&#10;#izlkimcdsr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#izlkimcdsr .gt_font_normal {
  font-weight: normal;
}
&#10;#izlkimcdsr .gt_font_bold {
  font-weight: bold;
}
&#10;#izlkimcdsr .gt_font_italic {
  font-style: italic;
}
&#10;#izlkimcdsr .gt_super {
  font-size: 65%;
}
&#10;#izlkimcdsr .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#izlkimcdsr .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#izlkimcdsr .gt_indent_1 {
  text-indent: 5px;
}
&#10;#izlkimcdsr .gt_indent_2 {
  text-indent: 10px;
}
&#10;#izlkimcdsr .gt_indent_3 {
  text-indent: 15px;
}
&#10;#izlkimcdsr .gt_indent_4 {
  text-indent: 20px;
}
&#10;#izlkimcdsr .gt_indent_5 {
  text-indent: 25px;
}
</style>
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
