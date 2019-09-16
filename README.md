# Alpscarf

`alpscarf` is an R package for visualizing AOI visits in augmented scarf plots.
The visualization is originally developed (but not limited) in the context of eye-tracking research.

##  Installation

You can install `alpscarf` from `github` using the `devtools` package.

```r
devtools::install_github("Chia-KaiYang/alpscarf")
```
## Usage

Read help information of `alpscarf`. 
The package requires two dataset as inputs:
1. AOI visits which contains at least 3 columns: "p_name" "AOI" "dwell_duration"
    * All AOIs belong to the same "p_name" should represent the AOI visit (in order) of the participant "p_name".
    * The dwell_duration corresponds to the total dwell time of one dwell.
1. Expected visit order and color definition, which contains at least three columns: "AOI" "AOI_order" "color"
    * Expected visit order: the AOI_order should be continuous integers and correspond to the expected visit order of each AOI. For example, if one expected participants to visit the AOI "A" first, then move to "B" and "C", the AOI_order of "A" should be 1, "2" for "B", and "3" for "C".
    * Color definition: a set of colors in HEX code. It is a 1-to-1 mapping between each AOI and color. In above example, if red (#ff0000) is asssigned to "A", green (#00ff00) to "B", and blue (#0000ff) to "C", the color definition set = {"#ff0000", "#00ff00", "#0000ff"} for the AOI "A", "B", "C"

The package would first calculate the height (`alpscarf_height_trans`) and position (`alpscarf_width_trans`) of each bar in Alpscarf, and visualize in scarf plots with mountains and valleys (`alpscarf_plot_gen`). Additionally, the package calculates several descriptive stats, and measures of sequence alignment (`alpscarf_calculate_statistics`) with the use of [stringdist](https://github.com/markvanderloo/stringdist)

## Example

In `/vignettes/alpscarf.Rmd` you would find an example which guides users to generate Alpscarf step by step.

# (new) Intertactive demo

In `/app/app.R` you would find a shiny app which provides an interative demo of Alpscarf. Install the `shiny` package first and you can interactively experience how different modes (e.g., transition-/duration focus, unnormalized/normalized view) play their roles in Alpscarf.

The interactive demo already comes with a sample data for demosntration purpose. It also supports users to play with their own data. The provided data **must** includes two `csv` files: (similar to the Usage section), separated by **commas**. Do not leave any cell free, all cells must be filled with the according info.
1. `AOI visits` with 3 columns: "p_name" "AOI" "dwell_duration". Below table shows how such dataset should look like.
   
|p_name | AOI | dwell_duration|
|---|:---:|---:|
|P1 | A | 40|
|P1 | B | 110|
|P1 | B | 70|
|P1 | A | 35|
|P1 | C | 18|
|P1 | C | 120|
|P2 | B | 200|
|P2 | B | 100|
|P2 | C | 25|
|P2 | A | 35|
|P2 | A | 99|

2. `Expected visit order and color definition` with 3 columns: "AOI" "AOI_order" "color". Below table below shows how such dataset should look like. In the `csv` file, use quotation mark for the HEX color code (e.g., "#ff0000").

|AOI | AOI_order | color|
|:---:|:---:|:---:|
|A | 1 | #ff0000|
|B | 2 | #00ff00|
|C | 3 | #0000ff|

## How to cite

If you use Alpscarf in your research, we would appreciate if you can insert the following citation


> Chia-Kai Yang and Chat Wacharamanotham. 2018. Alpscarf: Augmenting Scarf Plots for Exploring Temporal Gaze Patterns. In Extended Abstracts of the 2018 CHI Conference on Human Factors in Computing Systems (CHI EA '18). ACM, New York, NY, USA, Paper LBW503, 6 pages. DOI: https://doi.org/10.1145/3170427.3188490


In bibtex:

```
@inproceedings{Yang:2018:AAS:3170427.3188490,
 author = {Yang, Chia-Kai and Wacharamanotham, Chat},
 title = {Alpscarf: Augmenting Scarf Plots for Exploring Temporal Gaze Patterns},
 booktitle = {Extended Abstracts of the 2018 CHI Conference on Human Factors in Computing Systems},
 series = {CHI EA '18},
 year = {2018},
 isbn = {978-1-4503-5621-3},
 location = {Montreal QC, Canada},
 pages = {LBW503:1--LBW503:6},
 articleno = {LBW503},
 numpages = {6},
 url = {http://doi.acm.org/10.1145/3170427.3188490},
 doi = {10.1145/3170427.3188490},
 acmid = {3188490},
 publisher = {ACM},
 address = {New York, NY, USA},
 keywords = {eye movement, scarf plot, transitions, visualization},
} 
```

## See also

* Paper and video previews [Alpscarf website](https://chia-kaiyang.github.io/project/alpscarf/)

