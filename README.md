# alpscarf

`alpscarf` is an R package for visualizing AOI visits in augmented scarf plots.
The visualization is originally developed (but not limited) in the context of eye-tracking research.

##  Installation

You can install `alpscarf` from `github` using the `devtools` package.

```r
devtools::install_github("Chia-KaiYang/alpscarf")
```
## Usage

Read help information of `alpscarf`. 
The package requires three dataset as inputs:
1. AOI visits which contains at least 3 columns: "p_name" "AOI" "dwell_duration"
    * All AOIs belong to the same "p_name" should represent the AOI visit (in order) of the participant "p_name".
    * The dwell_duration corresponds to the total dwell time of one dwell.
1. Expected visit order, which contains at least two columns: "AOI" and "AOI_order"
    * The AOI_order should be continuous integers and correspond to the expected visit order of AOIs. For example, if one expected participants to visit the AOI "A" first, then move to "B" and "C", the AOI_order of "A" should be 1, "2" for "B", and "3" for "C".
1. Color definition, a set of color definitions in HEX code, is a 1-to-1 mapping to the expected visit order (AOI_order).
    * In above example, if red (#ff0000) is asssigned to "A", green (#00ff00) to "B", and blue (#0000ff) to "C", the color definition set = {#ff0000, #00ff00, #0000ff}

The package would first calculate the height (`alpscarf_height_trans`) and position (`alpscarf_width_trans`) of each bar in Alpscarf, and visualize in scarf plots with mountains and valleys (`alpscarf_plot_gen`). Additionally, the package calculates several descriptive stats, and measures of sequence alignment (`alpscarf_calculate_statistics`) with the use of [stringdist](https://github.com/markvanderloo/stringdist)

## Example

In `/vignettes/alpscarf.Rmd` you would find an example which guides users to generate Alpscarf step by step.

## Intertactive demo

In `/app/app.R` you would find a shiny app which provides an interative demo of Alpscarf. Install the `shiny` package first and you can interactively experience how different modes (e.g., transition-/duration focus, unnormalized/normalized view) play their roles in Alpscarf.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

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

* Paper and video previews [Alpscarf website](https://zpac.ch/alpscarf)

