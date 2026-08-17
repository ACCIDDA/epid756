# Additional resources for key tidyverse packages

## `dplyr`

 - [Main package site](https://dplyr.tidyverse.org/)
 - [`ifelse()` vs. `if_else()`](https://medium.com/@statisticswithoutborders/r-function-of-the-week-ifelse-vs-if-else-bed37f474fca)
 - [`select()`](https://dplyr.tidyverse.org/reference/select.html) - in addition to selecting, also used for renaming and relocating variables; can use with helper operators and functions.
 - [`across()`](https://dplyr.tidyverse.org/articles/colwise.html) applies operations of dplyr functions to multiple variables at once.
 - [`rowwise()`](https://dplyr.tidyverse.org/articles/rowwise.html) applies operations of dplyr functions by each row.
 - [`lead()` and `lag()`](https://dplyr.tidyverse.org/reference/lead-lag.html) finds the next or the last value in a vector – useful for longitudinal data.
 - [`bind_rows()` and `bind_cols()`](https://dplyr.tidyverse.org/reference/bind.html) concatenate ≥2 objects side-by-side into 1 object, based only on dimensions (rows or columns) and not underlying values of the data.
 - [`join()`](https://dplyr.tidyverse.org/reference/join.html) family of functions merge data not only by dimensions but using the underlying data values (i.e., match rows based on values of ≥1 variables).
 - [visualization of join operations](https://github.com/gadenbuie/tidyexplain)

## `tidyr`

 - [Main package site](https://tidyr.tidyverse.org/)
 - [`pivot_longer()` and `pivot_wider()`](https://tidyr.tidyverse.org/articles/pivot.html) makes long data "longer" or "wider".
 - [visualization of pivoting operations](https://github.com/gadenbuie/tidyexplain?tab=readme-ov-file#tidy-data)

## `ggplot2`

 - [Main package site](https://ggplot2.tidyverse.org/) 
 - Reference the [cheatsheet](https://posit.co/resources/cheatsheets/) for much more on geoms, stats, scales, coordinate systems, position adjustments, themes, etc.
 - Adding color to plots:
   * [Move beyond defaults](http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/) (equal luminance, not colorblind-friendly)
   * Use [`scale_brewer()`](https://ggplot2.tidyverse.org/reference/scale_brewer.html) for palettes available that are well-suited to maps or other displays of discrete values; [web tool for picking brewer palattes](https://colorbrewer2.org/)
   * [Advice for building your own color palette](https://www.r-bloggers.com/2022/06/custom-colour-palettes-for-ggplot2/)
   * [Quick reference for color specification by name or hexcode](https://sape.inf.usi.ch/quick-reference/ggplot2/colour):     
