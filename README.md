## microRutils: Mike Rowe's Miscellaneous R Utility Functions

The microRutils package contains various functions for text manipulation,
plotting and general utility. These have been developed over the last decade or so; they range from the picayune to the highly useful.

Version: 1.0.3.20210610
### General Utility:

* `cbindSafe(), rbindSafe()`: Column-/row-wise concatenation of objects with potentially different rows/columns.
* `compressMatrix()`: Resize a matrix by combining adjacent elements.
* `express()`: Programmatically generate expression objects.
* `getLapplyIndex()`: Access index of list element from within lapply().
* `nameslike()`: Find named elements, rows or columns using a regular expression.
* `nonUniq()`: Find replicated elements in a vector.
* `numberReplicates()`: Number recurring elements within a vector.
* `Order()`: Order items using columns of a data.frame as keys.
* `prepDir()`: Create a new directory.
* `pvalOrder()`: Order p-values by levels, then numeric value.
* `rowMads()`: Fast calculation of MADs by row.
* `rowSds()`: Fast calculation of standard deviations of rows.
* `rowZs()`: Fast calculation of Z-scores by row.
* `samediff()`: Find elements in common, and unique to, two vectors.
* `selectRows()`: Row selection of a data.frame.
* `uniqRowFreqs()`: Count occurrences of replicate rows in a data.frame.
* `untab()`: Convert a table object to an array.

### Text Manipulation:

* `is.char(), as.char()`: Alises for as.character() and is.character().
* `asciiToChar()`: Conversion between letters and ASCII codes.
* `cString()`: Create a c() expression from character vector.
* `orString()`: Generate an OR regular expression substring.
* `p.print()`: Formats p-values for readability.
* `percent()`: Format numeric values as (character) percentages.
* `regextr()`: Substring extraction by regular expression.
* `rename()`: Rename elements, rows or columns, etc. of an object.
* `strrev()`: Reverse character order within each element of a character vector.
* `yyyymmdd()`: Generate a formatted time stamp.

### Plotting Functions:

* `circle()`: Plot a circle or regular polygon.
* Custom colormaps for heat maps: `aqua.brown(), grayscale, hot(), hot2(), jet(), orange.blue(),
purple.green(), purple.orange(), red.green(), yellow.blue()`
* `enclose.plot()`: Draw edge around a plot.
* `groupedTickLabels()`: Label axes with hierarchically organized groups.
* `Hist()`: Modified version of the graphics::hist() histogram function.
* `Image()`: Modified version of the graphics::image() plotting function.
* `logtix()`: Add log-scaled tick marks to an axis.
* `plot2Dhist()`: Generate and plot a 2D-histogram as a heatmap.
* `sym()`: Get a numeric range that is symmetric around zero.
* `T.width()`: Determine width of caps on error bars as fraction of plot width.
* `VennPlot()`: Draw a Venn plot with proportional areas.
* `x.pt(), y.pt()`: Get the location value associated with a fraction of x- or y-axis.

