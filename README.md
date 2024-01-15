## A shiny app for plotting your bedgraph genomic profiles.

[This application](https://thenotoriousmbg.shinyapps.io/te_profiles/) works with any bedgraph file that you can either upload from your local computer or through a URL. Simply load your bedgraph file, and within a few seconds, the app generates an overview plot. You can either use this plot as is or further refine your analysis by selecting a specific sequence through a dropdown menu. The app allows you to choose between PNG or PDF formats for your plot output.

For the best experience, it is recommended to:
- Ensure coverage of all chromosomes. This means that for stretches with no signal, the fourth column should be set to 0. To avoid inflating the file size with empty ranges, it's efficient to consolidate consecutive ranges with the same signal into a single range. For example:
```
  chr1 0 100 0.0
  chr1 100 200 0.0
  chr1 200 300 0.0
  chr1 300 400 0.43
  ```
  can be transformed into

  ```
  chr1 0 300 0.0
  chr1 300 400 0.43
  ```
- Avoid using sequences longer than 10 kb for generating your bedgraph. This app was originally developed for analyzing Drosophila transposable elements consensus sequences, which are typically shorter than 10 kb. However, it can potentially be used for any sequence within this size limit.

Examples of bedgraph files to use with the app can be found [here](https://share.genome.au.dk/YGcn3w9QN7U/).
