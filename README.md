
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wig

<!-- badges: start -->
<!-- badges: end -->

The goal of `{wig}` is to import
[WIG](https://m.ensembl.org/info/website/upload/wig.html) data into R in
long format.

## Installation

You can install the current version of `{wig}` with:

``` r
# install.packages("remotes")
remotes::install_github("ramiromagno/wig")
```

## Usage

To import a WIG file, simply use the function `import_wig()`.

``` r
library(wig)
wig_file <- system.file("extdata", file = 'hg19-pik3ca.wig', package = "wig", mustWork = TRUE)

(wig_data <- import_wig(wig_file))
#> # A tibble: 26,000 × 3
#>    chr         pos   val
#>    <chr>     <int> <dbl>
#>  1 chr3  178861001     2
#>  2 chr3  178861002     2
#>  3 chr3  178861003     2
#>  4 chr3  178861004     2
#>  5 chr3  178861005     2
#>  6 chr3  178861006     2
#>  7 chr3  178861007     2
#>  8 chr3  178861008     2
#>  9 chr3  178861009     2
#> 10 chr3  178861010     2
#> # … with 25,990 more rows
```

``` r
library(ggplot2)
ggplot(data = wig_data, mapping = aes(x = pos, y = val)) +
  geom_line(size = 0.1) +
  xlab('Chr 3 genomic position') +
  ylab('H3K4me3 raw counts')
```

<img src="man/figures/README-unnamed-chunk-3-1.svg" width="100%" />

The file `hg19-pik3ca.wig` is an example WIG file that contains H3K4me3
ChIP-Seq analysis of breast variant human mammary epithelial cell from
RM035 (HS2615) using Illumina Genome Analyzer IIx. This WIG file has
already been trimmed to a region where the gene PIK3CA can be found:
chromosome 3, starting position 178,861,000 and ending position
178,894,000 (assembly hg19).

If you are interested, you can find more details about this sample in
GEO: <https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSM613874>.

## Admonition

The function `import_wig()` is to be used with smallish files, i.e.,
files whose genomic data comprises regions on the tens or few hundreds
of kilobases.

If you really need to do serious work with WIG, bigWIG, or other type of
genome annotation files, you are probably better off using packages from
the R Bioconductor ecosystem,
e.g. [rtracklayer](https://bioconductor.org/packages/release/bioc/html/rtracklayer.html).

## How to extract a region from a WIG file (outside of R)

If you have a WIG file that comprises a long genomic region, but you are
only interested on a small genomic region, here are the steps to extract
that region:

1.  Download these two command-line tools: `bigWigToWig` and
    `wigToBigWig` from
    <https://hgdownload.soe.ucsc.edu/admin/exe/linux.x86_64/>. Navigate
    up one-level if your Operating System (OS) is not Linux and find the
    compiled tools for your OS.
2.  Start by converting your WIG file to BigWIG with `wigToBigWig`:

``` bash
wigToBigWig <my-not-so-small-wig-file>.wig.gz chromInfo.txt <a-Big-Wig-file>.bw
```

You are going to need a file with chromosome lengths. E.g., for assembly
hg19 you can get `chromInfo.txt.gz` provided by UCSC from:
<ftp://hgdownload.cse.ucsc.edu/goldenPath/hg19/database/chromInfo.txt.gz>.

3.  Then, you can use `bigWigToWig` to select a specific region and
    convert back to WIG, e.g.:

``` bash
bigWigToWig -chrom=chr3 -start=178861000 -end=178894000 <a-Big-WIG-file>.bw <my-small-wig-file>.wig
```
