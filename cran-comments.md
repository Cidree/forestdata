## R CMD check results
There were no ERRORS or WARNINGS.

There is one note on `devtools::check()`:
```
checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''
```

As stated in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be a bug/crash in MiKTeX and can likely be ignored.

There is one note in `devtools::check_win_devel()`:

```
checking CRAN incoming feasibility ... [17s] NOTE
Maintainer: 'Adrián Cidre González <adrian.cidre@gmail.com>'
```

## Version note

This version fixes [https://cran.r-project.org/web/checks/check_results_forestdata.html](https://cran.r-project.org/web/checks/check_results_forestdata.html).
