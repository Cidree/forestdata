
## Test environments

* local Windows 11 Pro 22H2, 64 bit, R 4.4.2

* R Under development (unstable) x86_64-w64-mingw32 R 4.5.0 beta

* R-devel on Ubuntu 22.05.5 LTS, with `rhub::rhub_check()`

* R-devel on Microsoft Windows Server 2022, R-devel, 64 bit, with `rhub::rhub_check()`

## R CMD check results

* local Windows 11 Pro 22H2: 

```
checking for future file timestamps ... NOTE
  unable to verify current time

0 errors ✔ | 0 warnings ✔ | 1 note ✖
```

* R Under development (unstable) x86_64-w64-mingw32:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Adrián Cidre González <adrian.cidre@gmail.com>'

0 errors ✔ | 0 warnings ✔ | 1 note ✖
```

* Rest of testing environments:

```
0 errors ✔ | 0 warnings ✔ | 0 notes ✔  
```

## Version note

This version fixes [https://cran.r-project.org/web/checks/check_results_forestdata.html](https://cran.r-project.org/web/checks/check_results_forestdata.html).
