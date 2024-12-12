
## Test environments

* local Windows 11 Pro 22H2, 64 bit, R 4.4.2

* R Under development (unstable) (2024-12-10 r87437 ucrt)

* R-devel on Ubuntu 22.05.5 LTS, with `rhub::rhub_check()`

* R-devel on Microsoft Windows Server 2022, R-devel, 64 bit, with `rhub::rhub_check()`

## R CMD check results

* local Windows 11 Pro 22H2: 
```
 checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

0 errors ✔ | 0 warnings ✔ | 1 note ✖
```

As stated in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be a bug/crash in MiKTeX and can likely be ignored.

* Rest of testing environments:

```
0 errors ✔ | 0 warnings ✔ | 0 notes ✔  
```

## Version note

This version fixes [https://cran.r-project.org/web/checks/check_results_forestdata.html](https://cran.r-project.org/web/checks/check_results_forestdata.html).
