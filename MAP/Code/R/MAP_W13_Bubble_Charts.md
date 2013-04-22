MAP Analysis of KAPS 
========================================================

Bubble Charts for Ellen
------------------------------------------------------------------------------


```r
library(reshape)
```

```
## Loading required package: plyr
```

```
## Attaching package: 'reshape'
```

```
## The following object(s) are masked from 'package:plyr':
## 
## rename, round_any
```

```r
library(ggplot2)
library(grid)
library(RJDBC)
```

```
## Loading required package: DBI
```

```
## Loading required package: rJava
```

```
## Warning: replacing previous import 'show' when loading 'rJava'
```

```r
library(plyr)
library(data.table)

setwd("/Users/chaid/Dropbox/Consulting/KIPP Ascend/Testing Analysis/MAP/Analysis/KAP")
```





```
##            ID StudentFirstName StudentLastName          SchoolName
##   1: 44339927           Andrew           Roman KIPP ASCEND PRIMARY
##   2: 44339927           Andrew           Roman KIPP ASCEND PRIMARY
##   3: 44433818            Brian           Pride KIPP ASCEND PRIMARY
##   4: 44433818            Brian           Pride KIPP ASCEND PRIMARY
##   5: 44433826          Zarreia        Triplett KIPP ASCEND PRIMARY
##  ---                                                              
## 355: 50247181           Treyon          Wright KIPP ASCEND PRIMARY
## 356: 50247694          Gregory   Sauls-Sanders KIPP ASCEND PRIMARY
## 357: 50247694          Gregory   Sauls-Sanders KIPP ASCEND PRIMARY
## 358: 50247704           Jacara          Walker KIPP ASCEND PRIMARY
## 359: 50247704           Jacara          Walker KIPP ASCEND PRIMARY
##      Fall11_Grade                   ClassName     Subject Fall11_GM
##   1:            1       Grade_1A Boesch NYU 1 Mathematics      True
##   2:            1       Grade_1A Boesch NYU 1     Reading      True
##   3:            1 Grade_1B Timmins Berkeley 1 Mathematics      True
##   4:            1 Grade_1B Timmins Berkeley 1     Reading      True
##   5:            1      Grade_1D Cousin Iowa 1     Reading      True
##  ---                                                               
## 355:            0    Grade_KB Pletz Harvard 1 Mathematics      True
## 356:            1      Grade_1D Cousin Iowa 1 Mathematics      True
## 357:            1      Grade_1D Cousin Iowa 1     Reading      True
## 358:            1       Grade_1A Boesch NYU 1     Reading      True
## 359:            1       Grade_1A Boesch NYU 1 Mathematics      True
##              Fall11_TT Fall11_RIT Fall11_Pctl TypicalFallToSpringGrowth
##   1: Survey with Goals        170          70                     14.71
##   2: Survey with Goals        174          86                     16.74
##   3: Survey with Goals        156          31                     17.12
##   4: Survey with Goals        151          23                     16.49
##   5: Survey with Goals        166          67                     16.66
##  ---                                                                   
## 355: Survey with Goals        137          28                     16.92
## 356: Survey with Goals        167          62                     15.23
## 357: Survey with Goals        163          58                     16.62
## 358: Survey with Goals        162          55                     16.61
## 359: Survey with Goals        170          70                     14.71
##      ReportedFallToSpringGrowth SDFallToSpringGrowth
##   1:                         15                 6.85
##   2:                         17                 8.13
##   3:                         17                 6.85
##   4:                         16                 8.13
##   5:                         17                 8.13
##  ---                                                
## 355:                         17                 8.24
## 356:                         15                 6.85
## 357:                         17                 8.13
## 358:                         17                 8.13
## 359:                         15                 6.85
##      TypicalFallToWinterGrowth ReportedFallToWinterGrowth
##   1:                      8.86                          9
##   2:                     10.44                         10
##   3:                     10.06                         10
##   4:                     10.30                         10
##   5:                     10.39                         10
##  ---                                                     
## 355:                      8.09                          8
## 356:                      9.12                          9
## 357:                     10.37                         10
## 358:                     10.36                         10
## 359:                      8.86                          9
##      SDFallToWinterGrowth TypicalFallToFallGrowth ReportedFallToFallGrowth
##   1:                 5.02                   13.63                       14
##   2:                 5.65                   15.52                       16
##   3:                 5.02                   17.05                       17
##   4:                 5.65                   15.17                       15
##   5:                 5.65                   15.40                       15
##  ---                                                                      
## 355:                 5.35                   20.19                       20
## 356:                 5.02                   14.36                       14
## 357:                 5.65                   15.35                       15
## 358:                 5.65                   15.34                       15
## 359:                 5.02                   13.63                       14
##      SDFallToFallGrowth TypicalSpringToSpringGrowth
##   1:               8.72                       16.21
##   2:              10.50                       15.74
##   3:               8.72                       18.60
##   4:              10.50                       20.04
##   5:              10.50                       17.24
##  ---                                               
## 355:              10.17                       28.77
## 356:               8.72                       16.72
## 357:              10.50                       17.80
## 358:              10.50                       17.98
## 359:               8.72                       16.21
##      ReportedSpringToSpringGrowth SDSpringToSpringGrowth
##   1:                           16                   8.21
##   2:                           16                   9.86
##   3:                           19                   8.21
##   4:                           20                   9.86
##   5:                           17                   9.86
##  ---                                                    
## 355:                           29                   8.72
## 356:                           17                   8.21
## 357:                           18                   9.86
## 358:                           18                   9.86
## 359:                           16                   8.21
##      TypicalWinterToSpringGrowth ReportedWinterToSpringGrowth
##   1:                        5.85                            6
##   2:                        6.31                            6
##   3:                        7.06                            7
##   4:                        6.19                            6
##   5:                        6.27                            6
##  ---                                                         
## 355:                        8.83                            9
## 356:                        6.11                            6
## 357:                        6.25                            6
## 358:                        6.25                            6
## 359:                        5.85                            6
##      SDWinterToSpringGrowth Fall11_Quartile Winter12_Grade Winter12_RIT
##   1:                   5.02               3              1          175
##   2:                   5.32               4              1          164
##   3:                   5.02               2              1          161
##   4:                   5.32               1              1          135
##   5:                   5.32               3              1          175
##  ---                                                                   
## 355:                   5.70               2              0          152
## 356:                   5.02               3              1          182
## 357:                   5.32               3              1          172
## 358:                   5.32               3              1          169
## 359:                   5.02               3              1          180
##      Winter12_Pctl Winter12_Quartile Year
##   1:            58                 3 2012
##   2:            31                 2 2012
##   3:            19                 1 2012
##   4:             1                 1 2012
##   5:            63                 3 2012
##  ---                                     
## 355:            54                 3 2012
## 356:            77                 4 2012
## 357:            54                 3 2012
## 358:            45                 2 2012
## 359:            72                 3 2012
```

```
##             ID StudentFirstName StudentLastName                SchoolName
##    1: 39910470      Chardonne't          Thomas KIPP Ascend Middle School
##    2: 39910470      Chardonne't          Thomas KIPP Ascend Middle School
##    3: 39984121             Demi          Thomas KIPP Ascend Middle School
##    4: 39984121             Demi          Thomas KIPP Ascend Middle School
##    5: 40534857          Amarion           Mayes KIPP Ascend Middle School
##   ---                                                                    
## 1217: 50295673         Benjamin            Swan       KIPP Ascend Primary
## 1218: 50295673         Benjamin            Swan       KIPP Ascend Primary
## 1219: 50283712            Doxey          Kamara KIPP Create Middle School
## 1220: 50283712            Doxey          Kamara KIPP Create Middle School
## 1221: 50283712            Doxey          Kamara KIPP Create Middle School
##       Fall12_Grade  ClassName         Subject Fall12_GM         Fall12_TT
##    1:            8         ND         Reading      TRUE Survey With Goals
##    2:            8         ND     Mathematics      TRUE Survey With Goals
##    3:            8         ND         Reading      TRUE Survey With Goals
##    4:            8         ND     Mathematics      TRUE Survey With Goals
##    5:            8         ND         Reading      TRUE Survey With Goals
##   ---                                                                    
## 1217:            1     Loyola     Mathematics      TRUE Survey With Goals
## 1218:            1     Loyola         Reading      TRUE Survey With Goals
## 1219:            5 Pittsburgh         Reading      TRUE Survey With Goals
## 1220:            5 Pittsburgh General Science      TRUE Survey With Goals
## 1221:            5 Pittsburgh     Mathematics      TRUE Survey With Goals
##       Fall12_RIT Fall12_Pctl TypicalFallToSpringGrowth
##    1:        202          12                      4.26
##    2:        198           3                      4.39
##    3:        215          39                      3.42
##    4:        221          29                      4.32
##    5:        214          36                      3.48
##   ---                                                 
## 1217:        165          56                     15.57
## 1218:        154          31                     16.53
## 1219:        232          96                      3.98
## 1220:        217          93                      3.12
## 1221:        211          45                      8.12
##       ReportedFallToSpringGrowth SDFallToSpringGrowth
##    1:                          4                 6.63
##    2:                          4                 6.42
##    3:                          3                 6.63
##    4:                          4                 6.42
##    5:                          3                 6.63
##   ---                                                
## 1217:                         16                 6.85
## 1218:                         17                 8.13
## 1219:                          4                 6.13
## 1220:                          3                 6.10
## 1221:                          8                 5.99
##       TypicalFallToWinterGrowth ReportedFallToWinterGrowth
##    1:                      2.50                          3
##    2:                      2.59                          3
##    3:                      2.05                          2
##    4:                      2.55                          3
##    5:                      2.08                          2
##   ---                                                     
## 1217:                      9.29                          9
## 1218:                     10.32                         10
## 1219:                      2.15                          2
## 1220:                      2.00                          2
## 1221:                      4.85                          5
##       SDFallToWinterGrowth TypicalFallToFallGrowth
##    1:                 5.05                    4.47
##    2:                 4.97                    4.44
##    3:                 5.05                    3.31
##    4:                 4.97                    4.35
##    5:                 5.05                    3.40
##   ---                                             
## 1217:                 5.02                   14.85
## 1218:                 5.65                   15.22
## 1219:                 4.71                    3.19
## 1220:                 4.85                    2.85
## 1221:                 4.81                    7.64
##       ReportedFallToFallGrowth SDFallToFallGrowth
##    1:                        4               8.24
##    2:                        4               7.90
##    3:                        3               8.24
##    4:                        4               7.90
##    5:                        3               8.24
##   ---                                            
## 1217:                       15               8.72
## 1218:                       15              10.50
## 1219:                        3               7.45
## 1220:                        3               7.40
## 1221:                        8               7.22
##       TypicalSpringToSpringGrowth ReportedSpringToSpringGrowth
##    1:                        2.00                            2
##    2:                        2.00                            2
##    3:                        2.00                            2
##    4:                        2.00                            2
##    5:                        2.00                            2
##   ---                                                         
## 1217:                       17.06                           17
## 1218:                       19.48                           19
## 1219:                        3.71                            4
## 1220:                        2.00                            2
## 1221:                        5.83                            6
##       SDSpringToSpringGrowth TypicalWinterToSpringGrowth
##    1:                   9.59                        2.00
##    2:                   9.21                        2.00
##    3:                   9.59                        2.00
##    4:                   9.21                        2.00
##    5:                   9.59                        2.00
##   ---                                                   
## 1217:                   8.21                        6.28
## 1218:                   9.86                        6.21
## 1219:                   7.48                        2.00
## 1220:                   7.12                        2.00
## 1221:                   7.41                        3.26
##       ReportedWinterToSpringGrowth SDWinterToSpringGrowth Fall12_Quartile
##    1:                            2                   4.85               1
##    2:                            2                   4.79               1
##    3:                            2                   4.85               2
##    4:                            2                   4.79               2
##    5:                            2                   4.85               2
##   ---                                                                    
## 1217:                            6                   5.02               3
## 1218:                            6                   5.32               2
## 1219:                            2                   4.87               4
## 1220:                            2                   4.70               4
## 1221:                            3                   4.67               2
##       Winter13_Grade Winter13_RIT Winter13_Pctl Winter13_Quartile Year
##    1:              8          217            39                 2 2013
##    2:              8          188             1                 1 2013
##    3:              8          214            31                 2 2013
##    4:              8          224            30                 2 2013
##    5:              8          216            36                 2 2013
##   ---                                                                 
## 1217:              1          183            80                 4 2013
## 1218:              1          187            89                 4 2013
## 1219:              5          240            98                 4 2013
## 1220:              5          224            97                 4 2013
## 1221:              5          225            69                 3 2013
```

```
##             ID StudentFirstName StudentLastName                SchoolName
##    1: 44339927           Andrew           Roman       KIPP ASCEND PRIMARY
##    2: 44339927           Andrew           Roman       KIPP ASCEND PRIMARY
##    3: 44433818            Brian           Pride       KIPP ASCEND PRIMARY
##    4: 44433818            Brian           Pride       KIPP ASCEND PRIMARY
##    5: 44433826          Zarreia        Triplett       KIPP ASCEND PRIMARY
##   ---                                                                    
## 1576: 50295673         Benjamin            Swan       KIPP Ascend Primary
## 1577: 50295673         Benjamin            Swan       KIPP Ascend Primary
## 1578: 50283712            Doxey          Kamara KIPP Create Middle School
## 1579: 50283712            Doxey          Kamara KIPP Create Middle School
## 1580: 50283712            Doxey          Kamara KIPP Create Middle School
##       Fall_Grade                   ClassName         Subject Fall_GM
##    1:          1       Grade_1A Boesch NYU 1     Mathematics    True
##    2:          1       Grade_1A Boesch NYU 1         Reading    True
##    3:          1 Grade_1B Timmins Berkeley 1     Mathematics    True
##    4:          1 Grade_1B Timmins Berkeley 1         Reading    True
##    5:          1      Grade_1D Cousin Iowa 1         Reading    True
##   ---                                                               
## 1576:          1                      Loyola     Mathematics    TRUE
## 1577:          1                      Loyola         Reading    TRUE
## 1578:          5                  Pittsburgh         Reading    TRUE
## 1579:          5                  Pittsburgh General Science    TRUE
## 1580:          5                  Pittsburgh     Mathematics    TRUE
##                 Fall_TT Fall_RIT Fall_Pctl TypicalFallToSpringGrowth
##    1: Survey with Goals      170        70                     14.71
##    2: Survey with Goals      174        86                     16.74
##    3: Survey with Goals      156        31                     17.12
##    4: Survey with Goals      151        23                     16.49
##    5: Survey with Goals      166        67                     16.66
##   ---                                                               
## 1576: Survey With Goals      165        56                     15.57
## 1577: Survey With Goals      154        31                     16.53
## 1578: Survey With Goals      232        96                      3.98
## 1579: Survey With Goals      217        93                      3.12
## 1580: Survey With Goals      211        45                      8.12
##       ReportedFallToSpringGrowth SDFallToSpringGrowth
##    1:                         15                 6.85
##    2:                         17                 8.13
##    3:                         17                 6.85
##    4:                         16                 8.13
##    5:                         17                 8.13
##   ---                                                
## 1576:                         16                 6.85
## 1577:                         17                 8.13
## 1578:                          4                 6.13
## 1579:                          3                 6.10
## 1580:                          8                 5.99
##       TypicalFallToWinterGrowth ReportedFallToWinterGrowth
##    1:                      8.86                          9
##    2:                     10.44                         10
##    3:                     10.06                         10
##    4:                     10.30                         10
##    5:                     10.39                         10
##   ---                                                     
## 1576:                      9.29                          9
## 1577:                     10.32                         10
## 1578:                      2.15                          2
## 1579:                      2.00                          2
## 1580:                      4.85                          5
##       SDFallToWinterGrowth TypicalFallToFallGrowth
##    1:                 5.02                   13.63
##    2:                 5.65                   15.52
##    3:                 5.02                   17.05
##    4:                 5.65                   15.17
##    5:                 5.65                   15.40
##   ---                                             
## 1576:                 5.02                   14.85
## 1577:                 5.65                   15.22
## 1578:                 4.71                    3.19
## 1579:                 4.85                    2.85
## 1580:                 4.81                    7.64
##       ReportedFallToFallGrowth SDFallToFallGrowth
##    1:                       14               8.72
##    2:                       16              10.50
##    3:                       17               8.72
##    4:                       15              10.50
##    5:                       15              10.50
##   ---                                            
## 1576:                       15               8.72
## 1577:                       15              10.50
## 1578:                        3               7.45
## 1579:                        3               7.40
## 1580:                        8               7.22
##       TypicalSpringToSpringGrowth ReportedSpringToSpringGrowth
##    1:                       16.21                           16
##    2:                       15.74                           16
##    3:                       18.60                           19
##    4:                       20.04                           20
##    5:                       17.24                           17
##   ---                                                         
## 1576:                       17.06                           17
## 1577:                       19.48                           19
## 1578:                        3.71                            4
## 1579:                        2.00                            2
## 1580:                        5.83                            6
##       SDSpringToSpringGrowth TypicalWinterToSpringGrowth
##    1:                   8.21                        5.85
##    2:                   9.86                        6.31
##    3:                   8.21                        7.06
##    4:                   9.86                        6.19
##    5:                   9.86                        6.27
##   ---                                                   
## 1576:                   8.21                        6.28
## 1577:                   9.86                        6.21
## 1578:                   7.48                        2.00
## 1579:                   7.12                        2.00
## 1580:                   7.41                        3.26
##       ReportedWinterToSpringGrowth SDWinterToSpringGrowth Fall_Quartile
##    1:                            6                   5.02             3
##    2:                            6                   5.32             4
##    3:                            7                   5.02             2
##    4:                            6                   5.32             1
##    5:                            6                   5.32             3
##   ---                                                                  
## 1576:                            6                   5.02             3
## 1577:                            6                   5.32             2
## 1578:                            2                   4.87             4
## 1579:                            2                   4.70             4
## 1580:                            3                   4.67             2
##       Winter_Grade Winter_RIT Winter_Pctl Winter_Quartile Year
##    1:            1        175          58               3 2012
##    2:            1        164          31               2 2012
##    3:            1        161          19               1 2012
##    4:            1        135           1               1 2012
##    5:            1        175          63               3 2012
##   ---                                                         
## 1576:            1        183          80               4 2013
## 1577:            1        187          89               4 2013
## 1578:            5        240          98               4 2013
## 1579:            5        224          97               4 2013
## 1580:            5        225          69               3 2013
```

```
## Error: missing value where TRUE/FALSE needed
```

```
## Error: missing value where TRUE/FALSE needed
```

```
##             ID StudentFirstName StudentLastName          SchoolName
##    1: 44339927           Andrew           Roman KIPP ASCEND PRIMARY
##    2: 44339927           Andrew           Roman KIPP ASCEND PRIMARY
##    3: 44433818            Brian           Pride KIPP ASCEND PRIMARY
##    4: 44433818            Brian           Pride KIPP ASCEND PRIMARY
##    5: 44433826          Zarreia        Triplett KIPP ASCEND PRIMARY
##   ---                                                              
## 1058: 50295665            Cesar           Avila KIPP Ascend Primary
## 1059: 50295672        Gabrielle            Swan KIPP Ascend Primary
## 1060: 50295672        Gabrielle            Swan KIPP Ascend Primary
## 1061: 50295673         Benjamin            Swan KIPP Ascend Primary
## 1062: 50295673         Benjamin            Swan KIPP Ascend Primary
##       Fall_Grade                   ClassName     Subject Fall_GM
##    1:          1       Grade_1A Boesch NYU 1 Mathematics    True
##    2:          1       Grade_1A Boesch NYU 1     Reading    True
##    3:          1 Grade_1B Timmins Berkeley 1 Mathematics    True
##    4:          1 Grade_1B Timmins Berkeley 1     Reading    True
##    5:          1      Grade_1D Cousin Iowa 1     Reading    True
##   ---                                                           
## 1058:          2                    Columbia     Reading    TRUE
## 1059:          K                    Michigan Mathematics    TRUE
## 1060:          K                    Michigan     Reading    TRUE
## 1061:          1                      Loyola Mathematics    TRUE
## 1062:          1                      Loyola     Reading    TRUE
##                 Fall_TT Fall_RIT Fall_Pctl TypicalFallToSpringGrowth
##    1: Survey with Goals      170        70                     14.71
##    2: Survey with Goals      174        86                     16.74
##    3: Survey with Goals      156        31                     17.12
##    4: Survey with Goals      151        23                     16.49
##    5: Survey with Goals      166        67                     16.66
##   ---                                                               
## 1058: Survey With Goals      196        90                     10.97
## 1059: Survey With Goals      133        18                     17.55
## 1060: Survey With Goals      146        62                     14.70
## 1061: Survey With Goals      165        56                     15.57
## 1062: Survey With Goals      154        31                     16.53
##       ReportedFallToSpringGrowth SDFallToSpringGrowth
##    1:                         15                 6.85
##    2:                         17                 8.13
##    3:                         17                 6.85
##    4:                         16                 8.13
##    5:                         17                 8.13
##   ---                                                
## 1058:                         11                 7.70
## 1059:                         18                 8.24
## 1060:                         15                 7.23
## 1061:                         16                 6.85
## 1062:                         17                 8.13
##       TypicalFallToWinterGrowth ReportedFallToWinterGrowth
##    1:                      8.86                          9
##    2:                     10.44                         10
##    3:                     10.06                         10
##    4:                     10.30                         10
##    5:                     10.39                         10
##   ---                                                     
## 1058:                      6.30                          6
## 1059:                      8.38                          8
## 1060:                      8.23                          8
## 1061:                      9.29                          9
## 1062:                     10.32                         10
##       SDFallToWinterGrowth TypicalFallToFallGrowth
##    1:                 5.02                   13.63
##    2:                 5.65                   15.52
##    3:                 5.02                   17.05
##    4:                 5.65                   15.17
##    5:                 5.65                   15.40
##   ---                                             
## 1058:                 5.47                   10.30
## 1059:                 5.35                   21.02
## 1060:                 5.15                   17.45
## 1061:                 5.02                   14.85
## 1062:                 5.65                   15.22
##       ReportedFallToFallGrowth SDFallToFallGrowth
##    1:                       14               8.72
##    2:                       16              10.50
##    3:                       17               8.72
##    4:                       15              10.50
##    5:                       15              10.50
##   ---                                            
## 1058:                       10               9.86
## 1059:                       21              10.17
## 1060:                       17               9.31
## 1061:                       15               8.72
## 1062:                       15              10.50
##       TypicalSpringToSpringGrowth ReportedSpringToSpringGrowth
##    1:                       16.21                           16
##    2:                       15.74                           16
##    3:                       18.60                           19
##    4:                       20.04                           20
##    5:                       17.24                           17
##   ---                                                         
## 1058:                       10.55                           11
## 1059:                       29.75                           30
## 1060:                       20.76                           21
## 1061:                       17.06                           17
## 1062:                       19.48                           19
##       SDSpringToSpringGrowth TypicalWinterToSpringGrowth
##    1:                   8.21                        5.85
##    2:                   9.86                        6.31
##    3:                   8.21                        7.06
##    4:                   9.86                        6.19
##    5:                   9.86                        6.27
##   ---                                                   
## 1058:                   7.95                        4.67
## 1059:                   8.72                        9.17
## 1060:                  10.50                        6.47
## 1061:                   8.21                        6.28
## 1062:                   9.86                        6.21
##       ReportedWinterToSpringGrowth SDWinterToSpringGrowth Fall_Quartile
##    1:                            6                   5.02             3
##    2:                            6                   5.32             4
##    3:                            7                   5.02             2
##    4:                            6                   5.32             1
##    5:                            6                   5.32             3
##   ---                                                                  
## 1058:                            5                   5.18             4
## 1059:                            9                   5.70             1
## 1060:                            6                   5.15             3
## 1061:                            6                   5.02             3
## 1062:                            6                   5.32             2
##       Winter_Grade Winter_RIT Winter_Pctl Winter_Quartile Year Quartile
##    1:            1        175          58               3 2012       NA
##    2:            1        164          31               2 2012       NA
##    3:            1        161          19               1 2012       NA
##    4:            1        135           1               1 2012        1
##    5:            1        175          63               3 2012       NA
##   ---                                                                  
## 1058:            2        205          93               4 2013       NA
## 1059:            0        177          98               4 2013        1
## 1060:            0        162          84               4 2013       NA
## 1061:            1        183          80               4 2013       NA
## 1062:            1        187          89               4 2013       NA
```

```
##             ID StudentFirstName StudentLastName          SchoolName
##    1: 44339927           Andrew           Roman KIPP ASCEND PRIMARY
##    2: 44339927           Andrew           Roman KIPP ASCEND PRIMARY
##    3: 44433818            Brian           Pride KIPP ASCEND PRIMARY
##    4: 44433818            Brian           Pride KIPP ASCEND PRIMARY
##    5: 44433826          Zarreia        Triplett KIPP ASCEND PRIMARY
##   ---                                                              
## 1058: 50295665            Cesar           Avila KIPP Ascend Primary
## 1059: 50295672        Gabrielle            Swan KIPP Ascend Primary
## 1060: 50295672        Gabrielle            Swan KIPP Ascend Primary
## 1061: 50295673         Benjamin            Swan KIPP Ascend Primary
## 1062: 50295673         Benjamin            Swan KIPP Ascend Primary
##       Fall_Grade                   ClassName     Subject Fall_GM
##    1:          1       Grade_1A Boesch NYU 1 Mathematics    True
##    2:          1       Grade_1A Boesch NYU 1     Reading    True
##    3:          1 Grade_1B Timmins Berkeley 1 Mathematics    True
##    4:          1 Grade_1B Timmins Berkeley 1     Reading    True
##    5:          1      Grade_1D Cousin Iowa 1     Reading    True
##   ---                                                           
## 1058:          2                    Columbia     Reading    TRUE
## 1059:          K                    Michigan Mathematics    TRUE
## 1060:          K                    Michigan     Reading    TRUE
## 1061:          1                      Loyola Mathematics    TRUE
## 1062:          1                      Loyola     Reading    TRUE
##                 Fall_TT Fall_RIT Fall_Pctl TypicalFallToSpringGrowth
##    1: Survey with Goals      170        70                     14.71
##    2: Survey with Goals      174        86                     16.74
##    3: Survey with Goals      156        31                     17.12
##    4: Survey with Goals      151        23                     16.49
##    5: Survey with Goals      166        67                     16.66
##   ---                                                               
## 1058: Survey With Goals      196        90                     10.97
## 1059: Survey With Goals      133        18                     17.55
## 1060: Survey With Goals      146        62                     14.70
## 1061: Survey With Goals      165        56                     15.57
## 1062: Survey With Goals      154        31                     16.53
##       ReportedFallToSpringGrowth SDFallToSpringGrowth
##    1:                         15                 6.85
##    2:                         17                 8.13
##    3:                         17                 6.85
##    4:                         16                 8.13
##    5:                         17                 8.13
##   ---                                                
## 1058:                         11                 7.70
## 1059:                         18                 8.24
## 1060:                         15                 7.23
## 1061:                         16                 6.85
## 1062:                         17                 8.13
##       TypicalFallToWinterGrowth ReportedFallToWinterGrowth
##    1:                      8.86                          9
##    2:                     10.44                         10
##    3:                     10.06                         10
##    4:                     10.30                         10
##    5:                     10.39                         10
##   ---                                                     
## 1058:                      6.30                          6
## 1059:                      8.38                          8
## 1060:                      8.23                          8
## 1061:                      9.29                          9
## 1062:                     10.32                         10
##       SDFallToWinterGrowth TypicalFallToFallGrowth
##    1:                 5.02                   13.63
##    2:                 5.65                   15.52
##    3:                 5.02                   17.05
##    4:                 5.65                   15.17
##    5:                 5.65                   15.40
##   ---                                             
## 1058:                 5.47                   10.30
## 1059:                 5.35                   21.02
## 1060:                 5.15                   17.45
## 1061:                 5.02                   14.85
## 1062:                 5.65                   15.22
##       ReportedFallToFallGrowth SDFallToFallGrowth
##    1:                       14               8.72
##    2:                       16              10.50
##    3:                       17               8.72
##    4:                       15              10.50
##    5:                       15              10.50
##   ---                                            
## 1058:                       10               9.86
## 1059:                       21              10.17
## 1060:                       17               9.31
## 1061:                       15               8.72
## 1062:                       15              10.50
##       TypicalSpringToSpringGrowth ReportedSpringToSpringGrowth
##    1:                       16.21                           16
##    2:                       15.74                           16
##    3:                       18.60                           19
##    4:                       20.04                           20
##    5:                       17.24                           17
##   ---                                                         
## 1058:                       10.55                           11
## 1059:                       29.75                           30
## 1060:                       20.76                           21
## 1061:                       17.06                           17
## 1062:                       19.48                           19
##       SDSpringToSpringGrowth TypicalWinterToSpringGrowth
##    1:                   8.21                        5.85
##    2:                   9.86                        6.31
##    3:                   8.21                        7.06
##    4:                   9.86                        6.19
##    5:                   9.86                        6.27
##   ---                                                   
## 1058:                   7.95                        4.67
## 1059:                   8.72                        9.17
## 1060:                  10.50                        6.47
## 1061:                   8.21                        6.28
## 1062:                   9.86                        6.21
##       ReportedWinterToSpringGrowth SDWinterToSpringGrowth Fall_Quartile
##    1:                            6                   5.02             3
##    2:                            6                   5.32             4
##    3:                            7                   5.02             2
##    4:                            6                   5.32             1
##    5:                            6                   5.32             3
##   ---                                                                  
## 1058:                            5                   5.18             4
## 1059:                            9                   5.70             1
## 1060:                            6                   5.15             3
## 1061:                            6                   5.02             3
## 1062:                            6                   5.32             2
##       Winter_Grade Winter_RIT Winter_Pctl Winter_Quartile Year Quartile
##    1:            1        175          58               3 2012       NA
##    2:            1        164          31               2 2012       NA
##    3:            1        161          19               1 2012        2
##    4:            1        135           1               1 2012        1
##    5:            1        175          63               3 2012       NA
##   ---                                                                  
## 1058:            2        205          93               4 2013       NA
## 1059:            0        177          98               4 2013        1
## 1060:            0        162          84               4 2013       NA
## 1061:            1        183          80               4 2013       NA
## 1062:            1        187          89               4 2013        2
```

```
##             ID StudentFirstName StudentLastName          SchoolName
##    1: 44339927           Andrew           Roman KIPP ASCEND PRIMARY
##    2: 44339927           Andrew           Roman KIPP ASCEND PRIMARY
##    3: 44433818            Brian           Pride KIPP ASCEND PRIMARY
##    4: 44433818            Brian           Pride KIPP ASCEND PRIMARY
##    5: 44433826          Zarreia        Triplett KIPP ASCEND PRIMARY
##   ---                                                              
## 1058: 50295665            Cesar           Avila KIPP Ascend Primary
## 1059: 50295672        Gabrielle            Swan KIPP Ascend Primary
## 1060: 50295672        Gabrielle            Swan KIPP Ascend Primary
## 1061: 50295673         Benjamin            Swan KIPP Ascend Primary
## 1062: 50295673         Benjamin            Swan KIPP Ascend Primary
##       Fall_Grade                   ClassName     Subject Fall_GM
##    1:          1       Grade_1A Boesch NYU 1 Mathematics    True
##    2:          1       Grade_1A Boesch NYU 1     Reading    True
##    3:          1 Grade_1B Timmins Berkeley 1 Mathematics    True
##    4:          1 Grade_1B Timmins Berkeley 1     Reading    True
##    5:          1      Grade_1D Cousin Iowa 1     Reading    True
##   ---                                                           
## 1058:          2                    Columbia     Reading    TRUE
## 1059:          K                    Michigan Mathematics    TRUE
## 1060:          K                    Michigan     Reading    TRUE
## 1061:          1                      Loyola Mathematics    TRUE
## 1062:          1                      Loyola     Reading    TRUE
##                 Fall_TT Fall_RIT Fall_Pctl TypicalFallToSpringGrowth
##    1: Survey with Goals      170        70                     14.71
##    2: Survey with Goals      174        86                     16.74
##    3: Survey with Goals      156        31                     17.12
##    4: Survey with Goals      151        23                     16.49
##    5: Survey with Goals      166        67                     16.66
##   ---                                                               
## 1058: Survey With Goals      196        90                     10.97
## 1059: Survey With Goals      133        18                     17.55
## 1060: Survey With Goals      146        62                     14.70
## 1061: Survey With Goals      165        56                     15.57
## 1062: Survey With Goals      154        31                     16.53
##       ReportedFallToSpringGrowth SDFallToSpringGrowth
##    1:                         15                 6.85
##    2:                         17                 8.13
##    3:                         17                 6.85
##    4:                         16                 8.13
##    5:                         17                 8.13
##   ---                                                
## 1058:                         11                 7.70
## 1059:                         18                 8.24
## 1060:                         15                 7.23
## 1061:                         16                 6.85
## 1062:                         17                 8.13
##       TypicalFallToWinterGrowth ReportedFallToWinterGrowth
##    1:                      8.86                          9
##    2:                     10.44                         10
##    3:                     10.06                         10
##    4:                     10.30                         10
##    5:                     10.39                         10
##   ---                                                     
## 1058:                      6.30                          6
## 1059:                      8.38                          8
## 1060:                      8.23                          8
## 1061:                      9.29                          9
## 1062:                     10.32                         10
##       SDFallToWinterGrowth TypicalFallToFallGrowth
##    1:                 5.02                   13.63
##    2:                 5.65                   15.52
##    3:                 5.02                   17.05
##    4:                 5.65                   15.17
##    5:                 5.65                   15.40
##   ---                                             
## 1058:                 5.47                   10.30
## 1059:                 5.35                   21.02
## 1060:                 5.15                   17.45
## 1061:                 5.02                   14.85
## 1062:                 5.65                   15.22
##       ReportedFallToFallGrowth SDFallToFallGrowth
##    1:                       14               8.72
##    2:                       16              10.50
##    3:                       17               8.72
##    4:                       15              10.50
##    5:                       15              10.50
##   ---                                            
## 1058:                       10               9.86
## 1059:                       21              10.17
## 1060:                       17               9.31
## 1061:                       15               8.72
## 1062:                       15              10.50
##       TypicalSpringToSpringGrowth ReportedSpringToSpringGrowth
##    1:                       16.21                           16
##    2:                       15.74                           16
##    3:                       18.60                           19
##    4:                       20.04                           20
##    5:                       17.24                           17
##   ---                                                         
## 1058:                       10.55                           11
## 1059:                       29.75                           30
## 1060:                       20.76                           21
## 1061:                       17.06                           17
## 1062:                       19.48                           19
##       SDSpringToSpringGrowth TypicalWinterToSpringGrowth
##    1:                   8.21                        5.85
##    2:                   9.86                        6.31
##    3:                   8.21                        7.06
##    4:                   9.86                        6.19
##    5:                   9.86                        6.27
##   ---                                                   
## 1058:                   7.95                        4.67
## 1059:                   8.72                        9.17
## 1060:                  10.50                        6.47
## 1061:                   8.21                        6.28
## 1062:                   9.86                        6.21
##       ReportedWinterToSpringGrowth SDWinterToSpringGrowth Fall_Quartile
##    1:                            6                   5.02             3
##    2:                            6                   5.32             4
##    3:                            7                   5.02             2
##    4:                            6                   5.32             1
##    5:                            6                   5.32             3
##   ---                                                                  
## 1058:                            5                   5.18             4
## 1059:                            9                   5.70             1
## 1060:                            6                   5.15             3
## 1061:                            6                   5.02             3
## 1062:                            6                   5.32             2
##       Winter_Grade Winter_RIT Winter_Pctl Winter_Quartile Year Quartile
##    1:            1        175          58               3 2012        3
##    2:            1        164          31               2 2012       NA
##    3:            1        161          19               1 2012        2
##    4:            1        135           1               1 2012        1
##    5:            1        175          63               3 2012        3
##   ---                                                                  
## 1058:            2        205          93               4 2013       NA
## 1059:            0        177          98               4 2013        1
## 1060:            0        162          84               4 2013        3
## 1061:            1        183          80               4 2013        3
## 1062:            1        187          89               4 2013        2
```

```
##             ID StudentFirstName StudentLastName          SchoolName
##    1: 44339927           Andrew           Roman KIPP ASCEND PRIMARY
##    2: 44339927           Andrew           Roman KIPP ASCEND PRIMARY
##    3: 44433818            Brian           Pride KIPP ASCEND PRIMARY
##    4: 44433818            Brian           Pride KIPP ASCEND PRIMARY
##    5: 44433826          Zarreia        Triplett KIPP ASCEND PRIMARY
##   ---                                                              
## 1058: 50295665            Cesar           Avila KIPP Ascend Primary
## 1059: 50295672        Gabrielle            Swan KIPP Ascend Primary
## 1060: 50295672        Gabrielle            Swan KIPP Ascend Primary
## 1061: 50295673         Benjamin            Swan KIPP Ascend Primary
## 1062: 50295673         Benjamin            Swan KIPP Ascend Primary
##       Fall_Grade                   ClassName     Subject Fall_GM
##    1:          1       Grade_1A Boesch NYU 1 Mathematics    True
##    2:          1       Grade_1A Boesch NYU 1     Reading    True
##    3:          1 Grade_1B Timmins Berkeley 1 Mathematics    True
##    4:          1 Grade_1B Timmins Berkeley 1     Reading    True
##    5:          1      Grade_1D Cousin Iowa 1     Reading    True
##   ---                                                           
## 1058:          2                    Columbia     Reading    TRUE
## 1059:          K                    Michigan Mathematics    TRUE
## 1060:          K                    Michigan     Reading    TRUE
## 1061:          1                      Loyola Mathematics    TRUE
## 1062:          1                      Loyola     Reading    TRUE
##                 Fall_TT Fall_RIT Fall_Pctl TypicalFallToSpringGrowth
##    1: Survey with Goals      170        70                     14.71
##    2: Survey with Goals      174        86                     16.74
##    3: Survey with Goals      156        31                     17.12
##    4: Survey with Goals      151        23                     16.49
##    5: Survey with Goals      166        67                     16.66
##   ---                                                               
## 1058: Survey With Goals      196        90                     10.97
## 1059: Survey With Goals      133        18                     17.55
## 1060: Survey With Goals      146        62                     14.70
## 1061: Survey With Goals      165        56                     15.57
## 1062: Survey With Goals      154        31                     16.53
##       ReportedFallToSpringGrowth SDFallToSpringGrowth
##    1:                         15                 6.85
##    2:                         17                 8.13
##    3:                         17                 6.85
##    4:                         16                 8.13
##    5:                         17                 8.13
##   ---                                                
## 1058:                         11                 7.70
## 1059:                         18                 8.24
## 1060:                         15                 7.23
## 1061:                         16                 6.85
## 1062:                         17                 8.13
##       TypicalFallToWinterGrowth ReportedFallToWinterGrowth
##    1:                      8.86                          9
##    2:                     10.44                         10
##    3:                     10.06                         10
##    4:                     10.30                         10
##    5:                     10.39                         10
##   ---                                                     
## 1058:                      6.30                          6
## 1059:                      8.38                          8
## 1060:                      8.23                          8
## 1061:                      9.29                          9
## 1062:                     10.32                         10
##       SDFallToWinterGrowth TypicalFallToFallGrowth
##    1:                 5.02                   13.63
##    2:                 5.65                   15.52
##    3:                 5.02                   17.05
##    4:                 5.65                   15.17
##    5:                 5.65                   15.40
##   ---                                             
## 1058:                 5.47                   10.30
## 1059:                 5.35                   21.02
## 1060:                 5.15                   17.45
## 1061:                 5.02                   14.85
## 1062:                 5.65                   15.22
##       ReportedFallToFallGrowth SDFallToFallGrowth
##    1:                       14               8.72
##    2:                       16              10.50
##    3:                       17               8.72
##    4:                       15              10.50
##    5:                       15              10.50
##   ---                                            
## 1058:                       10               9.86
## 1059:                       21              10.17
## 1060:                       17               9.31
## 1061:                       15               8.72
## 1062:                       15              10.50
##       TypicalSpringToSpringGrowth ReportedSpringToSpringGrowth
##    1:                       16.21                           16
##    2:                       15.74                           16
##    3:                       18.60                           19
##    4:                       20.04                           20
##    5:                       17.24                           17
##   ---                                                         
## 1058:                       10.55                           11
## 1059:                       29.75                           30
## 1060:                       20.76                           21
## 1061:                       17.06                           17
## 1062:                       19.48                           19
##       SDSpringToSpringGrowth TypicalWinterToSpringGrowth
##    1:                   8.21                        5.85
##    2:                   9.86                        6.31
##    3:                   8.21                        7.06
##    4:                   9.86                        6.19
##    5:                   9.86                        6.27
##   ---                                                   
## 1058:                   7.95                        4.67
## 1059:                   8.72                        9.17
## 1060:                  10.50                        6.47
## 1061:                   8.21                        6.28
## 1062:                   9.86                        6.21
##       ReportedWinterToSpringGrowth SDWinterToSpringGrowth Fall_Quartile
##    1:                            6                   5.02             3
##    2:                            6                   5.32             4
##    3:                            7                   5.02             2
##    4:                            6                   5.32             1
##    5:                            6                   5.32             3
##   ---                                                                  
## 1058:                            5                   5.18             4
## 1059:                            9                   5.70             1
## 1060:                            6                   5.15             3
## 1061:                            6                   5.02             3
## 1062:                            6                   5.32             2
##       Winter_Grade Winter_RIT Winter_Pctl Winter_Quartile Year Quartile
##    1:            1        175          58               3 2012        3
##    2:            1        164          31               2 2012        4
##    3:            1        161          19               1 2012        2
##    4:            1        135           1               1 2012        1
##    5:            1        175          63               3 2012        3
##   ---                                                                  
## 1058:            2        205          93               4 2013        4
## 1059:            0        177          98               4 2013        1
## 1060:            0        162          84               4 2013        3
## 1061:            1        183          80               4 2013        3
## 1062:            1        187          89               4 2013        2
```

```
## Error: RHS of assignment to new column 'Quartile' is zero length but not
## empty list(). For new columns the RHS must either be empty list() to
## create an empty list column, or, have length > 0; e.g. NA_integer_, 0L,
## etc.
```

```
## Error: RHS of assignment to new column 'Quartile' is zero length but not
## empty list(). For new columns the RHS must either be empty list() to
## create an empty list column, or, have length > 0; e.g. NA_integer_, 0L,
## etc.
```

```
## Error: RHS of assignment to new column 'Quartile' is zero length but not
## empty list(). For new columns the RHS must either be empty list() to
## create an empty list column, or, have length > 0; e.g. NA_integer_, 0L,
## etc.
```

```
## Error: RHS of assignment to new column 'Quartile' is zero length but not
## empty list(). For new columns the RHS must either be empty list() to
## create an empty list column, or, have length > 0; e.g. NA_integer_, 0L,
## etc.
```

```
## Error: RHS of assignment to new column 'MetExpGrowth' is zero length but
## not empty list(). For new columns the RHS must either be empty list() to
## create an empty list column, or, have length > 0; e.g. NA_integer_, 0L,
## etc.
```

```
## Error: object 'Quartile' not found
```

```
## Error: RHS of assignment to new column 'Quartile' is zero length but not
## empty list(). For new columns the RHS must either be empty list() to
## create an empty list column, or, have length > 0; e.g. NA_integer_, 0L,
## etc.
```

```
## Error: RHS of assignment to new column 'Quartile' is zero length but not
## empty list(). For new columns the RHS must either be empty list() to
## create an empty list column, or, have length > 0; e.g. NA_integer_, 0L,
## etc.
```

```
## Error: RHS of assignment to new column 'Quartile' is zero length but not
## empty list(). For new columns the RHS must either be empty list() to
## create an empty list column, or, have length > 0; e.g. NA_integer_, 0L,
## etc.
```

```
## Error: RHS of assignment to new column 'Quartile' is zero length but not
## empty list(). For new columns the RHS must either be empty list() to
## create an empty list column, or, have length > 0; e.g. NA_integer_, 0L,
## etc.
```

```
## Error: RHS of assignment to new column 'MetExpGrowth' is zero length but
## not empty list(). For new columns the RHS must either be empty list() to
## create an empty list column, or, have length > 0; e.g. NA_integer_, 0L,
## etc.
```

```
## Error: object 'Quartile' not found
```



```
## Error: At least one layer must contain all variables used for facetting
```

```
## Error: At least one layer must contain all variables used for facetting
```

```
## Error: object 'kccp.prop' not found
```

```
## Error: object 'p.kccp' not found
```

```
## Error: object 'p.kccp' not found
```

```
## Error: object 'kams.prop' not found
```

```
## Error: object 'p.kccp' not found
```

```
## Error: object 'p.kccp' not found
```

