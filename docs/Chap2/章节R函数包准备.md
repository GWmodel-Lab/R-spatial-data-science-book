### tidyverse

`tidyverse` 包是由 Hadley Wickham 及其团队开发的数据科学工具的集合，是基于整洁数据原则的一套一致、优雅的工具。它提供了一种现代、优雅的方式进行数据科学工作，采用管道式和泛函编程技术，涵盖了整个数据科学流程，包括数据导入、清洗、操作、可视化、建模以及生成可重现的交互式报告。

在 `tidyverse` 中，操作数据的方式非常优雅，每一步都清晰地表达了“做什么”。通过使用管道操作符 `%>%`，可以流畅地链式调用各种数据处理函数，使得代码读起来像一篇自然流畅的叙述，毫无滞涩。

`tidyverse` 包含了多个数据科学和数据分析相关的包，其中包括 `ggplot2`、`dplyr`、`tidyr`、`readr`、`purrr`、`tibble` 等。使用 `tidyverse` 的优势在于，你只需加载 `tidyverse` 包，而不必逐个加载其中的每个子包，因为 `tidyverse` 会自动引入所需的包。

```R
library(tidyverse)
```

通过这种简洁而一致的工作流，`tidyverse` 提供了强大而灵活的工具，使得数据科学工作更加高效和愉悦。

数据读写的包与函数汇总
#### readr 包

`readr` 包提供了读写带分隔符的文本文件的功能，包括常见的 csv 和 tsv 格式，同时支持序列化的 R 对象 rds。使用 readr 包的相关函数可以将数据读入 R 中，并以数据框（data.frame）的形式存储：

+ 读入数据到数据框：`read_csv()` 和 `read_tsv()`
+ 读入欧式格式数据：`read_csv2()` 和 `read_tsv2()`
+ 读写 rds 数据：`read_rds()` 和 `write_rds()`
+ 写出数据到文件：`write_csv()`, `write_tsv()`, `write_csv2()`, `write_tsv2()`
+ 转化数据类型：`parse_number()`, `parse_logical()`, `parse_factor()` 等

> 欧式格式数据指的是与北美地区使用的逗号（`,`）作为小数点分隔符不同的一种数值格式。在欧洲一些国家，小数点通常表示为逗号（`,`），而不是点号（`.`）。使用 readr 包的相关函数，你可以轻松地读取和处理这种欧式格式的数据。

#### readxl 包（不包含于tidyverse中）

`readxl` 包专门用于读取 Excel 文件，包括同一个工作簿中的不同工作表。该包提供了以下函数：

+ `read_excel()`: 自动检测 xls 或 xlsx 文件
+ `read_xls()`: 读取 xls 文件
+ `read_xlsx()`: 读取 xlsx 文件

如果你需要处理 Excel 文件，readxl 包是一个非常方便的选择。此外，你还可以考虑使用 `openxlsx` 包，它也提供了读写 Excel 文件的功能。

#### haven 包

`haven` 包专门用于读写 SPSS、Stata 和 SAS 数据文件。该包提供了以下函数：

+ 读: `read_spss()`, `read_dta()`, `read_sas()`
+ 写: `write_spss(),` `write_stata()`, `write_sas()`

如果你的数据来自于这些统计软件的输出，haven 包可以帮助你高效地导入和导出数据。

#### jsonlite 包

`jsonlite` 包提供了读写 JSON 数据的功能，同时支持将 R 数据结构与 JSON 相互转换：

+ 读：`read_json()`, `fromJSON()`
+ 写：`write_json()`, `toJSON()`

当你需要处理 JSON 格式的数据时，jsonlite 包是一个强大而灵活的工具。

#### readtext 包

`readtext` 包专门用于读取全部文本文件的内容到数据框，每个文件变成一行。这在文本挖掘或数据收集时非常有用。此外，readtext 包还支持读取多种其他格式的文件，包括 csv、tab、json、xml、html、pdf、doc、docx、rtf、xls、xlsx 等。使用 readtext 包，你可以方便地将不同格式的文本数据导入 R 环境。