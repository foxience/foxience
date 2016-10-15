library(tidyr)
library(dplyr)

# Use 'select' to get some variables (columns) from data frame
# Lấy data frame từ data frame iris bao gồm các cột 
# Sepal.Length, Sepal.Width, Species và ID
select(iris, Sepal.Length, Sepal.Width, Species, ID)

# Use 'filter' to filter out observations (rows) from data frame
# Lọc ra những mẫu nào có Species là setosa
filter(iris, Species == 'setosa')

# 'mutate' is used to create a new variable (column) based on other variables
# Tạo 1 cột mới có tên là Sepal.Ratio được tính bằng tỉ lệ Sepal.Length va Sepal.Width
mutate(iris, Sepal.Ratio = Sepal.Length / Sepal.Width)

# special operator %>%
# %>% là ký tự đặc biệt cho phép mình dùng kết quả của phép tính trước 
# làm đầu vào của phép tính sau
iris %>%    # dùng data frame iris cho các phép biến đổi sau
  select(Sepal.Length, Sepal.Width, Species, ID) %>%
  mutate(Sepal.Ratio = Sepal.Length / Sepal.Width) %>%
  filter(Sepal.Ratio > 1.5)

# chuỗi biến đổi trên sẽ đi từ data frame iris
# select lấy ra các cột Sepal.Length, Sepal.Width, Species, ID
# mutate tạo thêm 1 cột Sepal.Ratio
# filter lọc ra những mẫu nào có Sepal.Ratio trên 1.5

# ** Lưu ý %>% dùng data frame kết quả của phép biến đổi trước 
# để làm data frame đầu vào cho phép biến đổi sau 
  