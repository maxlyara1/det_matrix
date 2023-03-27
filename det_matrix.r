# Определение функции для вычисления определителя
det_matrix <- function(matrix) {
  # Проверка на то, что входной параметр является квадратной матрицей
  if (dim(matrix)[1] != dim(matrix)[2]) {
    stop("На ввод должна подаваться квадратная матрица")
  }
  
  # Cлучаи для определителей матриц 1 и 2 порядка
  if (dim(matrix)[1] == 1) {
    return(matrix[1, 1])
  } 
  else if (dim(matrix)[1] == 2) {
    return(matrix[1, 1]*matrix[2, 2] - matrix[1, 2]*matrix[2, 1])
  }
  
  # Случай для определителей матриц порядка 3+ (через рекурсию)
  det = 0
  for (i in 1:nrow(matrix)) {
    znak = (-1)^(i+1)
    minor = matrix[-i, -1]
    det = det + znak*matrix[i, 1]*det_matrix(minor)
  }
  return(det)
}

matrix <- matrix(c(
                1, 3, 1, 3, 6,
                4, 5, 1, 8, 7,
                3, 0, 0, 4, 15,
                2, 0, 1, 2, 9,
                6, 8, 9, 10, 1), nrow=5, ncol=5, byrow=TRUE)
cat('Определитель равен:',det_matrix(matrix))
