program myprog
  use utility
  use gauss
  implicit none

  integer, allocatable :: edge_matrix(:, :)
  integer num_columns, num_rows, i, j
  integer, allocatable :: test_ops(:, :)
  print *, "Hello main"
  num_columns = 10
  num_rows = 5
  allocate(edge_matrix(num_rows, num_columns))
  do i = 1, num_rows
    do j = 1, num_columns
      edge_matrix(i, j) = random_int(0, 1)
    enddo
  enddo

  do j=1, num_rows
    write(*,"(10(I2,a))") (edge_matrix(j, i),',',i = 1, num_columns)
  enddo

  call echelon(edge_matrix)
  call row_sort(edge_matrix)

  print *, "============================================"

  do j=1, num_rows
    write(*,"(10(I2,a))") (edge_matrix(j, i),',',i = 1, num_columns)
  enddo

  call backsolve(edge_matrix)
  test_ops = decode(edge_matrix)

  print *, "============================================"
  do j=1, num_columns
    write(*,"(10(I2,a))") (test_ops(j, i),',',i = 1, num_columns)
  enddo

end program myprog
