program myprog
  use utility
  use gauss
  implicit none

  integer, allocatable :: edge_matrix(:, :)
  integer num_columns, num_rows, i, j
  real time1, time2
  integer, allocatable :: test_ops(:, :)
  num_columns = 1400
  num_rows = 700
  call cpu_time(time1)
  allocate(edge_matrix(num_rows, num_columns))
  do i = 1, num_rows
    do j = 1, num_columns
      edge_matrix(i, j) = random_int(0, 1)
    enddo
  enddo
  call cpu_time(time2)
  print *, "init: ", time2 - time1

  ! do j=1, num_rows
  !   write(*,"(10(I2,a))") (edge_matrix(j, i),',',i = 1, num_columns)
  ! enddo

  call cpu_time(time1)
  call echelon(edge_matrix)
  call cpu_time(time2)
  print *, "echelon: ", time2 - time1

  call cpu_time(time1)
  call row_sort(edge_matrix)
  call cpu_time(time2)
  print *, "sort: ", time2 - time1

  ! print *, "============================================"

  ! do j=1, num_rows
  !  write(*,"(10(I2,a))") (edge_matrix(j, i),',',i = 1, num_columns)
  ! enddo

  call cpu_time(time1)
  call backsolve(edge_matrix)
  call cpu_time(time2)
  print *, "backsolve: ", time2 - time1

  call cpu_time(time1)
  test_ops = decode(edge_matrix)
  call cpu_time(time2)
  print *, "decode: ", time2 - time1

  ! print *, "============================================"
  ! do j=1, num_columns
  !  write(*,"(10(I2,a))") (test_ops(j, i),',',i = 1, num_columns)
  ! enddo

end program myprog
