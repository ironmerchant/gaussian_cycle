module gauss
  implicit none
  contains

    subroutine swap_rows(edge_matrix, i, j)
      integer, dimension(:, :), intent(inout) :: edge_matrix
      integer, allocatable, dimension(:) :: temp_row
      integer, intent(in) :: i, j
      temp_row = edge_matrix(j, :)
      edge_matrix(j, :) = edge_matrix(i, :)
      edge_matrix(i, :) = temp_row
    endsubroutine swap_rows

    subroutine echelon(matrix)
      integer, dimension(:, :), intent(inout) :: matrix
      integer num_rows, num_columns, column, row, i, i_max
      column = 1
      row = 1
      num_rows = size(matrix(:,1))
      num_columns = size(matrix(1,:))
      do while ((row <= num_rows) .and. (column <= num_columns))
        if (matrix(row, column) == 0) then
          i_max = maxloc(matrix(row + 1:num_rows, column), row) + row + 1
          if (matrix(i_max, column) == 1) call swap_rows(matrix, row, i_max)
        endif

        if (matrix(row, column) == 1) then
          do i = row + 1, num_rows
            if (matrix(i, column) == 1) matrix(i, :) = ieor(matrix(i, :), matrix(row, :))
          enddo
        endif

        row = row + 1
        column = column + 1

      enddo
    endsubroutine echelon

end module gauss
