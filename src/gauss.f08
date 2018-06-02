module gauss
  implicit none
  contains

    subroutine swap_rows(matrix, i, j)
      integer, intent(inout) :: matrix(:, :)
      integer, intent(in) :: i, j
      integer, allocatable :: temp_row(:)
      temp_row = matrix(j, :)
      matrix(j, :) = matrix(i, :)
      matrix(i, :) = temp_row
    endsubroutine swap_rows

    subroutine echelon(matrix)
      integer, intent(inout) :: matrix(:, :)
      integer :: num_rows, num_columns, column = 1, row = 1, i, i_max
      num_rows = size(matrix(:, 1))
      num_columns = size(matrix(1, :))
      do while ((row <= num_rows) .and. (column <= num_columns))
        if (matrix(row, column) == 0) then
          i_max = maxloc(matrix(row + 1 : num_rows, column), row) + row + 1
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

    subroutine backsolve(matrix)
      integer, intent(inout) :: matrix(:, :)
      integer :: row, i, i_max
      do row = size(matrix(:,1)), 1, -1
        i_max = maxloc(matrix(row, :), row)
        if (matrix(row, i_max) == 1 ) then
          do i = row - 1, 1, -1
            if (matrix(i, i_max) == 1) then
              matrix(i, :) = ieor(matrix(row, :), matrix(i,:))
            endif
          enddo
        endif
      enddo
    endsubroutine

end module gauss
