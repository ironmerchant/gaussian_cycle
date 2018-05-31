module gauss
  implicit none
  contains

    pure function maxarg(vector, offset) result(maxi)
      integer, dimension(:), intent(in) :: vector
      integer, intent(in) :: offset
      integer, dimension(:), allocatable :: max_set
      integer maxi
      max_set = maxloc(vector)
      maxi = max_set(0) + offset
    end function maxarg

    subroutine swap_rows(edge_matrix, i, j)
      integer, dimension(:, :), intent(inout) :: edge_matrix
      integer, allocatable, dimension(:) :: temp_row
      integer, intent(in) :: i, j
      temp_row = edge_matrix(j, :)
      edge_matrix(j, :) = edge_matrix(i, :)
      edge_matrix(i, :) = temp_row
    end subroutine swap_rows

    subroutine xorelimination(matrix)
      integer, dimension(:, :), intent(inout) :: matrix
      integer num_rows, num_columns, column, row
      integer i, j, i_max !initialization of the pivot row
      column = 1
      row = 1
      num_rows = size(matrix(:,1))
      num_columns = size(matrix(1,:))
      do while ((row <= num_rows) .and. (column <= num_columns))
        if (matrix(row, column) == 0) then
          i_max = maxarg(matrix(:, column), row + 1)
          if (matrix(i_max, column) == 1) call swap_rows(matrix, row, i_max)
        endif

        if (matrix(row, column) == 1) then
          do i = row + 1, num_rows
            if (matrix(i, column) == 1) then
              do j = 1, num_columns
                matrix(i, j) = xor(matrix(i, j), matrix(row, j))
              enddo
            endif
          enddo
        endif

        row = row + 1
        column = column + 1

      enddo
    end subroutine xorelimination

end module gauss
