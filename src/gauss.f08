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
    end subroutine swap_rows

    subroutine echelon(matrix)
      integer, intent(inout) :: matrix(:, :)
      integer :: num_rows, i, i_max, diag
      num_rows = size(matrix, dim=1)
      do diag = 1, num_rows
        if (matrix(diag, diag) == 0) then
          i_max = maxloc(matrix(diag + 1:, diag), 1) + diag + 1
          if (matrix(i_max, diag) == 1) call swap_rows(matrix, diag, i_max)
        endif
        if (matrix(diag, diag) == 1) then
          do i = diag + 1, num_rows
            if (matrix(i, diag) == 1) matrix(i, :) = ieor(matrix(i, :), matrix(diag, :))
          enddo
        endif
      enddo
    end subroutine echelon

    subroutine backsolve(matrix)
      integer, intent(inout) :: matrix(:, :)
      integer :: row, i, i_max
      do row = size(matrix, dim=1), 1, -1
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

    subroutine row_sort(matrix)
      integer, intent(inout) :: matrix(:, :)
      integer :: n, i, j, max_rowa, max_rowb
      n = size(matrix, dim=1)

      do i = 1, n
        do j = 1, n-i
          max_rowa = maxloc(matrix(j, :), 1)
          max_rowb = maxloc(matrix(j+1, :), 1)
          if (max_rowa > max_rowb) then
            call swap_rows(matrix, j, j+1)
          endif
        enddo
      enddo
    endsubroutine row_sort


    pure function decode(matrix) result(out_matrix)
      integer, intent(in) :: matrix(:, :)
      integer, allocatable :: out_matrix(:, :)
      integer, allocatable :: temp_vector(:)
      integer :: col, row, num_cols, i_max

      num_cols = size(matrix, dim=2)
      allocate(out_matrix(num_cols, num_cols))
      out_matrix(:, :) = 0

      do col = 1, num_cols
        if (all(matrix(:, col) == 0)) then
          out_matrix(col, col) = 1
        endif
      enddo

      do row = 1, size(matrix, dim=1)
        if (matrix(row, row) == 1) then
          temp_vector = matrix(row,:)
          temp_vector(row) = 0
          out_matrix(:, row) = ieor(temp_vector, out_matrix(:, row))
        elseif (any(matrix(row, :) == 1)) then
          i_max = maxloc(matrix(row, :), 1)
          temp_vector = matrix(row,:)
          temp_vector(i_max) = 0
          out_matrix(:, i_max) = ieor(temp_vector, out_matrix(:, i_max))
        else
          out_matrix(row, row) = 1
        endif
      enddo

      out_matrix = transpose(out_matrix)
      do col = 1, num_cols
        if (all(out_matrix(col, :) == 0)) then
          out_matrix(col, col) = 1
        endif
      enddo
    endfunction decode

end module gauss
