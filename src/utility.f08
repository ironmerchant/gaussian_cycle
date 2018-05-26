module utility
contains
  subroutine qstderr(message)
    use iso_fortran_env, only : error_unit ! access computing environment
    character(len=*) :: message
    write(error_unit,'(a)') message ! write message to standard error
    print *, "MORE ERROR"
  end subroutine qstderr

  function random_int(left, right) result(rand)
    real :: random_real
    integer :: left, right, rand
    call random_number(random_real)
    rand = left + floor((right+1-left)*random_real)
  end function random_int

end module utility
