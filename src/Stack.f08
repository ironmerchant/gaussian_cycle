module class_Stack
  type :: link
    integer :: i
    type (link), pointer :: previous
  end type link

  type, public :: StackIterator
    integer, private :: index
    type(link), pointer, private :: current
    contains
      procedure :: create => stack_iterator_create
      procedure :: next => stack_iterator_next
      procedure :: has_next => stack_iterator_has_next
      procedure :: get_index => stack_iterator_get_index
  end type StackIterator

  type, public :: Stack
    type (link), private, pointer :: current
    integer :: length
    contains
      procedure :: init => stack_init
      procedure :: push => stack_push
      procedure :: pop => stack_pop
      procedure :: dealloc => stack_dealloc
      procedure :: has_items => stack_has_items
      procedure :: peek => stack_peek
      procedure :: get_head => stack_get_head
  end type Stack

contains
  subroutine stack_iterator_create(self, in_stack)
    Class(StackIterator), intent(inout) :: self
    Class(Stack), intent(inout) :: in_stack
    self%current => in_stack%get_head()
    self%index = 0
  end subroutine stack_iterator_create

  function stack_iterator_next(self) result(out_vertex_label)
    Class(StackIterator), intent(inout) :: self
    integer :: out_vertex_label
    out_vertex_label = self%current%i
    self%current => self%current%previous
    self%index = self%index + 1
  end function stack_iterator_next

  function stack_iterator_has_next(self) result(out_bool)
    Class(StackIterator), intent(inout) :: self
    logical :: out_bool
    out_bool = associated(self%current%previous)
  end function stack_iterator_has_next

  function stack_iterator_get_index(self) result(current_index)
     Class(StackIterator), intent(inout) :: self
     integer :: current_index
     current_index = self%index
  end function stack_iterator_get_index

  function stack_get_head(self) result(out_pointer)
    Class(Stack), intent(inout) :: self
    type (link), pointer :: out_pointer
    out_pointer => self%current
  end function stack_get_head

  subroutine stack_init(self)
    Class(Stack), intent(inout) :: self
    nullify(self%current)
    self%length = 0
  end subroutine stack_init

  subroutine stack_dealloc(self)
    Class(Stack), intent(inout) :: self
    integer :: temp_datum
    do while(self%length > 0)
      temp_datum = self%pop()
    enddo
  end subroutine

  subroutine stack_push(self, datum)
    Class(Stack), intent(inout) :: self
    integer, intent(in) :: datum
    type(link), pointer :: new_leader, temp_link

    self%length = self%length + 1 ! increment length
    allocate(new_leader)  ! allocate a new leader link
    temp_link => self%current  ! save the current pointer as a temp pointer
    self%current => new_leader ! set the current pointer as the new leader
    self%current%i = datum  ! set current to new datum
    self%current%previous => temp_link ! restore the old current as the now previous link
  end subroutine stack_push

  function stack_pop(self) result(out_datum)
    use utility
    Class(Stack), intent(inout) :: self
    type(link), pointer :: temp_link
    character(len=31) :: message
    integer :: out_datum
    out_datum = 0
    if(self%length > 0) then
      out_datum = self%current%i  ! output the datum from the current link
      temp_link => self%current ! save the current link as temp_link
      self%current => self%current%previous ! set the current link to the previous
      deallocate(temp_link) ! deallocate the old leader
      self%length = self%length - 1 ! decrement the length
    else
      message = "Tried to pop empty stack!" ! throw an error if empty
      call qstderr(message)
      stop -1 ! die
    end if
  end function stack_pop

  function stack_has_items(self) result(peek_bool)
    Class(Stack), intent(inout) :: self
    logical :: peek_bool
    peek_bool = .false.
    if( self%length > 0 ) then
      peek_bool = .true.
    endif
  end function stack_has_items

  function stack_peek(self) result(out_datum)
    Class(Stack), intent(inout) :: self
    integer :: out_datum
    out_datum = self%current%i
  end function stack_peek

end module class_Stack
