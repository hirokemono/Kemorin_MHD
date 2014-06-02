!>@file   t_read_control_arrays.f90
!!@brief  module t_read_control_arrays
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine dealloc_control_array_real(array_real)
!!      subroutine dealloc_control_array_r2(array_r2)
!!      subroutine dealloc_control_array_r3(array_r3)
!!      subroutine dealloc_control_array_c_r2(array_cr2)
!!      subroutine dealloc_control_array_i_c_r(array_icr)
!!      subroutine dealloc_control_array_i2_r(array_i2r)
!!      subroutine dealloc_control_array_i2_r2(array_i2r2)
!!
!!      subroutine read_control_array_real(label, array_real)
!!      subroutine read_control_array_r2(label, array_r2)
!!      subroutine read_control_array_r3(label, array_r3)
!!      subroutine read_control_array_c_r2(label, array_cr2)
!!      subroutine read_control_array_i_c_r(label, array_icr)
!!      subroutine read_control_array_i2_r(label, array_i2r)
!!      subroutine read_control_array_i2_r2(label, array_i2r2)
!!@endverbatim
!!
!!@n @param  label           label for control items
!!@n @param  array_real      structures for array
!!@n @param  array_r2        structures for array
!!@n @param  array_r3        structures for array
!!@n @param  array_cr2       structures for array
!!@n @param  array_icr       structures for array
!!@n @param  array_i2r       structures for array
!!@n @param  array_i2r2      structures for array
!!
      module t_read_control_arrays
!
      use m_precision
!
      implicit none
!
!>  Structure for real control array 
      type ctl_array_real
!>     number of array items
        integer(kind=kint) :: num
!>     array counter
        integer(kind=kint) :: icou
!>     array for 1st real
        real(kind = kreal), pointer :: vect(:)
      end type ctl_array_real
!
!>  Structure for two reals control array 
      type ctl_array_r2
!>     number of array items
        integer(kind=kint) :: num
!>     array counter
        integer(kind=kint) :: icou
!>     array for 1st real
        real(kind = kreal), pointer :: vec1(:)
!>     array for 2nd real
        real(kind = kreal), pointer :: vec2(:)
      end type ctl_array_r2
!
!>  Structure for three reals control array 
      type ctl_array_r3
!>     number of array items
        integer(kind=kint) :: num
!>     array counter
        integer(kind=kint) :: icou
!>     array for 1st real
        real(kind = kreal), pointer :: vec1(:)
!>     array for 2nd real
        real(kind = kreal), pointer :: vec2(:)
!>     array for 3rd real
        real(kind = kreal), pointer :: vec3(:)
      end type ctl_array_r3
!
!>  Structure for charactor and two reals control array 
      type ctl_array_cr2
!>     number of array items
        integer(kind=kint) :: num
!>     array counter
        integer(kind=kint) :: icou
!>     array for character
        character(len=kchara), pointer :: c_tbl(:)
!>     array for 1st real
        real(kind = kreal), pointer :: vec1(:)
!>     array for 2nd real
        real(kind = kreal), pointer :: vec2(:)
      end type ctl_array_cr2
!
!>  Structure for real, charactor, and integere control array 
      type ctl_array_icr
!>     number of array items
        integer(kind=kint) :: num
!>     array counter
        integer(kind=kint) :: icou
!>     array for 1st integer
        integer(kind=kint), pointer :: ivec(:)
!>     array for character
        character(len=kchara), pointer :: c_tbl(:)
!>     array for 1st real
        real(kind = kreal), pointer :: vect(:)
      end type ctl_array_icr
!
!>  Structure for 1 real and 2 integers control array 
      type ctl_array_i2r
!>     number of array items
        integer(kind=kint) :: num
!>     array counter
        integer(kind=kint) :: icou
!>     array for 1st integer
        integer(kind=kint), pointer :: int1(:)
!>     array for 2nd integer
        integer(kind=kint), pointer :: int2(:)
!>     array for 1st real
        real(kind = kreal), pointer :: vect(:)
      end type ctl_array_i2r
!
!>  Structure for 2 reals and 2 integeres control array 
      type ctl_array_i2r2
!>     number of array items
        integer(kind=kint) :: num
!>     array counter
        integer(kind=kint) :: icou
!>     array for 1st integer
        integer(kind=kint), pointer :: int1(:)
!>     array for 2nd integer
        integer(kind=kint), pointer :: int2(:)
!>     array for 1st real
        real(kind = kreal), pointer :: vec1(:)
!>     array for 2nd real
        real(kind = kreal), pointer :: vec2(:)
      end type ctl_array_i2r2
!
      private :: alloc_control_array_real
      private :: alloc_control_array_r2, alloc_control_array_r3
      private :: alloc_control_array_c_r2, alloc_control_array_i_c_r
      private :: alloc_control_array_i2_r, alloc_control_array_i2_r2
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_real(array_real)
!
      type(ctl_array_real), intent(inout) :: array_real
!
!
      allocate( array_real%vect(array_real%num) )
!
      if(array_real%num .eq. 0) return
      array_real%vect = 0.0d0
!
      end subroutine alloc_control_array_real
!
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_r2(array_r2)
!
      type(ctl_array_r2), intent(inout) :: array_r2
!
!
      allocate( array_r2%vec1(array_r2%num) )
      allocate( array_r2%vec2(array_r2%num) )
!
      if(array_r2%num .eq. 0) return
      array_r2%vec1 = 0.0d0
      array_r2%vec2 = 0.0d0
!
      end subroutine alloc_control_array_r2
!
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_r3(array_r3)
!
      type(ctl_array_r3), intent(inout) :: array_r3
!
!
      allocate( array_r3%vec1(array_r3%num) )
      allocate( array_r3%vec2(array_r3%num) )
      allocate( array_r3%vec3(array_r3%num) )
!
      if(array_r3%num .eq. 0) return
      array_r3%vec1 = 0.0d0
      array_r3%vec2 = 0.0d0
      array_r3%vec3 = 0.0d0
!
      end subroutine alloc_control_array_r3
!
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_c_r2(array_cr2)
!
      type(ctl_array_cr2), intent(inout) :: array_cr2
!
!
      allocate( array_cr2%c_tbl(array_cr2%num) )
      allocate( array_cr2%vec1(array_cr2%num) )
      allocate( array_cr2%vec2(array_cr2%num) )
!
      if(array_cr2%num .eq. 0) return
      array_cr2%vec1 = 0.0d0
      array_cr2%vec2 = 0.0d0
!
      end subroutine alloc_control_array_c_r2
!
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_i_c_r(array_icr)
!
      type(ctl_array_icr), intent(inout) :: array_icr
!
!
      allocate( array_icr%ivec(array_icr%num) )
      allocate( array_icr%c_tbl(array_icr%num) )
      allocate( array_icr%vect(array_icr%num) )
!
      if(array_icr%num .eq. 0) return
      array_icr%ivec =     0
      array_icr%vect = 0.0d0
!
      end subroutine alloc_control_array_i_c_r
!
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_i2_r(array_i2r)
!
      type(ctl_array_i2r), intent(inout) :: array_i2r
!
!
      allocate( array_i2r%int1(array_i2r%num) )
      allocate( array_i2r%int2(array_i2r%num) )
      allocate( array_i2r%vect(array_i2r%num) )
!
      if(array_i2r%num .eq. 0) return
      array_i2r%int1 =     0
      array_i2r%int2 =     0
      array_i2r%vect = 0.0d0
!
      end subroutine alloc_control_array_i2_r
!
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_i2_r2(array_i2r2)
!
      type(ctl_array_i2r2), intent(inout) :: array_i2r2
!
!
      allocate( array_i2r2%int1(array_i2r2%num) )
      allocate( array_i2r2%int2(array_i2r2%num) )
      allocate( array_i2r2%vec1(array_i2r2%num) )
      allocate( array_i2r2%vec2(array_i2r2%num) )
!
      if(array_i2r2%num .eq. 0) return
      array_i2r2%int1 =     0
      array_i2r2%int2 =     0
      array_i2r2%vec1 = 0.0d0
      array_i2r2%vec2 = 0.0d0
!
      end subroutine alloc_control_array_i2_r2
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_real(array_real)
!
      type(ctl_array_real), intent(inout) :: array_real
!
!
      deallocate(array_real%vect)
!
      end subroutine dealloc_control_array_real
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_r2(array_r2)
!
      type(ctl_array_r2), intent(inout) :: array_r2
!
!
      deallocate(array_r2%vec1, array_r2%vec2)
!
      end subroutine dealloc_control_array_r2
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_r3(array_r3)
!
      type(ctl_array_r3), intent(inout) :: array_r3
!
!
      deallocate(array_r3%vec1, array_r3%vec2, array_r3%vec3)
!
      end subroutine dealloc_control_array_r3
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_c_r2(array_cr2)
!
      type(ctl_array_cr2), intent(inout) :: array_cr2
!
!
      deallocate(array_cr2%c_tbl, array_cr2%vec1, array_cr2%vec2)
!
      end subroutine dealloc_control_array_c_r2
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_i_c_r(array_icr)
!
      type(ctl_array_icr), intent(inout) :: array_icr
!
!
      deallocate( array_icr%ivec, array_icr%c_tbl, array_icr%vect)
!
      end subroutine dealloc_control_array_i_c_r
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_i2_r(array_i2r)
!
      type(ctl_array_i2r), intent(inout) :: array_i2r
!
!
      deallocate( array_i2r%int1, array_i2r%int2, array_i2r%vect)
!
      end subroutine dealloc_control_array_i2_r
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_i2_r2(array_i2r2)
!
      type(ctl_array_i2r2), intent(inout) :: array_i2r2
!
!
      deallocate( array_i2r2%int1, array_i2r2%int2)
      deallocate( array_i2r2%vec1, array_i2r2%vec2)
!
      end subroutine dealloc_control_array_i2_r2
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_array_real(label, array_real)
!
      use m_read_control_elements
!
      character(len=kchara), intent(in) :: label
      type(ctl_array_real), intent(inout) :: array_real
!
!
      call find_control_array_flag(label, array_real%num)
      if(array_real%num.gt.0 .and. array_real%icou.eq.0) then
        call alloc_control_array_real(array_real)
        call read_control_array_real_list(label, array_real%num,        &
     &      array_real%icou, array_real%vect)
      end if
!
      end subroutine read_control_array_real
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_r2(label, array_r2)
!
      use m_read_control_elements
!
      character(len=kchara), intent(in) :: label
      type(ctl_array_r2), intent(inout) :: array_r2
!
!
      call find_control_array_flag(label, array_r2%num)
      if(array_r2%num.gt.0 .and. array_r2%icou.eq.0) then
        call alloc_control_array_r2(array_r2)
        call read_control_array_real2_list(label, array_r2%num,         &
     &      array_r2%icou, array_r2%vec1, array_r2%vec2)
      end if
!
      end subroutine read_control_array_r2
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_r3(label, array_r3)
!
      use m_read_control_elements
!
      character(len=kchara), intent(in) :: label
      type(ctl_array_r3), intent(inout) :: array_r3
!
!
      call find_control_array_flag(label, array_r3%num)
      if(array_r3%num.gt.0 .and. array_r3%icou.eq.0) then
        call alloc_control_array_r3(array_r3)
        call read_control_array_real3_list(label, array_r3%num,         &
     &      array_r3%icou, array_r3%vec1, array_r3%vec2, array_r3%vec3)
      end if
!
      end subroutine read_control_array_r3
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_c_r2(label, array_cr2)
!
      use m_read_control_elements
!
      character(len=kchara), intent(in) :: label
      type(ctl_array_cr2), intent(inout) :: array_cr2
!
!
      call find_control_array_flag(label, array_cr2%num)
      if(array_cr2%num.gt.0 .and. array_cr2%icou.eq.0) then
        call alloc_control_array_c_r2(array_cr2)
        call read_control_array_c_r2_list(label, array_cr2%num,         &
     &      array_cr2%icou, array_cr2%c_tbl,                            &
     &      array_cr2%vec1, array_cr2%vec2)
      end if
!
      end subroutine read_control_array_c_r2
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_i_c_r(label, array_icr)
!
      use m_read_control_elements
!
      character(len=kchara), intent(in) :: label
      type(ctl_array_icr), intent(inout) :: array_icr
!
!
      call find_control_array_flag(label, array_icr%num)
      if(array_icr%num.gt.0 .and. array_icr%icou.eq.0) then
        call alloc_control_array_i_c_r(array_icr)
        call read_control_array_i_c_r_list(label, array_icr%num,        &
     &    array_icr%icou, array_icr%ivec, array_icr%c_tbl,              &
     &    array_icr%vect)
      end if
!
      end subroutine read_control_array_i_c_r
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_i2_r(label, array_i2r)
!
      use m_read_control_elements
!
      character(len=kchara), intent(in) :: label
      type(ctl_array_i2r), intent(inout) :: array_i2r
!
!
      call find_control_array_flag(label, array_i2r%num)
      if(array_i2r%num.gt.0 .and. array_i2r%icou.eq.0) then
        call alloc_control_array_i2_r(array_i2r)
        call read_control_array_i2_r_list(label, array_i2r%num,         &
     &    array_i2r%icou, array_i2r%int1, array_i2r%int2,               &
     &    array_i2r%vect)
      end if
!
      end subroutine read_control_array_i2_r
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_i2_r2(label, array_i2r2)
!
      use m_read_control_elements
!
      character(len=kchara), intent(in) :: label
      type(ctl_array_i2r2), intent(inout) :: array_i2r2
!
!
      call find_control_array_flag(label, array_i2r2%num)
      if(array_i2r2%num.gt.0 .and. array_i2r2%icou.eq.0) then
        call alloc_control_array_i2_r2(array_i2r2)
        call read_control_array_i2_r2_list(label, array_i2r2%num,       &
     &    array_i2r2%icou, array_i2r2%int1, array_i2r2%int2,            &
     &    array_i2r2%vec1, array_i2r2%vec2)
      end if
!
      end subroutine read_control_array_i2_r2
!
!   --------------------------------------------------------------------
!
      end module t_read_control_arrays
