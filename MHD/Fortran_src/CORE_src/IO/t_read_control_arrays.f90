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
!!      subroutine dealloc_control_array_int(array_int)
!!      subroutine dealloc_control_array_chara(array_chara)
!!
!!      subroutine read_control_array_r1(label, array_real)
!!      subroutine read_control_array_r2(label, array_r2)
!!      subroutine read_control_array_i1(label, array_int)
!!      subroutine read_control_array_c1(label, array_chara)
!!@endverbatim
!!
!!@n @param  label           label for control items
!!@n @param  array_real      structures for array
!!@n @param  array_r2        structures for array
!!@n @param  array_r3        structures for array
!!@n @param  array_int       structures for array
!!@n @param  array_i2        structures for array
!!@n @param  array_chara     structures for array
!!@n @param  array_c2        structures for array
!!@n @param  array_c3        structures for array
!!@n @param  array_ci        structures for array
!!@n @param  array_cr        structures for array
!!@n @param  array_cr2       structures for array
!!@n @param  array_c2r       structures for array
!!@n @param  array_icr       structures for array
!!@n @param  array_ir        structures for array
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
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for 1st real
        real(kind = kreal), allocatable :: vect(:)
      end type ctl_array_real
!
!>  Structure for two reals control array 
      type ctl_array_r2
!>     number of array items
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for 1st real
        real(kind = kreal), allocatable :: vec1(:)
!>     array for 2nd real
        real(kind = kreal), allocatable :: vec2(:)
      end type ctl_array_r2
!
!
!>  Structure for integer control array 
      type ctl_array_int
!>     number of array items
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for 1st real
        integer(kind = kint), allocatable :: ivec(:)
      end type ctl_array_int
!
!>  Structure for character control array 
      type ctl_array_chara
!>     number of array items
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for 1st character
        character(len=kchara), allocatable :: c_tbl(:)
      end type ctl_array_chara
!
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
      subroutine alloc_control_array_int(array_int)
!
      type(ctl_array_int), intent(inout) :: array_int
!
!
      allocate( array_int%ivec(array_int%num) )
!
      if(array_int%num .eq. 0) return
      array_int%ivec = 0
!
      end subroutine alloc_control_array_int
!
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_chara(array_chara)
!
      type(ctl_array_chara), intent(inout) :: array_chara
!
!
      allocate( array_chara%c_tbl(array_chara%num) )
!
      end subroutine alloc_control_array_chara
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_real(array_real)
!
      type(ctl_array_real), intent(inout) :: array_real
!
!
      if(allocated(array_real%vect) .eqv. .FALSE.) return
      deallocate(array_real%vect)
      array_real%num = 0
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
      if(allocated(array_r2%vec1) .eqv. .FALSE.) return
      deallocate(array_r2%vec1, array_r2%vec2)
      array_r2%num = 0
!
      end subroutine dealloc_control_array_r2
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_int(array_int)
!
      type(ctl_array_int), intent(inout) :: array_int
!
!
      if(allocated(array_int%ivec) .eqv. .FALSE.) return
      deallocate(array_int%ivec)
      array_int%num = 0
!
      end subroutine dealloc_control_array_int
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_chara(array_chara)
!
      type(ctl_array_chara), intent(inout) :: array_chara
!
!
      if(allocated(array_chara%c_tbl) .eqv. .FALSE.) return
      deallocate(array_chara%c_tbl)
      array_chara%num = 0
!
      end subroutine dealloc_control_array_chara
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_array_r1(label, array_real)
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
      end subroutine read_control_array_r1
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
      subroutine read_control_array_i1(label, array_int)
!
      use m_read_control_elements
!
      character(len=kchara), intent(in) :: label
      type(ctl_array_int), intent(inout) :: array_int
!
!
      call find_control_array_flag(label, array_int%num)
      if(array_int%num.gt.0 .and. array_int%icou.eq.0) then
        call alloc_control_array_int(array_int)
        call read_control_array_int_list(label, array_int%num,          &
     &      array_int%icou, array_int%ivec)
      end if
!
      end subroutine read_control_array_i1
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_c1(label, array_chara)
!
      use m_read_control_elements
!
      character(len=kchara), intent(in) :: label
      type(ctl_array_chara), intent(inout) :: array_chara
!
!
      call find_control_array_flag(label, array_chara%num)
      if(array_chara%num.gt.0 .and. array_chara%icou.eq.0) then
        call alloc_control_array_chara(array_chara)
        call read_control_array_chara_list(label, array_chara%num,      &
     &      array_chara%icou, array_chara%c_tbl)
      end if
!
      end subroutine read_control_array_c1
!
!   --------------------------------------------------------------------
!
      end module t_read_control_arrays
