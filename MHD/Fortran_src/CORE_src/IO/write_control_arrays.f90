!>@file   write_control_arrays.f90
!!@brief  module write_control_arrays
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine write_control_array_r1                               &
!!     &         (id_file, level, label, array_real)
!!        type(ctl_array_real), intent(in) :: array_real
!!      subroutine write_control_array_r2                               &
!!     &         (id_file, level, label, array_r2)
!!        type(ctl_array_r2), intent(in) :: array_r2
!!      subroutine write_control_array_i1                               &
!!     &         (id_file, level, label, array_int)
!!        type(ctl_array_int), intent(in) :: array_int
!!      subroutine write_control_array_c1                               &
!!     &          (id_file, level, label, array_chara)
!!        type(ctl_array_chara), intent(in) :: array_chara
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
!!@n @param  array_i2r       structures for array
!!@n @param  array_i2r2      structures for array
!!
      module write_control_arrays
!
      use m_precision
      use t_read_control_arrays
      use skip_comment_f
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_r1                                 &
     &         (id_file, level, label, array_real)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_real), intent(in) :: array_real
!
!
      if(array_real%num .gt. 0) then
        write(id_file,'(a1)') '!'
        call write_control_array_real_list(id_file, level, label,       &
     &      array_real%num, array_real%vect)
      end if
!
      end subroutine write_control_array_r1
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_r2                                 &
     &         (id_file, level, label, array_r2)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_r2), intent(in) :: array_r2
!
!
      if(array_r2%num .gt. 0) then
        write(id_file,'(a1)') '!'
        call write_control_array_real2_list(id_file, level, label,      &
     &      array_r2%num, array_r2%vec1, array_r2%vec2)
      end if
!
      end subroutine write_control_array_r2
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_i1                                 &
     &         (id_file, level, label, array_int)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_int), intent(in) :: array_int
!
!
      if(array_int%num.gt.0 .and. array_int%icou.eq.0) then
        write(id_file,'(a1)') '!'
        call write_control_array_int_list(id_file, level, label,        &
     &      array_int%num, array_int%ivec)
      end if
!
      end subroutine write_control_array_i1
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c1                                 &
     &          (id_file, level, label, array_chara)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_chara), intent(in) :: array_chara
!
!
      if(array_chara%num .gt. 0) then
        write(id_file,'(a1)') '!'
        call write_control_array_chara_list(id_file, level, label,      &
     &      array_chara%num, array_chara%c_tbl)
      end if
!
      end subroutine write_control_array_c1
!
!   --------------------------------------------------------------------
!
      end module write_control_arrays
