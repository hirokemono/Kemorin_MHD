!>@file   c_link_sph_mntr_ctl_arrays.f90
!!@brief  module c_link_sph_mntr_ctl_arrays
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!!
!>@brief C binding routines for sphere_data_control structure
!!@verbatim
!!      type(c_ptr) function c_sph_monitor_ctl_block_name(c_ctl)        &
!!     &          bind(C, NAME = 'c_sph_monitor_ctl_block_name')
!!      type(c_ptr) function c_sph_monitor_ctl_iflag(c_ctl)             &
!!     &          bind(C, NAME = 'c_sph_monitor_ctl_iflag')
!!
!!      type(c_ptr) function c_sph_monitor_ctl_v_pwr_name(c_ctl)        &
!!     &          bind(C, NAME = 'c_sph_monitor_ctl_v_pwr_name')
!!      integer(c_int) function c_sph_monitor_num_vspec_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_sph_monitor_num_vspec_ctl')
!!      type(c_ptr) function c_sph_monitor_vspec_ctl(idx_in, c_ctl)     &
!!     &          bind(C, NAME = 'c_sph_monitor_vspec_ctl')
!!
!!      type(c_ptr) function c_append_sph_mntr_vspec_ctl                &
!!     &                   (idx, c_name, c_ctl)                         &
!!     &                   bind(C, NAME = 'c_append_sph_mntr_vspec_ctl')
!!      type(c_ptr) function c_delete_sph_mntr_vspec_ctl(idx, c_ctl)    &
!!     &                   bind(C, NAME = 'c_delete_sph_mntr_vspec_ctl')
!!        integer(c_int), value, intent(in) :: num
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_data_on_circles_block_name(c_ctl)        &
!!     &          bind(C, NAME = 'c_data_on_circles_block_name')
!!      integer(c_int) function c_data_on_circles_num(c_ctl)            &
!!     &          bind(C, NAME = 'c_data_on_circles_num')
!!      type(c_ptr) function c_data_on_circles_meq_ctl(idx_in, c_ctl)   &
!!     &          bind(C, NAME = 'c_data_on_circles_meq_ctl')
!!        integer(c_int), value, intent(in) :: idx_in
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_append_circles_meq_ctl                   &
!!     &                   (idx, c_name, c_ctl)                         &
!!     &                   bind(C, NAME = 'c_append_circles_meq_ctl')
!!      type(c_ptr) function c_delete_circles_meq_ctl(idx, c_ctl)       &
!!     &                   bind(C, NAME = 'c_delete_circles_meq_ctl')
!!        integer(c_int), value, intent(in) :: idx
!!        character(C_char), intent(in) :: c_name(*)
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_sph_mntr_ctl_arrays
!
      use iso_c_binding
      use t_ctl_data_4_sph_monitor
      use ctl_array_chara_to_c
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_monitor_ctl_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_sph_monitor_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_monitor_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_sph_monitor_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_monitor_ctl_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_sph_monitor_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_monitor_ctl_iflag = C_loc(f_ctl%i_sph_monitor)
      end function c_sph_monitor_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_monitor_ctl_v_pwr_name(c_ctl)          &
     &          bind(C, NAME = 'c_sph_monitor_ctl_v_pwr_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_monitor_ctl_v_pwr_name = C_loc(f_ctl%v_pwr_name)
      end function c_sph_monitor_ctl_v_pwr_name
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_sph_monitor_num_vspec_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_sph_monitor_num_vspec_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_monitor_num_vspec_ctl = f_ctl%num_vspec_ctl
      end function c_sph_monitor_num_vspec_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_monitor_vspec_ctl(idx_in, c_ctl)       &
     &          bind(C, NAME = 'c_sph_monitor_vspec_ctl')
      integer(c_int), value, intent(in) :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_monitor_vspec_ctl = C_loc(f_ctl%v_pwr(idx_in+1))
      end function c_sph_monitor_vspec_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_append_sph_mntr_vspec_ctl                  &
     &                   (idx, c_name, c_ctl)                           &
     &                   bind(C, NAME = 'c_append_sph_mntr_vspec_ctl')
      use ctl_data_volume_spectr_IO
!
      integer(c_int), value, intent(in) :: idx
      character(C_char), intent(in) :: c_name(*)
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call append_volume_spectr_ctls(idx, copy_char_from_c(c_name),     &
     &                               f_ctl)
      c_append_sph_mntr_vspec_ctl = C_loc(f_ctl%v_pwr)
      f_ctl%v_pwr(idx+1)%i_vol_spectr_ctl = 1
!
      end function c_append_sph_mntr_vspec_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_delete_sph_mntr_vspec_ctl(idx, c_ctl)      &
     &                   bind(C, NAME = 'c_delete_sph_mntr_vspec_ctl')
      use ctl_data_volume_spectr_IO
!
      integer(c_int), value, intent(in) :: idx
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call delete_volume_spectr_ctls((idx+1), f_ctl)
      c_delete_sph_mntr_vspec_ctl = C_loc(f_ctl%v_pwr)
!
      end function c_delete_sph_mntr_vspec_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_data_on_circles_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_data_on_circles_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_data_on_circles_block_name = C_loc(f_ctl%d_circ_name)
      end function c_data_on_circles_block_name
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_data_on_circles_num(c_ctl)              &
     &          bind(C, NAME = 'c_data_on_circles_num')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_data_on_circles_num = f_ctl%num_circ_ctl
      write(*,*) 'c_data_on_circles_num', c_data_on_circles_num
      end function c_data_on_circles_num
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_data_on_circles_meq_ctl(idx_in, c_ctl)     &
     &          bind(C, NAME = 'c_data_on_circles_meq_ctl')
      integer(c_int), value, intent(in) :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_data_on_circles_meq_ctl = C_loc(f_ctl%meq_ctl(idx_in+1))
      end function c_data_on_circles_meq_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_append_circles_meq_ctl                     &
     &                   (idx, c_name, c_ctl)                           &
     &                   bind(C, NAME = 'c_append_circles_meq_ctl')
      use t_ctl_data_circles
!
      integer(c_int), value, intent(in) :: idx
      character(C_char), intent(in) :: c_name(*)
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call append_data_on_circles_ctl(idx, copy_char_from_c(c_name),    &
     &                                f_ctl)
      c_append_circles_meq_ctl = C_loc(f_ctl%meq_ctl)

      f_ctl%meq_ctl(idx+1)%i_mid_equator_ctl = 1
!
      end function c_append_circles_meq_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_delete_circles_meq_ctl(idx, c_ctl)         &
     &                   bind(C, NAME = 'c_delete_circles_meq_ctl')
      use t_ctl_data_circles
!
      integer(c_int), value, intent(in) :: idx
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call delete_data_on_circles_ctl((idx+1), f_ctl)
      c_delete_circles_meq_ctl = C_loc(f_ctl%meq_ctl)
!
      end function c_delete_circles_meq_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_sph_mntr_ctl_arrays
