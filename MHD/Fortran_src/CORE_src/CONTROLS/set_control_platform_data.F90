!>@file   set_control_platform_data.f90
!!@brief  module set_control_platform_data
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!>@brief  Set file headers and number of processor and threds
!!        from control data
!!
!!@verbatim
!!      subroutine turn_off_debug_flag_by_ctl(my_rank)
!!      subroutine set_control_smp_def(my_rank)
!!      subroutine set_control_mesh_def(mesh_file)
!!      subroutine set_control_sph_mesh(mesh_file, sph_file_param)
!!        type(field_IO_params), intent(inout) :: mesh_file
!!        type(field_IO_params), intent(inout) :: sph_file_param
!!      subroutine set_FEM_mesh_switch_4_SPH(iflag_access_FEM)
!!      subroutine set_control_restart_file_def(fld_IO)
!!@endverbatim
!!
!!@param my_rank  preocess ID
!
      module set_control_platform_data
!
      use m_precision
!
      use m_constants
      use m_ctl_data_4_platforms
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine turn_off_debug_flag_by_ctl(my_rank)
!
      use m_machine_parameter
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      i_debug = 0
      if(debug_flag_ctl%iflag .gt. 0) then
        if     (no_flag(debug_flag_ctl%charavalue)) then
          i_debug =     iflag_minimum_msg
        else if(yes_flag(debug_flag_ctl%charavalue)) then
          i_debug =     iflag_routine_msg
        else if(cmp_no_case(debug_flag_ctl%charavalue,'Full')           &
     &     .or. cmp_no_case(debug_flag_ctl%charavalue,'2')   ) then
          i_debug =     iflag_full_msg
        end if
      end if
!
      if(my_rank .eq. 0) iflag_debug = i_debug
!
      end subroutine turn_off_debug_flag_by_ctl
!
! ----------------------------------------------------------------------
!
      subroutine set_control_smp_def(my_rank)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: my_rank
      integer, external :: omp_get_max_threads
      integer :: np_smp4
!
!
      np_smp = 1
      if(num_smp_ctl%iflag .gt. 0) np_smp = num_smp_ctl%intvalue
!
#ifdef _OPENMP
      if (int(np_smp) .lt. omp_get_max_threads()) then
        if(my_rank .eq. 0) write(*,*)                                   &
     &               'Number of SMP threads is chenged to', np_smp
        np_smp4 = int(np_smp)
        call omp_set_num_threads(np_smp4)
      end if
#endif
!
      end subroutine set_control_smp_def
!
! -----------------------------------------------------------------------
!
      subroutine set_control_mesh_def(mesh_file)
!
      use m_read_mesh_data
      use m_file_format_switch
      use t_file_IO_parameter
      use skip_comment_f
!
      type(field_IO_params), intent(inout) :: mesh_file
!
!
      if (mesh_file_prefix%iflag .gt. 0) then
        mesh_file%file_prefix = mesh_file_prefix%charavalue
      else
        mesh_file%file_prefix = def_mesh_file_head
      end if
!
!   set data format
      call choose_para_file_format                                      &
     &   (mesh_file_fmt_ctl, mesh_file%iflag_format)
!
      end subroutine set_control_mesh_def
!
! -----------------------------------------------------------------------
!
      subroutine set_control_sph_mesh(mesh_file, sph_file_param)
!
      use m_file_format_switch
      use t_file_IO_parameter
      use sph_file_IO_select
!
      type(field_IO_params), intent(inout) :: mesh_file
      type(field_IO_params), intent(inout) :: sph_file_param
!
!   set data format
!
      call choose_para_file_format                                      &
     &  (sph_file_fmt_ctl, iflag_sph_file_fmt)
      call choose_para_file_format                                      &
     &   (spectr_file_fmt_ctl, sph_file_param%iflag_format)
!
!   set file header at once
!
      if(sph_file_prefix%iflag .gt. 0) then
        sph_file_head =  sph_file_prefix%charavalue
        call copy_mesh_format_and_prefix                                &
     &     (sph_file_prefix%charavalue, iflag_sph_file_fmt, mesh_file)
      end if
!
      sph_file_param%iflag_IO = spectr_file_head_ctl%iflag
      if(sph_file_param%iflag_IO .gt. 0) then
        sph_file_param%file_prefix = spectr_file_head_ctl%charavalue
      end if
!
      end subroutine set_control_sph_mesh
!
! ----------------------------------------------------------------------
!
      subroutine set_FEM_mesh_switch_4_SPH(iflag_access_FEM)
!
      use skip_comment_f
!
      integer(kind = kint), intent(inout) :: iflag_access_FEM
!
!
      iflag_access_FEM = 0
      if(FEM_mesh_output_switch%iflag .gt. 0) then
        if(yes_flag(FEM_mesh_output_switch%charavalue)) then
          iflag_access_FEM = 1
        end if
      else if(excluding_FEM_mesh_ctl%iflag .gt. 0) then
        if(no_flag(excluding_FEM_mesh_ctl%charavalue)) then
          iflag_access_FEM = 1
        end if
      end if
!
      end subroutine set_FEM_mesh_switch_4_SPH
!
! ----------------------------------------------------------------------
!
      subroutine set_control_restart_file_def(fld_IO)
!
      use t_field_data_IO
      use m_file_format_switch
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      if (restart_file_prefix%iflag .gt. 0) then
        fld_IO%file_prefix = restart_file_prefix%charavalue
      end if
!
      call choose_para_file_format                                      &
     &   (restart_file_fmt_ctl, fld_IO%iflag_file_fmt)
!
      end subroutine set_control_restart_file_def
!
! -----------------------------------------------------------------------
!
      end module set_control_platform_data
