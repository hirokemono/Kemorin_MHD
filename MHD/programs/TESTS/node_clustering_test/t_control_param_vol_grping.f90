!>@file   t_control_param_vol_grping.f90
!!@brief  module t_ctl_data_volume_grouping
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine s_set_ctl_params_4_test_mesh(part_tctl, T_files)
!!        type(mesh_test_control), intent(in) :: part_tctl
!!        type(mesh_test_files_param), intent(inout) :: T_files
!!@endverbatim
!
      module t_control_param_vol_grping
!
      use m_precision
      use t_file_IO_parameter
!
      implicit none
!
      type mesh_test_files_param
!>        Integer flag to output surface data
        integer(kind = kint) :: iflag_output_SURF = 0
!>        Structure of mesh file IO paramters
        type(field_IO_params) :: mesh_file_IO
      end type mesh_test_files_param
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine s_set_ctl_params_4_test_mesh(part_tctl, T_files)
!
      use calypso_mpi
      use t_ctl_data_volume_grouping
      use m_machine_parameter
      use m_file_format_switch
      use set_control_platform_data
      use set_ctl_parallel_platform
!
      type(new_patition_test_control), intent(inout) :: part_tctl
      type(mesh_test_files_param), intent(inout) :: T_files
!
!
      call turn_off_debug_flag_by_ctl(my_rank, part_tctl%plt)
      call set_control_mesh_def(part_tctl%plt, T_files%mesh_file_IO)
      call set_FEM_surface_output_flag                                  &
     &   (part_tctl%Fmesh_ctl, T_files%iflag_output_SURF)
      if(iflag_debug.gt.0) write(*,*)                                   &
     &   'mesh_file_head:  ', trim(T_files%mesh_file_IO%file_prefix)
!
      np_smp = 1
      if(part_tctl%plt%num_smp_ctl%iflag .gt. 0) then
        np_smp = part_tctl%plt%num_smp_ctl%intvalue
      end if
      if (iflag_debug.gt.0) write(*,*) 'np_smp', np_smp
!
      end subroutine s_set_ctl_params_4_test_mesh
!
!  ---------------------------------------------------------------------
!
      end module t_control_param_vol_grping
