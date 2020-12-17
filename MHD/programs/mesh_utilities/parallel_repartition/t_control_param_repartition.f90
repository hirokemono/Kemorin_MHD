!>@file   t_control_param_repartition.f90
!!@brief  module t_control_param_repartition
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine set_control_param_repartition(part_tctl, part_prog_p)
!!        type(mesh_test_control), intent(in) :: part_tctl
!!        type(vol_partion_prog_param), intent(inout) :: part_prog_p
!!@endverbatim
!
      module t_control_param_repartition
!
      use m_precision
      use t_file_IO_parameter
      use t_control_param_vol_grping
!
      implicit none
!
      character(len = kchara), parameter, private                       &
     &             :: default_newmesh_head = 'repartition_mesh'
!
      type vol_partion_prog_param
!>        Structure of mesh file IO paramters
        type(field_IO_params) :: mesh_file
!>        Structure of mesh file IO paramters
        type(field_IO_params) :: new_mesh_file
!
!>        Integer flag to output surface data
        integer(kind = kint) :: iflag_output_SURF = 0
!
!>        Structure for original field file  paramters
        type(field_IO_params) :: org_ucd_file
!>        Structure for new field file  paramters
        type(field_IO_params) :: new_ucd_file
!
!>        Structure for repartitioning parameters
        type(volume_partioning_param) :: part_param
      end type vol_partion_prog_param
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_control_param_repartition(part_tctl, part_prog_p)
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
!
      use t_ctl_file_volume_grouping
      use m_machine_parameter
      use m_file_format_switch
      use set_control_platform_item
      use set_control_platform_data
      use set_ctl_parallel_platform
      use parallel_ucd_IO_select
!
      type(new_patition_test_control), intent(inout) :: part_tctl
      type(vol_partion_prog_param), intent(inout) :: part_prog_p
!
!
      call turn_off_debug_flag_by_ctl(my_rank, part_tctl%plt)
      call check_control_num_domains(part_tctl%plt)
      call set_control_mesh_def(part_tctl%plt, part_prog_p%mesh_file)
      call set_control_smp_def(my_rank, part_tctl%plt)
!
      call check_control_num_domains(part_tctl%new_plt)
      call set_parallel_file_ctl_params(default_newmesh_head,           &
     &    part_tctl%new_plt%mesh_file_prefix,                           &
     &    part_tctl%new_plt%mesh_file_fmt_ctl,                          &
     &    part_prog_p%new_mesh_file)
!
      call set_FEM_surface_output_flag                                  &
     &   (part_tctl%Fmesh_ctl, part_prog_p%iflag_output_SURF)
      if(iflag_debug.gt.0) write(*,*)                                   &
     &   'mesh_file_head:  ', trim(part_prog_p%mesh_file%file_prefix)
!
      call set_merged_ucd_file_define(part_tctl%plt,                    &
     &                                part_prog_p%org_ucd_file)
      call set_merged_ucd_file_define(part_tctl%new_plt,                &
     &                                part_prog_p%new_ucd_file)
!
      call set_ctl_param_vol_grping(default_newmesh_head,               &
     &    part_tctl%new_part_ctl, part_prog_p%part_param)
!
      if(part_prog_p%part_param%new_nprocs                              &
     &      .ne. part_tctl%new_plt%ndomain_ctl%intvalue) then
        write(e_message,'(a)')                                          &
     &      'Number of subdomains should be num. of original mesh'
        call calypso_MPI_abort(ierr_P_MPI, e_message)
      end if
!
      end subroutine set_control_param_repartition
!
!  ---------------------------------------------------------------------
!
      end module t_control_param_repartition
