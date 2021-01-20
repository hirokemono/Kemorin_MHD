!>@file   t_ctl_param_volume_repart.f90
!!@brief  module t_ctl_param_volume_repart
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine set_ctl_param_vol_repart(viz_repart_c, repart_p)
!!        type(viz_repartition_ctl), intent(inout) :: viz_repart_c
!!        type(volume_repart_params), intent(inout) :: repart_p
!!@endverbatim
!
      module t_ctl_param_volume_repart
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
!>      Structure for volume repartitiong paramteres
      type volume_repart_params
!>        Logical flag for repartitiong
        logical :: flag_repartition = .FALSE.
!>        Structure for repartitioning parameters
        type(volume_partioning_param) :: part_param
!
!>        Structure of mesh file IO paramters
        type(field_IO_params) :: viz_mesh_file
!
!>        Integer flag to output surface data
        integer(kind = kint) :: iflag_output_SURF = 0
!>        Structure for new field file  paramters
        type(field_IO_params) :: viz_ucd_file
      end type volume_repart_params
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_param_vol_repart(viz_repart_c, repart_p)
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
!
      use t_ctl_data_volume_repart
      use m_machine_parameter
      use m_file_format_switch
      use set_control_platform_item
      use set_control_platform_data
      use set_ctl_parallel_platform
      use parallel_ucd_IO_select
!
      type(viz_repartition_ctl), intent(inout) :: viz_repart_c
      type(volume_repart_params), intent(inout) :: repart_p
!
!
      call check_control_num_domains(viz_repart_c%viz_plt)
!
      if(viz_repart_c%viz_plt%mesh_file_prefix%iflag .le. 0) then
        repart_p%viz_mesh_file%iflag_format = id_no_file
      else
        call set_parallel_file_ctl_params(default_newmesh_head,         &
     &      viz_repart_c%viz_plt%mesh_file_prefix,                      &
     &      viz_repart_c%viz_plt%mesh_file_fmt_ctl,                     &
     &      repart_p%viz_mesh_file)
      end if
!
      call set_FEM_surface_output_flag                                  &
     &   (viz_repart_c%Fmesh_ctl, repart_p%iflag_output_SURF)
!
      call set_merged_ucd_file_define(viz_repart_c%viz_plt,             &
     &                                repart_p%viz_ucd_file)
!
      if(i_viz_repartition_ctl .gt. 0) flag_repartition = .TRUE.
      call set_ctl_param_vol_grping(default_newmesh_head,               &
     &    viz_repart_c%new_part_ctl, repart_p%part_param)
!
      if(repart_p%part_param%new_nprocs                                 &
     &      .ne. viz_repart_c%viz_plt%ndomain_ctl%intvalue) then
        write(e_message,'(a)')                                          &
     &      'Number of subdomains should be num. of original mesh'
        call calypso_MPI_abort(ierr_P_MPI, e_message)
      end if
!
      end subroutine set_ctl_param_vol_repart
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_param_volume_repart
