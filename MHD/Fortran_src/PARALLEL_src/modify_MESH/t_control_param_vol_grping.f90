!>@file   t_control_param_vol_grping.f90
!!@brief  module t_control_param_vol_grping
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine s_set_ctl_params_4_test_mesh(part_tctl, part_param)
!!        type(mesh_test_control), intent(in) :: part_tctl
!!        type(volume_partioning_param), intent(inout) :: part_param
!!@endverbatim
!
      module t_control_param_vol_grping
!
      use m_precision
      use t_file_IO_parameter
!
!
      implicit none
!
      character(len = kchara), parameter, private                       &
     &             :: default_newmesh_head = 'repartition_mesh'
!
      type volume_partioning_param
!>        Integer flag to output surface data
        integer(kind = kint) :: iflag_output_SURF = 0
!>        Structure of mesh file IO paramters
        type(field_IO_params) :: mesh_file_IO
!
!>        Structure of mesh file IO paramters
        type(field_IO_params) :: new_mesh_file_IO
!>        Data transfer table file parameters
        type(field_IO_params) :: transfer_iable_IO
!
!>        Structure for original field file  paramters
        type(field_IO_params) :: org_ucd_file_IO
!>        Structure for new field file  paramters
        type(field_IO_params) :: new_ucd_file_IO
!
!>        number of subdomains for original partition
        integer(kind = kint) :: org_nprocs
!>        number of subdomains for new partition
        integer(kind = kint) :: new_nprocs
!>        number of subdomains in each direction for new partition
        integer(kind = kint) :: ndomain_eb(3)
!>        number of blocks in each direction for new partition
        integer(kind = kint) :: ndivide_eb(3)
!
!>        Sleeve size level
        integer(kind = kint) :: num_FEM_sleeve = 1
      end type volume_partioning_param
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine s_set_ctl_params_4_test_mesh(part_tctl, part_param)
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
!
      use t_ctl_data_volume_grouping
      use m_machine_parameter
      use m_file_format_switch
      use set_control_platform_item
      use set_control_platform_data
      use set_ctl_parallel_platform
      use set_num_domain_each_dir
      use ucd_IO_select
!
      type(new_patition_test_control), intent(inout) :: part_tctl
      type(volume_partioning_param), intent(inout) :: part_param
!
!
      call turn_off_debug_flag_by_ctl(my_rank, part_tctl%plt)
      call check_control_num_domains(part_tctl%plt)
      call set_control_mesh_def(part_tctl%plt, part_param%mesh_file_IO)
      call set_control_smp_def(my_rank, part_tctl%plt)
!
      call check_control_num_domains(part_tctl%new_plt)
      call set_parallel_file_ctl_params(default_newmesh_head,           &
     &    part_tctl%new_plt%mesh_file_prefix,                           &
     &    part_tctl%new_plt%mesh_file_fmt_ctl,                          &
     &    part_param%new_mesh_file_IO)
!
      call set_parallel_file_ctl_params(default_newmesh_head,           &
     &    part_tctl%new_part_ctl%repart_table_head_ctl,                 &
     &    part_tctl%new_part_ctl%repart_table_fmt_ctl,                  &
     &    part_param%transfer_iable_IO)
!
      call set_FEM_surface_output_flag                                  &
     &   (part_tctl%Fmesh_ctl, part_param%iflag_output_SURF)
      if(iflag_debug.gt.0) write(*,*)                                   &
     &   'mesh_file_head:  ', trim(part_param%mesh_file_IO%file_prefix)
!
      call set_ucd_file_define(part_tctl%plt,                           &
     &                         part_param%org_ucd_file_IO)
      call set_ucd_file_define(part_tctl%new_plt,                       &
     &                         part_param%new_ucd_file_IO)
!
      part_param%num_FEM_sleeve = 1
      if(part_tctl%new_part_ctl%sleeve_level_ctl%iflag .gt. 0) then
        part_param%num_FEM_sleeve                                       &
     &      = max(part_tctl%new_part_ctl%sleeve_level_ctl%intvalue, 1)
      end if
!
      part_param%new_nprocs = nprocs
      call set_control_EQ_XYZ                                           &
     &   (part_tctl%new_part_ctl%ndomain_section_ctl,                   &
     &    part_param%new_nprocs, part_param%ndomain_eb)
      if(part_param%new_nprocs                                          &
     &      .ne. part_tctl%new_plt%ndomain_ctl%intvalue) then
        write(e_message,'(a)')                                          &
     &      'Number of subdomains should be num. of original mesh'
        call calypso_MPI_abort(ierr_P_MPI, e_message)
      end if
!
      if(part_tctl%new_part_ctl%ratio_of_grouping_ctl%iflag .eq. 0) then
        part_param%ndivide_eb(1:3) = 100 * part_param%ndomain_eb(1:3)
      else
        part_param%ndivide_eb(1:3) = part_param%ndomain_eb(1:3)         &
     &       * part_tctl%new_part_ctl%ratio_of_grouping_ctl%intvalue
      end if
!
      if(my_rank .eq. 0) then
        write(*,*) 'ndomain_eb', part_param%ndomain_eb(1:3)
        write(*,*) 'ndivide_eb', part_param%ndivide_eb(1:3)
      end if
!
      end subroutine s_set_ctl_params_4_test_mesh
!
!  ---------------------------------------------------------------------
!
      end module t_control_param_vol_grping
