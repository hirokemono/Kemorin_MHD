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
!>        Structure of mesh file IO paramters
        type(field_IO_params) :: new_mesh_file_IO
!
!
!>        number of subdomains for new partition
        integer(kind = kint) :: new_nprocs
!>        number of subdomains in each direction for new partition
        integer(kind = kint) :: ndomain_eb(3)
!>        number of blocks in each direction for new partition
        integer(kind = kint) :: ndivide_eb(3)
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
      use m_error_IDs
      use m_machine_parameter
!
      use t_ctl_data_volume_grouping
      use m_machine_parameter
      use m_file_format_switch
      use set_control_platform_data
      use set_ctl_parallel_platform
      use set_num_domain_each_dir
!
      type(new_patition_test_control), intent(inout) :: part_tctl
      type(mesh_test_files_param), intent(inout) :: T_files
!
!
      call turn_off_debug_flag_by_ctl(my_rank, part_tctl%plt)
      call check_control_num_domains(part_tctl%plt)
      call set_control_mesh_def(part_tctl%plt, T_files%mesh_file_IO)
      call set_control_smp_def(my_rank, part_tctl%plt)
!
      call check_control_num_domains(part_tctl%new_plt)
      call set_control_mesh_def(part_tctl%new_plt,                      &
     &                          T_files%new_mesh_file_IO)
!
      call set_FEM_surface_output_flag                                  &
     &   (part_tctl%Fmesh_ctl, T_files%iflag_output_SURF)
      if(iflag_debug.gt.0) write(*,*)                                   &
     &   'mesh_file_head:  ', trim(T_files%mesh_file_IO%file_prefix)
!
!
      call set_control_EQ_SPH                                           &
     &   (part_tctl%new_part_ctl%ndomain_section_ctl,                   &
     &    T_files%new_nprocs, T_files%ndomain_eb)
      if(T_files%new_nprocs                                             &
     &      .ne. part_tctl%new_plt%ndomain_ctl%intvalue) then
        write(e_message,'(a)')                                          &
     &      'Number of subdomains should be num. of original mesh'
        call calypso_MPI_abort(ierr_P_MPI, e_message)
      end if
!
      if(part_tctl%new_part_ctl%ratio_of_grouping_ctl%iflag .eq. 0) then
        T_files%ndivide_eb(1:3) = 100 * T_files%ndomain_eb(1:3)
      else
        T_files%ndivide_eb(1:3) = T_files%ndomain_eb(1:3)               &
     &       * part_tctl%new_part_ctl%ratio_of_grouping_ctl%intvalue
      end if
!
      if(my_rank .eq. 0) then
        write(*,*) 'ndomain_eb', T_files%ndomain_eb(1:3)
        write(*,*) 'ndivide_eb', T_files%ndivide_eb(1:3)
      end if
!
      end subroutine s_set_ctl_params_4_test_mesh
!
!  ---------------------------------------------------------------------
!
      end module t_control_param_vol_grping
