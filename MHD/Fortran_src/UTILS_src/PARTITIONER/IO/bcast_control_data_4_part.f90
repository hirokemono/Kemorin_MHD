!>@file   bcast_control_data_4_part.f90
!!@brief  module bcast_control_data_4_part
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2007
!
!>@brief  Control data for partitioner
!!
!!@verbatim
!!      subroutine bcast_part_control_data(part_ctl)
!!        type(control_data_4_partitioner), intent(inout) :: part_ctl
!!@endverbatim
!
      module bcast_control_data_4_part
!
      use m_precision
!
      use calypso_mpi
      use t_control_data_4_part
!
      private :: bcast_ctl_data_4_decomp
      private :: bcast_ctl_data_4_part_ghost
      private :: bcast_ctl_data_4_ele_ordeirng
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine bcast_part_control_data(part_ctl)
!
      use bcast_4_platform_ctl
!
      type(control_data_4_partitioner), intent(inout) :: part_ctl
!
      call bcast_ctl_data_4_platform(part_ctl%part_plt)
      call bcast_ctl_data_4_platform(part_ctl%single_plt)
      call bcast_FEM_mesh_control(part_ctl%part_Fmesh)
!
      call bcast_ctl_data_4_decomp(part_ctl)
      call bcast_ctl_data_4_part_ghost(part_ctl)
      call bcast_ctl_data_4_ele_ordeirng(part_ctl)
!
      call MPI_BCAST(part_ctl%i_part_ctl, 1,                            &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_part_control_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine bcast_ctl_data_4_decomp(part_ctl)
!
      use bcast_control_arrays
!
      type(control_data_4_partitioner), intent(inout) :: part_ctl
!
      call bcast_ctl_type_c1(part_ctl%part_method_ctl)
      call bcast_ctl_type_c1(part_ctl%element_overlap_ctl)
      call bcast_ctl_type_i1(part_ctl%sleeve_level_old)
!
      call bcast_ctl_array_ci(part_ctl%RCB_dir_ctl)
      call bcast_ctl_array_ci(part_ctl%ndomain_section_ctl)
      call bcast_ctl_array_c1(part_ctl%ele_grp_layering_ctl)
!
      call bcast_ctl_type_c1(part_ctl%sphere_file_name_ctl)
      call bcast_ctl_type_c1(part_ctl%metis_input_file_ctl)
      call bcast_ctl_type_c1(part_ctl%metis_domain_file_ctl)
      call bcast_ctl_type_c1(part_ctl%domain_group_file_ctl)
      call bcast_ctl_type_c1(part_ctl%finer_mesh_head_ctl)
      call bcast_ctl_type_c1(part_ctl%finer_mesh_fmt_ctl)
      call bcast_ctl_type_c1(part_ctl%itp_tbl_head_ctl)
      call bcast_ctl_type_c1(part_ctl%itp_tbl_format_ctl)
!
      call MPI_BCAST(part_ctl%i_decomp_ctl, 1,                          &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_data_4_decomp
!
! -----------------------------------------------------------------------
!
      subroutine bcast_ctl_data_4_part_ghost(part_ctl)
!
      use bcast_control_arrays
!
      type(control_data_4_partitioner), intent(inout) :: part_ctl
!
      call bcast_ctl_type_c1(part_ctl%new_part_method_ctl)
      call bcast_ctl_type_c1(part_ctl%selective_ghost_ctl)
!
      call MPI_BCAST(part_ctl%i_part_ghost_ctl, 1,                      &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_data_4_part_ghost
!
! -----------------------------------------------------------------------
!
      subroutine bcast_ctl_data_4_ele_ordeirng(part_ctl)
!
      use bcast_control_arrays
!
      type(control_data_4_partitioner), intent(inout) :: part_ctl
!
!
      call bcast_ctl_array_c1(part_ctl%ele_grp_ordering_ctl)
!
      call MPI_BCAST(part_ctl%i_ele_ordering_ctl, 1,                    &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_data_4_ele_ordeirng
!
! -----------------------------------------------------------------------
!
      end module bcast_control_data_4_part
