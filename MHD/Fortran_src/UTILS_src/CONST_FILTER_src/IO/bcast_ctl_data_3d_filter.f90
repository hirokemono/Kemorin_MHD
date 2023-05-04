!>@file   bcast_ctl_data_3d_filter.f90
!!@brief  module bcast_ctl_data_3d_filter
!!
!!@author  H. Matsui
!!@date Programmed in May. 2006
!
!>@brief Broadcast control data
!!
!!@verbatim
!!      subroutine bcast_filter_area_ctl(fil3_ctl)
!!        type(ctl_data_3d_filter), intent(inout) :: fil3_ctl
!!      subroutine bcast_element_size_ctl(fil3_ctl)
!!        type(ctl_data_3d_filter), intent(inout) :: fil3_ctl
!!      subroutine bcast_dx_solver_param_ctl(fil3_ctl)
!!        type(ctl_data_3d_filter), intent(inout) :: fil3_ctl
!!      subroutine bcast_org_filter_fnames_ctl(org_fil_files_ctl)
!!        type(org_filter_prefix_ctls), intent(inout)                   &
!!       &                             :: org_fil_files_ctl
!!@endverbatim
!!
      module bcast_ctl_data_3d_filter
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use t_ctl_data_3d_filter
!
      implicit  none
!
      private :: bcast_dx_solver_param_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_filter_area_ctl(fil3_ctl)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(ctl_data_3d_filter), intent(inout) :: fil3_ctl
!
!
      call bcast_ctl_array_c1(fil3_ctl%filter_area_ctl)
!
      call calypso_mpi_bcast_one_int(fil3_ctl%i_filter_area_ctl, 0)
!
      end subroutine bcast_filter_area_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_element_size_ctl(fil3_ctl)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(ctl_data_3d_filter), intent(inout) :: fil3_ctl
!
!
      call bcast_dx_solver_param_ctl(fil3_ctl)
      call bcast_ctl_type_c1(fil3_ctl%mass_matrix_type_ctl)
!
      call calypso_mpi_bcast_one_int(fil3_ctl%i_deltax_ctl, 0)
!
      end subroutine bcast_element_size_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_dx_solver_param_ctl(fil3_ctl)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(ctl_data_3d_filter), intent(inout) :: fil3_ctl
!
!
      call bcast_ctl_type_c1(fil3_ctl%method_esize_ctl)
      call bcast_ctl_type_c1(fil3_ctl%precond_esize_ctl)
!
      call bcast_ctl_type_r1(fil3_ctl%eps_esize_ctl)
      call bcast_ctl_type_r1(fil3_ctl%sigma_esize_ctl)
      call bcast_ctl_type_r1(fil3_ctl%sigma_diag_esize_ctl)
!
      call bcast_ctl_type_i1(fil3_ctl%itr_esize_ctl)
!
      call calypso_mpi_bcast_one_int(fil3_ctl%i_esize_solver_ctl, 0)
!
      end subroutine bcast_dx_solver_param_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_org_filter_fnames_ctl(org_fil_files_ctl)
!
      use t_ctl_data_org_filter_fname
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(org_filter_prefix_ctls), intent(inout) :: org_fil_files_ctl
!
!
      call bcast_ctl_type_c1(org_fil_files_ctl%org_filter_head_ctl)
      call bcast_ctl_type_c1                                            &
     &   (org_fil_files_ctl%org_filter_coef_head_ctl)
      call bcast_ctl_type_c1                                            &
     &   (org_fil_files_ctl%org_filter_elen_head_ctl)
      call bcast_ctl_type_c1                                            &
     &   (org_fil_files_ctl%org_filter_moms_head_ctl)
!
      call calypso_mpi_bcast_one_int                                    &
     &   (org_fil_files_ctl%i_org_filter_fnames, 0)
!
      end subroutine bcast_org_filter_fnames_ctl
!
!  ---------------------------------------------------------------------
!
      end module bcast_ctl_data_3d_filter
