!>@file   load_mesh_and_field_4_psf.f90
!!@brief  module load_mesh_and_field_4_psf
!!
!!@author H. Matsui
!!@date Programmed in July, 2020
!
!>@brief Load FEM data loading for visualize program
!!
!!@verbatim
!!      subroutine set_control_params_4_sections                        &
!!     &         (sec_viz_ctl, sfcing, t_viz_param, ierr)
!!        type(control_data_section_only), intent(in) :: sec_viz_ctl
!!        type(FEM_mesh_field_4_surfacing), intent(inout) :: sfcing
!!        type(time_step_param_w_viz), intent(inout) :: t_viz_param
!!@endverbatim
!
      module load_mesh_and_field_4_psf
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_step_parameter
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_ucd_data
      use t_IO_step_parameter
      use t_file_IO_parameter
      use t_VIZ_only_step_parameter
      use t_field_list_for_vizs
!
      implicit none
!
      private :: add_field_in_viz_ctls_w_SGS
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      end module load_mesh_and_field_4_psf
