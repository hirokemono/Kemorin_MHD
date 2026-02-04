!>@file   sph_initial_licv_control.f90
!!@brief  module sph_initial_licv_control
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2026
!
!>@brief Set initial field for liniear convection evolution
!!
!!@verbatim
!!      subroutine s_sph_initial_licv_control(MHD_files, MHD_step, sph, &
!!     &          ipol, refs, rj_fld, sph_fst_IO)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(sph_grids), intent(in) :: sph
!!        type(phys_address), intent(in) :: ipol
!!        type(MHD_step_param), intent(in) :: MHD_step
!!        type(radial_reference_field), intent(in) :: refs
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(field_IO), intent(inout) :: sph_fst_IO
!!@endverbatim
!
      module sph_initial_licv_control
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_parameter
      use t_phys_data
      use t_phys_address
      use t_radial_reference_field
!
      implicit none
!
      private :: sph_initial_field_4_licv
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_sph_initial_licv_control(MHD_files, MHD_step, sph,   &
     &          ipol, refs, rj_fld, sph_fst_IO)
!
      use calypso_mpi
      use m_initial_field_control
!
      use t_MHD_step_parameter
      use t_control_parameter
      use t_field_data_IO
!
      use set_sph_restart_IO
      use sph_mhd_rst_IO_control
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(MHD_step_param), intent(in) :: MHD_step
      type(radial_reference_field), intent(in) :: refs
!
      type(phys_data), intent(inout) :: rj_fld
      type(field_IO), intent(inout) :: sph_fst_IO
!
!
      call sph_initial_field_4_licv(sph, refs, ipol, rj_fld)
!
      if(iflag_debug .gt. 0) write(*,*) 'copy_time_step_data'
      call set_sph_restart_num_to_IO(rj_fld, sph_fst_IO)
!
      if(iflag_debug .gt. 0) write(*,*) 'output_sph_restart_control'
      call output_sph_restart_control(MHD_step%init_d%i_time_step,      &
     &    MHD_files%fst_file_IO, MHD_step%time_d, rj_fld,               &
     &    MHD_step%rst_step, sph_fst_IO)
      call calypso_mpi_barrier
!
      end subroutine s_sph_initial_licv_control
!
!-----------------------------------------------------------------------
!
      subroutine sph_initial_field_4_licv(sph, refs, ipol, rj_fld)
!
      use t_base_field_labels
      use initialize_sph_dynamo
!
      type(sph_grids), intent(in) :: sph
      type(radial_reference_field), intent(in) :: refs
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if((ipol%base%i_temp*refs%iref_base%i_temp) .gt. 0)  then
        call init_sph_scalar_with_noise(sph, refs%iref_base%i_temp,     &
     &      refs%ref_field, ipol%base%i_temp, rj_fld)
      end if
!
      if((ipol%base%i_light*refs%iref_base%i_light) .gt. 0) then
        call init_sph_scalar_with_noise(sph, refs%iref_base%i_light,    &
     &      refs%ref_field, ipol%base%i_light, rj_fld)
      end if
!
      end subroutine sph_initial_field_4_licv
!
!-----------------------------------------------------------------------
!
      end module sph_initial_licv_control
