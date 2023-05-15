!>@file   initialize_sph_snap_noviz.f90
!!@brief  module initialize_sph_snap_noviz
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to evaluate snapshots from spectr data
!!        without visualization routines
!!
!!@verbatim
!!      subroutine s_initialize_sph_snap_noviz(control_file_name,       &
!!     &                                       SSNAPs, FEM_DAT)
!!        character(len=kchara), intent(in) :: control_file_name
!!        type(spherical_MHD), intent(inout) :: SSNAPs
!!        type(FEM_mesh_field_data), intent(inout) :: FEM_DAT
!!@endverbatim
!
      module initialize_sph_snap_noviz
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
!
      use t_spherical_MHD
      use t_FEM_mesh_field_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_initialize_sph_snap_noviz(control_file_name,         &
     &                                       SSNAPs, FEM_DAT)
!
      use t_ctl_data_MHD
      use t_ctl_data_sph_MHD_w_psf
      use init_sph_MHD_elapsed_label
      use input_control_sph_MHD
      use parallel_FEM_mesh_init
      use FEM_analyzer_sph_MHD
      use SPH_analyzer_snap_w_vizs
!
!
      character(len=kchara), intent(in) :: control_file_name
      type(spherical_MHD), intent(inout) :: SSNAPs
      type(FEM_mesh_field_data), intent(inout) :: FEM_DAT
!
!>      Control struture for MHD simulation
      type(mhd_simulation_control), save :: DNS_MHD_ctl1
!>      Additional structures for spherical MHD dynamo with viz module
      type(add_psf_sph_mhd_ctl), save :: add_SMHD_ctl1
!
!
!   Load parameter file
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_MHD_psf'
      call input_control_SPH_MHD_psf(control_file_name,                 &
     &    SSNAPs%MHD_files, DNS_MHD_ctl1, add_SMHD_ctl1,                &
     &    SSNAPs%MHD_step, SSNAPs%SPH_model, SSNAPs%SPH_WK,             &
     &    SSNAPs%SPH_MHD, FEM_DAT)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!     --------------------- 
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_MHD'
      call FEM_mesh_initialization                                      &
     &   (FEM_DAT%geofem%mesh, FEM_DAT%geofem%group,                    &
     &    SSNAPs%m_SR%SR_sig, SSNAPs%m_SR%SR_i)
!
!        Initialize spherical transform dynamo
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_snap_vizs'
      call SPH_init_sph_snap_vizs                                       &
     &   (SSNAPs%MHD_files, FEM_DAT, SSNAPs%SPH_model,                  &
     &    SSNAPs%MHD_step, SSNAPs%SPH_MHD, SSNAPs%SPH_WK, SSNAPs%m_SR)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call calypso_MPI_barrier
      call reset_elapse_4_init_sph_mhd
!
      end subroutine s_initialize_sph_snap_noviz
!
! ----------------------------------------------------------------------
!
      end module initialize_sph_snap_noviz
