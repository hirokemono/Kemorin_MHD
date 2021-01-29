!>@file   SGS_MHD_zonal_mean_viz.f90
!!@brief  module SGS_MHD_zonal_mean_viz
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2012
!
!>@brief  Make zonal mean sections for SGS dynamo
!!
!!@verbatim
!!      subroutine SGS_MHD_zmean_sections(istep_psf, time_d,            &
!!     &          sph, fem, WK, SPH_SGS, nod_fld, zmeans, v_sol)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(sph_grids), intent(in) :: sph
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: fem
!!        type(works_4_sph_trans_MHD), intent(in) :: WK
!!        type(SPH_SGS_structure), intent(in) :: SPH_SGS
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(sph_zonal_mean_sectioning), intent(inout) :: zmeans
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!@endverbatim
!
      module SGS_MHD_zonal_mean_viz
!
      use m_precision
!
      use m_machine_parameter
      use m_work_time
      use calypso_mpi
!
      use t_SGS_control_parameter
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_spheric_parameter
      use t_sph_trans_arrays_MHD
      use t_sph_trans_arrays_SGS_MHD
      use t_SPH_SGS_structure
      use t_cross_section
      use t_SPH_MHD_zonal_mean_viz
      use t_vector_for_solver
!
      implicit  none
!
      private :: SGS_MHD_zonal_RMS_section
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_MHD_zmean_sections(istep_psf, time_d,              &
     &          sph, fem, WK, SPH_SGS, nod_fld, zmeans, v_sol)
!
      use FEM_analyzer_sph_SGS_MHD
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: istep_psf
      type(sph_grids), intent(in) :: sph
!
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: fem
      type(works_4_sph_trans_MHD), intent(in) :: WK
      type(SPH_SGS_structure), intent(in) :: SPH_SGS
!
      type(phys_data), intent(inout) :: nod_fld
      type(sph_zonal_mean_sectioning), intent(inout) :: zmeans
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      call SPH_MHD_zonal_mean_section                                   &
     &   (istep_psf, time_d, sph, fem, nod_fld, zmeans%zm_psf, v_sol)
!
      call SGS_MHD_zonal_RMS_section                                    &
     &   (istep_psf, time_d, SPH_SGS%SGS_par, sph, fem,                 &
     &    WK, SPH_SGS%trns_WK_LES, nod_fld, zmeans%zrms_psf, v_sol)
!
      end subroutine SGS_MHD_zmean_sections
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine SGS_MHD_zonal_RMS_section(istep_psf, time_d, SGS_par,  &
     &          sph, fem, WK, WK_LES, nod_fld, zrms_psf, v_sol)
!
      use m_elapsed_labels_4_VIZ
      use FEM_analyzer_sph_SGS_MHD
      use sph_rtp_zonal_rms_data
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: istep_psf
      type(SGS_paremeters), intent(in) :: SGS_par
      type(sph_grids), intent(in) :: sph
!
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: fem
      type(works_4_sph_trans_MHD), intent(in) :: WK
      type(works_4_sph_trans_SGS_MHD), intent(in) :: WK_LES
!
      type(phys_data), intent(inout) :: nod_fld
      type(sectioning_module), intent(inout) :: zrms_psf
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      if(zrms_psf%num_psf .le. 0) return
!
      if (iflag_debug.gt.0) write(*,*) 'SPH_to_FEM_bridge_SGS_MHD'
      call SPH_to_FEM_bridge_SGS_MHD                                    &
     &   (SGS_par, sph, WK, WK_LES, fem, nod_fld)
      call zonal_rms_all_rtp_field(sph%sph_rtp, fem%mesh%node, nod_fld)
!
      if (iflag_debug.gt.0) write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(fem%mesh, nod_fld, v_sol)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+6)
      if(iflag_debug.gt.0) write(*,*) 'SECTIONING_visualize RMS'
      call SECTIONING_visualize                                         &
     &   (istep_psf, time_d, fem, nod_fld, zrms_psf)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+6)
!
      end subroutine SGS_MHD_zonal_RMS_section
!
!  ---------------------------------------------------------------------
!
      end module SGS_MHD_zonal_mean_viz
