!>@file   SGS_MHD_zonal_mean_viz.f90
!!@brief  module SGS_MHD_zonal_mean_viz
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2012
!
!>@brief  Make zonal mean sections for SGS dynamo
!!
!!@verbatim
!!      subroutine SGS_MHD_zmean_sections(elps_VIZ, viz_step, time_d,   &
!!     &          sph, fem, WK, SPH_SGS, nod_fld, zmeans, m_SR)
!!        type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(sph_grids), intent(in) :: sph
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: fem
!!        type(works_4_sph_trans_MHD), intent(in) :: WK
!!        type(SPH_SGS_structure), intent(in) :: SPH_SGS
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(sph_zonal_mean_viz), intent(inout) :: zmeans
!!        type(mesh_SR), intent(inout) :: m_SR
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
      use t_mesh_SR
      use t_VIZ_step_parameter
      use t_elapsed_labels_4_VIZ
!
      implicit  none
!
      private :: SGS_MHD_zonal_RMS_vizs
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_MHD_zmean_sections(elps_VIZ, viz_step, time_d,     &
     &          sph, fem, WK, SPH_SGS, nod_fld, zmeans, m_SR)
!
      use FEM_analyzer_sph_SGS_MHD
      use nod_phys_send_recv
!
      type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
      type(VIZ_step_params), intent(in) :: viz_step
      type(sph_grids), intent(in) :: sph
!
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: fem
      type(works_4_sph_trans_MHD), intent(in) :: WK
      type(SPH_SGS_structure), intent(in) :: SPH_SGS
!
      type(phys_data), intent(inout) :: nod_fld
      type(sph_zonal_mean_viz), intent(inout) :: zmeans
      type(mesh_SR), intent(inout) :: m_SR
!
!
      call SPH_MHD_zonal_mean_vizs(elps_VIZ, viz_step, time_d,         &
     &    sph, fem, nod_fld, zmeans%zm_psf, zmeans%zm_maps, m_SR)
!
      call SGS_MHD_zonal_RMS_vizs(elps_VIZ, viz_step, time_d,           &
     &    SPH_SGS%SGS_par, sph, fem, WK, SPH_SGS%trns_WK_LES,           &
     &    nod_fld, zmeans%zrms_psf, zmeans%zrms_maps, m_SR)
!
      end subroutine SGS_MHD_zmean_sections
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine SGS_MHD_zonal_RMS_vizs                                 &
     &         (elps_VIZ, viz_step, time_d, SGS_par, sph, fem,          &
     &          WK, WK_LES, nod_fld, zrms_psf, zrms_maps, m_SR)
!
      use FEM_analyzer_sph_SGS_MHD
      use sph_rtp_zonal_rms_data
      use nod_phys_send_recv
      use map_projection
!
      type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
      type(VIZ_step_params), intent(in) :: viz_step
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
      type(map_rendering_module), intent(inout) :: zrms_maps
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if((zrms_psf%num_psf+zrms_maps%num_map) .le. 0) return
!
      if (iflag_debug.gt.0) write(*,*) 'SPH_to_FEM_bridge_SGS_MHD'
      call SPH_to_FEM_bridge_SGS_MHD                                    &
     &   (SGS_par, sph, WK, WK_LES, fem, nod_fld)
      call zonal_rms_all_rtp_field(sph%sph_rtp, fem%mesh%node, nod_fld)
!
      if (iflag_debug.gt.0) write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(fem%mesh, nod_fld,                      &
     &                          m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
      if(zrms_psf%num_psf .gt. 0) then
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+2)
        if (iflag_debug.gt.0) write(*,*) 'SECTIONING_visualize RMS'
        call SECTIONING_visualize                                       &
     &     (viz_step%istep_psf, elps_VIZ%elps_PSF,                      &
     &      time_d, fem, nod_fld, zrms_psf)
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+2)
      end if
!
      if(zrms_maps%num_map .gt. 0) then
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+6)
        call MAP_PROJECTION_visualize                                   &
     &     (viz_step%istep_map, elps_VIZ%elps_PSF, elps_VIZ%elps_MAP,   &
     &      time_d, fem, nod_fld, zRMS_maps, m_SR%SR_sig)
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+6)
      end if
!
      end subroutine SGS_MHD_zonal_RMS_vizs
!
!  ---------------------------------------------------------------------
!
      end module SGS_MHD_zonal_mean_viz
