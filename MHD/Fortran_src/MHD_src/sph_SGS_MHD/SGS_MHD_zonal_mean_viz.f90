!SGS_MHD_zonal_mean_viz.f90
!      module SGS_MHD_zonal_mean_viz
!
!      Written by H. Matsui on Apr., 2012
!
!!      subroutine SGS_MHD_zmean_sections(viz_step, time_d, SGS_par,    &
!!     &          sph, geofem, ele_mesh, WK, nod_fld, zmeans)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(sph_grids), intent(in) :: sph
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: geofem
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(works_4_sph_trans_MHD), intent(in) :: WK
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(sph_zonal_mean_sectioning), intent(inout) :: zmeans
!
      module SGS_MHD_zonal_mean_viz
!
      use m_precision
!
      use m_machine_parameter
      use m_work_time
      use calypso_mpi
!
      use t_VIZ_step_parameter
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_spheric_parameter
      use t_sph_trans_arrays_MHD
      use t_cross_section
      use t_SPH_MHD_zonal_mean_viz
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
      subroutine SGS_MHD_zmean_sections(viz_step, time_d, SGS_par,      &
     &          sph, geofem, ele_mesh, WK, nod_fld, zmeans)
!
      use FEM_analyzer_sph_SGS_MHD
      use nod_phys_send_recv
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(SGS_paremeters), intent(in) :: SGS_par
      type(sph_grids), intent(in) :: sph
!
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: geofem
      type(element_geometry), intent(in) :: ele_mesh
      type(works_4_sph_trans_MHD), intent(in) :: WK
!
      type(phys_data), intent(inout) :: nod_fld
      type(sph_zonal_mean_sectioning), intent(inout) :: zmeans
!
!
      call SPH_MHD_zonal_mean_section(viz_step, time_d,                 &
     &    sph, geofem, ele_mesh, nod_fld, zmeans%zm_psf)
      call SGS_MHD_zonal_RMS_section(viz_step, time_d, SGS_par,         &
     &    sph, geofem, ele_mesh, WK, nod_fld, zmeans%zrms_psf)
!
      end subroutine SGS_MHD_zmean_sections
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine SGS_MHD_zonal_RMS_section(viz_step, time_d, SGS_par,   &
     &          sph, geofem, ele_mesh, WK, nod_fld, zrms_psf)
!
      use m_elapsed_labels_4_VIZ
      use FEM_analyzer_sph_SGS_MHD
      use sph_rtp_zonal_rms_data
      use nod_phys_send_recv
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(SGS_paremeters), intent(in) :: SGS_par
      type(sph_grids), intent(in) :: sph
!
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: geofem
      type(element_geometry), intent(in) :: ele_mesh
      type(works_4_sph_trans_MHD), intent(in) :: WK
!
      type(phys_data), intent(inout) :: nod_fld
      type(sectioning_module), intent(inout) :: zrms_psf
!
!
      if(viz_step%PSF_t%istep_file.le.0) return
      if(zrms_psf%num_psf .le. 0) return
!
      if (iflag_debug.gt.0) write(*,*) 'SPH_to_FEM_bridge_SGS_MHD'
      call SPH_to_FEM_bridge_SGS_MHD                                    &
     &   (SGS_par, sph, WK, geofem, nod_fld)
      call zonal_rms_all_rtp_field                                      &
     &   (sph%sph_rtp, geofem%mesh%node, nod_fld)
!
      if (iflag_debug.gt.0) write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(geofem%mesh, nod_fld)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+6)
      if (iflag_debug.gt.0) write(*,*) 'SECTIONING_visualize RMS'
      call SECTIONING_visualize(viz_step%PSF_t%istep_file, time_d,      &
     &    ele_mesh, nod_fld, zrms_psf)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+6)
!
      end subroutine SGS_MHD_zonal_RMS_section
!
!  ---------------------------------------------------------------------
!
      end module SGS_MHD_zonal_mean_viz
