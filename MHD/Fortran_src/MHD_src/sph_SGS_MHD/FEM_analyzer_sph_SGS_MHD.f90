!>@file   FEM_analyzer_sph_SGS_MHD.f90
!!@brief  module FEM_analyzer_sph_SGS_MHD
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2017
!
!>@brief Top routines to transfer spherical harmonics grids data
!!       to FEM data for data visualization
!!
!!@verbatim
!!      subroutine SPH_to_FEM_bridge_SGS_MHD                            &
!!     &        (SGS_par, sph, WK, geofem, iphys, nod_fld)
!!      subroutine SPH_to_FEM_bridge_zRMS_snap                          &
!!     &        (SGS_par, sph, WK, geofem, iphys, nod_fld)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(sph_grids), intent(in) :: sph
!!        type(works_4_sph_trans_MHD), intent(in) :: WK
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!!
!!@n @param  i_step       Current time step
!!@n @param  visval       Return flag to call visualization routines
!
      module FEM_analyzer_sph_SGS_MHD
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
      use m_work_time
!
      use t_time_data
      use t_MHD_step_parameter
      use t_spheric_parameter
      use t_file_IO_parameter
      use t_SGS_control_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_SGS_MHD                              &
     &        (SGS_par, sph, WK, geofem, iphys, nod_fld)
!
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_sph_trans_arrays_MHD
      use t_SGS_control_parameter
!
      use FEM_analyzer_sph_MHD
      use SGS_MHD_fields_to_FEM
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(sph_grids), intent(in) :: sph
      type(works_4_sph_trans_MHD), intent(in) :: WK
      type(mesh_data), intent(in) :: geofem
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
!*
!*  -----------  data transfer to FEM array --------------
!*
      call SPH_to_FEM_bridge_MHD                                        &
     &   (sph%sph_params, sph%sph_rtp, WK, geofem%mesh, iphys, nod_fld)
!
!
      if(SGS_par%model_p%iflag_SGS .eq. 0) return
      call copy_SGS_MHD_fld_from_trans                                  &
     &   (sph, WK, geofem%mesh, iphys, nod_fld)
!
      end subroutine SPH_to_FEM_bridge_SGS_MHD
!
!-----------------------------------------------------------------------
!
      subroutine zonal_mean_FEM_bridge_SGS_MHD                          &
     &        (SGS_par, sph, WK, geofem, iphys, nod_fld)
!
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_sph_trans_arrays_MHD
      use t_SGS_control_parameter
!
      use FEM_analyzer_sph_MHD
      use SGS_MHD_fields_to_FEM
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(sph_grids), intent(in) :: sph
      type(mesh_data), intent(in) :: geofem
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
      type(works_4_sph_trans_MHD), intent(inout) :: WK
!*
!*  -----------  data transfer to FEM array --------------
!*
      call zonal_mean_to_FEM_bridge_MHD                                 &
     &   (sph%sph_params, sph%sph_rtp, WK, geofem%mesh, iphys, nod_fld)
!
!
      if(SGS_par%model_p%iflag_SGS .eq. 0) return
      call zmean_SGS_MHD_fld_from_trans                                 &
     &   (sph, WK, geofem%mesh, iphys, nod_fld)
!
      end subroutine zonal_mean_FEM_bridge_SGS_MHD
!
!-----------------------------------------------------------------------
!
      subroutine zonal_RMS_FEM_bridge_SGS_MHD                           &
     &        (SGS_par, sph, WK, geofem, iphys, nod_fld)
!
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_sph_trans_arrays_MHD
      use t_SGS_control_parameter
!
      use FEM_analyzer_sph_MHD
      use SGS_MHD_fields_to_FEM
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(sph_grids), intent(in) :: sph
      type(mesh_data), intent(in) :: geofem
      type(phys_address), intent(in) :: iphys
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(phys_data), intent(inout) :: nod_fld
!*
!*  -----------  data transfer to FEM array --------------
!*
      call zonal_mean_to_FEM_bridge_MHD                                 &
     &   (sph%sph_params, sph%sph_rtp, WK, geofem%mesh, iphys, nod_fld)
!
!
      if(SGS_par%model_p%iflag_SGS .eq. 0) return
      call zrms_SGS_MHD_fld_from_trans                                  &
     &   (sph, WK, geofem%mesh, iphys, nod_fld)
!
      end subroutine zonal_RMS_FEM_bridge_SGS_MHD
!
!-----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_zRMS_snap                            &
     &        (SGS_par, sph, WK, geofem, iphys, nod_fld)
!
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_sph_trans_arrays_MHD
      use t_SGS_control_parameter
!
      use sph_rtp_zonal_rms_data
      use SGS_MHD_fields_to_FEM
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(sph_grids), intent(in) :: sph
      type(works_4_sph_trans_MHD), intent(in) :: WK
      type(mesh_data), intent(in) :: geofem
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
!*
!*  -----------  data transfer to FEM array --------------
!*
      call SPH_to_FEM_bridge_SGS_MHD                                    &
     &   (SGS_par, sph, WK, geofem, iphys, nod_fld)
!
! ----  Take zonal mean
!
      if (iflag_debug.eq.1) write(*,*) 'zonal_cyl_rms_all_rtp_field'
!      call zonal_rms_all_rtp_field                                     &
!     &   (sph%sph_rtp, geofem%mesh%node, nod_fld)
      call zonal_cyl_rms_all_rtp_field                                  &
     &   (sph%sph_rtp, geofem%mesh%node, nod_fld)
!
      end subroutine SPH_to_FEM_bridge_zRMS_snap
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_sph_SGS_MHD
