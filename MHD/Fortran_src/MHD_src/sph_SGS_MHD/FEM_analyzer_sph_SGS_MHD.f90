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
!!     &        (SGS_par, sph_params, sph_rtp, WK, mesh, iphys, nod_fld)
!!      subroutine SPH_to_FEM_bridge_zm_SGS_snap                        &
!!     &        (SGS_par, sph_params, sph_rtp, WK, mesh, iphys, nod_fld)
!!      subroutine SPH_to_FEM_bridge_zRMS_snap                          &
!!     &        (SGS_par, sph_params, sph_rtp, WK, mesh, iphys, nod_fld)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(works_4_sph_trans_MHD), intent(in) :: WK
!!        type(mesh_geometry), intent(in) :: mesh
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
      use m_ucd_data
      use t_time_data
      use t_MHD_step_parameter
      use t_file_IO_parameter
      use t_SGS_control_parameter
!
      implicit none
!
      private :: copy_SGS_MHD_fld_from_trans
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_SGS_MHD                              &
     &        (SGS_par, sph_params, sph_rtp, WK, mesh, iphys, nod_fld)
!
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_sph_trans_arrays_MHD
      use t_SGS_control_parameter
!
      use FEM_analyzer_sph_MHD
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(works_4_sph_trans_MHD), intent(in) :: WK
      type(mesh_geometry), intent(in) :: mesh
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
!*
!*  -----------  data transfer to FEM array --------------
!*
      call SPH_to_FEM_bridge_MHD                                        &
     &   (sph_params, sph_rtp, WK, mesh, iphys, nod_fld)
!
!
      if(SGS_par%model_p%iflag_SGS .eq. 0) return
      call copy_SGS_MHD_fld_from_trans                                  &
     &   (sph_params, sph_rtp, WK, mesh, iphys, nod_fld)
!
      end subroutine SPH_to_FEM_bridge_SGS_MHD
!
!-----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_zm_SGS_snap                          &
     &        (SGS_par, sph_params, sph_rtp, WK, mesh, iphys, nod_fld)
!
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_sph_trans_arrays_MHD
      use t_SGS_control_parameter
!
      use FEM_analyzer_sph_MHD
      use sph_rtp_zonal_rms_data
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(works_4_sph_trans_MHD), intent(in) :: WK
      type(mesh_geometry), intent(in) :: mesh
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
!*
!*  -----------  data transfer to FEM array --------------
!*
      call SPH_to_FEM_bridge_SGS_MHD(SGS_par, sph_params, sph_rtp,      &
     &    WK, mesh, iphys, nod_fld)
!
! ----  Take zonal mean
!
      if (iflag_debug.eq.1) write(*,*) 'zonal_mean_all_rtp_field'
      call zonal_mean_all_rtp_field(sph_rtp, mesh%node, nod_fld)
!
      end subroutine SPH_to_FEM_bridge_zm_SGS_snap
!
! ----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_zRMS_snap                            &
     &        (SGS_par, sph_params, sph_rtp, WK, mesh, iphys, nod_fld)
!
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_sph_trans_arrays_MHD
      use t_SGS_control_parameter
!
      use sph_rtp_zonal_rms_data
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(works_4_sph_trans_MHD), intent(in) :: WK
      type(mesh_geometry), intent(in) :: mesh
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
!*
!*  -----------  data transfer to FEM array --------------
!*
      call SPH_to_FEM_bridge_SGS_MHD                                    &
     &   (SGS_par, sph_params, sph_rtp, WK, mesh, iphys, nod_fld)
!
! ----  Take zonal mean
!
      if (iflag_debug.eq.1) write(*,*) 'zonal_cyl_rms_all_rtp_field'
!      call zonal_rms_all_rtp_field(sph_rtp, mesh%node, nod_fld)
      call zonal_cyl_rms_all_rtp_field(sph_rtp, mesh%node, nod_fld)
!
      end subroutine SPH_to_FEM_bridge_zRMS_snap
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_SGS_MHD_fld_from_trans                            &
     &         (sph_params, sph_rtp, WK, mesh, iphys, nod_fld)
!
      use t_spheric_parameter
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_sph_trans_arrays_MHD
!
      use copy_snap_4_sph_trans
      use copy_MHD_4_sph_trans
      use coordinate_convert_4_sph
      use copy_SGS_4_sph_trans
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(works_4_sph_trans_MHD), intent(in) :: WK
      type(mesh_geometry), intent(in) :: mesh
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
!*
!
      if (iflag_debug.gt.0) write(*,*) 'copy_filtered_field_from_trans'
      call copy_filtered_field_from_trans                               &
     &   (sph_params%m_folding, sph_rtp, WK%trns_MHD,                   &
     &    mesh%node, iphys, nod_fld)
      if (iflag_debug.gt.0) write(*,*) 'copy_wide_SGS_field_from_trans'
      call copy_wide_SGS_field_from_trans                               &
     &   (sph_params%m_folding, sph_rtp, WK%trns_SGS,                   &
     &    mesh%node, iphys, nod_fld)
      if (iflag_debug.gt.0) write(*,*) 'copy_SGS_force_from_trans'
!      call copy_SGS_force_from_trans                                   &
!     &   (sph_params%m_folding, sph_rtp, WK%trns_SGS,                  &
!     &    mesh%node, iphys, nod_fld)
      call copy_SGS_snap_fld_from_trans                                 &
     &   (sph_params%m_folding, sph_rtp, WK%trns_snap,                  &
     &    mesh%node, iphys, nod_fld)
      call copy_SGS_diff_field_from_trans                               &
     &   (sph_params%m_folding, sph_rtp, WK%trns_snap,                  &
     &    mesh%node, iphys, nod_fld)
!
!
!!!!!   These routines are for debugging. Be careful!
!
!  Check nonlinear terms by filtered field as SGS term list
!      call copy_filtered_forces_to_snap                                &
!     &   (sph_params%m_folding, sph_rtp, WK%trns_MHD,                  &
!     &    mesh%node, iphys, nod_fld)
!
!  Check filtered nonlinear terms by using SGS term list
!      call copy_SGS_field_from_trans                                   &
!     &   (sph_params%m_folding, sph_rtp, WK%trns_SGS,                  &
!     &    mesh%node, iphys, nod_fld)
!
      end subroutine copy_SGS_MHD_fld_from_trans
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_sph_SGS_MHD
