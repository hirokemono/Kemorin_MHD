!>@file   SGS_MHD_fields_to_FEM.f90
!!@brief  module SGS_MHD_fields_to_FEM
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2017
!
!>@brief Top routines to transfer spherical harmonics grids data
!!       to FEM data for data visualization
!!
!!@verbatim
!!      subroutine copy_SGS_MHD_fld_from_trans                          &
!!     &         (sph, WK, mesh, iphys, nod_fld)
!!        type(sph_grids), intent(in) :: sph
!!        type(works_4_sph_trans_MHD), intent(in) :: WK
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!      subroutine zmean_SGS_MHD_fld_from_trans                         &
!!     &         (sph, WK, mesh, iphys, nod_fld)
!!      subroutine zrms_SGS_MHD_fld_from_trans                          &
!!     &         (sph, WK, mesh, iphys, nod_fld)
!!        type(sph_grids), intent(in) :: sph
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!@endverbatim
!
      module SGS_MHD_fields_to_FEM
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
      use m_work_time
!
      use t_MHD_step_parameter
      use t_spheric_parameter
      use t_file_IO_parameter
      use t_SGS_control_parameter
!
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_sph_trans_arrays_MHD
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_SGS_MHD_fld_from_trans                            &
     &         (sph, WK, mesh, iphys, nod_fld)
!
      use copy_MHD_4_sph_trans
      use coordinate_convert_4_sph
!      use filtered_forces_to_snap
!      use SGS_field_from_trans
      use filtered_field_from_trans
      use wide_SGS_field_from_trans
      use SGS_forces_from_trans
      use SGS_diff_field_from_trans
      use SGS_snap_field_from_trans
!
      type(sph_grids), intent(in) :: sph
      type(works_4_sph_trans_MHD), intent(in) :: WK
      type(mesh_geometry), intent(in) :: mesh
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
!*
!
      if (iflag_debug.gt.0) write(*,*) 'copy_filtered_field_from_trans'
      call copy_filtered_field_from_trans                               &
     &   (sph%sph_params, sph%sph_rtp, WK%trns_MHD,                     &
     &    mesh%node, iphys, nod_fld)
      if (iflag_debug.gt.0) write(*,*) 'copy_wide_SGS_field_from_trans'
      call copy_wide_SGS_field_from_trans                               &
     &   (sph%sph_params, sph%sph_rtp, WK%trns_SGS,                     &
     &    mesh%node, iphys, nod_fld)
      if (iflag_debug.gt.0) write(*,*) 'copy_SGS_force_from_trans'
!      call copy_SGS_force_from_trans                                   &
!     &   (sph%sph_params, sph%sph_rtp, WK%trns_SGS,                    &
!     &    mesh%node, iphys, nod_fld)
      call copy_SGS_snap_fld_from_trans                                 &
     &   (sph%sph_params, sph%sph_rtp, WK%trns_snap,                    &
     &    mesh%node, iphys, nod_fld)
      call copy_SGS_diff_field_from_trans                               &
     &   (sph%sph_params, sph%sph_rtp, WK%trns_snap,                    &
     &    mesh%node, iphys, nod_fld)
!
!
!!!!!   These routines are for debugging. Be careful!
!
!  Check nonlinear terms by filtered field as SGS term list
!      call copy_filtered_forces_to_snap                                &
!     &   (sph%sph_params, sph%sph_rtp, WK%trns_MHD,                    &
!     &    mesh%node, iphys, nod_fld)
!
!  Check filtered nonlinear terms by using SGS term list
!      call copy_SGS_field_from_trans                                   &
!     &   (sph%sph_params, sph%sph_rtp, WK%trns_SGS,                    &
!     &    mesh%node, iphys, nod_fld)
!
      end subroutine copy_SGS_MHD_fld_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine zmean_SGS_MHD_fld_from_trans                           &
     &         (sph, WK, mesh, iphys, nod_fld)
!
      use copy_MHD_4_sph_trans
      use coordinate_convert_4_sph
!      use filtered_forces_to_snap
!      use SGS_field_from_trans
      use filtered_field_from_trans
      use wide_SGS_field_from_trans
      use SGS_forces_from_trans
      use SGS_diff_field_from_trans
      use SGS_snap_field_from_trans
!
      type(sph_grids), intent(in) :: sph
      type(mesh_geometry), intent(in) :: mesh
      type(phys_address), intent(in) :: iphys
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(phys_data), intent(inout) :: nod_fld
!*
!
      if (iflag_debug.gt.0) write(*,*) 'copy_filtered_field_from_trans'
      call zmean_filtered_field_from_trans                              &
     &   (sph%sph_params, sph%sph_rtp, WK%trns_MHD,                     &
     &    mesh%node, iphys, nod_fld)
      if (iflag_debug.gt.0) write(*,*) 'copy_wide_SGS_field_from_trans'
      call zmean_wide_SGS_field_from_trans                              &
     &   (sph%sph_params, sph%sph_rtp, WK%trns_SGS,                     &
     &    mesh%node, iphys, nod_fld)
      if (iflag_debug.gt.0) write(*,*) 'copy_SGS_force_from_trans'
!      call zmean_SGS_force_from_trans                                  &
!     &   (sph%sph_params, sph%sph_rtp, WK%trns_SGS,                    &
!     &    mesh%node, iphys, nod_fld)
      call zmean_SGS_snap_fld_from_trans                                &
     &   (sph%sph_params, sph%sph_rtp, WK%trns_snap,                    &
     &    mesh%node, iphys, nod_fld)
      call zmean_SGS_diff_field_from_trans                              &
     &   (sph%sph_params, sph%sph_rtp, WK%trns_snap,                    &
     &    mesh%node, iphys, nod_fld)
!
!
!!!!!   These routines are for debugging. Be careful!
!
!  Check nonlinear terms by filtered field as SGS term list
!      call zmean_filtered_forces_to_snap                               &
!     &   (sph%sph_params, sph%sph_rtp, WK%trns_MHD,                    &
!     &    mesh%node, iphys, nod_fld)
!
!  Check filtered nonlinear terms by using SGS term list
!      call zmean_SGS_field_from_trans                                  &
!     &   (sph%sph_params, sph%sph_rtp, WK%trns_SGS,                    &
!     &    mesh%node, iphys, nod_fld)
!
      end subroutine zmean_SGS_MHD_fld_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine zrms_SGS_MHD_fld_from_trans                            &
     &         (sph, WK, mesh, iphys, nod_fld)
!
      use copy_MHD_4_sph_trans
      use coordinate_convert_4_sph
!      use filtered_forces_to_snap
!      use SGS_field_from_trans
      use filtered_field_from_trans
      use wide_SGS_field_from_trans
      use SGS_forces_from_trans
      use SGS_diff_field_from_trans
      use SGS_snap_field_from_trans
!
      type(sph_grids), intent(in) :: sph
      type(mesh_geometry), intent(in) :: mesh
      type(phys_address), intent(in) :: iphys
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(phys_data), intent(inout) :: nod_fld
!*
!
      if (iflag_debug.gt.0) write(*,*) 'copy_filtered_field_from_trans'
      call zrms_filtered_field_from_trans                               &
     &   (sph%sph_params, sph%sph_rtp, WK%trns_MHD,                     &
     &    mesh%node, iphys, nod_fld)
      if (iflag_debug.gt.0) write(*,*) 'copy_wide_SGS_field_from_trans'
      call zrms_wide_SGS_field_from_trans                               &
     &   (sph%sph_params, sph%sph_rtp, WK%trns_SGS,                     &
     &    mesh%node, iphys, nod_fld)
      if (iflag_debug.gt.0) write(*,*) 'copy_SGS_force_from_trans'
!      call zrms_SGS_force_from_trans                                   &
!     &   (sph%sph_params, sph%sph_rtp, WK%trns_SGS,                    &
!     &    mesh%node, iphys, nod_fld)
      call zrms_SGS_snap_fld_from_trans                                 &
     &   (sph%sph_params, sph%sph_rtp, WK%trns_snap,                    &
     &    mesh%node, iphys, nod_fld)
      call zrms_SGS_diff_field_from_trans                               &
     &   (sph%sph_params, sph%sph_rtp, WK%trns_snap,                    &
     &    mesh%node, iphys, nod_fld)
!
!
!!!!!   These routines are for debugging. Be careful!
!
!  Check nonlinear terms by filtered field as SGS term list
!      call zrms_filtered_forces_to_snap                                &
!     &   (sph%sph_params, sph%sph_rtp, WK%trns_MHD,                    &
!     &    mesh%node, iphys, nod_fld)
!
!  Check filtered nonlinear terms by using SGS term list
!      call zrms_SGS_field_from_trans                                   &
!     &   (sph%sph_params, sph%sph_rtp, WK%trns_SGS,                    &
!     &    mesh%node, iphys, nod_fld)
!
      end subroutine zrms_SGS_MHD_fld_from_trans
!
!-----------------------------------------------------------------------
!
      end module SGS_MHD_fields_to_FEM
