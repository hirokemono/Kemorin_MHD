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
!!      subroutine copy_SGS_MHD_fld_from_trans(sph, WK, mesh, nod_fld)
!!        type(sph_grids), intent(in) :: sph
!!        type(works_4_sph_trans_MHD), intent(in) :: WK
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(phys_data), intent(inout) :: nod_fld
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
      subroutine copy_SGS_MHD_fld_from_trans(sph, WK, mesh, nod_fld)
!
      use coordinate_convert_4_sph
      use set_address_sph_trans_snap
!
      type(sph_grids), intent(in) :: sph
      type(works_4_sph_trans_MHD), intent(in) :: WK
      type(mesh_geometry), intent(in) :: mesh
!
      type(phys_data), intent(inout) :: nod_fld
!*
!
      call calypso_mpi_barrier
      if (iflag_debug.gt.0) write(*,*) 'copy_field_from_transform SGS'
      call copy_field_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_SGS%backward, mesh, nod_fld)
      call calypso_mpi_barrier
      if (iflag_debug.gt.0) write(*,*) 'copy_field_from_transform DYCS'
      call copy_field_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_DYNS%backward, mesh, nod_fld)
      call calypso_mpi_barrier
      if (iflag_debug.gt.0) write(*,*) 'copy_force_from_transform SGS'
      call copy_force_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_SGS%forward, mesh, nod_fld)
      call calypso_mpi_barrier
      if (iflag_debug.gt.0) write(*,*) 'copy_force_from_transform DYCS'
      call copy_force_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_DYNS%forward, mesh, nod_fld)
!
!!!!!   These routines are for debugging. Be careful!
!
!  Check filtered nonlinear terms by using SGS term list
!      call copy_field_from_transform(sph%sph_params, sph%sph_rtp,      &
!     &    WK%trns_SGS%backward, mesh, nod_fld)
!
      end subroutine copy_SGS_MHD_fld_from_trans
!
!-----------------------------------------------------------------------
!
      end module SGS_MHD_fields_to_FEM
