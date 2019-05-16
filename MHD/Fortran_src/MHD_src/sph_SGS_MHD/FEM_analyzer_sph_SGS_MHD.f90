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
!!     &        (SGS_par, sph, WK, geofem, nod_fld)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(sph_grids), intent(in) :: sph
!!        type(works_4_sph_trans_MHD), intent(in) :: WK
!!        type(mesh_data), intent(in) :: geofem
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
     &        (SGS_par, sph, WK, geofem, nod_fld)
!
      use t_mesh_data
      use t_phys_data
      use t_sph_trans_arrays_MHD
      use t_SGS_control_parameter
!
      use set_address_sph_trans_snap
      use SGS_MHD_fields_to_FEM
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(sph_grids), intent(in) :: sph
      type(works_4_sph_trans_MHD), intent(in) :: WK
      type(mesh_data), intent(in) :: geofem
!
      type(phys_data), intent(inout) :: nod_fld
!*
!*  -----------  data transfer to FEM array --------------
!*
      if (iflag_debug.gt.0) write(*,*) 'copy_force_from_transform MHD'
      call copy_force_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_MHD%forward, geofem%mesh, nod_fld)
!
      if(SGS_par%model_p%iflag_SGS .gt. 0) then
        call copy_SGS_MHD_fld_from_trans(sph, WK, geofem%mesh, nod_fld)
      end if
!
!
      if (iflag_debug.gt.0) write(*,*) 'copy_field_from_transform SNAP'
      call copy_field_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_snap%backward, geofem%mesh, nod_fld)
      if (iflag_debug.gt.0) write(*,*) 'copy_force_from_transform SNAP'
      call copy_force_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_snap%forward, geofem%mesh, nod_fld)
!
      end subroutine SPH_to_FEM_bridge_SGS_MHD
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_sph_SGS_MHD
