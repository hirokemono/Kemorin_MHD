!>@file   FEM_analyzer_sph_MHD_w_viz.f90
!!@brief  module FEM_analyzer_sph_MHD_w_viz
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2014
!
!>@brief Top routines to transfer spherical harmonics grids data
!!       to FEM data for data visualization
!!
!!@verbatim
!!      subroutine FEM_initialize_w_viz                                 &
!!     &         (mesh, group, ele_mesh, next_tbl, jac_3d_q, jac_3d_l)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!        type(element_geometry), intent(inout) :: ele_mesh
!!        type(next_nod_ele_table), intent(inout) :: next_tbl
!!        type(jacobians_3d), intent(inout) :: jac_3d_q, jac_3d_l
!!@endverbatim
!!
!!@n @param  i_step       Current time step
!!@n @param  istep_psf    Time step increment for cross sectioning
!!@n @param  istep_iso    Time step increment for iso surfaces
!!@n @param  istep_pvr    Time step increment for volume rendering
!!@n @param  istep_fline  Time step increment for field line generation
!!@n @param  visval       Return flag to call visualization routines
!
      module FEM_analyzer_sph_MHD_w_viz
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_machine_parameter
      use m_work_time
!
      use t_mesh_data
      use t_next_node_ele_4_node
      use t_jacobian_3d
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_w_viz                                   &
     &         (mesh, group, ele_mesh, next_tbl, jac_3d_q, jac_3d_l)
!
      use m_t_step_parameter
      use m_fem_gauss_int_coefs
      use m_node_phys_data
      use m_cal_max_indices
!
      use set_ele_id_4_node_type
      use FEM_analyzer_sph_MHD
      use int_volume_of_domain
      use set_normal_vectors
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
      type(next_nod_ele_table), intent(inout) :: next_tbl
      type(jacobians_3d), intent(inout) :: jac_3d_q, jac_3d_l
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
!  --  init FEM mesh data
!
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_MHD'
      call FEM_initialize_sph_MHD(mesh, group, ele_mesh,                &
     &    iphys, nod_fld1, range)
!
!  -------------------------------
!
      if( (i_step_output_fline) .gt. 0) then
        if (iflag_debug.gt.0) write(*,*) 'set_ele_id_4_node'
        call set_ele_id_4_node                                          &
     &    (mesh%node, mesh%ele, next_tbl%neib_ele)
      end if
!
      if(i_step_output_pvr .le. 0) Return
!
!  -----  If there is no volume rendering... return
!
      if (iflag_debug.eq.1) write(*,*)  'maximum_integration_points'
      call maximum_integration_points(ione)
      call const_jacobian_volume_normals                                &
     &   (mesh, ele_mesh%surf, group, jac_3d_l, jac_3d_q)
!
      end subroutine FEM_initialize_w_viz
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_sph_MHD_w_viz
