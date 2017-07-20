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
!!     &         (ucd_param, MHD_step, mesh, group, ele_mesh,           &
!!     &          iphys, nod_fld, next_tbl, jacobians)
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(MHD_step_param), intent(in) :: MHD_step
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!        type(element_geometry), intent(inout) :: ele_mesh
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(next_nod_ele_table), intent(inout) :: next_tbl
!!        type(jacobians_type), intent(inout) :: jacobians
!!@endverbatim
!!
!!@n @param  i_step       Current time step
!!@n @param  visval       Return flag to call visualization routines
!
      module FEM_analyzer_sph_MHD_w_viz
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_MHD_step_parameter
      use m_machine_parameter
      use m_work_time
!
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_next_node_ele_4_node
      use t_jacobians
      use t_VIZ_step_parameter
      use t_MHD_step_parameter
      use t_file_IO_parameter
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
     &         (ucd_param, MHD_step, mesh, group, ele_mesh,             &
     &          iphys, nod_fld, next_tbl, jacobians)
!
      use m_fem_gauss_int_coefs
      use m_cal_max_indices
!
      use set_ele_id_4_node_type
      use FEM_analyzer_sph_MHD
      use int_volume_of_domain
      use set_normal_vectors
!
      type(field_IO_params), intent(in) :: ucd_param
      type(MHD_step_param), intent(in) :: MHD_step
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
      type(phys_address), intent(inout) :: iphys
      type(phys_data), intent(inout) :: nod_fld
      type(next_nod_ele_table), intent(inout) :: next_tbl
      type(jacobians_type), intent(inout) :: jacobians
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
!  --  init FEM mesh data
!
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_MHD'
      call FEM_initialize_sph_MHD(ucd_param, MHD_step,                  &
     &    mesh, group, ele_mesh, iphys, nod_fld, range)
!
!  -------------------------------
!
      if(MHD_step%viz_step%FLINE_t%increment .gt. 0) then
        if (iflag_debug.gt.0) write(*,*) 'set_ele_id_4_node'
        call set_ele_id_4_node                                          &
     &    (mesh%node, mesh%ele, next_tbl%neib_ele)
      end if
!
      if(MHD_step%viz_step%PVR_t%increment .le. 0) Return
!
!  -----  If there is no volume rendering... return
!
      if (iflag_debug.eq.1) write(*,*)  'maximum_integration_points'
      call maximum_integration_points(ione)
      call const_jacobian_volume_normals(my_rank, nprocs,               &
     &    mesh, ele_mesh%surf, group, jacobians)
!
      end subroutine FEM_initialize_w_viz
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_sph_MHD_w_viz
