!
!      module m_type_AMG_data_4_MHD
!
!     Written by H. Matsui on Dec., 2008
!
!      subroutine alloc_MG_djds_const_lin_idx(num_level)
!      subroutine dealloc_MG_djds_const_lin_idx
!      subroutine link_MG_djds_const_lin_idx(num_level)
!      subroutine unlink_MG_djds_const_lin_idx
!
!      subroutine alloc_MG_djds_tbl_lin_idx(num_level)
!      subroutine link_MG_djds_tbl_lin_idx(num_level)
!      subroutine unlink_MG_djds_tbl_lin_idx
!
      module m_type_AMG_data_4_MHD
!
      use m_precision
!
      use t_MGCG_data_4_MHD
      use m_type_AMG_data
      use t_geometry_data_MHD
      use t_coefs_element_4_MHD
      use t_finite_element_mat
      use t_finite_element_mat_MHD
      use t_nodal_bc_data
      use t_MHD_boundary_data
      use t_filter_elength
      use t_solver_djds
      use t_crs_connect
      use t_bc_data_MHD
      use t_boundary_field_IO
      use t_SGS_model_coefs
!
      implicit  none
!
!
      type(MGCG_MHD_data), save :: MGCG_MHD_FEM1
!MGCG_MHD_FEM1%MG_MHD_mesh
!
!      type(mesh_data_MHD), target, save :: MG_MHD_mesh(max_MG_level)
!   mesh data structure
!
      type(coefs_4_MHD_type), save :: ak_MHD_AMG(max_MG_level)
!   structure for coefs for poisson equations
!
      type(lumped_mass_mat_layerd), save :: MG_mk_MHD(max_MG_level)
!   mass matrix for layers
!
      type(nodal_boundarty_conditions), save                            &
     &                       :: MG_node_bc(max_MG_level)
      type(surface_boundarty_conditions), save                          &
     &                       :: MG_surf_bc(max_MG_level)
!   mesh data structure
!
      type(CRS_tables_MHD), save :: MG_MHD_CRS_table(max_MG_level)
!   CRS matrix for FEM assemble
!
      type(gradient_model_data_type), save                              &
     &                      :: MG_filter_MHD(max_MG_level)
!   filter moments data (need read routines!!)
!
      type(SGS_terms_address), save :: MG_ifld_diff(max_MG_level)
      type(SGS_coefficients_type), save :: MG_diff_coefs(max_MG_level)
!
      type(IO_boundary), save :: IO_MG_bc(max_MG_level)
!
      end module m_type_AMG_data_4_MHD
