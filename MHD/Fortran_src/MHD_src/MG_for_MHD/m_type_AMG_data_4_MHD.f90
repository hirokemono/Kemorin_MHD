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
      use m_type_AMG_data
      use m_type_AMG_mesh
      use t_geometry_data_MHD
      use t_coefs_element_4_MHD
      use t_finite_element_mat
      use t_finite_element_mat_MHD
      use t_nodal_bc_data
      use t_surface_bc_data
      use t_filter_elength
      use t_solver_djds
      use t_crs_connect
!
      implicit  none
!
      type(mesh_data_MHD), save :: MG_MHD_mesh(0:max_MG_level)
!   mesh data structure
!
      type(coefs_4_MHD_AMG), save :: ak_MHD_AMG(max_MG_level)
!   structure for coefs for poisson equations
!
      type(lumped_mass_mat_layerd), save ::    MG_mk_MHD(max_MG_level)
!   mass matrix for layers
!
      type(nodal_boundarty_conditions), save                            &
     &                       ::   MG_node_bc(0:max_MG_level)
      type(surface_boundarty_conditions), save                          &
     &                       :: MG_surf_bc(0:max_MG_level)
!   mesh data structure
!
      type(table_mat_const), target, save                               &
     &                       :: MG_djds_const_idx(max_MG_level)
      type(table_mat_const), target, save                               &
     &                       :: MG_djds_const_idx_fl(max_MG_level)
      type(table_mat_const), target, save                               &
     &                       :: MG_djds_const_idx_cd(max_MG_level)
!
      type(table_mat_const), pointer, save :: MG_djds_const_idx_l(:)
      type(table_mat_const), pointer, save :: MG_djds_const_idx_fll(:)
!   table for matrix assemble
!
      type(DJDS_ordering_table), target, save                           &
     &                      :: MG_djds_tbl_cd(0:max_MG_level)
!      type(DJDS_ordering_table), target, save                          &
!     &                      :: MG_djds_tbl_full_cd(0:max_MG_level)
!
      type CRS_tables_MHD
        type(CRS_matrix_connect) :: whole
        type(CRS_matrix_connect) :: fluid
        type(CRS_matrix_connect) :: conduct
        type(CRS_matrix_connect) :: insulate
      end type CRS_tables_MHD
!
      type(CRS_tables_MHD), save :: MG_MHD_CRS_table(max_MG_level)
!   CRS matrix for FEM assemble
!
      type(gradient_model_data_type), save                              &
     &                      :: MG_filter_MHD(max_MG_level)
!   filter moments data (need read routines!!)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_MG_djds_const_lin_idx(num_level)
!
      integer(kind = kint), intent(in) :: num_level
!
!
      allocate( MG_djds_const_idx_l(0:num_level) )
      allocate( MG_djds_const_idx_fll(0:num_level) )
!
      end subroutine alloc_MG_djds_const_lin_idx
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_MG_djds_const_lin_idx
!
!
      deallocate( MG_djds_const_idx_l       )
      deallocate( MG_djds_const_idx_fll     )
!
      end subroutine dealloc_MG_djds_const_lin_idx
!
!-----------------------------------------------------------------------
!
      subroutine link_MG_djds_const_lin_idx(num_level)
!
      integer(kind = kint), intent(in) :: num_level
!
!
      MG_djds_const_idx_l =>       MG_djds_const_idx
      MG_djds_const_idx_fll =>     MG_djds_const_idx_fl
!
      end subroutine link_MG_djds_const_lin_idx
!
!-----------------------------------------------------------------------
!
      subroutine unlink_MG_djds_const_lin_idx
!
!
      nullify( MG_djds_const_idx_l       )
      nullify( MG_djds_const_idx_fll     )
!
      end subroutine unlink_MG_djds_const_lin_idx
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine link_MG_djds_tbl_lin_idx(num_level)
!
      use m_solver_djds_MHD
!
      integer(kind = kint), intent(in) :: num_level
!
!
      MHD1_matrices%MG_DJDS_linear => MHD1_matrices%MG_DJDS_table
      MHD1_matrices%MG_DJDS_lin_fl => MHD1_matrices%MG_DJDS_fluid
!
      end subroutine link_MG_djds_tbl_lin_idx
!
!-----------------------------------------------------------------------
!
      subroutine unlink_MG_djds_tbl_lin_idx
!
      use m_solver_djds_MHD
!
!
      nullify( MHD1_matrices%MG_DJDS_linear        )
      nullify( MHD1_matrices%MG_DJDS_lin_fl      )
!
      end subroutine unlink_MG_djds_tbl_lin_idx
!
!-----------------------------------------------------------------------
!
      end module m_type_AMG_data_4_MHD
