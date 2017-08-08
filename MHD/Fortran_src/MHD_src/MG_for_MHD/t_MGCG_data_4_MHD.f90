!
!      module t_MGCG_data_4_MHD
!
!     Written by H. Matsui on Dec., 2008
!
!!@verbatim
!!      subroutine alloc_MGCG_MHD_data(MGCG_WK, MGCG_MHD_FEM)
!!      subroutine dealloc_MGCG_MHD_mesh(MGCG_MHD_FEM)
!!@endverbatim
!
      module t_MGCG_data_4_MHD
!
      use m_precision
!
      use t_MGCG_data
      use t_geometry_data_MHD
      use t_coefs_element_4_MHD
      use t_finite_element_mat
      use t_MHD_mass_matricxes
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
      type CRS_tables_MHD
        type(CRS_matrix_connect) :: whole
        type(CRS_matrix_connect) :: fluid
        type(CRS_matrix_connect) :: conduct
        type(CRS_matrix_connect) :: insulate
      end type CRS_tables_MHD
!
!
      type MGCG_MHD_data
!>        MHD mesh data structure
        type(mesh_data_MHD), allocatable :: MG_MHD_mesh(:)
!>        Nodal boundary
        type(nodal_boundarty_conditions), allocatable                   &
     &                       :: MG_node_bc(:)
!>        Surface boundary
        type(surface_boundarty_conditions), allocatable                 &
     &                       :: MG_surf_bc(:)
!
!>        structure for coefficients
        type(coefs_4_MHD_type), allocatable :: ak_MHD_AMG(:)
!
!>        mass matrices
        type(lumped_mass_mat_layerd), allocatable :: MG_mk_MHD(:)
!
!
!>        CRS matrix for FEM assemble
        type(CRS_tables_MHD), allocatable :: MG_MHD_CRS_table(:)
!
!>        filter moments data (need read routines!!)
        type(gradient_model_data_type), allocatable :: MG_filter_MHD(:)
!
        type(SGS_terms_address), allocatable :: MG_ifld_diff(:)
        type(SGS_coefficients_type), allocatable :: MG_diff_coefs(:)
!
        type(IO_boundary), allocatable :: IO_MG_bc(:)
      end type MGCG_MHD_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_MGCG_MHD_data(MGCG_WK, MGCG_MHD_FEM)
!
      type(MGCG_data), intent(in) :: MGCG_WK
      type(MGCG_MHD_data), intent(inout) :: MGCG_MHD_FEM
!
!
      allocate(MGCG_MHD_FEM%MG_MHD_mesh(MGCG_WK%num_MG_level))
      allocate(MGCG_MHD_FEM%MG_node_bc(MGCG_WK%num_MG_level))
      allocate(MGCG_MHD_FEM%MG_surf_bc(MGCG_WK%num_MG_level))
      allocate(MGCG_MHD_FEM%IO_MG_bc(MGCG_WK%num_MG_level))
!
      allocate(MGCG_MHD_FEM%MG_filter_MHD(MGCG_WK%num_MG_level))
!
      allocate(MGCG_MHD_FEM%MG_filter_MHD(MGCG_WK%num_MG_level))
      allocate(MGCG_MHD_FEM%MG_ifld_diff(MGCG_WK%num_MG_level))
      allocate(MGCG_MHD_FEM%MG_diff_coefs(MGCG_WK%num_MG_level))
      allocate(MGCG_MHD_FEM%MG_diff_coefs(MGCG_WK%num_MG_level))
!
      allocate(MGCG_MHD_FEM%MG_mk_MHD(MGCG_WK%num_MG_level))
      allocate(MGCG_MHD_FEM%MG_MHD_CRS_table(MGCG_WK%num_MG_level))
!
      end subroutine alloc_MGCG_MHD_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_MGCG_MHD_data(MGCG_MHD_FEM)
!
      type(MGCG_MHD_data), intent(inout) :: MGCG_MHD_FEM
!
!
      deallocate(MGCG_MHD_FEM%MG_MHD_mesh)
      deallocate(MGCG_MHD_FEM%MG_node_bc, MGCG_MHD_FEM%MG_surf_bc)
      deallocate(MGCG_MHD_FEM%IO_MG_bc)
      deallocate(MGCG_MHD_FEM%MG_filter_MHD)
!
      deallocate(MGCG_MHD_FEM%MG_filter_MHD, MGCG_MHD_FEM%MG_ifld_diff)
      deallocate(MGCG_MHD_FEM%MG_diff_coefs)
      deallocate(MGCG_MHD_FEM%MG_diff_coefs)
      deallocate(MGCG_MHD_FEM%MG_mk_MHD, MGCG_MHD_FEM%MG_MHD_CRS_table)
!
      end subroutine dealloc_MGCG_MHD_data
!
!  ---------------------------------------------------------------------
!
      end module t_MGCG_data_4_MHD
