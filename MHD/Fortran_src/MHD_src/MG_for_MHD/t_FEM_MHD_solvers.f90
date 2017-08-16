!>@file   t_FEM_MHD_solvers.f90
!!@brief  module t_FEM_MHD_solvers
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2002
!!@date Modified in Aug., 2017
!
!>     DJDS ordering table for MHD dynamo model
!
!!      subroutine set_MHD_connectivities(DJDS_param, mesh, fluid,      &
!!     &          solver_C, next_tbl, rhs_tbl, MHD_mat, DJDS_comm_fl)
!!        type(DJDS_poarameter), intent(in) :: DJDS_param
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(mpi_4_solver), intent(in) :: solver_C
!!
!!        type(next_nod_ele_table), intent(inout) :: next_tbl
!!        type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
!!        type(communication_table), intent(inout) :: DJDS_comm_fl
!
      module t_FEM_MHD_solvers
!
      use m_precision
      use t_control_parameter
      use t_iccg_parameter
      use t_FEM_control_parameter
      use t_mesh_data
      use t_solver_djds
      use t_vector_for_solver
      use t_solver_djds_MHD
      use t_MHD_matrices_pack
      use t_physical_property
      use t_material_property
!
      use t_MGCG_data
      use t_MGCG_data_4_MHD
!
      implicit none
!
!
!>      Matrix structure for FEM_MHD
      type FEM_MHD_solvers
!>        Strucutre of coefficients for each element
        type(coefs_4_MHD_type) :: ak_MHD
!
!>        Structure of matrices for MHD dynamo simulation
        type(MHD_MG_matrices) :: MHD_mat
!>          Structure of matrices for all fields
        type(MHD_matrices_pack) :: solver_pack
!
!>        Structure for MPI communicator
        type(mpi_4_solver) :: solver_C
!>        Communication table structure for fluid
        type(communication_table) :: DJDS_comm_fl
!
!
!>        Structure for MGCG solver
        type(MGCG_data) :: MGCG_WK
!
!>        Structure for MGCG solver
        type(mesh_4_MGCG) :: MGCG_FEM
!
        type(MGCG_MHD_data) :: MGCG_MHD_FEM
      end type FEM_MHD_solvers
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_MHD_connectivities(DJDS_param, mesh, fluid,        &
     &          solver_C, next_tbl, rhs_tbl, MHD_mat, DJDS_comm_fl)
!
      use t_mesh_data
      use t_geometry_data_MHD
!
      use set_djds_connectivity_type
      use copy_mesh_structures
      use set_djds_connect_type_MHD
!
      type(DJDS_poarameter), intent(in) :: DJDS_param
      type(mesh_geometry), intent(in) :: mesh
      type(field_geometry_data), intent(in) :: fluid
      type(mpi_4_solver), intent(in) :: solver_C
!
      type(next_nod_ele_table), intent(inout) :: next_tbl
      type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
      type(MHD_MG_matrices), intent(inout) :: MHD_mat
      type(communication_table), intent(inout) :: DJDS_comm_fl
!
!
      call set_MHD_whole_connectivity                                   &
     &   (DJDS_param, mesh, solver_C, next_tbl, rhs_tbl,                &
     &    MHD_mat%MG_DJDS_table(0), MHD_mat%MG_comm_table(0))
!
      call set_MHD_djds_connectivities(DJDS_param,                      &
     &    mesh, fluid, DJDS_comm_fl, solver_C,                          &
     &    MHD_mat%MG_DJDS_table(0), MHD_mat%MG_DJDS_fluid(0),           &
     &    MHD_mat%MG_DJDS_linear(0), MHD_mat%MG_DJDS_lin_fl(0))
!
      call copy_comm_tbl_types                                          &
     &   (DJDS_comm_fl, MHD_mat%MG_comm_fluid(0))
      call deallocate_type_comm_tbl(DJDS_comm_fl)
!
      end subroutine set_MHD_connectivities
!
!-----------------------------------------------------------------------
!
      end module t_FEM_MHD_solvers
