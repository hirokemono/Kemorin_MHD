!>@file   m_solver_djds_MHD.f90
!!@brief  module m_solver_djds_MHD
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2002
!!@date Modified in Nov., 2013
!
!>     DJDS ordering table for MHD dynamo model
!!
!!      subroutine set_MHD_connectivities                               &
!!     &         (DJDS_param, mesh, fluid, next_tbl, rhs_tbl)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(DJDS_poarameter), intent(in) :: DJDS_param
!!        type(next_nod_ele_table), intent(inout) :: next_tbl
!!        type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
!!        type(field_geometry_data), intent(in) :: fluid
!
      module m_solver_djds_MHD
!
      use m_precision
      use t_control_parameter
      use t_iccg_parameter
      use t_FEM_control_parameter
      use t_mesh_data
      use t_comm_table
      use t_solver_djds
      use t_vector_for_solver
      use t_solver_djds_MHD
      use t_MHD_matrices_pack
      use t_physical_property
!
!
      implicit none
!
!>        Structure of matrices for MHD dynamo simulation
      type(MHD_MG_matrices), save :: MHD1_matrices
!
!>        Structure of matrices for all fields
      type(MHD_matrices_pack), save :: solver_pack1
!
!
!>      Structure for MPI communicator
      type(mpi_4_solver), save :: solver_C
!
!>      Communication table structure for fluid
      type(communication_table), save :: DJDS_comm_fl
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_MHD_connectivities                                 &
     &         (DJDS_param, mesh, fluid, next_tbl, rhs_tbl)
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
!
      type(next_nod_ele_table), intent(inout) :: next_tbl
      type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
!
!
      call set_MHD_whole_connectivity                                   &
     &   (DJDS_param, mesh, solver_C,                                   &
     &    next_tbl, rhs_tbl, MHD1_matrices%MG_DJDS_table(0),            &
     &    MHD1_matrices%MG_comm_table(0))
!
      call set_MHD_djds_connectivities(DJDS_param,                      &
     &    mesh, fluid, DJDS_comm_fl, solver_C,                          &
     &    MHD1_matrices%MG_DJDS_table(0),                               &
     &    MHD1_matrices%MG_DJDS_fluid(0),                               &
     &    MHD1_matrices%MG_DJDS_linear(0),                              &
     &    MHD1_matrices%MG_DJDS_lin_fl(0))
!
      call copy_comm_tbl_types                                          &
     &   (DJDS_comm_fl, MHD1_matrices%MG_comm_fluid(0))
      call deallocate_type_comm_tbl(DJDS_comm_fl)
!
      end subroutine set_MHD_connectivities
!
!-----------------------------------------------------------------------
!
      end module m_solver_djds_MHD
