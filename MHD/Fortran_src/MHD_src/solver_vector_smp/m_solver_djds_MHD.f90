!>@file   m_solver_djds_MHD.f90
!!@brief  module m_solver_djds_MHD
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2002
!!@date Modified in Nov., 2013
!
!>     DJDS ordering table for MHD dynamo model
!!
!
      module m_solver_djds_MHD
!
      use m_precision
      use t_comm_table
      use t_solver_djds
      use t_vector_for_solver
      use t_solver_djds_MHD
!
      implicit none
!
!
!>        Structure of matrices for MHD dynamo simulation
      type(MHD_MG_matrices), save :: MHD1_matrices
!MHD1_matrices%MG_DJDS_table
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
      subroutine set_MHD_whole_connectivity                             &
     &         (nod_comm, node, ele, next_tbl, rhs_tbl)
!
      use t_comm_table
      use t_geometry_data
      use t_next_node_ele_4_node
      use t_table_FEM_const
!
      use set_table_type_RHS_assemble
      use set_MHD_connectivity
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(next_nod_ele_table), intent(inout) :: next_tbl
      type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
!
!C +-------------------------------+
!  +   set RHS assemble table      +
!C +-------------------------------+
      call s_set_table_type_RHS_assemble                                &
     &   (node, ele, next_tbl, rhs_tbl)
!
!C +-------------------------------+
!  +   set Matrix assemble table   +
!C +-------------------------------+
      call set_djds_whole_connectivity(nod_comm, node, solver_C,        &
     &    next_tbl%neib_nod, MHD1_matrices%MG_comm_table(0),            &
     &    MHD1_matrices%MG_DJDS_table(0))
!
      call link_comm_tbl_types                                          &
     &   (nod_comm, MHD1_matrices%MG_comm_table(0))
!
      end subroutine set_MHD_whole_connectivity
!
!-----------------------------------------------------------------------
!
      subroutine set_MHD_layerd_connectivity(node, ele, fluid)
!
      use t_geometry_data
      use t_geometry_data_MHD
!
      use set_MHD_connectivity
      use copy_mesh_structures
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
!      type(field_geometry_data), intent(in) :: conduct, insulate
!
!
      call set_djds_layer_connectivity(node, ele, ele%nnod_4_ele,       &
     &    fluid%iele_start_fld, fluid%iele_end_fld,                     &
     &    DJDS_comm_fl, solver_C, MHD1_matrices%MG_DJDS_fluid(0))
!
      if (ele%nnod_4_ele .ne. num_t_linear) then
        call set_djds_layer_connectivity(node, ele, num_t_linear,       &
     &      ione, ele%numele, MHD1_matrices%MG_comm_table(0), solver_C, &
     &      MHD1_matrices%MG_DJDS_linear(0))
        call set_djds_layer_connectivity(node, ele, num_t_linear,       &
     &      fluid%iele_start_fld, fluid%iele_end_fld,                   &
     &      DJDS_comm_fl, solver_C, MHD1_matrices%MG_DJDS_lin_fl(0))
      else
        call link_djds_connect_structs                                  &
     &     (MHD1_matrices%MG_DJDS_table(0),                             &
     &      MHD1_matrices%MG_DJDS_linear(0))
        call link_djds_connect_structs(MHD1_matrices%MG_DJDS_fluid(0),  &
     &      MHD1_matrices%MG_DJDS_lin_fl(0))
      end if
!
      call copy_comm_tbl_types                                          &
     &   (DJDS_comm_fl, MHD1_matrices%MG_comm_fluid(0))
      call deallocate_type_comm_tbl(DJDS_comm_fl)
!
      end subroutine set_MHD_layerd_connectivity
!
!-----------------------------------------------------------------------
!
      end module m_solver_djds_MHD
