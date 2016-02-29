!>@file   m_solver_djds_MHD.f90
!!@brief  module m_solver_djds_MHD
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2002
!!@date Modified in Nov., 2013
!
!>     DJDS ordering table for MHD dynamo model
!!
!!      subroutine deallocate_comm_table_fluid
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
!
!>      Structure of matrix for time evolution of temperature
      type(DJDS_MATRIX), save :: Tmat_DJDS
!
!>      Structure of matrix for time evolution of conposition variation
      type(DJDS_MATRIX), save :: Cmat_DJDS
!
!
!>      Structure for MPI communicator
      type(mpi_4_solver), save :: solver_C
!
!>      DJDS ordering structures for entire domain
      type(DJDS_ordering_table), save :: DJDS_entire
!>      DJDS ordering structures for linear entire domain
      type(DJDS_ordering_table), save :: DJDS_linear
!>      Communication table structure for entire domain
      type(communication_table), save :: DJDS_comm_etr
!
!>      DJDS ordering structures for fluid region
      type(DJDS_ordering_table), save :: DJDS_fluid
!>      DJDS ordering structures for linear fluid region
      type(DJDS_ordering_table), save :: DJDS_fl_l
!>      Communication table structure for fluid
      type(communication_table), save :: DJDS_comm_fl
!
!>      DJDS ordering structures for conductor region
      type(DJDS_ordering_table), save :: DJDS_conduct
!>      DJDS ordering structures for linear conductor region
      type(DJDS_ordering_table), save :: DJDS_cd_l
!>      Communication table structure for conductor
      type(communication_table), save :: DJDS_comm_cd
!
!>      DJDS ordering structures for conductor region
      type(DJDS_ordering_table), save :: DJDS_insulator
!>      DJDS ordering structures for linear conductor region
      type(DJDS_ordering_table), save :: DJDS_ins_l
!>      Communication table structure for insulator
      type(communication_table), save :: DJDS_comm_ins
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
     &    next_tbl%neib_nod, DJDS_comm_etr, DJDS_entire)
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
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
!      type(field_geometry_data), intent(in) :: conduct, insulate
!
!
      call set_djds_layer_connectivity(node, ele, ele%nnod_4_ele,       &
     &    fluid%iele_start_fld, fluid%iele_end_fld,                     &
     &    DJDS_comm_fl, solver_C, DJDS_fluid)
!
      if (ele%nnod_4_ele .ne. num_t_linear) then
        call set_djds_layer_connectivity(node, ele, num_t_linear,       &
     &      ione, ele%numele, DJDS_comm_etr, solver_C, DJDS_linear)
        call set_djds_layer_connectivity(node, ele, num_t_linear,       &
     &      fluid%iele_start_fld, fluid%iele_end_fld,                   &
     &      DJDS_comm_fl, solver_C, DJDS_fl_l)
      else
        call link_djds_connect_structs(DJDS_entire, DJDS_linear)
        call link_djds_connect_structs(DJDS_fluid, DJDS_fl_l)
      end if
!
!
!      call set_djds_layer_connectivity(node, ele, ele%nnod_4_ele,      &
!     &    conduct%iele_start_fld, conduct%iele_end_fld,                &
!     &    DJDS_comm_etr, solver_C, DJDS_conduct)
!      call set_djds_layer_connectivity(node, ele, ele%nnod_4_ele,      &
!     &    insulate%iele_start_fld, insulate%iele_end_fld,              &
!     &    DJDS_comm_etr, solver_C, DJDS_insulator)
!
!      if ( ele%nnod_4_ele .ne. num_t_linear) then
!        call set_djds_layer_connectivity(node, ele, num_t_linear,      &
!     &      conduct%iele_start_fld, conduct%iele_end_fld,              &
!     &      DJDS_comm_etr, solver_C, DJDS_cd_l)
!        call set_djds_layer_connectivity(node, ele, num_t_linear,      &
!     &      insulate%iele_start_fld, insulate%iele_end_fld,            &
!     &      DJDS_comm_etr, solver_C, DJDS_ins_l)
!      else
!        call link_djds_connect_structs(DJDS_conduct, DJDS_cd_l)
!        call link_djds_connect_structs(DJDS_insulator, DJDS_ins_l)
!      end if
!
      end subroutine set_MHD_layerd_connectivity
!
!-----------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_comm_table_fluid
!
!
      call deallocate_type_comm_tbl(DJDS_comm_fl)
!
      end subroutine deallocate_comm_table_fluid
!
!  ---------------------------------------------------------------------
!
      end module m_solver_djds_MHD
