!>@file   set_djds_connect_type_MHD.f90
!!@brief  module set_djds_connect_type_MHD
!!
!!@author H. Matsui
!!@date        Written by H. Matsui in Dec., 2008
!!@n      modified by H. Matsui on Nov., 2013
!
!>@brief  Construct index table for DJDS solver
!!
!!@verbatim
!!      subroutine set_MHD_whole_connectivity                           &
!!     &         (DJDS_param, mesh, solver_C, next_tbl, rhs_tbl,        &
!!     &          DJDS_table, solver_comm_tbl)
!!      subroutine set_MHD_djds_connectivities                          &
!!     &         (DJDS_param, mesh, fluid, DJDS_comm_fl, solver_C,      &
!!     &          DJDS_table, DJDS_fluid, DJDS_linear, DJDS_linear_fl)
!!      subroutine empty_whole_djds_connectivity                        &
!!     &         (mesh, neib_tbl, rhs_tbl, DJDS_tbl)
!!      subroutine empty_MHD_djds_connectivities                        &
!!     &         (mesh, DJDS_fluid, DJDS_linear, DJDS_linear_fl)
!!        type(DJDS_poarameter), intent(in) :: DJDS_param
!!        type(MGCG_data), intent(in) :: MGCG_WK
!!        type(mesh_4_MGCG), intent(in) :: MGCG_FEM
!!        type(MGCG_MHD_data), intent(in) :: MGCG_MHD_FEM
!!        type(MHD_MG_matrices), intent(inout) :: MHD_matrices
!!@endverbatim
!
      module set_djds_connect_type_MHD
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_constants
      use m_geometry_constants
!
      use t_iccg_parameter
      use t_crs_connect
      use t_solver_djds
      use t_solver_djds_MHD
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_MHD_whole_connectivity                             &
     &         (DJDS_param, mesh, solver_C, next_tbl, rhs_tbl,          &
     &          DJDS_table, solver_comm_tbl)
!
      use t_comm_table
      use t_geometry_data
      use t_next_node_ele_4_node
      use t_table_FEM_const
!
      use set_table_4_RHS_assemble
      use set_djds_connectivity_type
      use copy_mesh_structures
!
      type(mesh_geometry), intent(in) :: mesh
      type(mpi_4_solver), intent(in) :: solver_C
      type(DJDS_poarameter), intent(in) :: DJDS_param
!
      type(next_nod_ele_table), intent(inout) :: next_tbl
      type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
      type(DJDS_ordering_table), intent(inout) :: DJDS_table
      type(communication_table), intent(inout) :: solver_comm_tbl
!
!C +-------------------------------+
!  +   set RHS assemble table      +
!C +-------------------------------+
      call s_set_RHS_assemble_table(mesh, next_tbl, rhs_tbl)
!
!C +-------------------------------+
!  +   set Matrix assemble table   +
!C +-------------------------------+
      call s_set_djds_connectivity(mesh%nod_comm, mesh%node, solver_C,  &
     &    next_tbl, DJDS_param, DJDS_table)
!
      call link_comm_tbl_types                                          &
     &   (mesh%nod_comm, solver_comm_tbl)
!
      end subroutine set_MHD_whole_connectivity
!
!-----------------------------------------------------------------------
!
      subroutine set_MHD_djds_connectivities                            &
     &         (DJDS_param, mesh, fluid, DJDS_comm_fl, solver_C,        &
     &          DJDS_table, DJDS_fluid, DJDS_linear, DJDS_linear_fl)
!
      use t_mesh_data
      use t_geometry_data_MHD
      use t_next_node_ele_4_node
      use set_djds_connectivity_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(field_geometry_data), intent(in) :: fluid
      type(communication_table), intent(in) :: DJDS_comm_fl
      type(mpi_4_solver), intent(in) :: solver_C
      type(DJDS_poarameter), intent(in) :: DJDS_param
      type(DJDS_ordering_table), intent(in) :: DJDS_table
!
      type(DJDS_ordering_table), intent(inout) :: DJDS_fluid
      type(DJDS_ordering_table), intent(inout) :: DJDS_linear
      type(DJDS_ordering_table), intent(inout) :: DJDS_linear_fl
!
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &       'set_djds_layer_connectivity fluid'
      call set_djds_layer_connectivity                                  &
     &   (mesh%ele%nnod_4_ele, mesh%node, mesh%ele,                     &
     &    fluid%iele_start_fld, fluid%iele_end_fld, DJDS_comm_fl,       &
     &    solver_C, DJDS_param, DJDS_fluid)
!
      if(mesh%ele%nnod_4_ele .eq. num_t_linear) then
        call link_djds_connect_structs(DJDS_table,  DJDS_linear)
        call link_djds_connect_structs(DJDS_fluid, DJDS_linear_fl)
      else
        call set_djds_layer_connectivity                                &
     &     (num_t_linear, mesh%node, mesh%ele, ione, mesh%ele%numele,   &
     &      mesh%nod_comm, solver_C, DJDS_param, DJDS_linear)
        call set_djds_layer_connectivity                                &
     &     (num_t_linear, mesh%node, mesh%ele,                          &
     &      fluid%iele_start_fld, fluid%iele_end_fld, DJDS_comm_fl,     &
     &      solver_C, DJDS_param, DJDS_linear_fl)
      end if
!
      end subroutine set_MHD_djds_connectivities
!
!-----------------------------------------------------------------------
!
      subroutine empty_whole_djds_connectivity                          &
     &         (mesh, neib_tbl, rhs_tbl, DJDS_tbl)
!
      use t_mesh_data
      use t_geometry_data_MHD
      use t_next_node_ele_4_node
      use set_table_4_RHS_assemble
      use set_djds_connectivity_type
!
      type(mesh_geometry), intent(in) :: mesh
!
      type(next_nod_ele_table), intent(inout) ::    neib_tbl
      type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
      type(DJDS_ordering_table), intent(inout) :: DJDS_tbl
!
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &            'empty_table_type_RHS_assemble'
      call empty_table_type_RHS_assemble(mesh%node, rhs_tbl, neib_tbl)
!
      if(iflag_debug .gt. 0) write(*,*)  'empty_djds_connectivity'
      call empty_djds_connectivity(mesh%nod_comm, mesh%node, DJDS_tbl)
!
      end subroutine empty_whole_djds_connectivity
!
!-----------------------------------------------------------------------
!
      subroutine empty_MHD_djds_connectivities                          &
     &         (mesh, DJDS_fluid, DJDS_linear, DJDS_linear_fl)
!
      use t_mesh_data
      use t_geometry_data_MHD
      use t_next_node_ele_4_node
      use set_djds_connectivity_type
!
      type(mesh_geometry), intent(in) :: mesh
!
      type(DJDS_ordering_table), intent(inout) :: DJDS_fluid
      type(DJDS_ordering_table), intent(inout) :: DJDS_linear
      type(DJDS_ordering_table), intent(inout) :: DJDS_linear_fl
!
!
      call empty_djds_connectivity                                      &
     &   (mesh%nod_comm, mesh%node, DJDS_fluid)
      call empty_djds_connectivity                                      &
     &   (mesh%nod_comm, mesh%node, DJDS_linear)
      call empty_djds_connectivity                                      &
     &   (mesh%nod_comm, mesh%node, DJDS_linear_fl)
!
      end subroutine empty_MHD_djds_connectivities
!
!-----------------------------------------------------------------------
!
      end module set_djds_connect_type_MHD
