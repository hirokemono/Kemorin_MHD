!>@file   m_solver_djds_MHD.f90
!!@brief  module m_solver_djds_MHD
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2002
!!@date Modified in Nov., 2013
!
!>     DJDS ordering table for MHD dynamo model
!!
!!      subroutine link_MG_DJDS_MHD_structures
!!
      module m_solver_djds_MHD
!
      use m_precision
      use m_iccg_parameter
      use t_comm_table
      use t_solver_djds
      use t_vector_for_solver
      use t_solver_djds_MHD
      use t_MHD_matrices_pack
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
      private :: set_residual_4_crank
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_aiccg_matrices(node)
!
      use m_control_parameter
      use m_physical_property
      use allocate_MHD_AMG_array
!
      type(node_data), intent(in) :: node
!
!
      call set_residual_4_crank(fl_prop1, cd_prop1, ht_prop1, cp_prop1)
!
      call alloc_aiccg_matrices                                         &
     &   (node, fl_prop1, cd_prop1, ht_prop1, cp_prop1,                 &
     &    MHD1_matrices%MG_DJDS_table(0),                               &
     &    MHD1_matrices%MG_DJDS_fluid(0),                               &
     &    MHD1_matrices%MG_DJDS_linear(0),                              &
     &    MHD1_matrices%MG_DJDS_lin_fl(0),                              &
     &    MHD1_matrices%Vmat_MG_DJDS(0), MHD1_matrices%Bmat_MG_DJDS(0), &
     &    MHD1_matrices%Tmat_MG_DJDS(0), MHD1_matrices%Cmat_MG_DJDS(0), &
     &    MHD1_matrices%Pmat_MG_DJDS(0), MHD1_matrices%Fmat_MG_DJDS(0))
!
      end subroutine allocate_aiccg_matrices
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_aiccg_matrices
!
      use m_control_parameter
      use m_physical_property
!
      call dealloc_aiccg_matrices                                       &
     &   (fl_prop1, cd_prop1, ht_prop1, cp_prop1,                       &
     &    MHD1_matrices%Vmat_MG_DJDS(0), MHD1_matrices%Bmat_MG_DJDS(0), &
     &    MHD1_matrices%Tmat_MG_DJDS(0), MHD1_matrices%Cmat_MG_DJDS(0), &
     &    MHD1_matrices%Pmat_MG_DJDS(0), MHD1_matrices%Fmat_MG_DJDS(0))
!
      end subroutine deallocate_aiccg_matrices
!
! ----------------------------------------------------------------------
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
      use copy_mesh_structures
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
      call set_djds_whole_connectivity                                  &
     &   (nod_comm, node, solver_C, next_tbl%neib_nod,                  &
     &    DJDS_param1, MHD1_matrices%MG_DJDS_table(0))
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
!
!
      call set_djds_layer_connectivity(node, ele, ele%nnod_4_ele,       &
     &    fluid%iele_start_fld, fluid%iele_end_fld, DJDS_comm_fl,       &
     &    solver_C, DJDS_param1, MHD1_matrices%MG_DJDS_fluid(0))
!
      if (ele%nnod_4_ele .ne. num_t_linear) then
        call set_djds_layer_connectivity(node, ele, num_t_linear,       &
     &      ione, ele%numele, MHD1_matrices%MG_comm_table(0), solver_C, &
     &      DJDS_param1, MHD1_matrices%MG_DJDS_linear(0))
        call set_djds_layer_connectivity(node, ele, num_t_linear,       &
     &      fluid%iele_start_fld, fluid%iele_end_fld, DJDS_comm_fl,     &
     &      solver_C, DJDS_param1, MHD1_matrices%MG_DJDS_lin_fl(0))
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
!-----------------------------------------------------------------------
!
      subroutine set_residual_4_crank                                   &
     &         (fl_prop, cd_prop, ht_prop, cp_prop)
!
      use m_machine_parameter
      use m_t_int_parameter
      use m_iccg_parameter
!
      use t_physical_property
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
!
      if (fl_prop%iflag_scheme .ge. id_Crank_nicolson) then
        eps_4_velo_crank = eps_crank * fl_prop%coef_diffuse * dt**2
        if(iflag_debug.eq.1)                                            &
     &     write(12,*) 'eps_4_velo', eps_4_velo_crank
      end if
!
      if (ht_prop%iflag_scheme .ge. id_Crank_nicolson) then
        eps_4_temp_crank = eps_crank * ht_prop%coef_diffuse * dt**2
        if(iflag_debug.eq.1)                                            &
     &     write(12,*) 'eps_4_temp_crank', eps_4_temp_crank
      end if
!
      if (   cd_prop%iflag_Bevo_scheme .ge. id_Crank_nicolson           &
     &  .or. cd_prop%iflag_Aevo_scheme .ge. id_Crank_nicolson) then
!
        if(eps_4_magne_crank .le. 0.0d0) then
          eps_4_magne_crank = eps_crank * cd_prop%coef_diffuse * dt**2
        end if
        if(iflag_debug.eq.1)                                            &
     &     write(12,*) 'eps_4_magne_crank', eps_4_magne_crank
      end if
!
      if (cp_prop%iflag_scheme .ge. id_Crank_nicolson) then
        eps_4_comp_crank = eps_crank * cp_prop%coef_diffuse * dt**2
        if(iflag_debug.eq.1)                                            &
     &     write(12,*) 'iflag_t_evo_4_composit', cp_prop%iflag_scheme
      end if
!
      end subroutine set_residual_4_crank
!
! ----------------------------------------------------------------------
!
      end module m_solver_djds_MHD
