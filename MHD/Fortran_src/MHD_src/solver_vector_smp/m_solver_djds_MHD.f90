!>@file   m_solver_djds_MHD.f90
!!@brief  module m_solver_djds_MHD
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2002
!!@date Modified in Nov., 2013
!
!>     DJDS ordering table for MHD dynamo model
!!
!!      subroutine allocate_aiccg_matrices                              &
!!     &         (dt, node, fl_prop, cd_prop, ht_prop, cp_prop, FEM_prm)
!!      subroutine deallocate_aiccg_matrices                            &
!!     &         (fl_prop, cd_prop, ht_prop, cp_prop)
!!      subroutine set_MHD_connectivities                               &
!!     &         (DJDS_param, mesh, fluid, next_tbl, rhs_tbl)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(DJDS_poarameter), intent(in) :: DJDS_param
!!        type(next_nod_ele_table), intent(inout) :: next_tbl
!!        type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
!!        type(field_geometry_data), intent(in) :: fluid
!
      module m_solver_djds_MHD
!
      use m_precision
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
      private :: set_residual_4_crank
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_aiccg_matrices                                &
     &         (dt, node, fl_prop, cd_prop, ht_prop, cp_prop, FEM_prm)
!
      use allocate_MHD_AMG_array
!
      real(kind = kreal), intent(in) :: dt
      type(node_data), intent(in) :: node
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
!
      type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
!
!
!
      call set_residual_4_crank                                         &
     &   (dt, fl_prop, cd_prop, ht_prop, cp_prop, FEM_prm)
!
      call alloc_aiccg_matrices                                         &
     &   (node, fl_prop, cd_prop, ht_prop, cp_prop,                     &
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
      subroutine deallocate_aiccg_matrices                              &
     &         (fl_prop, cd_prop, ht_prop, cp_prop)
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
!
!
      call dealloc_aiccg_matrices                                       &
     &   (fl_prop, cd_prop, ht_prop, cp_prop,                           &
     &    MHD1_matrices%Vmat_MG_DJDS(0), MHD1_matrices%Bmat_MG_DJDS(0), &
     &    MHD1_matrices%Tmat_MG_DJDS(0), MHD1_matrices%Cmat_MG_DJDS(0), &
     &    MHD1_matrices%Pmat_MG_DJDS(0), MHD1_matrices%Fmat_MG_DJDS(0))
!
      end subroutine deallocate_aiccg_matrices
!
! ----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!
      subroutine set_residual_4_crank                                   &
     &         (dt, fl_prop, cd_prop, ht_prop, cp_prop, FEM_prm)
!
      use m_machine_parameter
!
      real(kind = kreal), intent(in) :: dt
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
!
      type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
!
!
      if (fl_prop%iflag_scheme .ge. id_Crank_nicolson) then
        FEM_prm%eps_4_velo_crank                                        &
     &      = FEM_PRM%eps_crank * fl_prop%coef_diffuse * dt**2
        if(iflag_debug.eq.1)                                            &
     &     write(12,*) 'eps_4_velo', FEM_prm%eps_4_velo_crank
      end if
!
      if (ht_prop%iflag_scheme .ge. id_Crank_nicolson) then
        FEM_prm%eps_4_temp_crank                                        &
     &      = FEM_PRM%eps_crank * ht_prop%coef_diffuse * dt**2
        if(iflag_debug.eq.1)                                            &
     &     write(12,*) 'eps_4_temp_crank', FEM_prm%eps_4_temp_crank
      end if
!
      if (   cd_prop%iflag_Bevo_scheme .ge. id_Crank_nicolson           &
     &  .or. cd_prop%iflag_Aevo_scheme .ge. id_Crank_nicolson) then
!
        if(FEM_prm%eps_4_magne_crank .le. 0.0d0) then
          FEM_prm%eps_4_magne_crank                                     &
     &        = FEM_PRM%eps_crank * cd_prop%coef_diffuse * dt**2
        end if
        if(iflag_debug.eq.1)                                            &
     &     write(12,*) 'eps_4_magne_crank', FEM_prm%eps_4_magne_crank
      end if
!
      if (cp_prop%iflag_scheme .ge. id_Crank_nicolson) then
        FEM_prm%eps_4_comp_crank                                        &
     &      = FEM_PRM%eps_crank * cp_prop%coef_diffuse * dt**2
        if(iflag_debug.eq.1)                                            &
     &     write(12,*) 'eps_4_comp_crank', FEM_prm%eps_4_comp_crank
      end if
!
      end subroutine set_residual_4_crank
!
! ----------------------------------------------------------------------
!
      end module m_solver_djds_MHD
