!>@file   cal_current_by_vecp.f90
!!@brief  module cal_current_by_vecp
!!
!!@author H. Matsui 
!!@date Programmed by H. Matsui in 2002
!!        modified by H. Matsui in Aug., 2007
!!
!>@brief  Finite elememt integration to evaluate current density
!!
!!@verbatim
!!      subroutine int_current_diffuse                                  &
!!     &         (FEM_prm, nod_comm, node, ele, surf, sf_grp, Asf_bcs,  &
!!     &          iphys_base, jacs, rhs_tbl, m_lump, mhd_fem_wk,        &
!!     &          fem_wk, surf_wk, f_l, f_nl, nod_fld,                  &
!!     &          v_sol, SR_sig, SR_r)
!!
!!      subroutine int_surf_temp_diffuse(FEM_prm, node, ele, surf,      &
!!     &          sf_grp, Tsf_bcs, iphys_base, nod_fld, jacs, rhs_tbl,  &
!!     &          ak_d_temp, fem_wk, surf_wk, f_l)
!!      subroutine int_surf_velo_diffuse(FEM_prm, node, ele, surf,      &
!!     &          sf_grp, Vsf_bcs, iphys_base, nod_fld, jacs, rhs_tbl,  &
!!     &          ak_d_velo, fem_wk, surf_wk, f_l)
!!      subroutine int_surf_vector_p_diffuse(FEM_prm, node, ele, surf,  &
!!     &          sf_grp, Asf_bcs, iphys_base, nod_fld, jacs, rhs_tbl,  &
!!     &          ak_d_magne, fem_wk, surf_wk, f_l)
!!      subroutine int_surf_magne_diffuse(FEM_prm, node, ele, surf,     &
!!     &          sf_grp, Bsf_bcs, iphys_base, nod_fld, jacs, rhs_tbl,  &
!!     &          ak_d_magne, fem_wk, surf_wk, f_l)
!!      subroutine int_surf_composit_diffuse(FEM_prm, node, ele, surf,  &
!!     &          sf_grp, Csf_bcs, iphys_base, nod_fld, jacs, rhs_tbl,  &
!!     &          ak_d_composit, fem_wk, surf_wk, f_l)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
!!        type(velocity_surf_bc_type), intent(in) :: Asf_bcs
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(scaler_surf_bc_type), intent(in) :: Tsf_bcs
!!        type(scaler_surf_bc_type), intent(in) :: Csf_bcs
!!        type(base_field_address), intent(in) :: iphys_base
!!        type(jacobians_type), intent(in) :: jacs
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data),    intent(inout) :: nod_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!@endverbatim
!
      module cal_current_by_vecp
!
      use m_precision
!
      use t_FEM_control_parameter
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_base_field_labels
      use t_phys_data
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_MHD_finite_element_mat
      use t_surface_bc_vector
      use t_surface_bc_velocity
      use t_vector_for_solver
      use t_solver_SR
!
      use m_machine_parameter
      use m_geometry_constants
      use m_phys_constants
!
      implicit none
!
      private :: int_vol_current_diffuse
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_current_diffuse                                    &
     &         (FEM_prm, nod_comm, node, ele, surf, sf_grp, Asf_bcs,    &
     &          iphys_base, jacs, rhs_tbl, m_lump, mhd_fem_wk,          &
     &          fem_wk, surf_wk, f_l, f_nl, nod_fld,                    &
     &          v_sol, SR_sig, SR_r)
!
      use cal_multi_pass
      use cal_for_ffs
      use cal_ff_smp_to_ffs
      use nod_phys_send_recv
      use int_surf_diffuse_terms
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(base_field_address), intent(in) :: iphys_base
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data),    intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!  Volume integration
!
      call int_vol_current_diffuse                                      &
     &   (iphys_base%i_vecp, FEM_prm%npoint_poisson_int,                &
     &    node, ele, nod_fld, jacs%g_FEM, jacs%jac_3d, rhs_tbl,         &
     &    fem_wk, f_nl)
!
!  for boundary conditions
!
      call int_surf_current_diffuse(node, ele, surf, sf_grp, nod_fld,   &
     &    jacs%g_FEM, jacs%jac_sf_grp, rhs_tbl, Asf_bcs%torque_lead,    &
     &    FEM_prm%npoint_t_evo_int, iphys_base%i_vecp,                  &
     &    fem_wk, surf_wk, f_l)
!
      call cal_multi_pass_4_vector_ff                                   &
     &   (ele%istack_ele_smp, FEM_prm, m_lump, nod_comm, node, ele,     &
     &    jacs%g_FEM, jacs%jac_3d, rhs_tbl, mhd_fem_wk%ff_m_smp,        &
     &    fem_wk, f_l, f_nl, v_sol, SR_sig, SR_r)
      call cal_ff_2_vector(node%numnod, node%istack_nod_smp,            &
     &    f_l%ff, m_lump%ml, nod_fld%ntot_phys,                         &
     &    iphys_base%i_current, nod_fld%d_fld)
!
!    communication
!
      call vector_send_recv(iphys_base%i_current, nod_comm,             &
     &                      nod_fld, v_sol, SR_sig, SR_r)
!
      end subroutine int_current_diffuse
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_current_diffuse(i_vecp, num_int, node, ele,    &
     &          nod_fld, g_FEM, jac_3d, rhs_tbl, fem_wk, f_nl)
!
      use nodal_fld_2_each_element
      use fem_skv_rot2_laplace
      use cal_skv_to_ff_smp
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data),    intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      integer(kind = kint), intent(in) :: i_vecp
      integer(kind = kint), intent(in) :: num_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer (kind = kint) :: k2
!
!
      call reset_ff_smp(n_vector, node, f_nl)
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do k2 = 1, ele%nnod_4_ele
        call vector_phys_2_each_element                                 &
     &     (node, ele, nod_fld, k2, i_vecp, fem_wk%vector_1)
        call fem_all_skv_rot2_laplace                                   &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      np_smp, ele%istack_ele_smp, g_FEM%max_int_point,            &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,         &
     &      num_int, k2, jac_3d%ntot_int, jac_3d%xjac,                  &
     &      jac_3d%dnx, jac_3d%dnx, fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_current_diffuse
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_surf_temp_diffuse(FEM_prm, node, ele, surf,        &
     &          sf_grp, Tsf_bcs, iphys_base, nod_fld, jacs, rhs_tbl,    &
     &          ak_d_temp, fem_wk, surf_wk, f_l)
!
      use t_surface_bc_scalar
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(scaler_surf_bc_type), intent(in) :: Tsf_bcs
      type(base_field_address), intent(in) :: iphys_base
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      real(kind = kreal), intent(in) :: ak_d_temp(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      call int_surf_diffuse_term(node, ele, surf, sf_grp,               &
     &    nod_fld, jacs%g_FEM, jacs%jac_sf_grp, rhs_tbl,                &
     &    Tsf_bcs%flux_lead, FEM_prm%npoint_t_evo_int,                  &
     &    ak_d_temp, iphys_base%i_temp, fem_wk, surf_wk, f_l)
!
      end subroutine int_surf_temp_diffuse
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_velo_diffuse(FEM_prm, node, ele, surf,        &
     &          sf_grp, Vsf_bcs, iphys_base, nod_fld, jacs, rhs_tbl,    &
     &          ak_d_velo, fem_wk, surf_wk, f_l)
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(base_field_address), intent(in) :: iphys_base
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      real(kind = kreal), intent(in) :: ak_d_velo(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      call int_surf_vect_diffuse_term(node, ele, surf, sf_grp,          &
     &    jacs%g_FEM, jacs%jac_sf_grp, nod_fld, rhs_tbl,                &
     &    Vsf_bcs%torque_lead, FEM_prm%npoint_t_evo_int,                &
     &    ak_d_velo, iphys_base%i_velo, fem_wk, surf_wk, f_l)
!
      end subroutine int_surf_velo_diffuse
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_vector_p_diffuse(FEM_prm, node, ele, surf,    &
     &          sf_grp, Asf_bcs, iphys_base, nod_fld, jacs, rhs_tbl,    &
     &          ak_d_magne, fem_wk, surf_wk, f_l)
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(base_field_address), intent(in) :: iphys_base
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      real(kind = kreal), intent(in) :: ak_d_magne(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      call int_surf_vect_diffuse_term(node, ele, surf, sf_grp,          &
     &    jacs%g_FEM, jacs%jac_sf_grp, nod_fld, rhs_tbl,                &
     &    Asf_bcs%torque_lead, FEM_prm%npoint_t_evo_int, ak_d_magne,    &
     &    iphys_base%i_vecp, fem_wk, surf_wk, f_l)
!
      end subroutine int_surf_vector_p_diffuse
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_magne_diffuse(FEM_prm, node, ele, surf,       &
     &          sf_grp, Bsf_bcs, iphys_base, nod_fld, jacs, rhs_tbl,    &
     &          ak_d_magne, fem_wk, surf_wk, f_l)
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(base_field_address), intent(in) :: iphys_base
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      real(kind = kreal), intent(in) :: ak_d_magne(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      call int_surf_vect_diffuse_term(node, ele, surf, sf_grp,          &
     &    jacs%g_FEM, jacs%jac_sf_grp, nod_fld, rhs_tbl,                &
     &    Bsf_bcs%torque_lead, FEM_prm%npoint_t_evo_int,                &
     &    ak_d_magne, iphys_base%i_magne, fem_wk, surf_wk, f_l)
!
      end subroutine int_surf_magne_diffuse
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_composit_diffuse(FEM_prm, node, ele, surf,    &
     &          sf_grp, Csf_bcs, iphys_base, nod_fld, jacs, rhs_tbl,    &
     &          ak_d_composit, fem_wk, surf_wk, f_l)
!
      use t_surface_bc_scalar
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(scaler_surf_bc_type), intent(in) :: Csf_bcs
      type(base_field_address), intent(in) :: iphys_base
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      real(kind = kreal), intent(in) :: ak_d_composit(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      call int_surf_diffuse_term(node, ele, surf, sf_grp, nod_fld,      &
     &    jacs%g_FEM, jacs%jac_sf_grp, rhs_tbl, Csf_bcs%flux_lead,      &
     &    FEM_prm%npoint_t_evo_int, ak_d_composit, iphys_base%i_light,  &
     &    fem_wk, surf_wk, f_l)
!
      end subroutine int_surf_composit_diffuse
!
!-----------------------------------------------------------------------
!
      end module cal_current_by_vecp
