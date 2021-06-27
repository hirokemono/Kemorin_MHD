!evolve_by_adams_bashforth.f90
!      module evolve_by_adams_bashforth
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!!      subroutine cal_velo_pre_adams                                   &
!!     &         (dt, FEM_prm, nod_comm, node, ele, fluid, fl_prop,     &
!!     &          iphys, iphys_LES, iphys_ele_base, ele_fld,            &
!!     &          g_FEM, jac_3d, rhs_tbl, mlump_fl, mhd_fem_wk, fem_wk, &
!!     &          f_l, f_nl, nod_fld, v_sol, SR_sig, SR_r)
!!      subroutine cal_magne_pre_adams(i_field, i_previous, dt,         &
!!     &          FEM_prm, nod_comm, node, ele, conduct,                &
!!     &          iphys_ele_base, ele_fld, g_FEM, jac_3d, rhs_tbl,      &
!!     &          mlump_cd, mhd_fem_wk, fem_wk, f_l, f_nl,              &
!!     &          nod_fld, v_sol, SR_sig, SR_r)
!!      subroutine cal_scalar_pre_adams                                 &
!!     &         (iflag_supg, i_field, i_previous, dt,                  &
!!     &          FEM_prm, nod_comm, node, ele, fluid,                  &
!!     &          iphys_ele_base, ele_fld, g_FEM, jac_3d, rhs_tbl,      &
!!     &          mlump_fl, mhd_fem_wk, fem_wk, f_l, f_nl,              &
!!     &          nod_fld, v_sol, SR_sig, SR_r)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(phys_address), intent(in) :: iphys
!!        type(SGS_model_addresses), intent(in) :: iphys_LES
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(lumped_mass_matrices), intent(in) :: mlump_fl
!!        type(lumped_mass_matrices), intent(in) :: mlump_cd
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!
      module evolve_by_adams_bashforth
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_physical_property
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_SGS_model_addresses
      use t_base_field_labels
      use t_fem_gauss_int_coefs
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_vector_for_solver
      use t_solver_SR
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_velo_pre_adams                                     &
     &         (dt, FEM_prm, nod_comm, node, ele, fluid, fl_prop,       &
     &          iphys, iphys_LES, iphys_ele_base, ele_fld,              &
     &          g_FEM, jac_3d, rhs_tbl, mlump_fl, mhd_fem_wk, fem_wk,   &
     &          f_l, f_nl, nod_fld, v_sol, SR_sig, SR_r)
!
      use cal_multi_pass
      use cal_sol_field_explicit
      use int_vol_coriolis_term
!
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: mlump_fl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call cal_t_evo_4_vector                                           &
     &   (FEM_prm%iflag_velo_supg, fluid%istack_ele_fld_smp, dt,        &
     &    FEM_prm, mlump_fl, nod_comm, node, ele,                       &
     &    iphys_ele_base, ele_fld, g_FEM, jac_3d, rhs_tbl,              &
     &    mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl, v_sol, SR_sig, SR_r)
!
      if (iflag_debug.eq.1)  write(*,*) 'int_coriolis_nod_exp'
      call int_coriolis_nod_exp(node, fl_prop, mlump_fl,                &
     &    iphys%base%i_velo, nod_fld, f_l, f_nl)
      if (iflag_debug.eq.1)  write(*,*) 'int_buoyancy_nod_exp'
      call int_buoyancy_nod_exp                                         &
     &   (node, fl_prop, mlump_fl, iphys, iphys_LES, nod_fld, f_nl)
!
      call cal_sol_vect_pre_fluid_adams                                 &
     &   (dt, node%numnod, node%istack_internal_smp, mlump_fl%ml,       &
     &    f_l%ff, f_nl%ff, nod_fld%ntot_phys, n_vector,                 &
     &    iphys%base%i_velo, iphys%exp_work%i_pre_mom, nod_fld%d_fld)
!
      end subroutine cal_velo_pre_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_magne_pre_adams(i_field, i_previous, dt,           &
     &          FEM_prm, nod_comm, node, ele, conduct,                  &
     &          iphys_ele_base, ele_fld, g_FEM, jac_3d, rhs_tbl,        &
     &          mlump_cd, mhd_fem_wk, fem_wk, f_l, f_nl,                &
     &          nod_fld, v_sol, SR_sig, SR_r)
!
      use cal_sol_field_explicit
      use cal_multi_pass
!
      integer(kind = kint), intent(in) :: i_field, i_previous
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: mlump_cd
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call cal_t_evo_4_vector_cd                                        &
     &   (FEM_prm%iflag_magne_supg, conduct%istack_ele_fld_smp, dt,     &
     &    FEM_prm, mlump_cd, nod_comm, node, ele,                       &
     &    iphys_ele_base, ele_fld, g_FEM, jac_3d, rhs_tbl,              &
     &    mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl, v_sol, SR_sig, SR_r)
      call cal_sol_vect_pre_conduct_adams                               &
     &   (dt, node%numnod, conduct%istack_inter_fld_smp,                &
     &    conduct%numnod_fld, conduct%inod_fld, mlump_cd%ml,            &
     &    f_l%ff, f_nl%ff, nod_fld%ntot_phys, n_vector,                 &
     &    i_field, i_previous, nod_fld%d_fld)
!
      end subroutine cal_magne_pre_adams
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_scalar_pre_adams                                   &
     &         (iflag_supg, i_field, i_previous, dt,                    &
     &          FEM_prm, nod_comm, node, ele, fluid,                    &
     &          iphys_ele_base, ele_fld, g_FEM, jac_3d, rhs_tbl,        &
     &          mlump_fl, mhd_fem_wk, fem_wk, f_l, f_nl,                &
     &          nod_fld, v_sol, SR_sig, SR_r)
!
      use cal_multi_pass
      use cal_sol_field_explicit
!
      integer(kind = kint), intent(in) :: iflag_supg
      integer(kind = kint), intent(in) :: i_field, i_previous
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: mlump_fl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call cal_t_evo_4_scalar                                           &
     &   (iflag_supg, fluid%istack_ele_fld_smp, dt, FEM_prm,            &
     &    mlump_fl, nod_comm, node, ele, iphys_ele_base, ele_fld,       &
     &    g_FEM, jac_3d, rhs_tbl, mhd_fem_wk%ff_m_smp,                  &
     &    fem_wk, f_l, f_nl, v_sol, SR_sig, SR_r)
!      call check_ff(my_rank, n_scalar, node, f_l)
!      call check_ff(my_rank, n_scalar, node, f_nl)
      call cal_sol_vect_pre_fluid_adams                                 &
     &   (dt, node%numnod, node%istack_internal_smp,                    &
     &    mlump_fl%ml, f_l%ff, f_nl%ff, nod_fld%ntot_phys,              &
     &    n_scalar, i_field, i_previous, nod_fld%d_fld)
!
      end subroutine cal_scalar_pre_adams
!
! ----------------------------------------------------------------------
!
      end module evolve_by_adams_bashforth
