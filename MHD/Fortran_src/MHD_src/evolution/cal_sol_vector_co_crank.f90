!
!      module cal_sol_vector_co_crank
!
!      Written by H. Matsui on March, 2006
!
!!      subroutine cal_velo_co_lumped_crank                             &
!!     &         (i_velo, nod_comm, node1, ele1, fluid1, nod_fld1,      &
!!     &          iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,             &
!!     &          mhd_fem1_wk, fem1_wk, f1_l, f1_nl)
!!      subroutine cal_magne_co_lumped_crank                            &
!!     &         (i_magne, nod_comm, node1, ele1, nod_fld1,             &
!!     &          iphys_ele, fld_ele1, nod_bc1_b, jac1_3d_q, rhs_tbl1,  &
!!     &          m1_lump, mhd_fem1_wk, fem1_wk, f1_l, f1_nl)
!!
!!      subroutine cal_velo_co_consist_crank(i_velo, coef_velo,         &
!!     &          node1, ele1, fluid1, nod_fld1, jac1_3d_q,             &
!!     &          rhs_tbl1, mhd_fem1_wk, fem1_wk, f1_l, f1_nl)
!!      subroutine cal_magne_co_consist_crank(i_magne, coef_magne,      &
!!     &          node1, ele1, conduct1, nod_fld1, nod_bc1_b, jac1_3d_q,&
!!     &          rhs_tbl1, mhd_fem1_wk, fem1_wk, f1_l, f1_nl)
!!        type(node_data), intent(in) :: node1
!!        type(element_data), intent(in) :: ele1
!!        type(field_geometry_data), intent(in) :: conduct1
!!        type(phys_data), intent(in) :: nod_fld1
!!        type(jacobians_3d), intent(in) :: jac1_3d_q
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl1
!!        type(vect_fixed_nod_bc_type), intent(in) :: nod_bc1_b
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem1_wk
!!        type(work_finite_element_mat), intent(inout) :: fem1_wk
!!        type(finite_ele_mat_node), intent(inout) :: f1_l, f1_nl
!
      module cal_sol_vector_co_crank
!
      use m_precision
      use m_machine_parameter
!
      use m_control_parameter
      use m_t_int_parameter
      use m_t_step_parameter
!
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_nodal_bc_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_velo_co_lumped_crank                               &
     &         (i_velo, nod_comm, node1, ele1, fluid1, nod_fld1,        &
     &          iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,               &
     &          mhd_fem1_wk, fem1_wk, f1_l, f1_nl)
!
      use int_vol_coriolis_term
      use cal_multi_pass
      use set_nodal_bc_id_data
      use cal_sol_vector_correct
!
      integer(kind = kint), intent(in) :: i_velo
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node1
      type(element_data), intent(in) :: ele1
      type(field_geometry_data), intent(in) :: fluid1
      type(phys_data), intent(in) :: nod_fld1
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: fld_ele1
      type(jacobians_3d), intent(in) :: jac1_3d_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl1
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem1_wk
      type(work_finite_element_mat), intent(inout) :: fem1_wk
      type(finite_ele_mat_node), intent(inout) :: f1_l, f1_nl
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_t_evo_4_vector_fl'
      call cal_t_evo_4_vector(iflag_velo_supg,                          &
     &    fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl, nod_comm,    &
     &    node1, ele1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,        &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'int_coriolis_nod_exp'
      call int_coriolis_nod_exp(node1, mhd_fem1_wk,                     &
     &    i_velo, nod_fld1, f1_l, f1_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'set_boundary_velo_4_rhs'
      call set_boundary_velo_4_rhs(node1, f1_l, f1_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_velo_co_crank'
      call cal_sol_velo_co_crank_lump                                   &
     &   (nod_fld1%n_point, node1%istack_internal_smp,                  &
     &    mhd_fem1_wk%mlump_fl%ml_o, nod_fld1%ntot_phys,                &
     &    i_velo, nod_fld1%d_fld, f1_nl%ff, f1_l%ff)
!
      end subroutine cal_velo_co_lumped_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_magne_co_lumped_crank                              &
     &         (i_magne, nod_comm, node1, ele1, nod_fld1,               &
     &          iphys_ele, fld_ele1, nod_bc1_b, jac1_3d_q, rhs_tbl1,    &
     &          m1_lump, mhd_fem1_wk, fem1_wk, f1_l, f1_nl)
!
      use cal_multi_pass
      use cal_sol_vector_correct
      use set_boundary_scalars
!
      integer(kind = kint), intent(in) :: i_magne
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node1
      type(element_data), intent(in) :: ele1
      type(phys_data), intent(in) :: nod_fld1
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: fld_ele1
      type(jacobians_3d), intent(in) :: jac1_3d_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl1
      type(vect_fixed_nod_bc_type), intent(in) :: nod_bc1_b
      type(lumped_mass_matrices), intent(in) :: m1_lump
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem1_wk
      type(work_finite_element_mat), intent(inout) :: fem1_wk
      type(finite_ele_mat_node), intent(inout) :: f1_l, f1_nl
!
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_t_evo_4_vector'
      call cal_t_evo_4_vector                                           &
     &   (iflag_mag_supg, ele1%istack_ele_smp, m1_lump, nod_comm,       &
     &    node1, ele1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,        &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
!
      if (iflag_debug.eq.1)   write(*,*) 'set_boundary_magne_4_rhs'
      call delete_vector_ffs_on_bc(node1, nod_bc1_b, f1_l, f1_nl)
!
      if (iflag_debug.eq.1)   write(*,*) 'cal_sol_magne_co_crank'
      call cal_sol_vect_co_crank                                        &
     &   (nod_fld1%n_point, node1%istack_internal_smp, m1_lump%ml_o,    &
     &    nod_fld1%ntot_phys, i_magne, nod_fld1%d_fld,                  &
     &    f1_nl%ff, f1_l%ff)
!
      end subroutine cal_magne_co_lumped_crank
!
! -----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_velo_co_consist_crank(i_velo, coef_velo,           &
     &          node1, ele1, fluid1, nod_fld1, jac1_3d_q,               &
     &          rhs_tbl1, mhd_fem1_wk, fem1_wk, f1_l, f1_nl)
!
      use int_vol_initial_MHD
      use cal_ff_smp_to_ffs
      use set_nodal_bc_id_data
      use cal_sol_vector_correct
!
      integer(kind = kint), intent(in) :: i_velo
      real(kind = kreal), intent(in) :: coef_velo
!
      type(node_data), intent(in) :: node1
      type(element_data), intent(in) :: ele1
      type(field_geometry_data), intent(in) :: fluid1
      type(phys_data), intent(in) :: nod_fld1
      type(jacobians_3d), intent(in) :: jac1_3d_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl1
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem1_wk
      type(work_finite_element_mat), intent(inout) :: fem1_wk
      type(finite_ele_mat_node), intent(inout) :: f1_l, f1_nl
!
!
      call reset_ff_t_smp(node1%max_nod_smp, mhd_fem1_wk)
!
      if (iflag_debug.eq.1) write(*,*) 'int_vol_initial_velo'
      call int_vol_initial_vector                                       &
     &   (fluid1%istack_ele_fld_smp, i_velo, coef_velo,                 &
     &    node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, fem1_wk,          &
     &    mhd_fem1_wk)
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'set_boundary_velo_4_rhs'
      call set_boundary_velo_4_rhs(node1, f1_l, f1_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_velo_co_crank_consist'
      call cal_sol_vect_co_crank_consist                                &
     &   (node1%numnod, node1%istack_internal_smp, f1_nl%ff, f1_l%ff)
!
      if (coef_velo .eq. 0.0d0) return
      call cal_ff_smp_2_ff(node1, rhs_tbl1, n_vector,                   &
     &    mhd_fem1_wk%ff_m_smp, f1_l%ff)
!
      end subroutine cal_velo_co_consist_crank
!
! -----------------------------------------------------------------------
!
      subroutine cal_magne_co_consist_crank(i_magne, coef_magne,        &
     &          node1, ele1, conduct1, nod_fld1, nod_bc1_b, jac1_3d_q,  &
     &          rhs_tbl1, mhd_fem1_wk, fem1_wk, f1_l, f1_nl)
!
      use int_vol_initial_MHD
      use cal_ff_smp_to_ffs
      use set_boundary_scalars
      use cal_sol_vector_correct
!
      integer(kind = kint), intent(in) :: i_magne
      real(kind = kreal), intent(in) :: coef_magne
!
      type(node_data), intent(in) :: node1
      type(element_data), intent(in) :: ele1
      type(field_geometry_data), intent(in) :: conduct1
      type(phys_data), intent(in) :: nod_fld1
      type(jacobians_3d), intent(in) :: jac1_3d_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl1
      type(vect_fixed_nod_bc_type), intent(in) :: nod_bc1_b
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem1_wk
      type(work_finite_element_mat), intent(inout) :: fem1_wk
      type(finite_ele_mat_node), intent(inout) :: f1_l, f1_nl
!
!
      call reset_ff_t_smp(node1%max_nod_smp, mhd_fem1_wk)
!
      if (iflag_debug.eq.1)  write(*,*) 'int_vol_initial_magne'
      call int_vol_initial_vector                                       &
     &   (conduct1%istack_ele_fld_smp, i_magne, coef_magne,             &
     &    node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, fem1_wk,          &
     &    mhd_fem1_wk)
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
!
      if(iflag_debug.eq.1)  write(*,*) 'set_boundary_magne_4_rhs'
      call delete_vector_ffs_on_bc(node1, nod_bc1_b, f1_l, f1_nl)
!
      if(iflag_debug.eq.1)  write(*,*) 'cal_sol_magne_co_crank_consist'
      call cal_sol_vect_co_crank_consist                                &
     &   (node1%numnod, node1%istack_internal_smp, f1_nl%ff, f1_l%ff)
!
      if (coef_magne .eq. 0.0d0) return
      call cal_ff_smp_2_ff(node1, rhs_tbl1, n_vector,                   &
     &    mhd_fem1_wk%ff_m_smp, f1_l%ff)
!
      end subroutine cal_magne_co_consist_crank
!
! -----------------------------------------------------------------------
!
      end module cal_sol_vector_co_crank
