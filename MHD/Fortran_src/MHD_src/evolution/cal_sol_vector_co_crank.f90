!
!      module cal_sol_vector_co_crank
!
!      Written by H. Matsui on March, 2006
!
!!      subroutine cal_velo_co_lumped_crank                             &
!!     &         (i_velo, nod_comm, node, ele, fluid, nod_fld,      &
!!     &          iphys_ele, fld_ele1, jac_3d, rhs_tbl,             &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl)
!!      subroutine cal_magne_co_lumped_crank                            &
!!     &         (i_magne, nod_comm, node, ele, nod_fld,             &
!!     &          iphys_ele, fld_ele1, nod_bc_b, jac_3d, rhs_tbl,  &
!!     &          m1_lump, mhd_fem_wk, fem_wk, f_l, f_nl)
!!
!!      subroutine cal_velo_co_consist_crank(i_velo, coef_velo,         &
!!     &          node, ele, fluid, nod_fld, jac_3d,             &
!!     &          rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl)
!!      subroutine cal_magne_co_consist_crank(i_magne, coef_magne,      &
!!     &          node, ele, conduct, nod_fld, nod_bc_b, jac_3d,&
!!     &          rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_b
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
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
     &         (i_velo, nod_comm, node, ele, fluid, nod_fld,            &
     &          iphys_ele, fld_ele1, jac_3d, rhs_tbl,                   &
     &          mhd_fem_wk, fem_wk, f_l, f_nl)
!
      use int_vol_coriolis_term
      use cal_multi_pass
      use set_nodal_bc_id_data
      use cal_sol_vector_correct
!
      integer(kind = kint), intent(in) :: i_velo
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: fld_ele1
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_t_evo_4_vector_fl'
      call cal_t_evo_4_vector(iflag_velo_supg,                          &
     &    fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl, nod_comm,      &
     &    node, ele, iphys_ele, fld_ele1, jac_3d, rhs_tbl,              &
     &    mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'int_coriolis_nod_exp'
      call int_coriolis_nod_exp(node, mhd_fem_wk,                       &
     &    i_velo, nod_fld, f_l, f_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'set_boundary_velo_4_rhs'
      call set_boundary_velo_4_rhs(node, f_l, f_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_velo_co_crank'
      call cal_sol_velo_co_crank_lump                                   &
     &   (nod_fld%n_point, node%istack_internal_smp,                    &
     &    mhd_fem_wk%mlump_fl%ml_o, nod_fld%ntot_phys,                  &
     &    i_velo, nod_fld%d_fld, f_nl%ff, f_l%ff)
!
      end subroutine cal_velo_co_lumped_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_magne_co_lumped_crank                              &
     &         (i_magne, nod_comm, node, ele, nod_fld,                  &
     &          iphys_ele, fld_ele1, nod_bc_b, jac_3d, rhs_tbl,         &
     &          m1_lump, mhd_fem_wk, fem_wk, f_l, f_nl)
!
      use cal_multi_pass
      use cal_sol_vector_correct
      use set_boundary_scalars
!
      integer(kind = kint), intent(in) :: i_magne
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: fld_ele1
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_b
      type(lumped_mass_matrices), intent(in) :: m1_lump
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_t_evo_4_vector'
      call cal_t_evo_4_vector                                           &
     &   (iflag_mag_supg, ele%istack_ele_smp, m1_lump, nod_comm,        &
     &    node, ele, iphys_ele, fld_ele1, jac_3d, rhs_tbl,              &
     &    mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
!
      if (iflag_debug.eq.1)   write(*,*) 'set_boundary_magne_4_rhs'
      call delete_vector_ffs_on_bc(node, nod_bc_b, f_l, f_nl)
!
      if (iflag_debug.eq.1)   write(*,*) 'cal_sol_magne_co_crank'
      call cal_sol_vect_co_crank                                        &
     &   (nod_fld%n_point, node%istack_internal_smp, m1_lump%ml_o,      &
     &    nod_fld%ntot_phys, i_magne, nod_fld%d_fld,                    &
     &    f_nl%ff, f_l%ff)
!
      end subroutine cal_magne_co_lumped_crank
!
! -----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_velo_co_consist_crank(i_velo, coef_velo,           &
     &          node, ele, fluid, nod_fld, jac_3d,                      &
     &          rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl)
!
      use int_vol_initial_MHD
      use cal_ff_smp_to_ffs
      use set_nodal_bc_id_data
      use cal_sol_vector_correct
!
      integer(kind = kint), intent(in) :: i_velo
      real(kind = kreal), intent(in) :: coef_velo
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
!
      call reset_ff_t_smp(node%max_nod_smp, mhd_fem_wk)
!
      if (iflag_debug.eq.1) write(*,*) 'int_vol_initial_velo'
      call int_vol_initial_vector                                       &
     &   (fluid%istack_ele_fld_smp, i_velo, coef_velo,                  &
     &    node, ele, nod_fld, jac_3d, rhs_tbl, fem_wk, mhd_fem_wk)
      call set_ff_nl_smp_2_ff(n_vector, node, rhs_tbl, f_l, f_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'set_boundary_velo_4_rhs'
      call set_boundary_velo_4_rhs(node, f_l, f_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_velo_co_crank_consist'
      call cal_sol_vect_co_crank_consist                                &
     &   (node%numnod, node%istack_internal_smp, f_nl%ff, f_l%ff)
!
      if (coef_velo .eq. 0.0d0) return
      call cal_ff_smp_2_ff(node, rhs_tbl, n_vector,                     &
     &    mhd_fem_wk%ff_m_smp, f_l%ff)
!
      end subroutine cal_velo_co_consist_crank
!
! -----------------------------------------------------------------------
!
      subroutine cal_magne_co_consist_crank(i_magne, coef_magne,        &
     &          node, ele, conduct, nod_fld, nod_bc_b, jac_3d,          &
     &          rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl)
!
      use int_vol_initial_MHD
      use cal_ff_smp_to_ffs
      use set_boundary_scalars
      use cal_sol_vector_correct
!
      integer(kind = kint), intent(in) :: i_magne
      real(kind = kreal), intent(in) :: coef_magne
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_b
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
!
      call reset_ff_t_smp(node%max_nod_smp, mhd_fem_wk)
!
      if (iflag_debug.eq.1)  write(*,*) 'int_vol_initial_magne'
      call int_vol_initial_vector                                       &
     &   (conduct%istack_ele_fld_smp, i_magne, coef_magne,              &
     &    node, ele, nod_fld, jac_3d, rhs_tbl, fem_wk, mhd_fem_wk)
      call set_ff_nl_smp_2_ff(n_vector, node, rhs_tbl, f_l, f_nl)
!
      if(iflag_debug.eq.1)  write(*,*) 'set_boundary_magne_4_rhs'
      call delete_vector_ffs_on_bc(node, nod_bc_b, f_l, f_nl)
!
      if(iflag_debug.eq.1)  write(*,*) 'cal_sol_magne_co_crank_consist'
      call cal_sol_vect_co_crank_consist                                &
     &   (node%numnod, node%istack_internal_smp, f_nl%ff, f_l%ff)
!
      if (coef_magne .eq. 0.0d0) return
      call cal_ff_smp_2_ff(node, rhs_tbl, n_vector,                     &
     &    mhd_fem_wk%ff_m_smp, f_l%ff)
!
      end subroutine cal_magne_co_consist_crank
!
! -----------------------------------------------------------------------
!
      end module cal_sol_vector_co_crank
