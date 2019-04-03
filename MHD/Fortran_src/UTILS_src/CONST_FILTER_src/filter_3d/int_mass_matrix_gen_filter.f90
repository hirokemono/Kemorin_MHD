!
!      module int_mass_matrix_gen_filter
!
!      Written by H. Matsui on an., 2006
!
!!      subroutine int_mass_matrix_4_filter(gfil_p, node, ele, g_FEM,   &
!!     &          jac_3d, rhs_tbl, fem_wk, f_l, m_lump)
!!        type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
!!        type(node_data),    intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!!        type(element_list_4_filter), intent(in) :: fil_elist
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!!        type(lumped_mass_matrices), intent(inout) :: m_lump
!
      module int_mass_matrix_gen_filter
!
!
      use m_precision
      use m_machine_parameter
!
      use m_geometry_constants
!
      use t_geometry_data
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_ctl_params_4_gen_filter
!
      implicit none
!
      private :: int_grped_mass_matrix_filter
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix_4_filter(gfil_p, node, ele, g_FEM,     &
     &          jac_3d, rhs_tbl, fem_wk, f_l, m_lump)
!
      use int_vol_mass_matrix
!
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!      type(element_list_4_filter), intent(in) :: fil_elist
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(lumped_mass_matrices), intent(inout) :: m_lump
!
!
!      if(gfil_p%id_filter_area_grp(1) .eq. -1) then
      call int_lumped_mass_matrix(node, ele, g_FEM, jac_3d, rhs_tbl,    &
     &    gfil_p%num_int_points, fem_wk, f_l, m_lump)
!      else
!        call int_grped_mass_matrix_filter(gfil_p%num_int_points,       &
!     &      node, ele, g_FEM, jac_3d, rhs_tbl, fil_elist,              &
!     &      fem_wk, f_l, m_lump)
!      end if
!
!      call check_mass_martix(my_rank, node%numnod, m_lump)
!
      end subroutine int_mass_matrix_4_filter
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_grped_mass_matrix_filter(num_int_points,           &
     &          node, ele, g_FEM, jac_3d, rhs_tbl, fil_elist,           &
     &          fem_wk, f_l, m_lump)
!
      use t_element_list_4_filter
      use int_grouped_mass_matrix
      use cal_ff_smp_to_ffs
!
      integer(kind = kint), intent(in) :: num_int_points
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(element_list_4_filter), intent(in) :: fil_elist
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(lumped_mass_matrices), intent(inout) :: m_lump
!
!
      if     (ele%nnod_4_ele.eq.num_t_quad                              &
     &   .or. ele%nnod_4_ele.eq.num_t_lag) then
        call int_grp_mass_matrix_HRZ_full                               &
     &     (node, ele, g_FEM, jac_3d, rhs_tbl,                          &
     &      fil_elist%iele_filter_smp_stack, fil_elist%nele_4_filter,   &
     &      fil_elist%iele_4_filter, num_int_points,                    &
     &      fem_wk, f_l, m_lump)
      else
        call int_grp_mass_matrix_diag                                   &
     &     (node, ele, g_FEM, jac_3d, rhs_tbl,                          &
     &      fil_elist%iele_filter_smp_stack, fil_elist%nele_4_filter,   &
     &      fil_elist%iele_4_filter, num_int_points,                    &
     &      fem_wk, f_l, m_lump)
      end if
!
      end subroutine int_grped_mass_matrix_filter
!
!-----------------------------------------------------------------------
!
      end module int_mass_matrix_gen_filter
