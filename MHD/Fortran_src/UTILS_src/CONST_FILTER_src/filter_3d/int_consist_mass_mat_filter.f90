!int_consist_mass_mat_filter.f90
!     module int_consist_mass_mat_filter
!
!     Written by H. Matsui on Oct., 2006
!
!!      subroutine set_consist_mass_matrix(node, ele, jac_3d,           &
!!     &          tbl_crs, rhs_tbl, mat_tbl, fem_wk, mass)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(CRS_matrix_connect), intent(inout) :: tbl_crs
!!        type(table_mat_const), intent(inout) :: mat_tbl
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(CRS_matrix), intent(inout) :: mass
!
      module int_consist_mass_mat_filter
!
      use m_precision
!
      use m_machine_parameter
      use m_ctl_params_4_gen_filter
!
      use t_geometry_data
      use t_jacobians
      use t_next_node_ele_4_node
      use t_table_FEM_const
      use t_finite_element_mat
      use t_crs_matrix
!
      implicit none
!
      private :: int_vol_consist_mass_matrix
      private :: int_whole_consist_mass_matrix
      private :: int_group_consist_mass_matrix
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_consist_mass_matrix(node, ele, jac_3d,             &
     &          neib_nod, rhs_tbl, tbl_crs, mat_tbl, fem_wk, mass)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(next_nod_id_4_nod), intent(in) :: neib_nod
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
      type(table_mat_const), intent(inout) :: mat_tbl
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(CRS_matrix), intent(inout) :: mass
!
!
!  ---------------------------------------------------
!       set CRS matrix connectivity for whole domain
!  ---------------------------------------------------
!
      if (iflag_debug.eq.1)  write(*,*) 's_set_crs_connection'
      call s_set_crs_connection(node, neib_nod, tbl_crs)
!
      if (iflag_debug.eq.1)  write(*,*) 'set_idx_list_whole_crs_mat'
      call set_idx_list_whole_crs_mat                                   &
     &   (node, ele, tbl_crs, rhs_tbl, mat_tbl)
!
!  ---------------------------------------------------
!        cal consist mass matrix
!  ---------------------------------------------------
!
      if (iflag_debug.eq.1)  write(*,*) 'int_vol_consist_mass_matrix'
      call int_vol_consist_mass_matrix(node, ele, jac_3d,               &
     &    tbl_crs, rhs_tbl, mat_tbl, fem_wk, mass)
!
      end subroutine set_consist_mass_matrix
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_consist_mass_matrix(node, ele, jac_3d,         &
     &          tbl_crs, rhs_tbl, mat_tbl, fem_wk, mass)
!
      use matrix_initialization
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: mat_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(CRS_matrix), intent(inout) :: mass
!
!
      mass%NB_crs =  1
      call alloc_crs_mat_data(tbl_crs, mass)
!
      call iccg_matrix_init(node, ele, rhs_tbl, mat_tbl,                &
     &    mass%ntot_A, mass%A_crs)
!
!      if (id_filter_area_grp(1) .eq. -1) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'int_whole_consist_mass_matrix'
        call int_whole_consist_mass_matrix                              &
     &     (ele, jac_3d, rhs_tbl, mat_tbl, fem_wk, mass)
!      else
!        if (iflag_debug.eq.1)                                          &
!     &    write(*,*) 'int_group_consist_mass_matrix'
!        call int_group_consist_mass_matrix                             &
!     &     (ele, jac_3d, rhs_tbl, mat_tbl, fem_wk, mass)
!      end if
!
      end subroutine int_vol_consist_mass_matrix
!
!-----------------------------------------------------------------------
!
      subroutine int_whole_consist_mass_matrix                          &
     &         (ele, jac_3d, rhs_tbl, mat_tbl, fem_wk, mass)
!
      use int_vol_mass_matrix
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: mat_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(CRS_matrix), intent(inout) :: mass
!
!
      call int_consist_mass_matrix(ele, jac_3d, rhs_tbl, mat_tbl,       &
     &    ele%istack_ele_smp, num_int_points, fem_wk,                   &
     &    mass%ntot_A, mass%A_crs)
!
      end subroutine int_whole_consist_mass_matrix
!
!-----------------------------------------------------------------------
!
      subroutine int_group_consist_mass_matrix                          &
     &         (ele, jac_3d, rhs_tbl, mat_tbl, fem_wk, mass)
!
      use m_element_list_4_filter
      use int_grouped_mass_matrix
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: mat_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(CRS_matrix), intent(inout) :: mass
!
!
      call int_grp_consist_mass_matrix                                  &
     &   (ele, jac_3d, rhs_tbl, mat_tbl, iele_filter_smp_stack,         &
     &    nele_4_filter, iele_4_filter, num_int_points,                 &
     &    fem_wk, mass%ntot_A, mass%A_crs)
!
      end subroutine int_group_consist_mass_matrix
!
!-----------------------------------------------------------------------
!
      end module int_consist_mass_mat_filter
