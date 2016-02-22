!
!      module cal_sol_vector_pre_crank
!
!      Written by H. Matsui on March, 2006
!
!!      subroutine cal_vector_pre_consist                               &
!!     &          (node, coef_field, ff_nl, numdir, if_pre, nod_fld, ff)
!!      type(node_data), intent(in) :: node
!!      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!      type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
!!      type(finite_ele_mat_node), intent(in) :: f_nl
!!      type(finite_ele_mat_node), intent(inout) :: f_l
!!      type(phys_data), intent(inout) :: nod_fld
!
!
      module cal_sol_vector_pre_crank
!
      use m_precision
!
      use m_phys_constants
      use cal_sol_field_explicit
!
      use t_geometry_data
      use t_phys_address
      use t_phys_data
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_vector_pre_consist                                 &
     &         (node, coef_field, numdir, if_pre, nod_fld, rhs_tbl,     &
     &          mhd_fem_wk, f_nl, f_l)
!
      use cal_ff_smp_to_ffs
!
      type(node_data), intent(in) :: node
      real(kind = kreal), intent(in) :: coef_field
!
      integer (kind = kint), intent(in) :: numdir, if_pre
!
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(in) :: f_nl
!
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_sol_vec_pre_consist                                      &
     &   (node%numnod, node%istack_internal_smp, f_nl%ff,               &
     &    nod_fld%ntot_phys, numdir, if_pre, nod_fld%d_fld, f_l%ff)
!
      if (coef_field.gt.0.0d0) then
        call cal_ff_smp_2_ff(node, rhs_tbl, numdir,                     &
     &      mhd_fem_wk%ff_m_smp, f_l%ff)
      end if
!
      end subroutine cal_vector_pre_consist
!
! -----------------------------------------------------------------------
!
      end module cal_sol_vector_pre_crank
