!int_vol_inertia.f90
!     module int_vol_inertia
!
!      Written by H. Matsui on june, 2005
!
!!      subroutine int_vol_scalar_inertia                               &
!!     &         (node, ele, jac_3d, rhs_tbl, nod_fld, iele_fsmp_stack, &
!!     &          n_int, i_scalar, ncomp_ele, iele_velo, d_ele, coef,   &
!!     &          fem_wk, f_nl)
!!      subroutine int_vol_vector_inertia                               &
!!     &         (node, ele, jac_3d, rhs_tbl, nod_fld, iele_fsmp_stack, &
!!     &          n_int, i_vector, ncomp_ele, iele_velo, d_ele, coef,   &
!!     &          fem_wk, f_nl)
!!
!!      subroutine int_vol_rot_inertia                                  &
!!     &         (node, ele, jac_3d, rhs_tbl, nod_fld, iele_fsmp_stack, &
!!     &          n_int, i_vector, ncomp_ele, iele_vort, d_ele, coef,   &
!!     &          fem_wk, f_nl)
!!
!!      subroutine int_vol_scalar_inertia_upw                           &
!!     &         (node, ele, jac_3d, rhs_tbl, nod_fld, iele_fsmp_stack, &
!!     &          n_int, i_scalar, ncomp_ele, iele_velo, ie_upw, d_ele, &
!!     &          coef, fem_wk, f_nl)
!!      subroutine int_vol_vector_inertia_upw                           &
!!     &         (node, ele, jac_3d, rhs_tbl, nod_fld, iele_fsmp_stack, &
!!     &          n_int, i_vector, ncomp_ele, iele_velo, ie_upw, d_ele, &
!!     &          coef, fem_wk, f_nl)
!!
!!      subroutine int_vol_rot_inertia_upw                              &
!!     &         (node, ele, jac_3d, rhs_tbl, nod_fld, iele_fsmp_stack, &
!!     &          n_int, i_vector, ncomp_ele, iele_vort, ie_upw, d_ele, &
!!     &          coef, fem_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(phys_data),    intent(in) :: nod_fld
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!
      module int_vol_inertia
!
      use m_precision
      use m_phys_constants
      use m_t_step_parameter
!
      use t_geometry_data
      use t_phys_data
      use t_jacobians
      use t_finite_element_mat
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_scalar_inertia                                 &
     &         (node, ele, jac_3d, rhs_tbl, nod_fld, iele_fsmp_stack,   &
     &          n_int, i_scalar, ncomp_ele, iele_velo, d_ele, coef,     &
     &          fem_wk, f_nl)
!
      use cal_skv_to_ff_smp
      use nodal_fld_cst_to_element
      use fem_skv_nonlinear_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: n_int, i_scalar
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind=kreal), intent(in) :: coef
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! -------- loop for shape function for the physical values
      do k2 = 1, ele%nnod_4_ele
        call scalar_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, i_scalar, coef, fem_wk%scalar_1)
        call fem_skv_scalar_inertia_type(iele_fsmp_stack, n_int, k2,    &
     &      fem_wk%scalar_1, d_ele(1,iele_velo), ele, jac_3d,           &
     &      fem_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_scalar_inertia
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_vector_inertia                                 &
     &         (node, ele, jac_3d, rhs_tbl, nod_fld, iele_fsmp_stack,   &
     &          n_int, i_vector, ncomp_ele, iele_velo, d_ele, coef,     &
     &          fem_wk, f_nl)
!
      use cal_skv_to_ff_smp
      use nodal_fld_cst_to_element
      use fem_skv_nonlinear_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: n_int, i_vector
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind=kreal), intent(in) :: coef
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the physical values
      do k2 = 1, ele%nnod_4_ele
        call vector_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, i_vector, coef, fem_wk%vector_1)
        call fem_skv_vector_inertia_type(iele_fsmp_stack, n_int, k2,    &
     &      fem_wk%vector_1, d_ele(1,iele_velo), ele, jac_3d,           &
     &      fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_vector_inertia
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_rot_inertia                                    &
     &         (node, ele, jac_3d, rhs_tbl, nod_fld, iele_fsmp_stack,   &
     &          n_int, i_vector, ncomp_ele, iele_vort, d_ele, coef,     &
     &          fem_wk, f_nl)
!
      use cal_skv_to_ff_smp
      use nodal_fld_cst_to_element
      use fem_skv_nonlinear_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: n_int, i_vector
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_vort
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind=kreal), intent(in) :: coef
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the physical values
      do k2 = 1, ele%nnod_4_ele
        call vector_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, i_vector, coef, fem_wk%vector_1)
        call fem_skv_rot_inertia_type(iele_fsmp_stack, n_int, k2,       &
     &      fem_wk%vector_1, d_ele(1,iele_vort), ele, jac_3d,           &
     &      fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_rot_inertia
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_scalar_inertia_upw                             &
     &         (node, ele, jac_3d, rhs_tbl, nod_fld, iele_fsmp_stack,   &
     &          n_int, i_scalar, ncomp_ele, iele_velo, ie_upw, d_ele,   &
     &          coef, fem_wk, f_nl)
!
      use cal_skv_to_ff_smp
      use nodal_fld_cst_to_element
      use fem_skv_nonlinear_upw_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: n_int, i_scalar
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo, ie_upw
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind=kreal), intent(in) :: coef
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! -------- loop for shape function for the physical values
      do k2 = 1, ele%nnod_4_ele
        call scalar_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, i_scalar, coef, fem_wk%scalar_1)
        call fem_skv_scalar_inertia_upwind                              &
     &     (iele_fsmp_stack, n_int, k2, dt,                             &
     &      fem_wk%scalar_1, d_ele(1,iele_velo), d_ele(1,ie_upw),       &
     &      ele, jac_3d, fem_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_scalar_inertia_upw
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_vector_inertia_upw                             &
     &         (node, ele, jac_3d, rhs_tbl, nod_fld, iele_fsmp_stack,   &
     &          n_int, i_vector, ncomp_ele, iele_velo, ie_upw, d_ele,   &
     &          coef, fem_wk, f_nl)
!
      use cal_skv_to_ff_smp
      use nodal_fld_cst_to_element
      use fem_skv_nonlinear_upw_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: n_int, i_vector
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo, ie_upw
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind=kreal), intent(in) :: coef
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the physical values
      do k2 = 1, ele%nnod_4_ele
        call vector_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, i_vector, coef, fem_wk%vector_1)
        call fem_skv_vector_inertia_upwind                              &
     &     (iele_fsmp_stack, n_int, k2, dt,                             &
     &      fem_wk%vector_1, d_ele(1,iele_velo), d_ele(1,ie_upw),       &
     &      ele, jac_3d, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_vector_inertia_upw
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_rot_inertia_upw                                &
     &         (node, ele, jac_3d, rhs_tbl, nod_fld, iele_fsmp_stack,   &
     &          n_int, i_vector, ncomp_ele, iele_vort, ie_upw, d_ele,   &
     &          coef, fem_wk, f_nl)
!
      use cal_skv_to_ff_smp
      use nodal_fld_cst_to_element
      use fem_skv_nonlinear_upw_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: n_int, i_vector
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_vort, ie_upw
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind=kreal), intent(in) :: coef
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the physical values
      do k2 = 1, ele%nnod_4_ele
        call vector_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, i_vector, coef, fem_wk%vector_1)
        call fem_skv_rot_inertia_upwind(iele_fsmp_stack, n_int, k2, dt, &
     &      fem_wk%vector_1, d_ele(1,iele_vort), d_ele(1,ie_upw),       &
     &      ele, jac_3d, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_rot_inertia_upw
!
! ----------------------------------------------------------------------
!
      end module int_vol_inertia
