!int_vol_fixed_field_ele.f90
!     module int_vol_fixed_field_ele
!
!        programmed by H.Matsui on July, 2005
!        modified by H.Matsui on AUg., 2007
!
!!      subroutine int_vol_fixed_poisson_surf                           &
!!     &         (node, ele, nod_fld, jac_3d_l, rhs_tbl, n_int,         &
!!     &          ibc_end, num_index_ibc, ele_bc_id, ibc_stack_smp,     &
!!     &          ibc_shape, i_field, fem_wk, f_l)
!!      subroutine int_vol_fixed_scalar_surf                            &
!!     &         (node, ele, nod_fld, jac_3d, rhs_tbl, n_int,           &
!!     &          ibc_end, num_index_ibc, ele_bc_id, ibc_stack_smp,     &
!!     &          ibc_shape, i_field, ak_d, coef_implicit, fem_wk, f_l)
!!      subroutine int_vol_fixed_vector_surf                            &
!!     &         (node, ele, nod_fld, jac_3d, rhs_tbl, n_int,           &
!!     &          nmax_index_ibc, ibc_end, num_index_ibc, ele_bc_id,    &
!!     &          ibc_stack_smp, ibc_shape, i_field, ak_d,              &
!!     &          coef_implicit, fem_wk, f_l)
!!
!!      subroutine int_vol_fixed_rotate_surf                            &
!!     &         (node, ele, nod_fld, jac_3d, rhs_tbl, n_int,           &
!!     &          ibc_end, num_index_ibc, ele_bc_id, ibc_stack_smp,     &
!!     &          ibc_shape, i_field, ak_d, coef_implicit, fem_wk, f_l)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data),    intent(in) :: nod_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      module int_vol_fixed_field_ele
!
      use m_precision
      use m_machine_parameter
!
      use m_phys_constants
      use m_geometry_constants
      use m_fem_gauss_int_coefs
      use t_geometry_data
      use t_phys_data
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
!
      use cal_skv_to_ff_smp
      use field_2_each_element_bc
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_fixed_poisson_surf                             &
     &         (node, ele, nod_fld, jac_3d_l, rhs_tbl, n_int,           &
     &          ibc_end, num_index_ibc, ele_bc_id, ibc_stack_smp,       &
     &          ibc_shape, i_field, fem_wk, f_l)
!
      use fem_skv_poisson_bc
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: ibc_end, num_index_ibc
      integer(kind=kint), intent(in) :: ele_bc_id(num_index_ibc)
      integer(kind=kint), intent(in)                                    &
     &                    :: ibc_stack_smp(0:ele%nnod_4_ele*np_smp)
      integer(kind=kint), intent(in) :: ibc_shape(ele%nnod_4_ele)
!
      integer(kind=kint), intent(in) :: i_field
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind = kint) :: istart_smp, kk, k2
!
!
      if (num_index_ibc .eq. 0) return
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
      do kk=1, ibc_end
        k2 = ibc_shape(kk)
!
        istart_smp = (kk-1)*np_smp
!
        call scalar_2_element_4_boundary(node%numnod,                   &
     &      ele%numele, ele%nnod_4_ele, ele%ie, num_index_ibc,          &
     &      ele_bc_id, ibc_stack_smp(istart_smp), k2,                   &
     &      nod_fld%ntot_phys, i_field, nod_fld%d_fld,                  &
     &      fem_wk%scalar_1)
!
!   'sf' = - \tilde{v}_{i,i} N(x)
!    skv = frac{ \partial \tilde{Phi}_{i}^{n-1} }{ \partial x_{i} }
!
        call fem_skv_poisson_fixed                                      &
     &     (ele%numele, num_t_linear, num_t_linear, np_smp,             &
     &      num_index_ibc, ele_bc_id, ibc_stack_smp(istart_smp),        &
     &      max_int_point, maxtot_int_3d, int_start3, owe3d,            &
     &      k2, n_int, jac_3d_l%ntot_int, jac_3d_l%xjac,                &
     &      jac_3d_l%dnx, jac_3d_l%dnx, fem_wk%scalar_1, fem_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_vol_fixed_poisson_surf
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_fixed_scalar_surf                              &
     &         (node, ele, nod_fld, jac_3d, rhs_tbl, n_int,             &
     &          ibc_end, num_index_ibc, ele_bc_id, ibc_stack_smp,       &
     &          ibc_shape, i_field, ak_d, coef_implicit, fem_wk, f_l)
!
      use fem_skv_poisson_bc
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: ibc_end, num_index_ibc
      integer(kind=kint), intent(in) :: ele_bc_id(num_index_ibc)
      integer(kind=kint), intent(in)                                    &
     &                    :: ibc_stack_smp(0:ele%nnod_4_ele*np_smp)
      integer(kind=kint), intent(in) :: ibc_shape(ele%nnod_4_ele)
!
      integer(kind=kint), intent(in) :: i_field
!
      real(kind = kreal), intent(in) :: coef_implicit
      real(kind = kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind = kint) :: istart_smp, kk, k2
!
!
      if (num_index_ibc .eq. 0) return
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
      do kk=1, ibc_end
        k2 = ibc_shape(kk)
!
        istart_smp = (kk-1)*np_smp
!
        call scalar_2_element_4_boundary(node%numnod,                   &
     &      ele%numele, ele%nnod_4_ele, ele%ie, num_index_ibc,          &
     &      ele_bc_id, ibc_stack_smp(istart_smp), k2,                   &
     &      nod_fld%ntot_phys, i_field, nod_fld%d_fld,                  &
     &      fem_wk%scalar_1)
!
!   'sf' = - \tilde{v}_{i,i} N(x)
!    skv = frac{ \partial \tilde{Phi}_{i}^{n-1} }{ \partial x_{i} }
!
!
        call fem_skv_scalar_diffuse_fixed                               &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, np_smp,         &
     &      num_index_ibc, ele_bc_id, ibc_stack_smp(istart_smp),        &
     &      max_int_point, maxtot_int_3d, int_start3, owe3d,            &
     &      k2, ione, n_int, jac_3d%ntot_int, jac_3d%xjac,              &
     &      jac_3d%dnx, jac_3d%dnx, ak_d, fem_wk%scalar_1, fem_wk%sk6)
      end do
!
      call add1_skv_coef_to_ff_v_smp                                    &
     &   (node, ele, rhs_tbl, coef_implicit, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_vol_fixed_scalar_surf
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_fixed_vector_surf                              &
     &         (node, ele, nod_fld, jac_3d, rhs_tbl, n_int,             &
     &          nmax_index_ibc, ibc_end, num_index_ibc, ele_bc_id,      &
     &          ibc_stack_smp, ibc_shape, i_field, ak_d,                &
     &          coef_implicit, fem_wk, f_l)
!
      use fem_skv_poisson_bc
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: nmax_index_ibc
      integer(kind=kint), intent(in) :: ibc_end(3), num_index_ibc(3)
      integer(kind=kint), intent(in) :: ele_bc_id(nmax_index_ibc,3)
      integer(kind=kint), intent(in)                                    &
     &                    :: ibc_stack_smp(0:ele%nnod_4_ele*np_smp,3)
      integer(kind=kint), intent(in) :: ibc_shape(ele%nnod_4_ele)
!
      integer(kind=kint), intent(in) :: i_field
!
      real(kind = kreal), intent(in) :: coef_implicit
      real(kind = kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind = kint) :: istart_smp, kk, k2, nd, i_comp
!
!
      if (nmax_index_ibc .eq. 0) return
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do nd = 1, n_vector
        if ( num_index_ibc(nd) .gt. 0 ) then
          i_comp = i_field + nd - 1
!
          do kk=1, ibc_end(nd)
            k2 = ibc_shape(kk)
!
            istart_smp = (kk-1)*np_smp
!
            call scalar_2_element_4_boundary(node%numnod,               &
     &          ele%numele, ele%nnod_4_ele, ele%ie, nmax_index_ibc,     &
     &          ele_bc_id(1,nd), ibc_stack_smp(istart_smp,nd), k2,      &
     &          nod_fld%ntot_phys, i_comp, nod_fld%d_fld,               &
     &          fem_wk%scalar_1)
!
!   'sf' = - \tilde{v}_{i,i} N(x)
!    skv = frac{ \partial \tilde{Phi}_{i}^{n-1} }{ \partial x_{i} }
!
            call fem_skv_scalar_diffuse_fixed                           &
     &         (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,             &
     &          np_smp, num_index_ibc(nd), ele_bc_id(1,nd),             &
     &          ibc_stack_smp(istart_smp,nd),                           &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          k2, nd, n_int, jac_3d%ntot_int, jac_3d%xjac,            &
     &          jac_3d%dnx, jac_3d%dnx, ak_d,                           &
     &          fem_wk%scalar_1, fem_wk%sk6)
          end do
        end if
      end do
!
      call add3_skv_coef_to_ff_v_smp                                    &
     &   (node, ele, rhs_tbl, coef_implicit, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_vol_fixed_vector_surf
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_fixed_rotate_surf                              &
     &         (node, ele, nod_fld, jac_3d, rhs_tbl, n_int,             &
     &          ibc_end, num_index_ibc, ele_bc_id, ibc_stack_smp,       &
     &          ibc_shape, i_field, ak_d, coef_implicit, fem_wk, f_l)
!
      use fem_skv_poisson_bc
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: ibc_end, num_index_ibc
      integer(kind=kint), intent(in) :: ele_bc_id(num_index_ibc)
      integer(kind=kint), intent(in)                                    &
     &                    :: ibc_stack_smp(0:ele%nnod_4_ele*np_smp)
      integer(kind=kint), intent(in) :: ibc_shape(ele%nnod_4_ele)
!
      integer(kind=kint), intent(in) :: i_field
!
      real(kind = kreal), intent(in) :: coef_implicit
      real(kind = kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind = kint) :: istart_smp, kk, k2, nd, i_comp
!
!
      if (num_index_ibc .eq. 0) return
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do nd = 1, n_vector
          i_comp = i_field + nd - 1
!
          do kk=1, ibc_end
            k2 = ibc_shape(kk)
!
            istart_smp = (kk-1)*np_smp
!
            call scalar_2_element_4_boundary(node%numnod,               &
     &          ele%numele, ele%nnod_4_ele, ele%ie, num_index_ibc,      &
     &          ele_bc_id, ibc_stack_smp(istart_smp), k2,               &
     &          nod_fld%ntot_phys, i_comp, nod_fld%d_fld,               &
     &          fem_wk%scalar_1)
!
!   'sf' = - \tilde{v}_{i,i} N(x)
!    skv = frac{ \partial \tilde{Phi}_{i}^{n-1} }{ \partial x_{i} }
!
            call fem_skv_scalar_diffuse_fixed                           &
     &         (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, np_smp,     &
     &          num_index_ibc, ele_bc_id, ibc_stack_smp(istart_smp),    &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          k2, nd, n_int, jac_3d%ntot_int, jac_3d%xjac,            &
     &          jac_3d%dnx, jac_3d%dnx, ak_d,                           &
     &          fem_wk%scalar_1, fem_wk%sk6)
          end do
      end do
!
      call add3_skv_coef_to_ff_v_smp                                    &
     &   (node, ele, rhs_tbl, coef_implicit, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_vol_fixed_rotate_surf
!
!-----------------------------------------------------------------------
!
      end module int_vol_fixed_field_ele
