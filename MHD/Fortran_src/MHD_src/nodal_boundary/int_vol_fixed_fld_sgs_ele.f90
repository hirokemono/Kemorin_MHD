!int_vol_fixed_fld_sgs_ele.f90
!     module int_vol_fixed_fld_sgs_ele
!
!        programmed by H.Matsui on July, 2005
!        modified by H.Matsui on AUg., 2007
!
!!      subroutine int_vol_fixed_sgs_poisson_surf                       &
!!     &         (node, ele, nod_fld, jac_3d_l, rhs_tbl,                &
!!     &          n_int, ibc_end, num_index_ibc, ele_bc_id,             &
!!     &          ibc_stack_smp, ibc_shape, i_filter, i_field,          &
!!     &          ncomp_diff, iak_diff, ak_diff, fem_wk, f_l)
!!
!!      subroutine int_vol_fixed_sgs_scalar_surf                        &
!!     &         (node, ele, nod_fld, jac_3d, rhs_tbl,                  &
!!     &          n_int, ibc_end, num_index_ibc, ele_bc_id,             &
!!     &          ibc_stack_smp, ibc_shape, i_filter, i_field,          &
!!     &          ak_diff, ak_d, coef_implicit, fem_wk, f_l)
!!      subroutine int_vol_fixed_sgs_vector_surf                        &
!!     &         (node, ele, nod_fld, jac_3d, rhs_tbl,                  &
!!     &          n_int, nmax_index_ibc, ibc_end, num_index_ibc,        &
!!     &          ele_bc_id, ibc_stack_smp, ibc_shape, i_filter,        &
!!     &          i_field,  ncomp_diff, iak_diff, ak_diff, ak_d,        &
!!     &          coef_implicit, fem_wk, f_l)
!!
!!      subroutine int_vol_fixed_rotate_sgs_surf                        &
!!     &         (node, ele, nod_fld, jac_3d, rhs_tbl,                  &
!!     &          n_int, ibc_end, num_index_ibc, ele_bc_id,             &
!!     &          ibc_stack_smp, ibc_shape, i_filter, i_field,          &
!!     &          ak_diff, ak_d, coef_implicit, fem_wk, f_l)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data),    intent(in) :: nod_fld
!!        type(jacobians_3d), intent(in) :: jac_3d_l
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!
      module int_vol_fixed_fld_sgs_ele
!
      use m_precision
      use m_machine_parameter
      use m_phys_constants
!
      use t_geometry_data
      use t_phys_data
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_filter_elength
!
      use cal_skv_to_ff_smp
      use fem_skv_poisson_sgs_bc_t
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
      subroutine int_vol_fixed_sgs_poisson_surf                         &
     &         (node, ele, nod_fld, jac_3d_l, rhs_tbl, FEM_elens,       &
     &          n_int, ibc_end, num_index_ibc, ele_bc_id,               &
     &          ibc_stack_smp, ibc_shape, i_filter, i_field,            &
     &          ncomp_diff, iak_diff, ak_diff, fem_wk, f_l)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: n_int, i_filter
      integer(kind=kint), intent(in) :: ibc_end, num_index_ibc
      integer(kind=kint), intent(in) :: ele_bc_id(num_index_ibc)
      integer(kind=kint), intent(in)                                    &
     &                    :: ibc_stack_smp(0:ele%nnod_4_ele*np_smp)
      integer(kind=kint), intent(in) :: ibc_shape(ele%nnod_4_ele)
!
      integer(kind=kint), intent(in) :: i_field
!
      integer(kind=kint), intent(in) :: ncomp_diff, iak_diff
      real (kind = kreal), intent(in) :: ak_diff(ele%numele,ncomp_diff)
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
     &      nod_fld%ntot_phys, i_field, nod_fld%d_fld, fem_wk%scalar_1)
!
!   'sf' = - \tilde{v}_{i,i} N(x)
!    skv = frac{ \partial \tilde{Phi}_{i}^{n-1} }{ \partial x_{i} }
!
!
        call fem_skv_poisson_sgs_fix_bc(ele, jac_3d_l, FEM_elens,       &
     &      num_index_ibc, ele_bc_id, ibc_stack_smp(istart_smp), k2,    &
     &      n_int, i_filter, ak_diff(1,iak_diff),                       &
     &      fem_wk%scalar_1, fem_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_vol_fixed_sgs_poisson_surf
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_fixed_sgs_scalar_surf                          &
     &         (node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,         &
     &          n_int, ibc_end, num_index_ibc, ele_bc_id,               &
     &          ibc_stack_smp, ibc_shape, i_filter, i_field,            &
     &          ak_diff, ak_d, coef_implicit, fem_wk, f_l)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: n_int, i_filter
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
      real(kind = kreal), intent(in) :: ak_diff(ele%numele)
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
     &      nod_fld%ntot_phys, i_field, nod_fld%d_fld, fem_wk%scalar_1)
!
!   'sf' = - \tilde{v}_{i,i} N(x)
!    skv = frac{ \partial \tilde{Phi}_{i}^{n-1} }{ \partial x_{i} }
!
!
        call fem_skv_diffuse_sgs_fix_bc(ele, jac_3d, FEM_elens,         &
     &      num_index_ibc, ele_bc_id, ibc_stack_smp(istart_smp),        &
     &      k2, ione, n_int, i_filter, ak_diff, ak_d, fem_wk%scalar_1,  &
     &      fem_wk%sk6)
      end do
!
      call add1_skv_coef_to_ff_v_smp                                    &
     &   (node, ele, rhs_tbl, coef_implicit, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_vol_fixed_sgs_scalar_surf
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_fixed_sgs_vector_surf                          &
     &         (node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,         &
     &          n_int, nmax_index_ibc, ibc_end, num_index_ibc,          &
     &          ele_bc_id, ibc_stack_smp, ibc_shape, i_filter,          &
     &          i_field,  ncomp_diff, iak_diff, ak_diff, ak_d,          &
     &          coef_implicit, fem_wk, f_l)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: n_int, i_filter
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
      integer(kind=kint), intent(in) :: ncomp_diff, iak_diff
      real (kind = kreal), intent(in) :: ak_diff(ele%numele,ncomp_diff)
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
            call fem_skv_diffuse_sgs_fix_bc                             &
     &         (ele, jac_3d, FEM_elens, nmax_index_ibc,                 &
     &          ele_bc_id(1,nd), ibc_stack_smp(istart_smp,nd), k2, nd,  &
     &          n_int, i_filter, ak_diff(1,iak_diff), ak_d,             &
     &          fem_wk%scalar_1, fem_wk%sk6)
          end do
        end if
      end do
!
      call add3_skv_coef_to_ff_v_smp                                    &
     &   (node, ele, rhs_tbl, coef_implicit, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_vol_fixed_sgs_vector_surf
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_fixed_rotate_sgs_surf                          &
     &         (node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,         &
     &          n_int, ibc_end, num_index_ibc, ele_bc_id,               &
     &          ibc_stack_smp, ibc_shape, i_filter, i_field,            &
     &          ak_diff, ak_d, coef_implicit, fem_wk, f_l)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: n_int, i_filter
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
      real(kind = kreal), intent(in) :: ak_diff(ele%numele)
      integer(kind=kint), intent(in) :: ncomp_diff, iak_diff
      real (kind = kreal), intent(in) :: ak_diff(ele%numele,ncomp_diff)
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
            call fem_skv_diffuse_sgs_fix_bc                             &
     &         (ele, jac_3d, FEM_elens, num_index_ibc,                  &
     &          ele_bc_id, ibc_stack_smp(istart_smp), k2, nd,           &
     &          n_int, i_filter, ak_diff(1,iak_diff), ak_d,             &
     &          fem_wk%scalar_1, fem_wk%sk6)
          end do
      end do
!
      call add3_skv_coef_to_ff_v_smp(node, ele, rhs_tbl,                &
     &    coef_implicit, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_vol_fixed_rotate_sgs_surf
!
!-----------------------------------------------------------------------
!
      end module int_vol_fixed_fld_sgs_ele
