!int_vol_fixed_fld_sgs_ele.f90
!     module int_vol_fixed_fld_sgs_ele
!
!        programmed by H.Matsui on July, 2005
!        modified by H.Matsui on AUg., 2007
!
!      subroutine int_vol_fixed_sgs_poisson_surf                        &
!     &         (n_int, ibc_end, num_index_ibc, ele_bc_id,              &
!     &          ibc_stack_smp, ibc_shape, i_field, ak_diff)
!
!      subroutine int_vol_fixed_sgs_scalar_surf                         &
!     &         (n_int, ibc_end, num_index_ibc, ele_bc_id,              &
!     &          ibc_stack_smp, ibc_shape, i_filter, i_field,           &
!     &          ak_diff, ak_d, coef_implicit)
!      subroutine int_vol_fixed_sgs_vector_surf                         &
!     &         (n_int, nmax_index_ibc, ibc_end, num_index_ibc,         &
!     &          ele_bc_id, ibc_stack_smp, ibc_shape, i_filter,         &
!     &          i_field, ak_diff, ak_d, coef_implicit)
!
!      subroutine int_vol_fixed_rotate_sgs_surf                         &
!     &         (n_int, ibc_end, num_index_ibc, ele_bc_id,              &
!     &          ibc_stack_smp, ibc_shape, i_filter, i_field,           &
!     &          ak_diff, ak_d, coef_implicit)
!
      module int_vol_fixed_fld_sgs_ele
!
      use m_precision
      use m_machine_parameter
!
      use m_geometry_data
      use m_phys_constants
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_skv_to_ff_smp_1st
      use fem_skv_poisson_sgs_bc_1
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
     &         (n_int, ibc_end, num_index_ibc, ele_bc_id,               &
     &          ibc_stack_smp, ibc_shape, i_filter, i_field, ak_diff)
!
      integer(kind=kint), intent(in) :: n_int, i_filter
      integer(kind=kint), intent(in) :: ibc_end, num_index_ibc
      integer(kind=kint), intent(in) :: ele_bc_id(num_index_ibc)
      integer(kind=kint), intent(in)                                    &
     &                    :: ibc_stack_smp(0:ele1%nnod_4_ele*np_smp)
      integer(kind=kint), intent(in) :: ibc_shape(ele1%nnod_4_ele)
!
      integer(kind=kint), intent(in) :: i_field
!
      real(kind = kreal), intent(in) :: ak_diff(ele1%numele)
!
      integer(kind = kint) :: istart_smp, kk, k2
!
!
      if (num_index_ibc .eq. 0) return
      call reset_sk6(n_scalar)
!
      do kk=1, ibc_end
        k2 = ibc_shape(kk)
!
        istart_smp = (kk-1)*np_smp
!
        call scalar_2_element_4_boundary                                &
     &     (ele1%numele, ele1%nnod_4_ele, ele1%ie, num_index_ibc,       &
     &      ele_bc_id, ibc_stack_smp(istart_smp), k2, i_field, phi_e)
!
!   'sf' = - \tilde{v}_{i,i} N(x)
!    skv = frac{ \partial \tilde{Phi}_{i}^{n-1} }{ \partial x_{i} }
!
!
        call fem_skv_poisson_sgs_fix_1st(num_index_ibc,                 &
     &      ele_bc_id, ibc_stack_smp(istart_smp), k2,                   &
     &      n_int, i_filter, ak_diff, phi_e, sk6)
      end do
!
      call add1_skv_to_ff_v_smp_1st(ff_smp, sk6)
!
      end subroutine int_vol_fixed_sgs_poisson_surf
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_fixed_sgs_scalar_surf                          &
     &         (n_int, ibc_end, num_index_ibc, ele_bc_id,               &
     &          ibc_stack_smp, ibc_shape, i_filter, i_field,            &
     &          ak_diff, ak_d, coef_implicit)
!
      integer(kind=kint), intent(in) :: n_int, i_filter
      integer(kind=kint), intent(in) :: ibc_end, num_index_ibc
      integer(kind=kint), intent(in) :: ele_bc_id(num_index_ibc)
      integer(kind=kint), intent(in)                                    &
     &                    :: ibc_stack_smp(0:ele1%nnod_4_ele*np_smp)
      integer(kind=kint), intent(in) :: ibc_shape(ele1%nnod_4_ele)
!
      integer(kind=kint), intent(in) :: i_field
!
      real(kind = kreal), intent(in) :: coef_implicit
      real(kind = kreal), intent(in) :: ak_d(ele1%numele)
      real(kind = kreal), intent(in) :: ak_diff(ele1%numele)
!
      integer(kind = kint) :: istart_smp, kk, k2
!
!
      if (num_index_ibc .eq. 0) return
      call reset_sk6(n_scalar)
!
      do kk=1, ibc_end
        k2 = ibc_shape(kk)
!
        istart_smp = (kk-1)*np_smp
!
        call scalar_2_element_4_boundary                                &
     &     (ele1%numele, ele1%nnod_4_ele, ele1%ie, num_index_ibc,       &
     &      ele_bc_id, ibc_stack_smp(istart_smp), k2, i_field, phi_e)
!
!   'sf' = - \tilde{v}_{i,i} N(x)
!    skv = frac{ \partial \tilde{Phi}_{i}^{n-1} }{ \partial x_{i} }
!
!
        call fem_skv_diffuse_sgs_fix_1st(num_index_ibc,                 &
     &      ele_bc_id, ibc_stack_smp(istart_smp), k2, ione,             &
     &      n_int, i_filter, ak_diff, ak_d, phi_e, sk6)
      end do
!
      call add1_skv_coef_to_ff_v_smp_1st(coef_implicit, ff_smp, sk6)
!
      end subroutine int_vol_fixed_sgs_scalar_surf
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_fixed_sgs_vector_surf                          &
     &         (n_int, nmax_index_ibc, ibc_end, num_index_ibc,          &
     &          ele_bc_id, ibc_stack_smp, ibc_shape, i_filter,          &
     &          i_field, ak_diff, ak_d, coef_implicit)
!
      integer(kind=kint), intent(in) :: n_int, i_filter
      integer(kind=kint), intent(in) :: nmax_index_ibc
      integer(kind=kint), intent(in) :: ibc_end(3), num_index_ibc(3)
      integer(kind=kint), intent(in) :: ele_bc_id(nmax_index_ibc,3)
      integer(kind=kint), intent(in)                                    &
     &                    :: ibc_stack_smp(0:ele1%nnod_4_ele*np_smp,3)
      integer(kind=kint), intent(in) :: ibc_shape(ele1%nnod_4_ele)
!
      integer(kind=kint), intent(in) :: i_field
!
      real(kind = kreal), intent(in) :: coef_implicit
      real(kind = kreal), intent(in) :: ak_d(ele1%numele)
      real(kind = kreal), intent(in) :: ak_diff(ele1%numele)
!
      integer(kind = kint) :: istart_smp, kk, k2, nd, i_comp
!
!
      if (nmax_index_ibc .eq. 0) return
      call reset_sk6(n_vector)
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
            call scalar_2_element_4_boundary                            &
     &         (ele1%numele, ele1%nnod_4_ele, ele1%ie, nmax_index_ibc,  &
     &          ele_bc_id(1,nd), ibc_stack_smp(istart_smp,nd),          &
     &          k2, i_comp, phi_e)
!
!   'sf' = - \tilde{v}_{i,i} N(x)
!    skv = frac{ \partial \tilde{Phi}_{i}^{n-1} }{ \partial x_{i} }
!
            call fem_skv_diffuse_sgs_fix_1st(nmax_index_ibc,            &
     &          ele_bc_id(1,nd), ibc_stack_smp(istart_smp,nd), k2, nd,  &
     &          n_int, i_filter, ak_diff, ak_d, phi_e, sk6)
          end do
        end if
      end do
!
      call add3_skv_coef_to_ff_v_smp_1st(coef_implicit, ff_smp, sk6)
!
      end subroutine int_vol_fixed_sgs_vector_surf
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_fixed_rotate_sgs_surf                          &
     &         (n_int, ibc_end, num_index_ibc, ele_bc_id,               &
     &          ibc_stack_smp, ibc_shape, i_filter, i_field,            &
     &          ak_diff, ak_d, coef_implicit)
!
      integer(kind=kint), intent(in) :: n_int, i_filter
      integer(kind=kint), intent(in) :: ibc_end, num_index_ibc
      integer(kind=kint), intent(in) :: ele_bc_id(num_index_ibc)
      integer(kind=kint), intent(in)                                    &
     &                    :: ibc_stack_smp(0:ele1%nnod_4_ele*np_smp)
      integer(kind=kint), intent(in) :: ibc_shape(ele1%nnod_4_ele)
!
      integer(kind=kint), intent(in) :: i_field
!
      real(kind = kreal), intent(in) :: coef_implicit
      real(kind = kreal), intent(in) :: ak_d(ele1%numele)
      real(kind = kreal), intent(in) :: ak_diff(ele1%numele)
!
      integer(kind = kint) :: istart_smp, kk, k2, nd, i_comp
!
!
      if (num_index_ibc .eq. 0) return
      call reset_sk6(n_vector)
!
      do nd = 1, n_vector
          i_comp = i_field + nd - 1
!
          do kk=1, ibc_end
            k2 = ibc_shape(kk)
!
            istart_smp = (kk-1)*np_smp
!
            call scalar_2_element_4_boundary                            &
     &         (ele1%numele, ele1%nnod_4_ele, ele1%ie, num_index_ibc,   &
     &          ele_bc_id, ibc_stack_smp(istart_smp),                   &
     &          k2, i_comp, phi_e)
!
!   'sf' = - \tilde{v}_{i,i} N(x)
!    skv = frac{ \partial \tilde{Phi}_{i}^{n-1} }{ \partial x_{i} }
!
            call fem_skv_diffuse_sgs_fix_1st(num_index_ibc,             &
     &          ele_bc_id, ibc_stack_smp(istart_smp), k2, nd,           &
     &          n_int, i_filter, ak_diff,  ak_d, phi_e, sk6)
          end do
      end do
!
      call add3_skv_coef_to_ff_v_smp_1st(coef_implicit, ff_smp, sk6)
!
      end subroutine int_vol_fixed_rotate_sgs_surf
!
!-----------------------------------------------------------------------
!
      end module int_vol_fixed_fld_sgs_ele
