!fem_skv_poisson_sgs_bc_1.f90
!     module fem_skv_poisson_sgs_bc_1
!
!        programmed by H.Matsui on July, 2005
!        modified by H.Matsui on AUg., 2007
!
!      subroutine fem_skv_poisson_sgs_fix_1st                           &
!     &        (num_index_ibc, ele_bc_id, ibc_stack_smp, k2, n_int,     &
!     &         i_filter, ak_diff, phi_e, sk_v)
!
!      subroutine fem_skv_diffuse_sgs_fix_1st                           &
!     &        (num_index_ibc, ele_bc_id, ibc_stack_smp, k2, nd, n_int, &
!     &         i_filter, ak_diff, ak_d, phi_e, sk_v)
!
      module fem_skv_poisson_sgs_bc_1
!
      use m_precision
      use m_constants
      use m_phys_constants
!
      use m_machine_parameter
      use m_geometry_constants
      use m_geometry_parameter
!
      use m_jacobians
      use m_filter_elength
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_poisson_sgs_fix_1st                            &
     &        (num_index_ibc, ele_bc_id, ibc_stack_smp, k2, n_int,      &
     &         i_filter, ak_diff, phi_e, sk_v)
!
      use fem_skv_poisson_sgs_bc
!
      integer(kind=kint), intent(in) :: num_index_ibc
      integer(kind=kint), intent(in) :: ele_bc_id(num_index_ibc)
      integer(kind=kint), intent(in) :: ibc_stack_smp(0:np_smp)
      integer(kind=kint), intent(in) :: i_filter
      integer(kind=kint), intent(in) :: n_int, k2
!
      real (kind=kreal), intent(in) :: ak_diff(numele)
      real (kind=kreal), intent(in) :: phi_e(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,num_t_linear)
!
!
      call fem_skv_poisson_sgs_fixed(numele, num_t_linear,              &
     &    num_t_linear, np_smp, num_index_ibc, ele_bc_id,               &
     &    ibc_stack_smp, k2, n_int, ntot_int_3d, xjac, dnx, dnx,        &
     &    xmom_1d_org(i_filter,2), nele_filter_mom,                     &
     &    elen_dx2_ele_dx2,  elen_dy2_ele_dx2,  elen_dz2_ele_dx2,       &
     &    elen_dxdy_ele_dx2, elen_dydz_ele_dx2, elen_dzdx_ele_dx2,      &
     &    ak_diff, phi_e, sk_v)
!
      end subroutine fem_skv_poisson_sgs_fix_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_diffuse_sgs_fix_1st                            &
     &        (num_index_ibc, ele_bc_id, ibc_stack_smp, k2, nd, n_int,  &
     &         i_filter, ak_diff, ak_d, phi_e, sk_v)
!
      use fem_skv_diffuse_sgs_bc
!
      integer(kind=kint), intent(in) :: num_index_ibc
      integer(kind=kint), intent(in) :: ele_bc_id(num_index_ibc)
      integer(kind=kint), intent(in) :: ibc_stack_smp(0:np_smp)
      integer(kind=kint), intent(in) :: i_filter
      integer(kind=kint), intent(in) :: n_int, k2, nd
!
      real (kind=kreal), intent(in) :: phi_e(numele)
      real (kind=kreal), intent(in) :: ak_diff(numele)
      real (kind=kreal), intent(in) :: ak_d(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_diffuse_sgs_fixed(numele, nnod_4_ele, nnod_4_ele,    &
     &    np_smp, num_index_ibc, ele_bc_id, ibc_stack_smp,              &
     &    k2, nd, n_int, ntot_int_3d, xjac, dwx, dwx,                   &
     &    xmom_1d_org(i_filter,2), nele_filter_mom,                     &
     &    elen_dx2_ele_dx2,  elen_dy2_ele_dx2,  elen_dz2_ele_dx2,       &
     &    elen_dxdy_ele_dx2, elen_dydz_ele_dx2, elen_dzdx_ele_dx2,      &
     &    ak_diff, ak_d, phi_e, sk_v)
!
      end subroutine fem_skv_diffuse_sgs_fix_1st
!
!-----------------------------------------------------------------------
!
      end module fem_skv_poisson_sgs_bc_1
