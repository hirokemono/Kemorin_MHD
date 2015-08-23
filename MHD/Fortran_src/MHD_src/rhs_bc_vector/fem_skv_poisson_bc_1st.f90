!fem_skv_poisson_bc_1st.f90
!     module fem_skv_poisson_bc_1st
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on Aug., 2007
!
!      subroutine fem_skv_poisson_bc_1(num_index_ibc, ele_bc_id,        &
!     &          ibc_stack_smp, k2, n_int, phi_e, sk_v)
!
!      subroutine fem_skv_scalar_diffuse_bc_1(num_index_ibc, ele_bc_id, &
!     &          ibc_stack_smp, k2, n_int, ak_d, phi_e, sk_v)
!      subroutine fem_skv_vector_diffuse_bc_1(num_index_ibc, ele_bc_id, &
!     &          ibc_stack_smp, k2, nd, n_int, ak_d, phi_e, sk_v)
!
      module fem_skv_poisson_bc_1st
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use m_geometry_data
      use m_jacobians
      use m_phys_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_poisson_bc_1(num_index_ibc, ele_bc_id,         &
     &          ibc_stack_smp, k2, n_int, phi_e, sk_v)
!
      use fem_skv_poisson_bc
!
      integer (kind=kint), intent(in) :: num_index_ibc
      integer (kind=kint), intent(in) :: ele_bc_id(num_index_ibc)
      integer (kind=kint), intent(in) :: ibc_stack_smp(0:np_smp)
!
      integer (kind=kint), intent(in) :: n_int, k2
      real (kind=kreal), intent(in) :: phi_e(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_poisson_fixed                                        &
     &   (ele1%numele, num_t_linear, num_t_linear,                      &
     &    np_smp, num_index_ibc, ele_bc_id, ibc_stack_smp, k2, n_int,   &
     &    jac1_3d_l%ntot_int, xjac, dnx, dnx,                           &
     &    phi_e, sk_v)
!
      end subroutine fem_skv_poisson_bc_1
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_diffuse_bc_1(num_index_ibc, ele_bc_id,  &
     &          ibc_stack_smp, k2, n_int, ak_d, phi_e, sk_v)
!
      use fem_skv_poisson_bc
!
      integer (kind=kint), intent(in) :: num_index_ibc
      integer (kind=kint), intent(in) :: ele_bc_id(num_index_ibc)
      integer (kind=kint), intent(in) :: ibc_stack_smp(0:np_smp)
!
      integer (kind=kint), intent(in) :: n_int, k2
      real (kind=kreal), intent(in) :: phi_e(ele1%numele)
      real (kind=kreal), intent(in) :: ak_d(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_scalar_diffuse_fixed                                 &
     &    (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele, np_smp,       &
     &     num_index_ibc, ele_bc_id, ibc_stack_smp, k2, ione, n_int,    &
     &     jac1_3d_q%ntot_int, jac1_3d_q%xjac, dwx, dwx,                &
     &     ak_d, phi_e, sk_v)
!
      end subroutine fem_skv_scalar_diffuse_bc_1
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_diffuse_bc_1(num_index_ibc, ele_bc_id,  &
     &          ibc_stack_smp, k2, nd, n_int, ak_d, phi_e, sk_v)
!
      use fem_skv_poisson_bc
!
      integer (kind=kint), intent(in) :: num_index_ibc
      integer (kind=kint), intent(in) :: ele_bc_id(num_index_ibc)
      integer (kind=kint), intent(in) :: ibc_stack_smp(0:np_smp)
!
      integer (kind=kint), intent(in) :: n_int, k2, nd
      real (kind=kreal), intent(in) :: phi_e(ele1%numele)
      real (kind=kreal), intent(in) :: ak_d(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_scalar_diffuse_fixed                                 &
     &    (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele, np_smp,       &
     &     num_index_ibc, ele_bc_id, ibc_stack_smp, k2, nd, n_int,      &
     &     jac1_3d_q%ntot_int, jac1_3d_q%xjac, dwx, dwx,                &
     &     ak_d, phi_e, sk_v)
!
      end subroutine fem_skv_vector_diffuse_bc_1
!
!-----------------------------------------------------------------------
!
      end module fem_skv_poisson_bc_1st
