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
      use m_geometry_data
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
      real (kind=kreal), intent(in) :: ak_diff(ele1%numele)
      real (kind=kreal), intent(in) :: phi_e(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(ele1%numele,n_sym_tensor,num_t_linear)
!
!
      call fem_skv_poisson_sgs_fixed(ele1%numele, num_t_linear,         &
     &  num_t_linear, np_smp, num_index_ibc, ele_bc_id,                 &
     &  ibc_stack_smp, k2, n_int, jac1_3d_l%ntot_int, xjac, dnx, dnx,   &
     &  FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                  &
     &  FEM1_elen%nele_filter_mom,                                      &
     &  FEM1_elen%elen_ele%diff2%df_x2, FEM1_elen%elen_ele%diff2%df_y2, &
     &  FEM1_elen%elen_ele%diff2%df_z2, FEM1_elen%elen_ele%diff2%df_xy, &
     &  FEM1_elen%elen_ele%diff2%df_yz, FEM1_elen%elen_ele%diff2%df_zx, &
     &  ak_diff, phi_e, sk_v)
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
      real (kind=kreal), intent(in) :: phi_e(ele1%numele)
      real (kind=kreal), intent(in) :: ak_diff(ele1%numele)
      real (kind=kreal), intent(in) :: ak_d(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_diffuse_sgs_fixed                                    &
     & (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele,                  &
     &  np_smp, num_index_ibc, ele_bc_id, ibc_stack_smp,                &
     &  k2, nd, n_int, jac1_3d_q%ntot_int, xjac, dwx, dwx,              &
     &  FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                  &
     &  FEM1_elen%nele_filter_mom,                                      &
     &  FEM1_elen%elen_ele%diff2%df_x2, FEM1_elen%elen_ele%diff2%df_y2, &
     &  FEM1_elen%elen_ele%diff2%df_z2, FEM1_elen%elen_ele%diff2%df_xy, &
     &  FEM1_elen%elen_ele%diff2%df_yz, FEM1_elen%elen_ele%diff2%df_zx, &
     &  ak_diff, ak_d, phi_e, sk_v)
!
      end subroutine fem_skv_diffuse_sgs_fix_1st
!
!-----------------------------------------------------------------------
!
      end module fem_skv_poisson_sgs_bc_1
