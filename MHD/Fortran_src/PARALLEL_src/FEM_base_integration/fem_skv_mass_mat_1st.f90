!fem_skv_mass_mat_1st.f90
!      module fem_skv_mass_mat_1st
!
!   Lumped mass matrix for each area
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     Modified by H. Matsui on March, 2006
!     Modified by H. Matsui on March, 2009
!
!      subroutine fem_skv_mass_matrix_1st(iele_fsmp_stack,              &
!     &          num_int, k2, sk_v)
!      subroutine fem_skv_mass_matrix_diag_1st(iele_fsmp_stack,         &
!     &          num_int, sk_v)
!
!      subroutine fem_skv_mass_mat_diag_HRZ_1st(iele_fsmp_stack,        &
!     &          num_int, sk_v)
!      subroutine sum_skv_diagonal_4_HRZ_1st(iele_fsmp_stack, sk_v,     &
!     &          sk_e, ml_e)
!      subroutine vol_average_skv_HRZ_1st(iele_fsmp_stack, sk_v, ml_e)
!
!
!      subroutine fem_grp_skv_mass_matrix_1                             &
!     &         (iele_fsmp_stack, nele_grp, iele_grp, num_int, k2, sk_v)
!      subroutine fem_grp_skv_mass_matrix_diag_1                        &
!     &         (iele_fsmp_stack, nele_grp, iele_grp, num_int, sk_v)
!      subroutine fem_grp_skv_mass_mat_diag_HRZ_1                       &
!     &         (iele_fsmp_stack, nele_grp, iele_grp, num_int, sk_v)
!
!      subroutine grp_volume_average_skv_HRZ_1                          &
!     &         (iele_fsmp_stack, nele_grp, iele_grp, sk_v, ml_e)
!
      module fem_skv_mass_mat_1st
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use m_phys_constants
      use m_geometry_data
      use m_jacobians
!
      use m_fem_gauss_int_coefs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_mass_matrix_1st(iele_fsmp_stack,               &
     &          num_int, k2, sk_v)
!
      use fem_skv_mass_mat
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: k2
      integer (kind=kint), intent(in) :: num_int
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_mass_matrix                                          &
     &   (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele,                &
     &    np_smp, iele_fsmp_stack, jac1_3d_q%ntot_int, num_int,         &
     &    jac1_3d_q%xjac, jac1_3d_q%an, jac1_3d_q%an, k2, sk_v)
!
      end  subroutine fem_skv_mass_matrix_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_mass_matrix_diag_1st(iele_fsmp_stack,          &
     &          num_int, sk_v)
!
      use fem_skv_mass_mat
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: num_int
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_mass_matrix_diag(ele1%numele, ele1%nnod_4_ele,       &
     &    np_smp, iele_fsmp_stack, jac1_3d_q%ntot_int,                  &
     &    num_int, jac1_3d_q%xjac, jac1_3d_q%an, sk_v)
!
      end  subroutine fem_skv_mass_matrix_diag_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_mass_mat_diag_HRZ_1st(iele_fsmp_stack,         &
     &          num_int, sk_v)
!
      use fem_skv_mass_mat
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: num_int
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_mass_mat_diag_HRZ(ele1%numele, ele1%nnod_4_ele,      &
     &    np_smp, iele_fsmp_stack, jac1_3d_q%ntot_int,                  &
     &    num_int, jac1_3d_q%an, jac1_3d_q%xjac, sk_v)
!
      end  subroutine fem_skv_mass_mat_diag_HRZ_1st
!
!-----------------------------------------------------------------------
!
      subroutine sum_skv_diagonal_4_HRZ_1st(iele_fsmp_stack, sk_v,      &
     &          sk_e, ml_e)
!
      use fem_skv_mass_mat
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in)                                     &
     &               :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
      real (kind=kreal), intent(inout) :: sk_e(ele1%numele)
      real (kind=kreal), intent(inout) :: ml_e(ele1%numele)
!
!
      call sum_skv_diagonal_4_HRZ(ele1%numele, ele1%nnod_4_ele,         &
     &    np_smp, iele_fsmp_stack, sk_v, sk_e, ml_e)
!
      end subroutine sum_skv_diagonal_4_HRZ_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine vol_average_skv_HRZ_1st(iele_fsmp_stack, sk_v, ml_e)
!
      use m_geometry_data
      use fem_skv_mass_mat
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in)  :: ml_e(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call volume_average_skv_HRZ(ele1%numele, ele1%nnod_4_ele,         &
     &    np_smp, iele_fsmp_stack, ele1%volume_ele, sk_v, ml_e)
!
      end subroutine vol_average_skv_HRZ_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_grp_skv_mass_matrix_1                              &
     &         (iele_fsmp_stack, nele_grp, iele_grp, num_int, k2, sk_v)
!
      use fem_grp_skv_mass_mat
!
      integer (kind=kint), intent(in) :: k2
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: num_int
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_grp_skv_mass_matrix                                      &
     &   (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele,                &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp,                  &
     &    jac1_3d_q%ntot_int, num_int, jac1_3d_q%xjac,                  &
     &    jac1_3d_q%an, jac1_3d_q%an, k2, sk_v)
!
      end  subroutine fem_grp_skv_mass_matrix_1
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_grp_skv_mass_matrix_diag_1                         &
     &         (iele_fsmp_stack, nele_grp, iele_grp, num_int, sk_v)
!
      use fem_grp_skv_mass_mat
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: num_int
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_grp_skv_mass_matrix_diag(ele1%numele, ele1%nnod_4_ele,   &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp,                  &
     &    jac1_3d_q%ntot_int, num_int, jac1_3d_q%xjac, jac1_3d_q%an,    &
     &    sk_v)
!
      end  subroutine fem_grp_skv_mass_matrix_diag_1
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_grp_skv_mass_mat_diag_HRZ_1                        &
     &         (iele_fsmp_stack, nele_grp, iele_grp, num_int, sk_v)
!
      use fem_grp_skv_mass_mat
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: num_int
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_grp_skv_mass_mat_diag_HRZ(ele1%numele, ele1%nnod_4_ele,  &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp,                  &
     &    jac1_3d_q%ntot_int, num_int, jac1_3d_q%xjac, jac1_3d_q%an,    &
     &    sk_v)
!
      end  subroutine fem_grp_skv_mass_mat_diag_HRZ_1
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine grp_volume_average_skv_HRZ_1                           &
     &         (iele_fsmp_stack, nele_grp, iele_grp, sk_v, ml_e)
!
      use fem_grp_skv_mass_mat
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in)  :: ml_e(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call grp_volume_average_skv_HRZ(ele1%numele, ele1%nnod_4_ele,     &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp, ele1%volume_ele, &
     &    sk_v, ml_e)
!
      end subroutine grp_volume_average_skv_HRZ_1
!
!-----------------------------------------------------------------------
!
      end module  fem_skv_mass_mat_1st
