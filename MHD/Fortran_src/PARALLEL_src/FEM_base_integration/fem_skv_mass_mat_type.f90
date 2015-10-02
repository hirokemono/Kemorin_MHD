!fem_skv_mass_mat_type.f90
!      module fem_skv_mass_mat_type
!
!   Lumped mass matrix for each area
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     Modified by H. Matsui on March, 2006
!     Modified by H. Matsui on March, 2009
!
!      subroutine fem_skv_mass_matrix_type(iele_fsmp_stack,             &
!     &          num_int, k2, ele, jac_3d, sk_v)
!        type(element_data), intent(in) :: ele
!        type(jacobians_3d), intent(in) :: jac_3d
!        integer (kind=kint), intent(in) :: num_int, k2
!        integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!        real (kind=kreal), intent(inout)                               &
!     &               :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!      subroutine fem_skv_mass_matrix_diag_type(iele_fsmp_stack,        &
!     &          num_int, ele, jac_3d, sk_v)
!        type(element_data), intent(in) :: ele
!        type(jacobians_3d), intent(in) :: jac_3d
!        integer (kind=kint), intent(in) :: num_int
!        integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!        real (kind=kreal), intent(inout)                               &
!     &               :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!      subroutine fem_skv_mass_mat_diag_HRZ_type(iele_fsmp_stack,       &
!     &          num_int, ele, jac_3d, sk_v)
!        type(element_data), intent(in) :: ele
!        type(jacobians_3d), intent(in) :: jac_3d
!        integer (kind=kint), intent(in) :: num_int
!        integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!        real (kind=kreal), intent(inout)                               &
!     &               :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!      subroutine sum_skv_diagonal_4_HRZ_type(iele_fsmp_stack, ele,     &
!     &          sk_v, ml_e)
!        type(element_data), intent(in) :: ele
!        type(work_finite_element_mat), intent(in) :: sk_v
!        integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!        real (kind=kreal), intent(inout) :: ml_e(ele%numele)
!
!      subroutine vol_average_skv_HRZ_type(iele_fsmp_stack, ele,        &
!     &          sk_v, ml_e)
!        type(element_data), intent(in) :: ele
!        real (kind=kreal), intent(in)  :: ml_e(ele%numele)
!        integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!        real (kind=kreal), intent(inout)                               &
!     &               :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
!      subroutine fem_grp_skv_mass_matrix_t(iele_fsmp_stack,            &
!     &          nele_grp, iele_grp, num_int, k2, ele, jac_3d, sk_v)
!      subroutine fem_grp_skv_mass_matrix_diag_t(iele_fsmp_stack,       &
!     &          nele_grp, iele_grp, num_int, ele, jac_3d, sk_v)
!      subroutine fem_grp_skv_mass_mat_diag_HRZ_t(iele_fsmp_stack,      &
!     &          nele_grp, iele_grp, num_int, ele, jac_3d, sk_v)
!
!      subroutine grp_volume_average_skv_HRZ_t(iele_fsmp_stack,         &
!     &          nele_grp, iele_grp, ele, sk_v, ml_e)
!
      module fem_skv_mass_mat_type
!
      use m_precision
      use m_constants
!
      use m_phys_constants
      use m_machine_parameter
      use m_fem_gauss_int_coefs
!
      use t_geometry_data
      use t_jacobians
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_mass_matrix_type(iele_fsmp_stack,              &
     &          num_int, k2, ele, jac_3d, sk_v)
!
      use fem_skv_mass_mat
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: k2
      integer (kind=kint), intent(in) :: num_int
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_mass_matrix                                          &
     &    (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, np_smp,          &
     &     iele_fsmp_stack, jac_3d%ntot_int, num_int,                   &
     &     jac_3d%xjac, jac_3d%an, jac_3d%an, k2, sk_v)
!
      end  subroutine fem_skv_mass_matrix_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_mass_matrix_diag_type(iele_fsmp_stack,         &
     &          num_int, ele, jac_3d, sk_v)
!
      use fem_skv_mass_mat
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: num_int
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_mass_matrix_diag(ele%numele, ele%nnod_4_ele,         &
     &    np_smp, iele_fsmp_stack, jac_3d%ntot_int, num_int,            &
     &    jac_3d%xjac, jac_3d%an, sk_v)
!
      end  subroutine fem_skv_mass_matrix_diag_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_mass_mat_diag_HRZ_type(iele_fsmp_stack,        &
     &          num_int, ele, jac_3d, sk_v)
!
      use fem_skv_mass_mat
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: num_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_mass_mat_diag_HRZ(ele%numele, ele%nnod_4_ele,        &
     &    np_smp, iele_fsmp_stack, jac_3d%ntot_int, num_int,            &
     &    jac_3d%an, jac_3d%xjac, sk_v)
!
      end  subroutine fem_skv_mass_mat_diag_HRZ_type
!
!-----------------------------------------------------------------------
!
      subroutine sum_skv_diagonal_4_HRZ_type(iele_fsmp_stack, ele,      &
     &          sk_v, ml_e)
!
      use fem_skv_mass_mat
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in)                                     &
     &               :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
      real (kind=kreal), intent(inout) :: ml_e(ele%numele)
!
!
      call sum_skv_diagonal_4_HRZ(ele%numele, ele%nnod_4_ele, np_smp,   &
     &    iele_fsmp_stack, sk_v, ml_e)
!
      end subroutine sum_skv_diagonal_4_HRZ_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine vol_average_skv_HRZ_type(iele_fsmp_stack, ele,         &
     &          sk_v, ml_e)
!
      use fem_skv_mass_mat
!
      type(element_data), intent(in) :: ele
      real (kind=kreal), intent(in)  :: ml_e(ele%numele)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call volume_average_skv_HRZ(ele%numele, ele%nnod_4_ele, np_smp,   &
     &    iele_fsmp_stack, ele%volume_ele, sk_v, ml_e)
!
      end subroutine vol_average_skv_HRZ_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_grp_skv_mass_matrix_t(iele_fsmp_stack,             &
     &          nele_grp, iele_grp, num_int, k2, ele, jac_3d, sk_v)
!
      use fem_grp_skv_mass_mat
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: k2
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: num_int
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_grp_skv_mass_matrix(ele%numele, ele%nnod_4_ele,          &
     &    ele%nnod_4_ele, np_smp, iele_fsmp_stack, nele_grp, iele_grp,  &
     &    jac_3d%ntot_int, num_int, jac_3d%xjac, jac_3d%an, jac_3d%an,  &
     &    k2, sk_v)
!
      end  subroutine fem_grp_skv_mass_matrix_t
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_grp_skv_mass_matrix_diag_t(iele_fsmp_stack,        &
     &          nele_grp, iele_grp, num_int, ele, jac_3d, sk_v)
!
      use fem_grp_skv_mass_mat
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: num_int
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_grp_skv_mass_matrix_diag(ele%numele, ele%nnod_4_ele,     &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp,                  &
     &    jac_3d%ntot_int, num_int, jac_3d%xjac, jac_3d%an, sk_v)
!
      end  subroutine fem_grp_skv_mass_matrix_diag_t
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_grp_skv_mass_mat_diag_HRZ_t(iele_fsmp_stack,       &
     &          nele_grp, iele_grp, num_int, ele, jac_3d, sk_v)
!
      use fem_grp_skv_mass_mat
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: num_int
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_grp_skv_mass_mat_diag_HRZ(ele%numele, ele%nnod_4_ele,    &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp,                  &
     &    jac_3d%ntot_int, num_int, jac_3d%xjac, jac_3d%an, sk_v)
!
      end  subroutine fem_grp_skv_mass_mat_diag_HRZ_t
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine grp_volume_average_skv_HRZ_t(iele_fsmp_stack,          &
     &          nele_grp, iele_grp, ele, sk_v, ml_e)
!
      use fem_grp_skv_mass_mat
!
      type(element_data), intent(in) :: ele
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in)  :: ml_e(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call grp_volume_average_skv_HRZ(ele%numele, ele%nnod_4_ele,       &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp, ele%volume_ele,  &
     &    sk_v, ml_e)
!
      end subroutine grp_volume_average_skv_HRZ_t
!
!-----------------------------------------------------------------------
!
      end module  fem_skv_mass_mat_type
