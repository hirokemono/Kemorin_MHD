!int_grouped_mass_matrix.f90
!      module int_grouped_mass_matrix
!
!   Lumped mass matrix for each area
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     Modified by H. Matsui on Oct. 2005
!
!      subroutine int_grp_consist_mass_matrix(iele_fsmp_stack,          &
!     &          nele_grp, iele_grp, n_int, nmat_size, aiccg)
!
!      subroutine int_grp_mass_matrix(iele_fsmp_stack,                  &
!     &          nele_grp, iele_grp, n_int)
!      subroutine int_grp_mass_matrix_diag(iele_fsmp_stack,             &
!     &          nele_grp, iele_grp, n_int)
!      subroutine int_grp_mass_matrix_HRZ_full(iele_fsmp_stack,         &
!     &          nele_grp, iele_grp, n_int)
!      subroutine int_grp_mass_matrix_HRZ(iele_fsmp_stack,              &
!     &          nele_grp, iele_grp, n_int)
!
      module int_grouped_mass_matrix
!
      use m_precision
!
      use m_geometry_data
      use m_sorted_node
      use m_jacobians
      use m_phys_constants
      use m_finite_element_matrix
!
      use fem_skv_mass_mat_type
      use cal_skv_to_ff_smp_1st
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_grp_consist_mass_matrix(iele_fsmp_stack,           &
     &          nele_grp, iele_grp, n_int, nmat_size, aiccg)
!
      use add_skv1_2_matrix_1st
!
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind = kint), intent(in) :: nmat_size
      real(kind=kreal), intent(inout) :: aiccg(0:nmat_size)
!
      integer (kind = kint) :: k2
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele1%nnod_4_ele
        call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
        call fem_grp_skv_mass_matrix_t(iele_fsmp_stack,                 &
     &      nele_grp, iele_grp, n_int, k2, ele1, jac1_3d_q,             &
     &      fem1_wk%sk6)
        call add_skv1_2_matrix11_1st(k2, fem1_wk%sk6, nmat_size, aiccg)
      end do
!
      end subroutine int_grp_consist_mass_matrix
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_grp_mass_matrix(iele_fsmp_stack,                   &
     &          nele_grp, iele_grp, n_int)
!
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind = kint) :: k2
!
!  ----------  clear the vector and lumped mass matrix
!
      call reset_ff_smp
      call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele1%nnod_4_ele
       call fem_grp_skv_mass_matrix_t(iele_fsmp_stack,                  &
     &     nele_grp, iele_grp, n_int, k2, ele1, jac1_3d_q, fem1_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_smp)
!
      end subroutine int_grp_mass_matrix
!
!-----------------------------------------------------------------------
!
      subroutine int_grp_mass_matrix_diag(iele_fsmp_stack,              &
     &          nele_grp, iele_grp, n_int)
!
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
!
      call reset_ff_smp
      call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
      call fem_grp_skv_mass_matrix_diag_t(iele_fsmp_stack,              &
     &    nele_grp, iele_grp, n_int, ele1, jac1_3d_q, fem1_wk%sk6)
!
      call add1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_smp)
!
      end subroutine int_grp_mass_matrix_diag
!
!-----------------------------------------------------------------------
!
      subroutine int_grp_mass_matrix_HRZ_full(iele_fsmp_stack,          &
     &          nele_grp, iele_grp, n_int)
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: n_int
!
!
      call reset_ff_smp
      call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
      call fem_grp_skv_mass_mat_diag_HRZ_t(iele_fsmp_stack,             &
     &    nele_grp, iele_grp, n_int, ele1, jac1_3d_q, fem1_wk%sk6)
      call sum_skv_diagonal_4_HRZ_type(iele_fsmp_stack, ele1,           &
     &    fem1_wk%sk6, fem1_wk%me_diag)
!
      call grp_volume_average_skv_HRZ_t(iele_fsmp_stack,                &
     &    nele_grp, iele_grp, ele1, fem1_wk%sk6, fem1_wk%me_diag)
!
      call add1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_smp)
!
      end subroutine int_grp_mass_matrix_HRZ_full
!
!-----------------------------------------------------------------------
!
      subroutine int_grp_mass_matrix_HRZ(iele_fsmp_stack,               &
     &          nele_grp, iele_grp, n_int)
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: n_int
!
!
      call reset_ff_smp
      call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
      call fem_grp_skv_mass_mat_diag_HRZ_t(iele_fsmp_stack,             &
     &    nele_grp, iele_grp, n_int, ele1, jac1_3d_q, fem1_wk%sk6)
      call grp_volume_average_skv_HRZ_t(iele_fsmp_stack,                &
     &    nele_grp, iele_grp, ele1, fem1_wk%sk6, fem1_wk%me_diag)
!
      call add1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_smp)
!
      end subroutine int_grp_mass_matrix_HRZ
!
!-----------------------------------------------------------------------
!
      end module int_grouped_mass_matrix
