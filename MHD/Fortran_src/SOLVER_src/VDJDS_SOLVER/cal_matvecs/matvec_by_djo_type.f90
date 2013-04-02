!matvec_by_djo_type.f90
!     module matvec_by_djo_type
!
!      Written by H. Matsui on Apr., 2012
!
!      subroutine matvec_djo_11_type(djo_tbl, mat11, NP, B, X, np_smp)
!      subroutine matvec_djo_13_type(djo_tbl, mat11, NP, B, X, np_smp)
!      subroutine matvec_djo_16_type(djo_tbl, mat11, NP, B, X, np_smp)
!      subroutine matvec_djo_1N_type(djo_tbl, mat11, NB, NP, B, X,      &
!     &          np_smp)
!
!      subroutine matvec_djo_33_type(djo_tbl, mat33, NP, B, X, np_smp)
!      subroutine matvec_djo_NN_type(djo_tbl, matNN, NB, NP, B, X,      &
!     &          np_smp)
!
      module matvec_by_djo_type
!
      use m_precision
!
      use t_solver_ordered_crs
      use matvec_by_djo
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine matvec_djo_11_type(djo_tbl, mat11, NP, B, X, np_smp)
!
      type(DJORS_CONNECT), intent(in) :: djo_tbl
      type(DJORS_MATRIX), intent(in) :: mat11
!
      integer(kind=kint), intent(in) :: NP, np_smp
      real(kind = kreal), intent(in) :: B(NP)
!
      real(kind = kreal), intent(inout) :: X(djo_tbl%NC)
!
!
      call matvec_djo_11(NP, djo_tbl%NC, djo_tbl%NCM, djo_tbl%INM,      &
     &    djo_tbl%IAM,  mat11%AM, B, X, djo_tbl%NUM_NCOMP,              &
     &    djo_tbl%INOD_DJO, djo_tbl%NUM_SUM, np_smp,                    &
     &    djo_tbl%IEND_SUM_smp)
!
      end subroutine matvec_djo_11_type
!
!-----------------------------------------------------------------------
!
      subroutine matvec_djo_13_type(djo_tbl, mat11, NP, B, X, np_smp)
!
      type(DJORS_CONNECT), intent(in) :: djo_tbl
      type(DJORS_MATRIX), intent(in) :: mat11
!
      integer(kind=kint), intent(in) :: NP, np_smp
      real(kind = kreal), intent(in) :: B(3*NP)
!
      real(kind = kreal), intent(inout) :: X(3*djo_tbl%NC)
!
!
      call matvec_djo_13(NP, djo_tbl%NC, djo_tbl%NCM, djo_tbl%INM,      &
     &    djo_tbl%IAM,  mat11%AM, B, X, djo_tbl%NUM_NCOMP,              &
     &    djo_tbl%INOD_DJO, djo_tbl%NUM_SUM, np_smp,                    &
     &    djo_tbl%IEND_SUM_smp)
!
      end subroutine matvec_djo_13_type
!
!-----------------------------------------------------------------------
!
      subroutine matvec_djo_16_type(djo_tbl, mat11, NP, B, X, np_smp)
!
      type(DJORS_CONNECT), intent(in) :: djo_tbl
      type(DJORS_MATRIX), intent(in) :: mat11
!
      integer(kind=kint), intent(in) :: NP, np_smp
      real(kind = kreal), intent(in) :: B(6*NP)
!
      real(kind = kreal), intent(inout) :: X(6*djo_tbl%NC)
!
!
      call matvec_djo_16(NP, djo_tbl%NC, djo_tbl%NCM, djo_tbl%INM,      &
     &    djo_tbl%IAM,  mat11%AM, B, X, djo_tbl%NUM_NCOMP,              &
     &    djo_tbl%INOD_DJO, djo_tbl%NUM_SUM, np_smp,                    &
     &    djo_tbl%IEND_SUM_smp)
!
      end subroutine matvec_djo_16_type
!
!-----------------------------------------------------------------------
!
      subroutine matvec_djo_1N_type(djo_tbl, mat11, NB, NP, B, X,       &
     &          np_smp)
!
      type(DJORS_CONNECT), intent(in) :: djo_tbl
      type(DJORS_MATRIX), intent(in) :: mat11
!
      integer(kind=kint), intent(in) :: NB, NP, np_smp
      real(kind = kreal), intent(in) :: B(NB*NP)
!
      real(kind = kreal), intent(inout) :: X(NB*djo_tbl%NC)
!
!
      call matvec_djo_1N(NB, NP, djo_tbl%NC, djo_tbl%NCM, djo_tbl%INM,  &
     &    djo_tbl%IAM,  mat11%AM, B, X, djo_tbl%NUM_NCOMP,              &
     &    djo_tbl%INOD_DJO, djo_tbl%NUM_SUM, np_smp,                    &
     &    djo_tbl%IEND_SUM_smp)
!
      end subroutine matvec_djo_1N_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine matvec_djo_33_type(djo_tbl, mat33, NP, B, X, np_smp)
!
      type(DJORS_CONNECT), intent(in) :: djo_tbl
      type(DJORS_MATRIX), intent(in) :: mat33
!
      integer(kind=kint), intent(in) :: NP, np_smp
      real(kind = kreal), intent(in) :: B(3*NP)
!
      real(kind = kreal), intent(inout) :: X(3*djo_tbl%NC)
!
!
      call matvec_djo_33(NP, djo_tbl%NC, djo_tbl%NCM, djo_tbl%INM,      &
     &    djo_tbl%IAM,  mat33%AM, B, X, djo_tbl%NUM_NCOMP,              &
     &    djo_tbl%INOD_DJO, djo_tbl%NUM_SUM, np_smp,                    &
     &    djo_tbl%IEND_SUM_smp)
!
      end subroutine matvec_djo_33_type
!
!-----------------------------------------------------------------------
!
      subroutine matvec_djo_NN_type(djo_tbl, matNN, NB, NP, B, X,       &
     &          np_smp)
!
      type(DJORS_CONNECT), intent(in) :: djo_tbl
      type(DJORS_MATRIX), intent(in) :: matNN
!
      integer(kind=kint), intent(in) :: NB, NP, np_smp
      real(kind = kreal), intent(in) :: B(NB*NP)
!
      real(kind = kreal), intent(inout) :: X(NB*djo_tbl%NC)
!
!
      call matvec_djo_NN(NB, NP, djo_tbl%NC, djo_tbl%NCM, djo_tbl%INM,  &
     &    djo_tbl%IAM, matNN%AM, B, X, djo_tbl%NUM_NCOMP,               &
     &    djo_tbl%INOD_DJO, djo_tbl%NUM_SUM, np_smp,                    &
     &    djo_tbl%IEND_SUM_smp)
!
      end subroutine matvec_djo_NN_type
!
!-----------------------------------------------------------------------
!
      end module matvec_by_djo_type
