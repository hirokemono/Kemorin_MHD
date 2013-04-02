!
!     module MC_Cuthill_McKee
!
!      Written by K. Nakajima in 2001
!        modified by H. Matsui on May, 2002
!        modified by H. Matsui on June, 2006
!        modified by H. Matsui on Oct., 2006
!        modified by H. Matsui on Jan., 2009
!C
!C     Cuthill-McKee method for VECTOR ICCG.
!C    
!      subroutine sRCM (NP, N, NPL_crs, NPU_crs, INL_crs, INU_crs,      &
!     &          IAL_crs, IAU_crs, NPL_mc, NPU_mc, NNL_mc, NNU_mc,      &
!     &          INL_mc, INU_mc, IAL_mc, IAU_mc,                        &
!     &          NO, IVECT, IV1, IV2, IW)
!      subroutine sMC (NP, N, NPL_crs, NPU_crs, INL_crs, INU_crs,       &
!     &          IAL_crs, IAU_crs, NPL_mc, NPU_mc, NNL_mc, NNU_mc,      &
!     &          INL_mc, INU_mc, IAL_mc, IAU_mc,                        &
!     &          NO, IVECT, IV1, IV2, IW, NCOLORk)
!
!      subroutine no_MC(NP, NPL_crs, NPU_crs, INL_crs, INU_crs,         &
!     &          IAL_crs, IAU_crs, NPL_mc, NPU_mc, NNL_mc, NNU_mc,      &
!     &          INL_mc, INU_mc, IAL_mc,  IAU_mc,  NO, IVECT, IV1, IV2)
!
!      subroutine matrix_transfer_mc(NP, NPL_crs, NPU_crs, INL_crs,     &
!     &          INU_crs, IAL_crs, IAU_crs, NPL_mc, NPU_mc,             &
!     &          NNL_mc, NNU_mc, INL_mc, INU_mc, IAL_mc, IAU_mc,        &
!     &          IV1, IV2)
!
      module MC_Cuthill_McKee
!
      use m_precision
!
      implicit none
!
      private :: matrix_transfer_mc
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sRCM (NP, N, NPL_crs, NPU_crs, INL_crs, INU_crs,       &
     &          IAL_crs, IAU_crs, NPL_mc, NPU_mc, NNL_mc, NNU_mc,       &
     &          INL_mc, INU_mc, IAL_mc, IAU_mc,                         &
     &          NO, IVECT, IV1, IV2, IW)
!
      use Cuthill_McKee
!
      integer(kind = kint), intent(in) :: NP, N
      integer(kind = kint), intent(in) :: NPL_crs, NPU_crs
      integer(kind = kint), intent(in) :: INL_crs(0:NP)
      integer(kind = kint), intent(in) :: INU_crs(0:NP)
      integer(kind = kint), intent(in) :: IAL_crs(NPL_crs)
      integer(kind = kint), intent(in) :: IAU_crs(NPU_crs)
!
      integer(kind = kint), intent(inout) :: NPL_mc, NPU_mc
      integer(kind = kint), intent(inout) :: NNL_mc(NP)
      integer(kind = kint), intent(inout) :: NNU_mc(NP)
      integer(kind = kint), intent(inout) :: INL_mc(0:NP)
      integer(kind = kint), intent(inout) :: INU_mc(0:NP)
      integer(kind = kint), intent(inout) :: IAL_mc(NPL_mc)
      integer(kind = kint), intent(inout) :: IAU_mc(NPU_mc)
!
      integer(kind = kint), intent(inout) :: NO
      integer(kind = kint), intent(inout) :: IVECT(0:NP)
      integer(kind = kint), intent(inout) :: IV1(NP), IV2(NP)
      integer(kind = kint), intent(inout) :: IW(NP)
!
!C
!C +---------------+
!C | MULTIcoloring |
!C +---------------+
!C===
      call set_RCM_ordering (NP, N, NPU_crs, INL_crs, INU_crs, IAU_crs, &
     &    NO, IVECT, IV1, IV2, IW)

!C
!C +-----------------+
!C | MATRIX transfer |
!C +-----------------+
!C===
!
      call matrix_transfer_mc(NP, NPL_crs, NPU_crs, INL_crs, INU_crs,   &
     &    IAL_crs, IAU_crs, NPL_mc, NPU_mc, NNL_mc, NNU_mc,             &
     &    INL_mc, INU_mc, IAL_mc, IAU_mc, IV1, IV2)
!
      end subroutine sRCM
!
! ----------------------------------------------------------------------
!
      subroutine sMC (NP, N, NPL_crs, NPU_crs, INL_crs, INU_crs,        &
     &          IAL_crs, IAU_crs, NPL_mc, NPU_mc, NNL_mc, NNU_mc,       &
     &          INL_mc, INU_mc, IAL_mc, IAU_mc,                         &
     &          NO, IVECT, IV1, IV2, IW, NCOLORk)
!
      use Cuthill_McKee
!
      integer(kind = kint), intent(in) :: NP, N
      integer(kind = kint), intent(in) :: NPL_crs, NPU_crs
      integer(kind = kint), intent(in) :: INL_crs(0:NP)
      integer(kind = kint), intent(in) :: INU_crs(0:NP)
      integer(kind = kint), intent(in) :: IAL_crs(NPL_crs)
      integer(kind = kint), intent(in) :: IAU_crs(NPU_crs)
!
      integer(kind = kint), intent(inout) :: NPL_mc, NPU_mc
      integer(kind = kint), intent(inout) :: NNL_mc(NP)
      integer(kind = kint), intent(inout) :: NNU_mc(NP)
      integer(kind = kint), intent(inout) :: INL_mc(0:NP)
      integer(kind = kint), intent(inout) :: INU_mc(0:NP)
      integer(kind = kint), intent(inout) :: IAL_mc(NPL_mc)
      integer(kind = kint), intent(inout) :: IAU_mc(NPU_mc)
!
      integer(kind = kint), intent(inout) :: NO, NCOLORk
!
      integer(kind = kint), intent(inout) :: IVECT(0:NP)
      integer(kind = kint), intent(inout) :: IV1(NP), IV2(NP)
      integer(kind = kint), intent(inout) :: IW(NP)
!
!C
!C +---------------+
!C | MULTIcoloring |
!C +---------------+
!C===
      call set_MC_ordering(NP, N, NPL_crs, NPU_crs, INL_crs, INU_crs,   &
     &    IAL_crs, IAU_crs, NO, IVECT, IV1, IV2, IW, NCOLORk)
!C
!C +-----------------+
!C | MATRIX transfer |
!C +-----------------+
!C===
!
      call matrix_transfer_mc(NP, NPL_crs, NPU_crs, INL_crs, INU_crs,   &
     &    IAL_crs, IAU_crs, NPL_mc, NPU_mc, NNL_mc, NNU_mc,             &
     &    INL_mc, INU_mc, IAL_mc, IAU_mc, IV1, IV2)
!
      end subroutine sMC
!
! ----------------------------------------------------------------------
!
      subroutine no_MC(NP, NPL_crs, NPU_crs, INL_crs, INU_crs,          &
     &          IAL_crs, IAU_crs, NPL_mc, NPU_mc, NNL_mc, NNU_mc,       &
     &          INL_mc, INU_mc, IAL_mc,  IAU_mc,  NO, IVECT, IV1, IV2)
!
      integer(kind = kint), intent(in) :: NP
      integer(kind = kint), intent(in) :: NPL_crs, NPU_crs
      integer(kind = kint), intent(in) :: INL_crs(0:NP)
      integer(kind = kint), intent(in) :: INU_crs(0:NP)
      integer(kind = kint), intent(in) :: IAL_crs(NPL_crs)
      integer(kind = kint), intent(in) :: IAU_crs(NPU_crs)
!
      integer(kind = kint), intent(inout) :: NO
      integer(kind = kint), intent(inout) :: IVECT(0:NP)
      integer(kind = kint), intent(inout) :: IV1(NP), IV2(NP)
!
      integer(kind = kint), intent(inout) :: NPL_mc, NPU_mc
      integer(kind = kint), intent(inout) :: NNL_mc(NP)
      integer(kind = kint), intent(inout) :: NNU_mc(NP)
      integer(kind = kint), intent(inout) :: INL_mc(0:NP)
      integer(kind = kint), intent(inout) :: INU_mc(0:NP)
      integer(kind = kint), intent(inout) :: IAL_mc(NPL_mc)
      integer(kind = kint), intent(inout) :: IAU_mc(NPU_mc)
!
      integer(kind = kint) :: i
!
!
      NO = 1
      IVECT(0:NP) = 0
!
      do i = 1, NP
        IV1(i) = i
        IV2(i) = i
      end do
!
      INL_mc(0) = INL_crs(0)
      INU_mc(0) = INU_crs(0)
      do i = 1, NP
        NNL_mc(i) = INL_crs(i) - INL_crs(i-1)
        NNU_mc(i) = INU_crs(i) - INU_crs(i-1)
        INL_mc(i) = INL_crs(i)
        INU_mc(i) = INU_crs(i)
      end do
!
      IAL_mc(1:NPL_mc) = IAL_crs(1:NPL_mc)
      IAU_mc(1:NPU_mc) = IAU_crs(1:NPU_mc)
!
      end subroutine no_MC
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine matrix_transfer_mc(NP, NPL_crs, NPU_crs, INL_crs,      &
     &          INU_crs, IAL_crs, IAU_crs, NPL_mc, NPU_mc,              &
     &          NNL_mc, NNU_mc, INL_mc, INU_mc, IAL_mc, IAU_mc,         &
     &          IV1, IV2)
!
      integer(kind = kint), intent(in) :: NP
      integer(kind = kint), intent(in) :: NPL_crs, NPU_crs
      integer(kind = kint), intent(in) :: INL_crs(0:NP)
      integer(kind = kint), intent(in) :: INU_crs(0:NP)
      integer(kind = kint), intent(in) :: IAL_crs(NPL_crs)
      integer(kind = kint), intent(in) :: IAU_crs(NPU_crs)
!
      integer(kind = kint), intent(in) :: IV1(NP), IV2(NP)
!
      integer(kind = kint), intent(inout) :: NPL_mc, NPU_mc
      integer(kind = kint), intent(inout) :: NNL_mc(NP)
      integer(kind = kint), intent(inout) :: NNU_mc(NP)
      integer(kind = kint), intent(inout) :: INL_mc(0:NP)
      integer(kind = kint), intent(inout) :: INU_mc(0:NP)
      integer(kind = kint), intent(inout) :: IAL_mc(NPL_mc)
      integer(kind = kint), intent(inout) :: IAU_mc(NPU_mc)
!
      integer(kind = kint) :: i, j, k, in, kn
!
!C
!C +-----------------+
!C | MATRIX transfer |
!C +-----------------+
!C===
!
!     transfer num_crs_l
!
      INL_mc(0) = 0
      do i= 1, NP
        in = IV1(i)
        NNL_mc(i) = INL_crs(in) - INL_crs(in-1)
        INL_mc(i) = INL_mc(i-1) + NNL_mc(i)
      enddo
!
!     transfer num_crs_u
!
      INU_mc(0) = 0
      do i= 1, NP
        in = IV1(i)
        NNU_mc(i) = INU_crs(in) - INU_crs(in-1)
        INU_mc(i) = INU_mc(i-1) + NNU_mc(i)
      end do
!
!     switch array
!
      do i = 1, NP
        in = IV1(i)
        do j = 1, NNL_mc(i)
          kn = INL_crs(in-1) + j
          k =  INL_mc(i-1) + j
          IAL_mc(k) = IAL_crs(kn)
        end do
      end do

      do i = 1, NP
        in = IV1(i)
        do j = 1, NNU_mc(i)
          kn = INU_crs(in-1) + j
          k =  INU_mc(i-1) + j
          IAU_mc(k) = IAU_crs(kn)
        end do
      end do
!
!   set new address
!
      do j = 1, NPL_crs
        in = IAL_mc(j)
        IAL_mc(j) = IV2(in)
      end do
!
      do j = 1, NPU_crs
        in = IAU_mc(j)
        IAU_mc(j) = IV2(in)
      end do
!
      end subroutine matrix_transfer_mc
!
! ----------------------------------------------------------------------
!
      end module MC_Cuthill_McKee
