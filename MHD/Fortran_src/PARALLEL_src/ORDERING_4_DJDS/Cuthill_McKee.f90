!
!     module Cuthill_McKee
!
!      Written by K. Nakajima in 2001
!        modified by H. Matsui on May, 2002
!        modified by H. Matsui on June, 2006
!        modified by H. Matsui on Oct., 2006
!        modified by H. Matsui on Jan., 2009
!        modified by H. Matsui on Jan., 2017
!
!C
!C     Cuthill-McKee method for VECTOR ICCG.
!C    
!
!!      subroutine set_RCM_ordering (NP, N, NPU_crs, INL_crs, INU_crs,  &
!!     &          IAU_crs, NO, IVECT, IV1, IV2, IW)
!!      subroutine set_MC_ordering(NP, N, NPL_crs, NPU_crs,             &
!!     &          INL_crs, INU_crs, IAL_crs, IAU_crs, NCOLOR_in,        &
!!     &          NO, IVECT, IV1, IV2, IW)
!
      module Cuthill_McKee
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_RCM_ordering (NP, N,  NPU_crs, INL_crs, INU_crs,   &
     &          IAU_crs, NO, IVECT, IV1, IV2, IW)
!
      integer(kind = kint), intent(in) :: NP, N
      integer(kind = kint), intent(in) :: NPU_crs
      integer(kind = kint), intent(in) :: INL_crs(0:NP)
      integer(kind = kint), intent(in) :: INU_crs(0:NP)
      integer(kind = kint), intent(in) :: IAU_crs(NPU_crs)
!
      integer(kind = kint), intent(inout) :: NO
      integer(kind = kint), intent(inout) :: IVECT(0:NP)
      integer(kind = kint), intent(inout) :: IV1(NP), IV2(NP)
      integer(kind = kint), intent(inout) :: IW(NP)
!
      integer(kind = kint) :: i, j
      integer(kind = kint) :: KC, KCT, KCT0, KMIN, KMAX
      integer(kind = kint) :: JC, II, jst, jed, num_l
!C
!C-- INIT.
      IW   = 0
      IV1  = 0
      IV2  = 0
      IVECT= 0

      IVECT(0)= 0
      IVECT(1)= 0
       NO   = 0
      KCT   = 0

!C
!C-- RCM ordering
!voption novec
      do i= 1, N
        num_l = INL_crs(i) - INL_crs(i-1)
        if (num_l.eq.0) then
          IVECT(1) = IVECT(1) + 1
          KCT      = KCT + 1
          IV1(KCT) = i
          IW (i)   = -1
         else
          IW(i) = 0
       endif
      enddo  

      KC  = KCT
      KCT0= 0

   20 continue
      KMIN = N
      KMAX = 1
      do j= 1, KC
        JC = IV1(KCT0+j)
        jst = INU_crs(JC-1) + 1
        jed = INU_crs(JC)
!voption novec
        do i= jst, jed
             II = IAU_crs(i)
          if (II.le.N) then
            IW(II)= IW(II) + 1
             KMIN = min0(II,KMIN)
             KMAX = max0(II,KMAX)
          endif
        enddo
      enddo

        NO = NO + 1
      KCT0 = KCT
!voption novec
      do i = KMIN, KMAX
        num_l = INL_crs(i) - INL_crs(i-1)
        if (IW(i).eq.num_l) then
              KCT = KCT + 1
          IW (i)  = -1
          IV1(KCT)= I
        endif
      enddo

       KC       = KCT - KCT0
      IVECT(NO+1) = KCT

      if (KC.ne.0 .and. NO.le.N) goto 20

      if (NO .eq. 0) then
       NO = 1
      end if

!C
!C-- NEWtoOLD, OLDtoNEW array
      do i= N+1, NP
        IV1(i)= i
      enddo

      do i= 1, NP
        IV2(IV1(i)) = i
      enddo
!
      end subroutine set_RCM_ordering
!
! ----------------------------------------------------------------------
!
      subroutine set_MC_ordering(NP, N, NPL_crs, NPU_crs,               &
     &          INL_crs, INU_crs, IAL_crs, IAU_crs, NCOLOR_in,          &
     &          NO, IVECT, IV1, IV2, IW)
!
      integer(kind = kint), intent(in) :: NP, N
      integer(kind = kint), intent(in) :: NPL_crs, NPU_crs
      integer(kind = kint), intent(in) :: INL_crs(0:NP)
      integer(kind = kint), intent(in) :: INU_crs(0:NP)
      integer(kind = kint), intent(in) :: IAL_crs(NPL_crs)
      integer(kind = kint), intent(in) :: IAU_crs(NPU_crs)
      integer(kind = kint), intent(in) :: NCOLOR_in
!
!
      integer(kind = kint), intent(inout) :: NO
!
      integer(kind = kint), intent(inout) :: IVECT(0:NP)
      integer(kind = kint), intent(inout) :: IV1(NP), IV2(NP)
      integer(kind = kint), intent(inout) :: IW(NP)
!
!
      integer(kind = kint) :: i, k, ik, ic, kst, ked, NCOLORk
      integer(kind = kint) :: ITEMcou, icou, icouK, icoug, icol
      integer(kind = kint), parameter :: NODmin = 1

!C
!C +-------+
!C | INIT. |
!C +-------+
!C===
      IW   = 0
      IV1  = 0
      IV2  = 0
      IVECT= 0

      IVECT(0)= 0
      IVECT(1)= 0
       NO   = 0
!      KCT   = 0

      NO= N
      do i= 1, NP
        IVECT(i)= i
        IV1  (i)= i
        IV2  (i)= i
      enddo

      IW        = 0
      IW(NODmin)= 1
!
!C===

!      open (28, file='COLOR.DAT', status='unknown')
!        read (28,*) NCOLOR_in
!      close (28)

      ITEMcou= N/NCOLOR_in
!
      write (*,*) 'color_mc:', NCOLOR_in
      write (*,*) 'ITEMcou:', ITEMcou

!C
!C +---------------+
!C | MULTIcoloring |
!C +---------------+
!C===
      icou = 1
      icouK= 1
      NCOLORk= NCOLOR_in
!
      do icol= 1, N
        NCOLORk= icol
!
        do i= 1, N
          if (IW(i).le.0) IW(i)= 0
        enddo
!
!
        do i= 1, N
!C
!C-- already COLORED
          if (IW(i).eq.icol) then
!
            kst = INL_crs(i-1) + 1
            ked = INL_crs(i)
            do k= kst, ked
              ik= IAL_crs(k)
              if (IW(ik).le.0) IW(ik)= -1
            enddo
!
            kst = INU_crs(i-1) + 1
            ked = INU_crs(i)
            do k = kst, ked
              ik = IAU_crs(k)
              if (IW(ik).le.0) IW(ik)= -1
            enddo
!
          endif
!C
!C-- not COLORED
          if (IW(i).eq.0) then
            icou  = icou  + 1
            icouK = icouK + 1
            IW(i) = icol
!
            kst = INL_crs(i-1) + 1
            ked = INL_crs(i)
            do k = kst, ked
              ik = IAL_crs(k)
              if (IW(ik).le.0) IW(ik)= -1
            enddo
!
            kst = INU_crs(i-1) + 1
            ked = INU_crs(i)
            do k = kst, ked
              ik= IAU_crs(k)
              if (IW(ik).le.0) IW(ik)= -1
            enddo
          endif
          if (icouK.eq.ITEMcou) goto 100
          if (icou .eq.N)       goto 200
        enddo      
 100    continue
        icouK= 0
      enddo

 200  continue
!C===

!C
!C +----------------+
!C | FINAL COLORING |
!C +----------------+
!C===

      NO   = NCOLORk
      IVECT= 0
      icoug= 0
      do ic= 1, NO
        icou= 0
!voption novec
        do i= 1, N
          if (IW(i).eq.ic) then
            icou = icou + 1
            icoug= icoug + 1
            IV1(icoug)= i
            IV2(i    )= icoug
          endif
        enddo
        IVECT(ic)= icou
      enddo

      do ic= 1, NO
!        write (*,*) ic, IVECT(ic)
        IVECT(ic)= IVECT(ic-1) + IVECT(ic)
      enddo
!C===
!
      end subroutine set_MC_ordering
!
! ----------------------------------------------------------------------
!
      end module Cuthill_McKee
