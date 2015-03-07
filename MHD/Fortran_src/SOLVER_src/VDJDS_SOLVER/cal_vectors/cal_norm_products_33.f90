!
!      module cal_norm_products_33
!
!     Written by Kemorin
!
!!       subroutine cal_local_norm_3(NP, PEsmpTOT, STACKmcG, B, BNRM)
!!           BNRM = B^2
!!       subroutine cal_local_s_product_3(NP, PEsmpTOT, STACKmcG,       &
!!     &           W, WT, SP)
!!           SP = W \cdot WT
!!       subroutine cal_local_sproduct_norm_3(NP, PEsmpTOT, STACKmcG,   &
!!     &           WN, WP, SP, BNRM)
!!           BNRM = WN^2
!!           SP = WN \cdot WP
!!       subroutine cal_local_sproduct_and_norm_3(NP, PEsmpTOT,         &
!!     &           STACKmcG, WN, WP1, WP2, SP, BNRM)
!!           BNRM = WN^2
!!           SP = WP1 \cdot WP2
!!
!!      subroutine cal_5_products_norm_3(NP, PEsmpTOT, STACKmcG,        &
!!     &          WY, WT, WTT, C0)
!!             C0(1) = WY^2
!!             C0(2) = WTT \cdot WT
!!             C0(3) = WY \cdot WT
!!             C0(4) = WTT \cdot WY
!!             C0(5) = WTT^2
!
      module cal_norm_products_33
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine cal_local_norm_3(NP, PEsmpTOT, STACKmcG, B, BNRM)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: B(3*NP)
       real(kind = kreal), intent(inout) :: BNRM
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
      BNRM   = 0.d0

      if(NP .le. 0) return
!
!$omp parallel do private(iS,iE,i)  reduction(+:BNRM)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!cdir nodep
!voption indep (B)
        do i= iS, iE
          BNRM = BNRM + B(3*i-2)**2 + B(3*i-1)**2 + B(3*i  )**2
        enddo
      enddo
!$omp end parallel do
!
       end subroutine cal_local_norm_3
!
!  ---------------------------------------------------------------------
!
       subroutine cal_local_s_product_3(NP, PEsmpTOT, STACKmcG,         &
     &           W, WT, SP)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in):: W(3*NP), WT(3*NP)
       real(kind = kreal), intent(inout) :: SP
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
      SP   = 0.d0

      if(NP .le. 0) return
!
!$omp parallel do private(iS,iE,i)  reduction(+:SP)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!cdir nodep
!voption indep (W,WT)
        do i= iS, iE
          SP = SP + W(3*i-2)*WT(3*i-2)                                  &
     &            + W(3*i-1)*WT(3*i-1)                                  &
     &            + W(3*i  )*WT(3*i  )
        end do
      end do
!$omp end parallel do

       end subroutine cal_local_s_product_3
!
!  ---------------------------------------------------------------------
!
       subroutine cal_local_sproduct_norm_3(NP, PEsmpTOT, STACKmcG,     &
     &           WN, WP, SP, BNRM)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in)   :: WN(3*NP), WP(3*NP)
       real(kind = kreal), intent(inout) :: SP, BNRM
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
      SP      = 0.d0
      BNRM    = 0.d0

      if(NP .le. 0) return
!
!$omp parallel do private(iS,iE,i)  reduction(+:SP,BNRM)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!cdir nodep
        do i= iS, iE
          SP  = SP    + WN(3*i-2) * WP(3*i-2)                           &
     &                + WN(3*i-1) * WP(3*i-1)                           &
     &                + WN(3*i  ) * WP(3*i  )
          BNRM = BNRM + WN(3*i-2) * WN(3*i-2)                           &
     &                + WN(3*i-1) * WN(3*i-1)                           &
     &                + WN(3*i  ) * WN(3*i  )
        enddo
      enddo
!$omp end parallel do
!
       end subroutine cal_local_sproduct_norm_3
!
!  ---------------------------------------------------------------------
!
       subroutine cal_local_sproduct_and_norm_3(NP, PEsmpTOT,           &
     &           STACKmcG, WN, WP1, WP2, SP, BNRM)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in)   :: WN(3*NP)
       real(kind = kreal), intent(in)   :: WP1(3*NP), WP2(3*NP)
       real(kind = kreal), intent(inout) :: SP, BNRM
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
      SP      = 0.d0
      BNRM    = 0.d0
!
      if(NP .le. 0) return
!
!$omp parallel do private(iS,iE,i)  reduction(+:SP,BNRM)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!cdir nodep
!voption indep (WN,WP1,WP2)
        do i= iS, iE
          SP =   SP   + WP1(3*i-2) * WP2(3*i-2)                         &
     &                + WP1(3*i-1) * WP2(3*i-1)                         &
     &                + WP1(3*i  ) * WP2(3*i  )
          BNRM = BNRM + WN(3*i-2)  * WN(3*i-2)                          &
     &                + WN(3*i-1)  * WN(3*i-1)                          &
     &                + WN(3*i  )  * WN(3*i  )
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_local_sproduct_and_norm_3
!
!  ---------------------------------------------------------------------
!
      subroutine cal_5_products_norm_3(NP, PEsmpTOT, STACKmcG,          &
     &          WY, WT, WTT, C0)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in)   :: WY(3*NP)
       real(kind = kreal), intent(in)   :: WT(3*NP), WTT(3*NP)
       real(kind = kreal), intent(inout) :: C0(5)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
       C0(1:5)= 0.0d0
!
      if(NP .le. 0) return
!
!$omp parallel do private(iS,iE,i) reduction(+:C0)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!cdir nodep
!voption indep (WY,WT,WTT)
        do i= iS, iE
          C0(1) = C0(1) + WY(3*i-2) *  WY(3*i-2)                        &
     &                  + WY(3*i-1) *  WY(3*i-1)                        &
     &                  + WY(3*i  ) *  WY(3*i  )
          C0(2) = C0(2) + WTT(3*i-2) * WT(3*i-2)                        &
     &                  + WTT(3*i-1) * WT(3*i-1)                        &
     &                  + WTT(3*i  ) * WT(3*i  )
          C0(3) = C0(3) + WY(3*i-2) *  WT(3*i-2)                        &
     &                  + WY(3*i-1) *  WT(3*i-1)                        &
     &                  + WY(3*i  ) *  WT(3*i  )
          C0(4) = C0(4) + WTT(3*i-2) * WY(3*i-2)                        &
     &                  + WTT(3*i-1) * WY(3*i-1)                        &
     &                  + WTT(3*i  ) * WY(3*i  )
          C0(5) = C0(5) + WTT(3*i-2) * WTT(3*i-2)                       &
     &                  + WTT(3*i-1) * WTT(3*i-1)                       &
     &                  + WTT(3*i  ) * WTT(3*i  )
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_5_products_norm_3
!
!  ---------------------------------------------------------------------
!
      end module cal_norm_products_33
