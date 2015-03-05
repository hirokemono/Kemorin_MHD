!
!      module cal_norm_products_nn
!
!     Written by Kemorin
!
!!       subroutine cal_local_norm_n(NP, NB, PEsmpTOT, STACKmcG, B, BNRM)
!!           BNRM = B^2
!!       subroutine cal_local_s_product_n(NP, NB, PEsmpTOT, STACKmcG,   &
!!     &           W, WT, SP)
!!           SP = W \cdot WT
!!       subroutine cal_local_sproduct_norm_n(NP, NB, PEsmpTOT,         &
!!     &           STACKmcG, WN, WP, SP, BNRM)
!!           BNRM = WN^2
!!           SP = WN \cdot WP
!!       subroutine cal_local_sproduct_and_norm_n(NP, NB, PEsmpTOT,     &
!!     &           STACKmcG, WN, WP1, WP2, SP, BNRM)
!!           BNRM = WN^2
!!           SP = WP1 \cdot WP2
!!
!!      subroutine cal_5_products_norm_n(NP, NB, PEsmpTOT, STACKmcG,    &
!!     &          WY, WT, WTT, C0)
!!             C0(1) = WY^2
!!             C0(2) = WTT \cdot WT
!!             C0(3) = WY \cdot WT
!!             C0(4) = WTT \cdot WY
!!             C0(5) = WTT^2
!
      module cal_norm_products_nn
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
       subroutine cal_local_norm_n(NP, NB, PEsmpTOT, STACKmcG, B, BNRM)
!
       integer(kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: B(NB*NP)
       real(kind = kreal), intent(inout) :: BNRM
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
      BNRM   = 0.d0

!$omp parallel do private(iS,iE,i) reduction(+:BNRM)
      do ip= 1, PEsmpTOT
        iS= NB * STACKmcG(ip-1) + 1
        iE= NB * STACKmcG(ip  )
!cdir nodep
!voption indep (B)
        do i= iS, iE
          BNRM = BNRM + B(i)*B(i)
        end do
      end do
!$omp end parallel do
!
       end subroutine cal_local_norm_n
!
!  ---------------------------------------------------------------------
!
       subroutine cal_local_s_product_n(NP, NB, PEsmpTOT, STACKmcG,     &
     &           W, WT, SP)
!
       integer(kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in):: W(NB*NP), WT(NB*NP)
       real(kind = kreal), intent(inout) :: SP
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
      SP   = 0.d0

!$omp parallel do private(iS,iE,i) reduction(+:SP)
      do ip= 1, PEsmpTOT
        iS= NB * STACKmcG(ip-1) + 1
        iE= NB * STACKmcG(ip  )
!cdir nodep
!voption indep (W,WT)
        do i= iS, iE
          SP = SP + W(i) * WT(i)
        enddo
      enddo
!$omp end parallel do

       end subroutine cal_local_s_product_n
!
!  ---------------------------------------------------------------------
!
       subroutine cal_local_sproduct_norm_n(NP, NB, PEsmpTOT,           &
     &           STACKmcG, WN, WP, SP, BNRM)
!
       integer(kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in)   :: WN(NB*NP), WP(NB*NP)
       real(kind = kreal), intent(inout) :: SP, BNRM
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
      SP      = 0.d0
      BNRM    = 0.d0

!$omp parallel do private(iS,iE,i) reduction(+:SP,BNRM)
      do ip= 1, PEsmpTOT
        iS= NB * STACKmcG(ip-1) + 1
        iE= NB * STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (WN,WP)
        do i= iS, iE
          SP  =  SP   + WN(i) * WP(i)
          BNRM = BNRM + WN(i) * WN(i)
        end do
      end do
!$omp end parallel do

       end subroutine cal_local_sproduct_norm_n
!
!  ---------------------------------------------------------------------
!
       subroutine cal_local_sproduct_and_norm_n(NP, NB, PEsmpTOT,       &
     &           STACKmcG, WN, WP1, WP2, SP, BNRM)
!
       integer(kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in)   :: WN(NB*NP)
       real(kind = kreal), intent(in)   :: WP1(NB*NP), WP2(NB*NP)
       real(kind = kreal), intent(inout) :: SP, BNRM
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
      SP      = 0.d0
      BNRM    = 0.d0

!$omp parallel do private(iS,iE,i) reduction(+:SP,BNRM)
      do ip= 1, PEsmpTOT
        iS= NB * STACKmcG(ip-1) + 1
        iE= NB * STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (WN,WP1,WP2)
        do i= iS, iE
          SP  =  SP   + WP1(i) * WP2(i)
          BNRM = BNRM + WN(i)  * WN(i)
        enddo
      enddo
!$omp end parallel do

       end subroutine cal_local_sproduct_and_norm_n
!
!  ---------------------------------------------------------------------
!
      subroutine cal_5_products_norm_n(NP, NB, PEsmpTOT, STACKmcG,      &
     &          WY, WT, WTT, C0)
!
       integer(kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in)   :: WY(NB*NP)
       real(kind = kreal), intent(in)   :: WT(NB*NP), WTT(NB*NP)
       real(kind = kreal), intent(inout) :: C0(5)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
       C0(1:5)= 0.0d0
!
!$omp parallel do private(iS,iE,i) reduction(+:C0)
      do ip= 1, PEsmpTOT
        iS= NB * STACKmcG(ip-1) + 1
        iE= NB * STACKmcG(ip  )
!cdir nodep
!voption indep (WY,WT,WTT)
        do i= iS, iE
          C0(1) = C0(1) + WY(i) *  WY(i)
          C0(2) = C0(2) + WTT(i) * WT(i)
          C0(3) = C0(3) + WY(i) *  WT(i)
          C0(4) = C0(4) + WTT(i) * WY(i)
          C0(5) = C0(5) + WTT(i) * WTT(i)
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_5_products_norm_n
!
!  ---------------------------------------------------------------------
!
      end module cal_norm_products_nn
