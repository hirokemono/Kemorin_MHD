!djds_norm_products_11.f90
!      module djds_norm_products_11
!
!     Written by Kemorin
!
!       subroutine djds_local_norm_1(NP, PEsmpTOT, STACKmcG, B,         &
!     &           BNRM, BNRM_smp)
!           BNRM = B^2
!       subroutine djds_local_s_product_1(NP, PEsmpTOT, STACKmcG,       &
!     &           W, WT, SP, SP_smp)
!           SP = W \cdot WT
!       subroutine djds_local_sproduct_norm_1(NP, PEsmpTOT, STACKmcG,   &
!     &           WN, WP, SP, BNRM, SP_smp, BNRM_smp)
!           BNRM = WN^2
!           SP = WN \cdot WP
!       subroutine djds_local_sproduct_and_norm_1(NP, PEsmpTOT,         &
!     &           STACKmcG, WN, WP1, WP2, SP, BNRM, SP_smp, BNRM_smp)
!           BNRM = WN^2
!           SP = WP1 \cdot WP2
!
!      subroutine djds_5_products_norm_1(NP, PEsmpTOT, STACKmcG,        &
!     &          WY, WT, WTT, CG, C0, C0_smp)
!             C0(1) = WY^2
!             C0(2) = WTT \cdot WT
!             C0(3) = WY \cdot WT
!             C0(4) = WTT \cdot WY
!             C0(5) = WTT^2
!
      module djds_norm_products_11
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
       subroutine djds_local_norm_1(NP, PEsmpTOT, STACKmcG, B,          &
      &          BNRM, BNRM_smp)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: B(NP)
       real(kind = kreal), intent(inout) :: BNRM_smp(PEsmpTOT)
       real(kind = kreal), intent(inout) :: BNRM
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
      BNRM   = 0.d0
      BNRM_smp= 0.d0

!$omp parallel do private(iS,iE,i)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (B)
        do i= iS, iE
          BNRM_smp(ip)= BNRM_smp(ip) + B(i)*B(i)
        enddo
      enddo
!$omp end parallel do

      do ip= 1, PEsmpTOT
        BNRM= BNRM + BNRM_smp(ip)
      enddo
!
       end subroutine djds_local_norm_1
!
!  ---------------------------------------------------------------------
!
       subroutine djds_local_s_product_1(NP, PEsmpTOT, STACKmcG,        &
     &           W, WT, SP, SP_smp)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in):: W(NP), WT(NP)
       real(kind = kreal), intent(inout) :: SP_smp(PEsmpTOT)
       real(kind = kreal), intent(inout) :: SP
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
      SP   = 0.d0
      SP_smp= 0.d0

!$omp parallel do private(iS,iE,i)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (W,WT)
        do i= iS, iE
          SP_smp(ip)= SP_smp(ip) + W(i) * WT(i)
        enddo
      enddo
!$omp end parallel do

      do ip= 1, PEsmpTOT
        SP= SP + SP_smp(ip)
      enddo

       end subroutine djds_local_s_product_1
!
!  ---------------------------------------------------------------------
!
       subroutine djds_local_sproduct_norm_1(NP, PEsmpTOT, STACKmcG,    &
     &           WN, WP, SP, BNRM, SP_smp, BNRM_smp)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in)   :: WN(NP), WP(NP)
       real(kind = kreal), intent(inout) :: SP_smp(PEsmpTOT)
       real(kind = kreal), intent(inout) :: BNRM_smp(PEsmpTOT)
       real(kind = kreal), intent(inout) :: SP, BNRM
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
      SP      = 0.d0
      BNRM    = 0.d0
      SP_smp  = 0.d0
      BNRM_smp= 0.d0

!$omp parallel do private(iS,iE,i)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (WN,WP)
        do i= iS, iE
          SP_smp(ip)  = SP_smp(ip)   + WN(i) * WP(i)
          BNRM_smp(ip)= BNRM_smp(ip) + WN(i) * WN(i)
        enddo
      enddo
!$omp end parallel do

      do ip= 1, PEsmpTOT
        SP   = SP   + SP_smp(ip)
        BNRM = BNRM + BNRM_smp(ip)
      enddo

       end subroutine djds_local_sproduct_norm_1
!
!  ---------------------------------------------------------------------
!
       subroutine djds_local_sproduct_and_norm_1(NP, PEsmpTOT,          &
     &           STACKmcG, WN, WP1, WP2, SP, BNRM, SP_smp, BNRM_smp)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in)   :: WN(NP)
       real(kind = kreal), intent(in)   :: WP1(NP), WP2(NP)
       real(kind = kreal), intent(inout) :: SP_smp(PEsmpTOT)
       real(kind = kreal), intent(inout) :: BNRM_smp(PEsmpTOT)
       real(kind = kreal), intent(inout) :: SP, BNRM
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
      SP      = 0.d0
      BNRM    = 0.d0
      SP_smp  = 0.d0
      BNRM_smp= 0.d0

!$omp parallel do private(iS,iE,i)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (WN,WP1,WP2)
        do i= iS, iE
          SP_smp(ip)  = SP_smp(ip)   + WP1(i) * WP2(i)
          BNRM_smp(ip)= BNRM_smp(ip) + WN(i)  * WN(i)
        enddo
      enddo
!$omp end parallel do

      do ip= 1, PEsmpTOT
        SP   = SP   + SP_smp(ip)
        BNRM = BNRM + BNRM_smp(ip)
      enddo

       end subroutine djds_local_sproduct_and_norm_1
!
!  ---------------------------------------------------------------------
!
      subroutine djds_5_products_norm_1(NP, PEsmpTOT, STACKmcG,         &
     &          WY, WT, WTT, CG, C0, C0_smp)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in)   :: WY(NP)
       real(kind = kreal), intent(in)   :: WT(NP), WTT(NP)
       real(kind = kreal), intent(inout) :: C0_smp(PEsmpTOT,5)
       real(kind = kreal), intent(inout) :: C0(5), CG(5)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
       C0(1:5)= 0.0d0
       CG(1:5)= 0.0d0
       C0_smp(1:PEsmpTOT,1:5)= 0.0d0
!
!$omp parallel do private(iS,iE,i)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (WY,WT,WTT)
        do i= iS, iE
          C0_smp(ip,1) = C0_smp(ip,1) + WY(i) *  WY(i)
          C0_smp(ip,2) = C0_smp(ip,2) + WTT(i) * WT(i)
          C0_smp(ip,3) = C0_smp(ip,3) + WY(i) *  WT(i)
          C0_smp(ip,4) = C0_smp(ip,4) + WTT(i) * WY(i)
          C0_smp(ip,5) = C0_smp(ip,5) + WTT(i) * WTT(i)
        enddo
      enddo
!$omp end parallel do

      do ip= 1, PEsmpTOT
        C0(1) = C0(1) + C0_smp(ip,1)
        C0(2) = C0(2) + C0_smp(ip,2)
        C0(3) = C0(3) + C0_smp(ip,3)
        C0(4) = C0(4) + C0_smp(ip,4)
        C0(5) = C0(5) + C0_smp(ip,5)
      enddo
!
      end subroutine djds_5_products_norm_1
!
!  ---------------------------------------------------------------------
!
!
      end module djds_norm_products_11
