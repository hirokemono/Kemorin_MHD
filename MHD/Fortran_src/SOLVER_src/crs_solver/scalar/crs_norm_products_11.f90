!
!      module crs_norm_products_11
!
!     Written by Kemorin
!
!       subroutine cal_local_norm_1(NP, N, B, BNRM)
!           BNRM = B^2
!       subroutine cal_local_s_product_1(NP, N, W, WT, SP)
!           SP = W \cdot WT
!       subroutine cal_local_sproduct_norm_1(NP, N, WN, WP, SP, BNRM)
!           BNRM = WN^2
!           SP = WN \cdot WP
!       subroutine cal_local_sproduct_and_norm_1(NP, N,                 &
!     &           WN, WP1, WP2, SP, BNRM)
!           BNRM = WN^2
!           SP = WP1 \cdot WP2
!
!      subroutine cal_5_products_norm_1(NP, N, WY, WT, WTT, CG, C0)
!             C0(1) = WY^2
!             C0(2) = WTT \cdot WT
!             C0(3) = WY \cdot WT
!             C0(4) = WTT \cdot WY
!             C0(5) = WTT^2
!
      module crs_norm_products_11
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
       subroutine cal_local_norm_1(NP, N, B, BNRM)
!
       integer(kind = kint), intent(in) :: NP, N
       real(kind = kreal), intent(in) :: B(NP)
       real(kind = kreal), intent(inout) :: BNRM
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
      BNRM = 0.d0
      do i= 1, N
        BNRM = BNRM + B(i)**2
      end do
!
       end subroutine cal_local_norm_1
!
!  ---------------------------------------------------------------------
!
       subroutine cal_local_s_product_1(NP, N, W, WT, SP)
!
       integer(kind = kint), intent(in) :: NP, N
       real(kind = kreal), intent(in):: W(NP), WT(NP)
       real(kind = kreal), intent(inout) :: SP
!
       integer (kind = kint) :: i
!
!
        SP = 0.d0
        do i= 1, N
          SP = SP + W(i)*WT(i)
        end do
!
      end subroutine cal_local_s_product_1
!
!  ---------------------------------------------------------------------
!
       subroutine cal_local_sproduct_norm_1(NP, N, WN, WP, SP, BNRM)
!
       integer(kind = kint), intent(in) :: NP, N
       real(kind = kreal), intent(in)   :: WN(NP), WP(NP)
       real(kind = kreal), intent(inout) :: SP, BNRM
!
       integer (kind = kint) :: i
!
!
      SP      = 0.d0
      BNRM    = 0.d0
!
      do i= 1, N
        SP  = SP   + WN(i) * WP(i)
        BNRM= BNRM + WN(i) * WN(i)
      end do

       end subroutine cal_local_sproduct_norm_1
!
!  ---------------------------------------------------------------------
!
       subroutine cal_local_sproduct_and_norm_1(NP, N,                  &
     &           WN, WP1, WP2, SP, BNRM)
!
       integer(kind = kint), intent(in) :: NP, N
       real(kind = kreal), intent(in)   :: WN(NP)
       real(kind = kreal), intent(in)   :: WP1(NP), WP2(NP)
       real(kind = kreal), intent(inout) :: SP, BNRM
!
       integer (kind = kint) :: i
!
!
      SP      = 0.d0
      BNRM    = 0.d0
      do i= 1, N
          SP  = SP   + WP1(i) * WP2(i)
          BNRM= BNRM + WN(i)  * WN(i)
      enddo
!
       end subroutine cal_local_sproduct_and_norm_1
!
!  ---------------------------------------------------------------------
!
      subroutine cal_5_products_norm_1(NP, N, WY, WT, WTT, CG, C0)
!
       integer(kind = kint), intent(in) :: NP, N
       real(kind = kreal), intent(in)   :: WY(NP)
       real(kind = kreal), intent(in)   :: WT(NP), WTT(NP)
       real(kind = kreal), intent(inout) :: C0(5), CG(5)
!
       integer (kind = kint) :: i
!
!
       C0(1:5)= 0.0d0
       CG(1:5)= 0.0d0
!
      do i= 1, N
        C0(1)= C0(1) + WY(i) *  WY(i)
        C0(2)= C0(2) + WTT(i) * WT(i)
        C0(3)= C0(3) + WY(i) *  WT(i)
        C0(4)= C0(4) + WTT(i) * WY(i)
        C0(5)= C0(5) + WTT(i) * WTT(i)
      enddo
!
      end subroutine cal_5_products_norm_1
!
!  ---------------------------------------------------------------------
!
      end module crs_norm_products_11
