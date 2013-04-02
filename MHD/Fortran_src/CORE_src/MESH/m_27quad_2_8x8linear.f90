!
!      module m_27quad_2_8x8linear
!
!      Written by H. Matsui on May, 2006
!
!      subroutine init_27quad_2_8x8linear
!      subroutine set_27quad_2_8x8linear(numele, numele8, ie_l, ie_q27)
!
      module m_27quad_2_8x8linear
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint), parameter :: id_quad27_8linear8(64)         &
     &  = (/  1,  9, 25, 12, 17, 23, 27, 21,                            &
     &        9,  2, 10, 25, 23, 18, 22, 27,                            &
     &       25, 10,  3, 11, 27, 22, 19, 24,                            &
     &       12, 25, 11,  4, 21, 27, 24, 20,                            &
     &       17, 23, 27, 21,  5, 13, 26, 16,                            &
     &       23, 18, 22, 27, 13,  6, 14, 26,                            &
     &       27, 22, 19, 24, 26, 14,  7, 15,                            &
     &       21, 27, 24, 20, 16, 26, 15,  8/)
!
      integer(kind = kint)  :: ie27(8,8)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_27quad_2_8x8linear
!
      use m_geometry_constants
!
      integer(kind = kint) :: iele, k1, j
!
!
      do iele = 1, 8
        do k1 = 1, num_t_linear
          j = (iele-1)*num_t_linear + k1
          ie27(k1,iele) = id_quad27_8linear8(j)
        end do
      end do
!
      end subroutine init_27quad_2_8x8linear
!
!  ---------------------------------------------------------------------
!
      subroutine set_27quad_2_8x8linear(nele_q, ie_q27, nele_l, ie_l)
!
      integer(kind = kint), intent(in) :: nele_q, nele_l
      integer(kind = kint), intent(in) :: ie_q27(nele_q,27)
      integer(kind = kint), intent(inout) :: ie_l(nele_l,8)
!
      integer(kind = kint) :: iele
      integer(kind = kint) :: iele1, iele2, iele3, iele4
      integer(kind = kint) :: iele5, iele6, iele7, iele8
!
!
!$omp parallel do &
!$omp&  private(iele1,iele2,iele3,iele4,iele5,iele6,iele7,iele8)
!$cdir parallel do &
!$cdir& private(iele1,iele2,iele3,iele4,iele5,iele6,iele7,iele8)
!poption parallel
        do iele = 1, nele_q
          iele1 = 8*(iele-1) + 1
          iele2 = 8*(iele-1) + 2
          iele3 = 8*(iele-1) + 3
          iele4 = 8*(iele-1) + 4
          iele5 = 8*(iele-1) + 5
          iele6 = 8*(iele-1) + 6
          iele7 = 8*(iele-1) + 7
          iele8 = 8*(iele-1) + 8
!
          ie_l(iele1,1) = ie_q27(iele,ie27(1,1))
          ie_l(iele1,2) = ie_q27(iele,ie27(2,1))
          ie_l(iele1,3) = ie_q27(iele,ie27(3,1))
          ie_l(iele1,4) = ie_q27(iele,ie27(4,1))
          ie_l(iele1,5) = ie_q27(iele,ie27(5,1))
          ie_l(iele1,6) = ie_q27(iele,ie27(6,1))
          ie_l(iele1,7) = ie_q27(iele,ie27(7,1))
          ie_l(iele1,8) = ie_q27(iele,ie27(8,1))
!
          ie_l(iele2,1) = ie_q27(iele,ie27(1,2))
          ie_l(iele2,2) = ie_q27(iele,ie27(2,2))
          ie_l(iele2,3) = ie_q27(iele,ie27(3,2))
          ie_l(iele2,4) = ie_q27(iele,ie27(4,2))
          ie_l(iele2,5) = ie_q27(iele,ie27(5,2))
          ie_l(iele2,6) = ie_q27(iele,ie27(6,2))
          ie_l(iele2,7) = ie_q27(iele,ie27(7,2))
          ie_l(iele2,8) = ie_q27(iele,ie27(8,2))
!
          ie_l(iele3,1) = ie_q27(iele,ie27(1,3))
          ie_l(iele3,2) = ie_q27(iele,ie27(2,3))
          ie_l(iele3,3) = ie_q27(iele,ie27(3,3))
          ie_l(iele3,4) = ie_q27(iele,ie27(4,3))
          ie_l(iele3,5) = ie_q27(iele,ie27(5,3))
          ie_l(iele3,6) = ie_q27(iele,ie27(6,3))
          ie_l(iele3,7) = ie_q27(iele,ie27(7,3))
          ie_l(iele3,8) = ie_q27(iele,ie27(8,3))
!
          ie_l(iele4,1) = ie_q27(iele,ie27(1,4))
          ie_l(iele4,2) = ie_q27(iele,ie27(2,4))
          ie_l(iele4,3) = ie_q27(iele,ie27(3,4))
          ie_l(iele4,4) = ie_q27(iele,ie27(4,4))
          ie_l(iele4,5) = ie_q27(iele,ie27(5,4))
          ie_l(iele4,6) = ie_q27(iele,ie27(6,4))
          ie_l(iele4,7) = ie_q27(iele,ie27(7,4))
          ie_l(iele4,8) = ie_q27(iele,ie27(8,4))
!
          ie_l(iele5,1) = ie_q27(iele,ie27(1,5))
          ie_l(iele5,2) = ie_q27(iele,ie27(2,5))
          ie_l(iele5,3) = ie_q27(iele,ie27(3,5))
          ie_l(iele5,4) = ie_q27(iele,ie27(4,5))
          ie_l(iele5,5) = ie_q27(iele,ie27(5,5))
          ie_l(iele5,6) = ie_q27(iele,ie27(6,5))
          ie_l(iele5,7) = ie_q27(iele,ie27(7,5))
          ie_l(iele5,8) = ie_q27(iele,ie27(8,5))
!
          ie_l(iele6,1) = ie_q27(iele,ie27(1,6))
          ie_l(iele6,2) = ie_q27(iele,ie27(2,6))
          ie_l(iele6,3) = ie_q27(iele,ie27(3,6))
          ie_l(iele6,4) = ie_q27(iele,ie27(4,6))
          ie_l(iele6,5) = ie_q27(iele,ie27(5,6))
          ie_l(iele6,6) = ie_q27(iele,ie27(6,6))
          ie_l(iele6,7) = ie_q27(iele,ie27(7,6))
          ie_l(iele6,8) = ie_q27(iele,ie27(8,6))
!
          ie_l(iele7,1) = ie_q27(iele,ie27(1,7))
          ie_l(iele7,2) = ie_q27(iele,ie27(2,7))
          ie_l(iele7,3) = ie_q27(iele,ie27(3,7))
          ie_l(iele7,4) = ie_q27(iele,ie27(4,7))
          ie_l(iele7,5) = ie_q27(iele,ie27(5,7))
          ie_l(iele7,6) = ie_q27(iele,ie27(6,7))
          ie_l(iele7,7) = ie_q27(iele,ie27(7,7))
          ie_l(iele7,8) = ie_q27(iele,ie27(8,7))
!
          ie_l(iele8,1) = ie_q27(iele,ie27(1,8))
          ie_l(iele8,2) = ie_q27(iele,ie27(2,8))
          ie_l(iele8,3) = ie_q27(iele,ie27(3,8))
          ie_l(iele8,4) = ie_q27(iele,ie27(4,8))
          ie_l(iele8,5) = ie_q27(iele,ie27(5,8))
          ie_l(iele8,6) = ie_q27(iele,ie27(6,8))
          ie_l(iele8,7) = ie_q27(iele,ie27(7,8))
          ie_l(iele8,8) = ie_q27(iele,ie27(8,8))
        end do
!$omp end parallel do
!
      end subroutine set_27quad_2_8x8linear
!
!  ---------------------------------------------------------------------
!
      subroutine gen_connect_quad27_from_quad20(nnod332, nele332,       &
     &          nsurf332, ie_332, isf_ele_332, ie_333)
!
      integer(kind = kint), intent(in) :: nnod332, nele332, nsurf332
      integer(kind = kint), intent(in) :: ie_332(nele332,20)
      integer(kind = kint), intent(in) :: isf_ele_332(nele332,6)
!
      integer(kind = kint), intent(inout) :: ie_333(nele332,27)
!
      integer(kind = kint) :: iele
!
!
!$omp parallel do
        do iele = 1, nele332
          ie_333(iele, 1) = ie_332(iele, 1)
          ie_333(iele, 2) = ie_332(iele, 2)
          ie_333(iele, 3) = ie_332(iele, 3)
          ie_333(iele, 4) = ie_332(iele, 4)
          ie_333(iele, 5) = ie_332(iele, 5)
          ie_333(iele, 6) = ie_332(iele, 6)
          ie_333(iele, 7) = ie_332(iele, 7)
          ie_333(iele, 8) = ie_332(iele, 8)
          ie_333(iele, 9) = ie_332(iele, 9)
          ie_333(iele,10) = ie_332(iele,10)
          ie_333(iele,11) = ie_332(iele,11)
          ie_333(iele,12) = ie_332(iele,12)
          ie_333(iele,13) = ie_332(iele,13)
          ie_333(iele,14) = ie_332(iele,14)
          ie_333(iele,15) = ie_332(iele,15)
          ie_333(iele,16) = ie_332(iele,16)
          ie_333(iele,17) = ie_332(iele,17)
          ie_333(iele,18) = ie_332(iele,18)
          ie_333(iele,19) = ie_332(iele,19)
          ie_333(iele,20) = ie_332(iele,20)
          ie_333(iele,21) = abs(isf_ele_332(iele,1)) + nnod332
          ie_333(iele,22) = abs(isf_ele_332(iele,2)) + nnod332
          ie_333(iele,23) = abs(isf_ele_332(iele,3)) + nnod332
          ie_333(iele,24) = abs(isf_ele_332(iele,4)) + nnod332
          ie_333(iele,25) = abs(isf_ele_332(iele,5)) + nnod332
          ie_333(iele,26) = abs(isf_ele_332(iele,6)) + nnod332
          ie_333(iele,27) = iele + nnod332 + nsurf332
        end do
!$omp end parallel do
!
      end subroutine gen_connect_quad27_from_quad20
!
!  ---------------------------------------------------------------------
!
      end module m_27quad_2_8x8linear
