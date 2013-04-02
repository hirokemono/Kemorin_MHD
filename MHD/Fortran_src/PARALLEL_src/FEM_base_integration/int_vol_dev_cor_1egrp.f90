!int_vol_dev_cor_1egrp.f90
!      module int_vol_dev_cor_1egrp
!
!     Written by H. Matsui on Aug., 2007
!     Modified by H. Matsui on Nov., 2008
!
!      subroutine int_vol_dev_cor_1egrp_l(numnod, numele, ie, e_multi,  &
!     &          nitem_grp, iele_grp, num_int, ntot_int_3d, xjac, an,   &
!     &          d1_nod, d2_nod, ave_1, ave_2, sig_1, sig_2, cor_l)
!      subroutine int_vol_dev_cor_1egrp_q(numnod, numele, ie, e_multi,  &
!     &          nitem_grp, iele_grp, num_int, ntot_int_3d, xjac, aw,   &
!     &          d1_nod, d2_nod, ave_1, ave_2, sig_1, sig_2, cor_l)
!
      module int_vol_dev_cor_1egrp
!
      use m_precision
      use m_constants
      use m_geometry_constants
      use m_fem_gauss_int_coefs
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_dev_cor_1egrp_l(numnod, numele, ie, e_multi,   &
     &          nitem_grp, iele_grp, num_int, ntot_int_3d, xjac, an,    &
     &          d1_nod, d2_nod, ave_1, ave_2, sig_1, sig_2, cor_l)
!
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,num_t_linear)
      real (kind = kreal), intent(in) :: e_multi(numele)
!
      integer (kind = kint), intent(in) :: nitem_grp
      integer (kind = kint), intent(in) :: iele_grp(nitem_grp)
!
      integer (kind = kint), intent(in) :: ntot_int_3d, num_int
      real(kind=kreal),   intent(in) :: xjac(numele, ntot_int_3d)
      real(kind=kreal),   intent(in) :: an(num_t_linear, ntot_int_3d)
!
      real(kind = kreal), intent(in) :: d1_nod(numnod), d2_nod(numnod)
      real(kind = kreal), intent(in) :: ave_1, ave_2
      real(kind = kreal), intent(inout) :: sig_1, sig_2, cor_l
!
      integer (kind = kint) :: iele, inum
      integer (kind = kint) :: ii, ix
      integer (kind = kint) :: i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
!
!
      sig_1 = zero
      sig_2 = zero
      cor_l = zero
      if(nitem_grp .le. izero) return
!
      do ii= 1, num_int * num_int * num_int 
        ix = int_start3(num_int) + ii
!
!$cdir nodep
        do inum = 1, nitem_grp
          iele = iele_grp(inum)
!
          i1 =  ie(iele, 1)
          i2 =  ie(iele, 2)
          i3 =  ie(iele, 3)
          i4 =  ie(iele, 4)
          i5 =  ie(iele, 5)
          i6 =  ie(iele, 6)
          i7 =  ie(iele, 7)
          i8 =  ie(iele, 8)
!
          sig_1 = sig_1                                                 &
     &            + (an(1, ix) * (d1_nod(i1 ) - ave_1)**2               &
     &             + an(2, ix) * (d1_nod(i2 ) - ave_1)**2               &
     &             + an(3, ix) * (d1_nod(i3 ) - ave_1)**2               &
     &             + an(4, ix) * (d1_nod(i4 ) - ave_1)**2               &
     &             + an(5, ix) * (d1_nod(i5 ) - ave_1)**2               &
     &             + an(6, ix) * (d1_nod(i6 ) - ave_1)**2               &
     &             + an(7, ix) * (d1_nod(i7 ) - ave_1)**2               &
     &             + an(8, ix) * (d1_nod(i8 ) - ave_1)**2 )             &
     &            * e_multi(iele) * xjac(iele,ix) * owe3d(ix)
!
          sig_2 = sig_2                                                 &
     &            + (an(1, ix) * (d2_nod(i1 ) - ave_2)**2               &
     &             + an(2, ix) * (d2_nod(i2 ) - ave_2)**2               &
     &             + an(3, ix) * (d2_nod(i3 ) - ave_2)**2               &
     &             + an(4, ix) * (d2_nod(i4 ) - ave_2)**2               &
     &             + an(5, ix) * (d2_nod(i5 ) - ave_2)**2               &
     &             + an(6, ix) * (d2_nod(i6 ) - ave_2)**2               &
     &             + an(7, ix) * (d2_nod(i7 ) - ave_2)**2               &
     &             + an(8, ix) * (d2_nod(i8 ) - ave_2)**2 )             &
     &            * e_multi(iele) * xjac(iele,ix) * owe3d(ix)
!
          cor_l = cor_l                                                 &
     &            + (an(1, ix) * (d1_nod(i1 ) - ave_1)                  &
     &                         * (d2_nod(i2 ) - ave_2)                  &
     &             + an(2, ix) * (d1_nod(i2 ) - ave_1)                  &
     &                         * (d2_nod(i2 ) - ave_2)                  &
     &             + an(3, ix) * (d1_nod(i3 ) - ave_1)                  &
     &                         * (d2_nod(i3 ) - ave_2)                  &
     &             + an(4, ix) * (d1_nod(i4 ) - ave_1)                  &
     &                         * (d2_nod(i4 ) - ave_2)                  &
     &             + an(5, ix) * (d1_nod(i5 ) - ave_1)                  &
     &                         * (d2_nod(i5 ) - ave_2)                  &
     &             + an(6, ix) * (d1_nod(i6 ) - ave_1)                  &
     &                         * (d2_nod(i6 ) - ave_2)                  &
     &             + an(7, ix) * (d1_nod(i7 ) - ave_1)                  &
     &                         * (d2_nod(i7 ) - ave_2)                  &
     &             + an(8, ix) * (d1_nod(i8 ) - ave_1)                  &
     &                         * (d2_nod(i8 ) - ave_2)    )             &
     &            * e_multi(iele) * xjac(iele,ix) * owe3d(ix)
        end do
      end do
!
      end subroutine int_vol_dev_cor_1egrp_l
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_dev_cor_1egrp_q(numnod, numele, ie, e_multi,   &
     &          nitem_grp, iele_grp, num_int, ntot_int_3d, xjac, aw,    &
     &          d1_nod, d2_nod, ave_1, ave_2, sig_1, sig_2, cor_l)
!
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,num_t_quad)
      real (kind = kreal), intent(in) :: e_multi(numele)
!
      integer (kind = kint), intent(in) :: nitem_grp
      integer (kind = kint), intent(in) :: iele_grp(nitem_grp)
!
      integer (kind = kint), intent(in) :: ntot_int_3d, num_int
      real(kind=kreal),   intent(in) :: xjac(numele, ntot_int_3d)
      real(kind=kreal),   intent(in) :: aw(num_t_quad, ntot_int_3d)
!
      real(kind = kreal), intent(in) :: d1_nod(numnod), d2_nod(numnod)
      real(kind = kreal), intent(in) :: ave_1, ave_2
      real(kind = kreal), intent(inout) :: sig_1, sig_2, cor_l
!
      integer (kind = kint) :: iele, inum
      integer (kind = kint) :: ii, ix
      integer (kind = kint) :: i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
      integer (kind = kint) :: i9,  i10, i11, i12, i13, i14, i15, i16
      integer (kind = kint) :: i17, i18, i19, i20
!
!
      sig_1 = zero
      sig_2 = zero
      cor_l = zero
      if(nitem_grp .le. izero) return
!
      do ii= 1, num_int * num_int * num_int 
        ix = int_start3(num_int) + ii
!
!$cdir nodep
        do inum = 1, nitem_grp
          iele = iele_grp(inum)
!
          i1 =  ie(iele, 1)
          i2 =  ie(iele, 2)
          i3 =  ie(iele, 3)
          i4 =  ie(iele, 4)
          i5 =  ie(iele, 5)
          i6 =  ie(iele, 6)
          i7 =  ie(iele, 7)
          i8 =  ie(iele, 8)
          i9 =  ie(iele, 9)
          i10 = ie(iele,10)
          i11 = ie(iele,11)
          i12 = ie(iele,12)
          i13 = ie(iele,13)
          i14 = ie(iele,14)
          i15 = ie(iele,15)
          i16 = ie(iele,16)
          i17 = ie(iele,17)
          i18 = ie(iele,18)
          i19 = ie(iele,19)
          i20 = ie(iele,20)
!
          sig_1 = sig_1                                                 &
     &            + (aw(1, ix) * (d1_nod(i1 ) - ave_1)**2               &
     &             + aw(2, ix) * (d1_nod(i2 ) - ave_1)**2               &
     &             + aw(3, ix) * (d1_nod(i3 ) - ave_1)**2               &
     &             + aw(4, ix) * (d1_nod(i4 ) - ave_1)**2               &
     &             + aw(5, ix) * (d1_nod(i5 ) - ave_1)**2               &
     &             + aw(6, ix) * (d1_nod(i6 ) - ave_1)**2               &
     &             + aw(7, ix) * (d1_nod(i7 ) - ave_1)**2               &
     &             + aw(8, ix) * (d1_nod(i8 ) - ave_1)**2               &
     &             + aw(9, ix) * (d1_nod(i9 ) - ave_1)**2               &
     &             + aw(10,ix) * (d1_nod(i10) - ave_1)**2               &
     &             + aw(11,ix) * (d1_nod(i11) - ave_1)**2               &
     &             + aw(12,ix) * (d1_nod(i12) - ave_1)**2               &
     &             + aw(13,ix) * (d1_nod(i13) - ave_1)**2               &
     &             + aw(14,ix) * (d1_nod(i14) - ave_1)**2               &
     &             + aw(15,ix) * (d1_nod(i15) - ave_1)**2               &
     &             + aw(16,ix) * (d1_nod(i16) - ave_1)**2               &
     &             + aw(17,ix) * (d1_nod(i17) - ave_1)**2               &
     &             + aw(18,ix) * (d1_nod(i18) - ave_1)**2               &
     &             + aw(19,ix) * (d1_nod(i19) - ave_1)**2               &
     &             + aw(20,ix) * (d1_nod(i20) - ave_1)**2 )             &
     &            * e_multi(iele) * xjac(iele,ix) * owe3d(ix)
!
          sig_2 = sig_2                                                 &
     &            + (aw(1, ix) * (d2_nod(i1 ) - ave_2)**2               &
     &             + aw(2, ix) * (d2_nod(i2 ) - ave_2)**2               &
     &             + aw(3, ix) * (d2_nod(i3 ) - ave_2)**2               &
     &             + aw(4, ix) * (d2_nod(i4 ) - ave_2)**2               &
     &             + aw(5, ix) * (d2_nod(i5 ) - ave_2)**2               &
     &             + aw(6, ix) * (d2_nod(i6 ) - ave_2)**2               &
     &             + aw(7, ix) * (d2_nod(i7 ) - ave_2)**2               &
     &             + aw(8, ix) * (d2_nod(i8 ) - ave_2)**2               &
     &             + aw(9, ix) * (d2_nod(i9 ) - ave_2)**2               &
     &             + aw(10,ix) * (d2_nod(i10) - ave_2)**2               &
     &             + aw(11,ix) * (d2_nod(i11) - ave_2)**2               &
     &             + aw(12,ix) * (d2_nod(i12) - ave_2)**2               &
     &             + aw(13,ix) * (d2_nod(i13) - ave_2)**2               &
     &             + aw(14,ix) * (d2_nod(i14) - ave_2)**2               &
     &             + aw(15,ix) * (d2_nod(i15) - ave_2)**2               &
     &             + aw(16,ix) * (d2_nod(i16) - ave_2)**2               &
     &             + aw(17,ix) * (d2_nod(i17) - ave_2)**2               &
     &             + aw(18,ix) * (d2_nod(i18) - ave_2)**2               &
     &             + aw(19,ix) * (d2_nod(i19) - ave_2)**2               &
     &             + aw(20,ix) * (d2_nod(i20) - ave_2)**2 )             &
     &            * e_multi(iele) * xjac(iele,ix) * owe3d(ix)
!
          cor_l = cor_l                                                 &
     &            + (aw(1, ix) * (d1_nod(i1 ) - ave_1)                  &
     &                         * (d2_nod(i2 ) - ave_2)                  &
     &             + aw(2, ix) * (d1_nod(i2 ) - ave_1)                  &
     &                         * (d2_nod(i2 ) - ave_2)                  &
     &             + aw(3, ix) * (d1_nod(i3 ) - ave_1)                  &
     &                         * (d2_nod(i3 ) - ave_2)                  &
     &             + aw(4, ix) * (d1_nod(i4 ) - ave_1)                  &
     &                         * (d2_nod(i4 ) - ave_2)                  &
     &             + aw(5, ix) * (d1_nod(i5 ) - ave_1)                  &
     &                         * (d2_nod(i5 ) - ave_2)                  &
     &             + aw(6, ix) * (d1_nod(i6 ) - ave_1)                  &
     &                         * (d2_nod(i6 ) - ave_2)                  &
     &             + aw(7, ix) * (d1_nod(i7 ) - ave_1)                  &
     &                         * (d2_nod(i7 ) - ave_2)                  &
     &             + aw(8, ix) * (d1_nod(i8 ) - ave_1)                  &
     &                         * (d2_nod(i8 ) - ave_2)                  &
     &             + aw(9, ix) * (d1_nod(i9 ) - ave_1)                  &
     &                         * (d2_nod(i9 ) - ave_2)                  &
     &             + aw(10,ix) * (d1_nod(i10) - ave_1)                  &
     &                         * (d2_nod(i10) - ave_2)                  &
     &             + aw(11,ix) * (d1_nod(i11) - ave_1)                  &
     &                         * (d2_nod(i11) - ave_2)                  &
     &             + aw(12,ix) * (d1_nod(i12) - ave_1)                  &
     &                         * (d2_nod(i12) - ave_2)                  &
     &             + aw(13,ix) * (d1_nod(i13) - ave_1)                  &
     &                         * (d2_nod(i13) - ave_2)                  &
     &             + aw(14,ix) * (d1_nod(i14) - ave_1)                  &
     &                         * (d2_nod(i14) - ave_2)                  &
     &             + aw(15,ix) * (d1_nod(i15) - ave_1)                  &
     &                         * (d2_nod(i15) - ave_2)                  &
     &             + aw(16,ix) * (d1_nod(i16) - ave_1)                  &
     &                         * (d2_nod(i16) - ave_2)                  &
     &             + aw(17,ix) * (d1_nod(i17) - ave_1)                  &
     &                         * (d2_nod(i17) - ave_2)                  &
     &             + aw(18,ix) * (d1_nod(i18) - ave_1)                  &
     &                         * (d2_nod(i18) - ave_2)                  &
     &             + aw(19,ix) * (d1_nod(i19) - ave_1)                  &
     &                         * (d2_nod(i19) - ave_2)                  &
     &             + aw(20,ix) * (d1_nod(i20) - ave_1)                  &
     &                         * (d2_nod(i20) - ave_2)    )             &
     &            * e_multi(iele) * xjac(iele,ix) * owe3d(ix)
        end do
      end do
!
      end subroutine int_vol_dev_cor_1egrp_q
!
!  ---------------------------------------------------------------------
!
      end module int_vol_dev_cor_1egrp
