!int_vol_2rms_ave_1egrp.f90
!      module int_vol_2rms_ave_1egrp
!
!     Written by H. Matsui on Aug., 2007
!     Modified by H. Matsui on Nov., 2008
!     Modified by H. Matsui on June, 2011
!
!!      subroutine int_vol_2rms_ave_1egrp_l                             &
!!     &         (numnod, numele, ie, interior_ele, nitem_grp, iele_grp,&
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          num_int, ntot_int_3d, xjac, an, d1_nod, d2_nod,       &
!!     &          ave_1, rms_1, ave_2, rms_2)
!!      subroutine int_vol_2rms_ave_1egrp_q                             &
!!     &         (numnod, numele, ie, interior_ele, nitem_grp, iele_grp,&
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          num_int, ntot_int_3d, xjac, aw, d1_nod, d2_nod,       &
!!     &          ave_1, rms_1, ave_2, rms_2)
!
      module int_vol_2rms_ave_1egrp
!
      use m_precision
      use m_constants
      use m_geometry_constants
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_2rms_ave_1egrp_l                               &
     &         (numnod, numele, ie, interior_ele, nitem_grp, iele_grp,  &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          num_int, ntot_int_3d, xjac, an, d1_nod, d2_nod,         &
     &          ave_1, rms_1, ave_2, rms_2)
!
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,num_t_linear)
      integer (kind = kint), intent(in) :: interior_ele(numele)
!
      integer (kind = kint), intent(in) :: nitem_grp
      integer (kind = kint), intent(in) :: iele_grp(nitem_grp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer (kind = kint), intent(in) :: ntot_int_3d, num_int
      real(kind=kreal),   intent(in) :: xjac(numele, ntot_int_3d)
      real(kind=kreal),   intent(in) :: an(num_t_linear, ntot_int_3d)
!
      real(kind = kreal), intent(in) :: d1_nod(numnod), d2_nod(numnod)
      real(kind = kreal), intent(inout) :: ave_1, rms_1, ave_2, rms_2
!
      integer (kind = kint) :: iele, inum
      integer (kind = kint) :: ii, ix
      integer (kind = kint) :: i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
!
!
      ave_1 = zero
      rms_1 = zero
      ave_2 = zero
      rms_2 = zero
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
          ave_1 = ave_1                                                 &
     &          + (an(1, ix)*d1_nod(i1 ) + an(2, ix)*d1_nod(i2 )        &
     &           + an(3, ix)*d1_nod(i3 ) + an(4, ix)*d1_nod(i4 )        &
     &           + an(5, ix)*d1_nod(i5 ) + an(6, ix)*d1_nod(i6 )        &
     &           + an(7, ix)*d1_nod(i7 ) + an(8, ix)*d1_nod(i8 ))       &
     &         * dble(interior_ele(iele)) * xjac(iele,ix) * owe3d(ix)
!
          rms_1 = rms_1                                                 &
     &          + (an(1, ix)*d1_nod(i1 )**2 + an(2, ix)*d1_nod(i2 )**2  &
     &           + an(3, ix)*d1_nod(i3 )**2 + an(4, ix)*d1_nod(i4 )**2  &
     &           + an(5, ix)*d1_nod(i5 )**2 + an(6, ix)*d1_nod(i6 )**2  &
     &           + an(7, ix)*d1_nod(i7 )**2 + an(8, ix)*d1_nod(i8 )**2) &
     &         * dble(interior_ele(iele)) * xjac(iele,ix) * owe3d(ix)
!
          ave_2 = ave_2                                                 &
     &          + (an(1, ix)*d2_nod(i1 ) + an(2, ix)*d2_nod(i2 )        &
     &           + an(3, ix)*d2_nod(i3 ) + an(4, ix)*d2_nod(i4 )        &
     &           + an(5, ix)*d2_nod(i5 ) + an(6, ix)*d2_nod(i6 )        &
     &           + an(7, ix)*d2_nod(i7 ) + an(8, ix)*d2_nod(i8 ))       &
     &         * dble(interior_ele(iele)) * xjac(iele,ix) * owe3d(ix)
!
          rms_2 = rms_2                                                 &
     &          + (an(1, ix)*d2_nod(i1 )**2 + an(2, ix)*d2_nod(i2 )**2  &
     &           + an(3, ix)*d2_nod(i3 )**2 + an(4, ix)*d2_nod(i4 )**2  &
     &           + an(5, ix)*d2_nod(i5 )**2 + an(6, ix)*d2_nod(i6 )**2  &
     &           + an(7, ix)*d2_nod(i7 )**2 + an(8, ix)*d2_nod(i8 )**2) &
     &         * dble(interior_ele(iele)) * xjac(iele,ix) * owe3d(ix)
        end do
      end do
!
      end subroutine int_vol_2rms_ave_1egrp_l
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_2rms_ave_1egrp_q                               &
     &         (numnod, numele, ie, interior_ele, nitem_grp, iele_grp,  &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          num_int, ntot_int_3d, xjac, aw, d1_nod, d2_nod,         &
     &          ave_1, rms_1, ave_2, rms_2)
!
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,num_t_quad)
      integer (kind = kint), intent(in) :: interior_ele(numele)
!
      integer (kind = kint), intent(in) :: nitem_grp
      integer (kind = kint), intent(in) :: iele_grp(nitem_grp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer (kind = kint), intent(in) :: ntot_int_3d, num_int
      real(kind=kreal),   intent(in) :: xjac(numele, ntot_int_3d)
      real(kind=kreal),   intent(in) :: aw(num_t_quad, ntot_int_3d)
!
      real(kind = kreal), intent(in) :: d1_nod(numnod), d2_nod(numnod)
      real(kind = kreal), intent(inout) :: ave_1, rms_1, ave_2, rms_2
!
      integer (kind = kint) :: iele, inum
      integer (kind = kint) :: ii, ix
      integer (kind = kint) :: i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
      integer (kind = kint) :: i9,  i10, i11, i12, i13, i14, i15, i16
      integer (kind = kint) :: i17, i18, i19, i20
!
!
      ave_1 = zero
      rms_1 = zero
      ave_2 = zero
      rms_2 = zero
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
          ave_1 = ave_1                                                 &
     &          + (aw(1, ix)*d1_nod(i1 ) + aw(2, ix)*d1_nod(i2 )        &
     &           + aw(3, ix)*d1_nod(i3 ) + aw(4, ix)*d1_nod(i4 )        &
     &           + aw(5, ix)*d1_nod(i5 ) + aw(6, ix)*d1_nod(i6 )        &
     &           + aw(7, ix)*d1_nod(i7 ) + aw(8, ix)*d1_nod(i8 )        &
     &           + aw(9, ix)*d1_nod(i9 ) + aw(10,ix)*d1_nod(i10)        &
     &           + aw(11,ix)*d1_nod(i11) + aw(12,ix)*d1_nod(i12)        &
     &           + aw(13,ix)*d1_nod(i13) + aw(14,ix)*d1_nod(i14)        &
     &           + aw(15,ix)*d1_nod(i15) + aw(16,ix)*d1_nod(i16)        &
     &           + aw(17,ix)*d1_nod(i17) + aw(18,ix)*d1_nod(i18)        &
     &           + aw(19,ix)*d1_nod(i19) + aw(20,ix)*d1_nod(i20))       &
     &         * dble(interior_ele(iele)) * xjac(iele,ix) * owe3d(ix)
!
          rms_1 = rms_1                                                 &
     &          + (aw(1, ix)*d1_nod(i1 )**2 + aw(2, ix)*d1_nod(i2 )**2  &
     &           + aw(3, ix)*d1_nod(i3 )**2 + aw(4, ix)*d1_nod(i4 )**2  &
     &           + aw(5, ix)*d1_nod(i5 )**2 + aw(6, ix)*d1_nod(i6 )**2  &
     &           + aw(7, ix)*d1_nod(i7 )**2 + aw(8, ix)*d1_nod(i8 )**2  &
     &           + aw(9, ix)*d1_nod(i9 )**2 + aw(10,ix)*d1_nod(i10)**2  &
     &           + aw(11,ix)*d1_nod(i11)**2 + aw(12,ix)*d1_nod(i12)**2  &
     &           + aw(13,ix)*d1_nod(i13)**2 + aw(14,ix)*d1_nod(i14)**2  &
     &           + aw(15,ix)*d1_nod(i15)**2 + aw(16,ix)*d1_nod(i16)**2  &
     &           + aw(17,ix)*d1_nod(i17)**2 + aw(18,ix)*d1_nod(i18)**2  &
     &           + aw(19,ix)*d1_nod(i19)**2 + aw(20,ix)*d1_nod(i20)**2) &
     &         * dble(interior_ele(iele)) * xjac(iele,ix) * owe3d(ix)
!
          ave_2 = ave_2                                                 &
     &          + (aw(1, ix)*d2_nod(i1 ) + aw(2, ix)*d2_nod(i2 )        &
     &           + aw(3, ix)*d2_nod(i3 ) + aw(4, ix)*d2_nod(i4 )        &
     &           + aw(5, ix)*d2_nod(i5 ) + aw(6, ix)*d2_nod(i6 )        &
     &           + aw(7, ix)*d2_nod(i7 ) + aw(8, ix)*d2_nod(i8 )        &
     &           + aw(9, ix)*d2_nod(i9 ) + aw(10,ix)*d2_nod(i10)        &
     &           + aw(11,ix)*d2_nod(i11) + aw(12,ix)*d2_nod(i12)        &
     &           + aw(13,ix)*d2_nod(i13) + aw(14,ix)*d2_nod(i14)        &
     &           + aw(15,ix)*d2_nod(i15) + aw(16,ix)*d2_nod(i16)        &
     &           + aw(17,ix)*d2_nod(i17) + aw(18,ix)*d2_nod(i18)        &
     &           + aw(19,ix)*d2_nod(i19) + aw(20,ix)*d2_nod(i20))       &
     &         * dble(interior_ele(iele)) * xjac(iele,ix) * owe3d(ix)
!
          rms_2 = rms_2                                                 &
     &          + (aw(1, ix)*d2_nod(i1 )**2 + aw(2, ix)*d2_nod(i2 )**2  &
     &           + aw(3, ix)*d2_nod(i3 )**2 + aw(4, ix)*d2_nod(i4 )**2  &
     &           + aw(5, ix)*d2_nod(i5 )**2 + aw(6, ix)*d2_nod(i6 )**2  &
     &           + aw(7, ix)*d2_nod(i7 )**2 + aw(8, ix)*d2_nod(i8 )**2  &
     &           + aw(9, ix)*d2_nod(i9 )**2 + aw(10,ix)*d2_nod(i10)**2  &
     &           + aw(11,ix)*d2_nod(i11)**2 + aw(12,ix)*d2_nod(i12)**2  &
     &           + aw(13,ix)*d2_nod(i13)**2 + aw(14,ix)*d2_nod(i14)**2  &
     &           + aw(15,ix)*d2_nod(i15)**2 + aw(16,ix)*d2_nod(i16)**2  &
     &           + aw(17,ix)*d2_nod(i17)**2 + aw(18,ix)*d2_nod(i18)**2  &
     &           + aw(19,ix)*d2_nod(i19)**2 + aw(20,ix)*d2_nod(i20)**2) &
     &         * dble(interior_ele(iele)) * xjac(iele,ix) * owe3d(ix)
        end do
      end do
!
      end subroutine int_vol_2rms_ave_1egrp_q
!
!  ---------------------------------------------------------------------
!
      end module int_vol_2rms_ave_1egrp
