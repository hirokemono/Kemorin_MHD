!
!      module set_int_point_position
!
!      Written by H. Matsui on Nov., 2006
!
!      subroutine s_set_int_point_position(nele_grp, iele_grp, an,      &
!     &          xx_int, yy_int, zz_int)
!
!      subroutine set_int_point_position_27(nele_grp, iele_grp, an,     &
!     &          xx_int, yy_int, zz_int)
!      subroutine set_int_point_position_20(nele_grp, iele_grp, an,     &
!     &          xx_int, yy_int, zz_int)
!      subroutine set_int_point_position_8(nele_grp, iele_grp, an,      &
!     &          xx_int, yy_int, zz_int)
!
      module set_int_point_position
!
      use m_precision
!
      use m_geometry_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_int_point_position(nele_grp, iele_grp, an,       &
     &          xx_int, yy_int, zz_int)
!
      use m_geometry_constants
      use m_geometry_data
!
      integer(kind = kint), intent(in) :: nele_grp
      integer(kind = kint), intent(in) :: iele_grp(nele_grp)
      real(kind = kreal), intent(in) :: an(ele1%nnod_4_ele)
!
      real(kind = kreal), intent(inout) :: xx_int(nele_grp)
      real(kind = kreal), intent(inout) :: yy_int(nele_grp)
      real(kind = kreal), intent(inout) :: zz_int(nele_grp)
!
!
      if      (ele1%nnod_4_ele .eq. num_t_lag) then
        call set_int_point_position_27(nele_grp, iele_grp, an,          &
     &          xx_int, yy_int, zz_int)
      else if (ele1%nnod_4_ele .eq. num_t_quad) then
        call set_int_point_position_20(nele_grp, iele_grp, an,          &
     &          xx_int, yy_int, zz_int)
      else if (ele1%nnod_4_ele .eq. num_t_linear) then
        call set_int_point_position_8(nele_grp, iele_grp, an,           &
     &          xx_int, yy_int, zz_int)
      end if
!
      end subroutine s_set_int_point_position
!
! ----------------------------------------------------------------------
!
      subroutine set_int_point_position_27(nele_grp, iele_grp, an,      &
     &          xx_int, yy_int, zz_int)
!
      use m_geometry_data
!
      integer(kind = kint), intent(in) :: nele_grp
      integer(kind = kint), intent(in) :: iele_grp(nele_grp)
      real(kind = kreal), intent(in) :: an(27)
!
      real(kind = kreal), intent(inout) :: xx_int(nele_grp)
      real(kind = kreal), intent(inout) :: yy_int(nele_grp)
      real(kind = kreal), intent(inout) :: zz_int(nele_grp)
!
      integer(kind = kint) :: inum, iele
      integer(kind = kint) :: i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
      integer(kind = kint) :: i9,  i10, i11, i12, i13, i14, i15, i16
      integer(kind = kint) :: i17, i18, i19, i20, i21, i22, i23, i24
      integer(kind = kint) :: i25, i26, i27
!
!
!$omp  parallel do                                                      &
!$omp& private(iele,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15, &
!$omp&         i16,i17,i18,i19,i20,i21,i22,i23,i24,i25,i26,i27)
        do inum = 1, nele_grp
!
          iele = iele_grp(inum)
!
          i1  = ie(iele, 1)
          i2  = ie(iele, 2)
          i3  = ie(iele, 3)
          i4  = ie(iele, 4)
          i5  = ie(iele, 5)
          i6  = ie(iele, 6)
          i7  = ie(iele, 7)
          i8  = ie(iele, 8)
          i9  = ie(iele, 9)
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
          i21 = ie(iele,21)
          i22 = ie(iele,22)
          i23 = ie(iele,23)
          i24 = ie(iele,24)
          i25 = ie(iele,25)
          i26 = ie(iele,26)
          i27 = ie(iele,27)
!
          xx_int(inum) =  xx(i1, 1)*an(1) + xx(i2, 1)*an(2)             &
     &                  + xx(i3, 1)*an(3) + xx(i4, 1)*an(4)             &
     &                  + xx(i5, 1)*an(5) + xx(i6, 1)*an(6)             &
     &                  + xx(i7, 1)*an(7) + xx(i8, 1)*an(8)             &
     &                  + xx(i9, 1)*an(9) + xx(i10,1)*an(10)            &
     &                  + xx(i11,1)*an(11) + xx(i12,1)*an(12)           &
     &                  + xx(i13,1)*an(13) + xx(i14,1)*an(14)           &
     &                  + xx(i15,1)*an(15) + xx(i16,1)*an(16)           &
     &                  + xx(i17,1)*an(17) + xx(i18,1)*an(18)           &
     &                  + xx(i19,1)*an(19) + xx(i20,1)*an(20)           &
     &                  + xx(i21,1)*an(21) + xx(i22,1)*an(22)           &
     &                  + xx(i23,1)*an(23) + xx(i24,1)*an(24)           &
     &                  + xx(i25,1)*an(25) + xx(i26,1)*an(26)           &
     &                  + xx(i27,1)*an(27)
!
          yy_int(inum) =  xx(i1, 2)*an(1) + xx(i2, 2)*an(2)             &
     &                  + xx(i3, 2)*an(3) + xx(i4, 2)*an(4)             &
     &                  + xx(i5, 2)*an(5) + xx(i6, 2)*an(6)             &
     &                  + xx(i7, 2)*an(7) + xx(i8, 2)*an(8)             &
     &                  + xx(i9, 2)*an(9) + xx(i10,2)*an(10)            &
     &                  + xx(i11,2)*an(11) + xx(i12,2)*an(12)           &
     &                  + xx(i13,2)*an(13) + xx(i14,2)*an(14)           &
     &                  + xx(i15,2)*an(15) + xx(i16,2)*an(16)           &
     &                  + xx(i17,2)*an(17) + xx(i18,2)*an(18)           &
     &                  + xx(i19,2)*an(19) + xx(i20,2)*an(20)           &
     &                  + xx(i21,2)*an(21) + xx(i22,2)*an(22)           &
     &                  + xx(i23,2)*an(23) + xx(i24,2)*an(24)           &
     &                  + xx(i25,2)*an(25) + xx(i26,2)*an(26)           &
     &                  + xx(i27,2)*an(27)
!
          zz_int(inum) =  xx(i1, 3)*an(1) + xx(i2, 3)*an(2)             &
     &                  + xx(i3, 3)*an(3) + xx(i4, 3)*an(4)             &
     &                  + xx(i5, 3)*an(5) + xx(i6, 3)*an(6)             &
     &                  + xx(i7, 3)*an(7) + xx(i8, 3)*an(8)             &
     &                  + xx(i9, 3)*an(9) + xx(i10,3)*an(10)            &
     &                  + xx(i11,3)*an(11) + xx(i12,3)*an(12)           &
     &                  + xx(i13,3)*an(13) + xx(i14,3)*an(14)           &
     &                  + xx(i15,3)*an(15) + xx(i16,3)*an(16)           &
     &                  + xx(i17,3)*an(17) + xx(i18,3)*an(18)           &
     &                  + xx(i19,3)*an(19) + xx(i20,3)*an(20)           &
     &                  + xx(i21,3)*an(21) + xx(i22,3)*an(22)           &
     &                  + xx(i23,3)*an(23) + xx(i24,3)*an(24)           &
     &                  + xx(i25,3)*an(25) + xx(i26,3)*an(26)           &
     &                  + xx(i27,3)*an(27)
!
      end do
!$omp end parallel do
!
      end subroutine set_int_point_position_27
!
! ----------------------------------------------------------------------
!
      subroutine set_int_point_position_20(nele_grp, iele_grp, an,      &
     &          xx_int, yy_int, zz_int)
!
      use m_finite_element_matrix
      use m_geometry_data
!
      integer(kind = kint), intent(in) :: nele_grp
      integer(kind = kint), intent(in) :: iele_grp(nele_grp)
      real(kind = kreal), intent(in) :: an(20)
!
      real(kind = kreal), intent(inout) :: xx_int(nele_grp)
      real(kind = kreal), intent(inout) :: yy_int(nele_grp)
      real(kind = kreal), intent(inout) :: zz_int(nele_grp)
!
      integer(kind = kint) :: inum, iele
      integer(kind = kint) :: i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
      integer(kind = kint) :: i9,  i10, i11, i12, i13, i14, i15, i16
      integer(kind = kint) :: i17, i18, i19, i20
!
!
!$omp  parallel do                                                      &
!$omp& private(iele,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15, &
!$omp&         i16,i17,i18,i19,i20)
        do inum = 1, nele_grp
!
          iele = iele_grp(inum)
!
          i1  = ie(iele, 1)
          i2  = ie(iele, 2)
          i3  = ie(iele, 3)
          i4  = ie(iele, 4)
          i5  = ie(iele, 5)
          i6  = ie(iele, 6)
          i7  = ie(iele, 7)
          i8  = ie(iele, 8)
          i9  = ie(iele, 9)
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
          xx_int(inum) =  xx(i1, 1)*an(1) + xx(i2, 1)*an(2)             &
     &                  + xx(i3, 1)*an(3) + xx(i4, 1)*an(4)             &
     &                  + xx(i5, 1)*an(5) + xx(i6, 1)*an(6)             &
     &                  + xx(i7, 1)*an(7) + xx(i8, 1)*an(8)             &
     &                  + xx(i9, 1)*an(9) + xx(i10,1)*an(10)            &
     &                  + xx(i11,1)*an(11) + xx(i12,1)*an(12)           &
     &                  + xx(i13,1)*an(13) + xx(i14,1)*an(14)           &
     &                  + xx(i15,1)*an(15) + xx(i16,1)*an(16)           &
     &                  + xx(i17,1)*an(17) + xx(i18,1)*an(18)           &
     &                  + xx(i19,1)*an(19) + xx(i20,1)*an(20)
!
          yy_int(inum) =  xx(i1, 2)*an(1) + xx(i2, 2)*an(2)             &
     &                  + xx(i3, 2)*an(3) + xx(i4, 2)*an(4)             &
     &                  + xx(i5, 2)*an(5) + xx(i6, 2)*an(6)             &
     &                  + xx(i7, 2)*an(7) + xx(i8, 2)*an(8)             &
     &                  + xx(i9, 2)*an(9) + xx(i10,2)*an(10)            &
     &                  + xx(i11,2)*an(11) + xx(i12,2)*an(12)           &
     &                  + xx(i13,2)*an(13) + xx(i14,2)*an(14)           &
     &                  + xx(i15,2)*an(15) + xx(i16,2)*an(16)           &
     &                  + xx(i17,2)*an(17) + xx(i18,2)*an(18)           &
     &                  + xx(i19,2)*an(19) + xx(i20,2)*an(20)
!
          zz_int(inum) =  xx(i1, 3)*an(1) + xx(i2, 3)*an(2)             &
     &                  + xx(i3, 3)*an(3) + xx(i4, 3)*an(4)             &
     &                  + xx(i5, 3)*an(5) + xx(i6, 3)*an(6)             &
     &                  + xx(i7, 3)*an(7) + xx(i8, 3)*an(8)             &
     &                  + xx(i9, 3)*an(9) + xx(i10,3)*an(10)            &
     &                  + xx(i11,3)*an(11) + xx(i12,3)*an(12)           &
     &                  + xx(i13,3)*an(13) + xx(i14,3)*an(14)           &
     &                  + xx(i15,3)*an(15) + xx(i16,3)*an(16)           &
     &                  + xx(i17,3)*an(17) + xx(i18,3)*an(18)           &
     &                  + xx(i19,3)*an(19) + xx(i20,3)*an(20)
!
      end do
!$omp end parallel do
!
      end subroutine set_int_point_position_20
!
! ----------------------------------------------------------------------
!
      subroutine set_int_point_position_8(nele_grp, iele_grp, an,       &
     &          xx_int, yy_int, zz_int)
!
      use m_finite_element_matrix
      use m_geometry_data
!
      integer(kind = kint), intent(in) :: nele_grp
      integer(kind = kint), intent(in) :: iele_grp(nele_grp)
      real(kind = kreal), intent(in) :: an(8)
!
      real(kind = kreal), intent(inout) :: xx_int(nele_grp)
      real(kind = kreal), intent(inout) :: yy_int(nele_grp)
      real(kind = kreal), intent(inout) :: zz_int(nele_grp)
!
      integer(kind = kint) :: inum, iele
      integer(kind = kint) :: i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
!
!
!$omp  parallel do private(iele,i1,i2,i3,i4,i5,i6,i7,i8)
        do inum = 1, nele_grp
!
          iele = iele_grp(inum)
!
          i1  = ie(iele, 1)
          i2  = ie(iele, 2)
          i3  = ie(iele, 3)
          i4  = ie(iele, 4)
          i5  = ie(iele, 5)
          i6  = ie(iele, 6)
          i7  = ie(iele, 7)
          i8  = ie(iele, 8)
!
          xx_int(inum) =  xx(i1, 1)*an(1) + xx(i2, 1)*an(2)             &
     &                  + xx(i3, 1)*an(3) + xx(i4, 1)*an(4)             &
     &                  + xx(i5, 1)*an(5) + xx(i6, 1)*an(6)             &
     &                  + xx(i7, 1)*an(7) + xx(i8, 1)*an(8)
!
          yy_int(inum) =  xx(i1, 2)*an(1) + xx(i2, 2)*an(2)             &
     &                  + xx(i3, 2)*an(3) + xx(i4, 2)*an(4)             &
     &                  + xx(i5, 2)*an(5) + xx(i6, 2)*an(6)             &
     &                  + xx(i7, 2)*an(7) + xx(i8, 2)*an(8)
!
          zz_int(inum) =  xx(i1, 3)*an(1) + xx(i2, 3)*an(2)             &
     &                  + xx(i3, 3)*an(3) + xx(i4, 3)*an(4)             &
     &                  + xx(i5, 3)*an(5) + xx(i6, 3)*an(6)             &
     &                  + xx(i7, 3)*an(7) + xx(i8, 3)*an(8)
!
      end do
!$omp end parallel do
!
      end subroutine set_int_point_position_8
!
! ----------------------------------------------------------------------
!
      end module set_int_point_position
