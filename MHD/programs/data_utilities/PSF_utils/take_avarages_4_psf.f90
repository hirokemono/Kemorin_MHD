!
!      module take_avarages_4_psf
!
!      Written by H. Matsui
!
!!      subroutine set_averaging_range(rmin, rmax, numele_psf)
!!      subroutine cal_rms_ave_4_psf(numnod_psf, numele_psf, ie_psf,    &
!!     &          ncomptot_psf, d_nod_psf, ave_psf, rms_psf, sdev_psf)
!!      subroutine cal_minmax_psf(numnod_psf, ncomptot_psf, d_nod_psf,  &
!!     &          xmin_psf, xmax_psf)
!!      subroutine cal_range_rms_ave_4_psf(numnod_psf, numele_psf,      &
!!     &          ie_psf, ncomptot_psf, d_nod_psf,                      &
!!     &          icomp_ref, iflag_ref, ref_value, area,                &
!!     &          ave_psf, rms_psf, sdev_psf)
!
      module take_avarages_4_psf
!
      use m_precision
      use m_norms_4_psf
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_averaging_range(rmin, rmax, numele_psf)
!
      integer(kind = kint), intent(in) :: numele_psf
      real(kind = kreal), intent(in) :: rmin, rmax
!
      integer(kind = kint) :: iele
!
!
      rflag_averaging(1:numele_psf) = 1.0d0
      if(rmax .le. rmin .or. rmax .eq. 0.0d0) return
      do iele = 1, numele_psf
        if(radius_ele_psf(iele) .lt. rmin                               &
     &     .or. radius_ele_psf(iele) .gt. rmax) then
          rflag_averaging(iele)  = 0.0d0
        end if
      end do
!
      end subroutine set_averaging_range
!
!-----------------------------------------------------------------------
!
      subroutine cal_rms_ave_4_psf(numnod_psf, numele_psf, ie_psf,      &
     &          ncomptot_psf, d_nod_psf, ave_psf, rms_psf, sdev_psf)
!
      integer(kind = kint), intent(in) :: numnod_psf, numele_psf
      integer(kind = kint), intent(in) :: ie_psf(numele_psf,3)
!
      integer(kind = kint), intent(in) :: ncomptot_psf
      real(kind = kreal), intent(in)                                    &
     &                   :: d_nod_psf(numnod_psf,ncomptot_psf)
!
      real(kind = kreal), intent(inout) :: ave_psf(ncomptot_psf)
      real(kind = kreal), intent(inout) :: rms_psf(ncomptot_psf)
      real(kind = kreal), intent(inout) :: sdev_psf(ncomptot_psf)
!
      integer(kind = kint) :: iele, icomp, i1, i2, i3
      real(kind = kreal) :: d_ele, coef
!
!
      ave_psf =  0.0d0
      rms_psf =  0.0d0
      sdev_psf = 0.0d0
!
!   Evaluate average and RMS
      do icomp = 1, ncomptot_psf
        do iele = 1, numele_psf
          coef = area_psf(iele) * rflag_averaging(iele)
!
          i1 = ie_psf(iele,1)
          i2 = ie_psf(iele,2)
          i3 = ie_psf(iele,3)
!
          d_ele = (d_nod_psf(i1,icomp) + d_nod_psf(i2,icomp)            &
     &           + d_nod_psf(i3,icomp) ) / 3.0d0
!
          rms_psf(icomp) = rms_psf(icomp)  + d_ele * d_ele * coef
          ave_psf(icomp) = ave_psf(icomp) + d_ele * coef
        end do
        rms_psf(icomp) = sqrt( rms_psf(icomp)/area_total_psf )
        ave_psf(icomp) = ave_psf(icomp)/area_total_psf
      end do
!
!   Evaluate standard deviation
      do icomp = 1, ncomptot_psf
        do iele = 1, numele_psf
          coef = area_psf(iele) * rflag_averaging(iele)
!
          i1 = ie_psf(iele,1)
          i2 = ie_psf(iele,2)
          i3 = ie_psf(iele,3)
!
          d_ele = (d_nod_psf(i1,icomp) + d_nod_psf(i2,icomp)            &
     &           + d_nod_psf(i3,icomp) ) / 3.0d0 - ave_psf(icomp)
!
          sdev_psf(icomp) = sdev_psf(icomp)  + d_ele * d_ele * coef
        end do
        sdev_psf(icomp) = sqrt( sdev_psf(icomp)/area_total_psf )
      end do
!
      end subroutine cal_rms_ave_4_psf
!
!-----------------------------------------------------------------------
!
      subroutine cal_minmax_psf(numnod_psf, ncomptot_psf, d_nod_psf,    &
     &          xmin_psf, xmax_psf)
!
      integer(kind = kint), intent(in) :: numnod_psf, ncomptot_psf
      real(kind = kreal), intent(in)                                    &
     &                   :: d_nod_psf(numnod_psf,ncomptot_psf)
!
      real(kind = kreal), intent(inout) :: xmin_psf(ncomptot_psf)
      real(kind = kreal), intent(inout) :: xmax_psf(ncomptot_psf)
!
      integer(kind = kint) :: inod, icomp
!
!
      do icomp = 1, ncomptot_psf
        xmin_psf(icomp) = 1.0d30
        do inod = 1, numnod_psf
          xmin_psf(icomp) = min(xmin_psf(icomp), d_nod_psf(inod,icomp))
        end do
        xmax_psf(icomp) = xmin_psf(icomp)
        do inod = 1, numnod_psf
          xmax_psf(icomp) = max(xmax_psf(icomp), d_nod_psf(inod,icomp))
        end do
      end do
!
      end subroutine cal_minmax_psf
!
!-----------------------------------------------------------------------
!
      subroutine cal_range_rms_ave_4_psf(numnod_psf, numele_psf,        &
     &          ie_psf, ncomptot_psf, d_nod_psf,                        &
     &          icomp_ref, iflag_ref, ref_value, area,                  &
     &          ave_psf, rms_psf, sdev_psf)
!
      integer(kind = kint), intent(in) :: numnod_psf, numele_psf
      integer(kind = kint), intent(in) :: ie_psf(numele_psf,3)
      integer(kind = kint), intent(in) :: ncomptot_psf
      real(kind = kreal), intent(in)                                    &
     &                   :: d_nod_psf(numnod_psf,ncomptot_psf)
!
      integer(kind = kint), intent(in) :: icomp_ref, iflag_ref
      real(kind = kreal), intent(in) :: ref_value
      real(kind = kreal), intent(inout) :: area
!
      real(kind = kreal), intent(inout) :: ave_psf(ncomptot_psf)
      real(kind = kreal), intent(inout) :: rms_psf(ncomptot_psf)
      real(kind = kreal), intent(inout) :: sdev_psf(ncomptot_psf)
!
      integer(kind = kint) :: iele, icomp, i1, i2, i3
      real(kind = kreal) :: d_ele, ref_ele
!      real(kind = kreal) :: x_ctr(3)
!
!   Evaluate average and RMS
      area =    0.0d0
      ave_psf = 0.0d0
      rms_psf = 0.0d0
      sdev_psf = 0.0d0
      do iele = 1, numele_psf
        if(rflag_averaging(iele).eq.0.0d0) cycle
!
        i1 = ie_psf(iele,1)
        i2 = ie_psf(iele,2)
        i3 = ie_psf(iele,3)
!
        ref_ele =  (d_nod_psf(i1,icomp_ref) + d_nod_psf(i2,icomp_ref)   &
     &              + d_nod_psf(i3,icomp_ref) ) / 3.0d0
!
        if(iflag_ref.eq.1 .and. ref_ele.lt.ref_value) cycle
        if(iflag_ref.eq.2 .and. ref_ele.gt.ref_value) cycle
!
!        x_ctr(1:3) = (xx_psf(i1,1:3) + xx_psf(i2,1:3) + xx_psf(i3,1:3))&
!     &              /3.0d0
!
        area = area + area_psf(iele)
        do icomp = 1, ncomptot_psf
          d_ele = (d_nod_psf(i1,icomp) + d_nod_psf(i2,icomp)            &
     &           + d_nod_psf(i3,icomp) ) / 3.0d0
!
          rms_psf(icomp) = rms_psf(icomp) + d_ele**2 * area_psf(iele)
          ave_psf(icomp) = ave_psf(icomp) + d_ele * area_psf(iele)
        end do
      end do
!
      do icomp = 1, ncomptot_psf
        rms_psf(icomp) = sqrt( rms_psf(icomp)/area )
        ave_psf(icomp) = ave_psf(icomp)/area
      end do
!
!   Evaluate standard deviation
      do iele = 1, numele_psf
        if(rflag_averaging(iele).eq.0.0d0) cycle
!
        i1 = ie_psf(iele,1)
        i2 = ie_psf(iele,2)
        i3 = ie_psf(iele,3)
!
        ref_ele =  (d_nod_psf(i1,icomp_ref) + d_nod_psf(i2,icomp_ref)   &
     &              + d_nod_psf(i3,icomp_ref) ) / 3.0d0
!
        if(iflag_ref.eq.1 .and. ref_ele.lt.ref_value) cycle
        if(iflag_ref.eq.2 .and. ref_ele.gt.ref_value) cycle
!
        area = area + area_psf(iele)
        do icomp = 1, ncomptot_psf
          d_ele = (d_nod_psf(i1,icomp) + d_nod_psf(i2,icomp)            &
     &           + d_nod_psf(i3,icomp) ) / 3.0d0 - ave_psf(icomp)
!
          sdev_psf(icomp) = sdev_psf(icomp) + d_ele**2 * area_psf(iele)
        end do
      end do
!
      do icomp = 1, ncomptot_psf
        sdev_psf(icomp) = sqrt( sdev_psf(icomp)/area )
      end do
!
      end subroutine cal_range_rms_ave_4_psf
!
!-----------------------------------------------------------------------
!
      end module take_avarages_4_psf
