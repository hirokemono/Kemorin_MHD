!
!      module take_avarages_4_psf
!
!      Written by H. Matsui
!
!      subroutine set_averaging_range(rmin, rmax)
!      subroutine cal_rms_ave_4_psf
!      subroutine cal_minmax_psf
!
      module take_avarages_4_psf
!
      use m_precision
      use m_psf_results
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
      subroutine set_averaging_range(rmin, rmax)
!
      real(kind = kreal), intent(in) :: rmin, rmax
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
      subroutine cal_rms_ave_4_psf
!
      integer(kind = kint) :: iele, icomp, i1, i2, i3
      real(kind = kreal) :: d_ele, coef
!
      ave_psf = 0.0d0
      rms_psf = 0.0d0
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
      end subroutine cal_rms_ave_4_psf
!
!-----------------------------------------------------------------------
!
      subroutine cal_minmax_psf
!
      integer(kind = kint) :: inod, icomp
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
      end module take_avarages_4_psf
