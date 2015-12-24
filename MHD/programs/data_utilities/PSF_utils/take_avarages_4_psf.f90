!
!      module take_avarages_4_psf
!
!      Written by H. Matsui
!
!!      subroutine set_averaging_range(rmin, rmax, psf_norm)
!!      subroutine cal_rms_ave_4_psf(numnod_psf, numele_psf, ie_psf,    &
!!     &          ncomptot_psf, d_nod_psf, psf_aves)
!!      subroutine cal_minmax_psf(numnod_psf, ncomptot_psf, d_nod_psf,  &
!!     &          psf_aves)
!!      subroutine cal_range_rms_ave_4_psf(numnod_psf, numele_psf,      &
!!     &          ie_psf, ncomptot_psf, d_nod_psf,                      &
!!     &          icomp_ref, iflag_ref, ref_value, area, psf_aves)
!
      module take_avarages_4_psf
!
      use m_precision
!
      use t_geometry_data
      use t_phys_data
      use t_norms_4_psf
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_averaging_range(rmin, rmax, psf_norm)
!
      real(kind = kreal), intent(in) :: rmin, rmax
!
      type(psf_normals), intent(inout) :: psf_norm
!
      integer(kind = kint) :: iele
!
!
      psf_norm%rflag_ave(1:psf_norm%nele) = 1.0d0
      if(rmax .le. rmin .or. rmax .eq. 0.0d0) return
      do iele = 1, psf_norm%nele
        if(psf_norm%r_ele(iele) .lt. rmin                               &
     &     .or. psf_norm%r_ele(iele) .gt. rmax) then
          psf_norm%rflag_ave(iele)  = 0.0d0
        end if
      end do
!
      end subroutine set_averaging_range
!
!-----------------------------------------------------------------------
!
      subroutine cal_rms_ave_4_psf(psf_ele,  psf_phys, psf_norm,        &
     &          psf_aves)
!
      type(element_data), intent(in) :: psf_ele
      type(phys_data), intent(in) :: psf_phys
!
      type(psf_normals), intent(in) :: psf_norm
!
      type(psf_averages), intent(inout) :: psf_aves
!
      integer(kind = kint) :: iele, icomp, i1, i2, i3
      real(kind = kreal) :: d_ele, coef
!
!
      psf_aves%ave =  0.0d0
      psf_aves%rms =  0.0d0
      psf_aves%sdev = 0.0d0
!
!   Evaluate average and RMS
      do icomp = 1, psf_phys%ntot_phys
        do iele = 1, psf_ele%numele
          coef = psf_norm%area_ele(iele) * psf_norm%rflag_ave(iele)
!
          i1 = psf_ele%ie(iele,1)
          i2 = psf_ele%ie(iele,2)
          i3 = psf_ele%ie(iele,3)
!
          d_ele = (psf_phys%d_fld(i1,icomp) + psf_phys%d_fld(i2,icomp)  &
     &           + psf_phys%d_fld(i3,icomp) ) / 3.0d0
!
          psf_aves%rms(icomp) = psf_aves%rms(icomp)                     &
     &                         + d_ele * d_ele * coef
          psf_aves%ave(icomp) = psf_aves%ave(icomp) + d_ele * coef
        end do
        psf_aves%rms(icomp)                                             &
     &        =  sqrt( psf_aves%rms(icomp) / psf_norm%area )
        psf_aves%ave(icomp) = psf_aves%ave(icomp) / psf_norm%area
      end do
!
!   Evaluate standard deviation
      do icomp = 1, psf_phys%ntot_phys
        do iele = 1, psf_ele%numele
          coef = psf_norm%area_ele(iele) * psf_norm%rflag_ave(iele)
!
          i1 = psf_ele%ie(iele,1)
          i2 = psf_ele%ie(iele,2)
          i3 = psf_ele%ie(iele,3)
!
          d_ele = (psf_phys%d_fld(i1,icomp) + psf_phys%d_fld(i2,icomp)  &
     &           + psf_phys%d_fld(i3,icomp) ) / 3.0d0                   &
     &           - psf_aves%ave(icomp)
!
          psf_aves%sdev(icomp) = psf_aves%sdev(icomp)                   &
     &                          + d_ele * d_ele * coef
        end do
        psf_aves%sdev(icomp)                                            &
     &           = sqrt( psf_aves%sdev(icomp) / psf_norm%area)
      end do
!
      end subroutine cal_rms_ave_4_psf
!
!-----------------------------------------------------------------------
!
      subroutine cal_minmax_psf(numnod_psf, ncomptot_psf, d_nod_psf,    &
     &          psf_aves)
!
      integer(kind = kint), intent(in) :: numnod_psf, ncomptot_psf
      real(kind = kreal), intent(in)                                    &
     &                   :: d_nod_psf(numnod_psf,ncomptot_psf)
!
      type(psf_averages), intent(inout) :: psf_aves
!
      integer(kind = kint) :: inod, icomp
!
!
      do icomp = 1, ncomptot_psf
        psf_aves%dmin(icomp) = 1.0d30
        do inod = 1, numnod_psf
          psf_aves%dmin(icomp)                                          &
     &          = min(psf_aves%dmin(icomp), d_nod_psf(inod,icomp))
        end do
        psf_aves%dmax(icomp) = psf_aves%dmin(icomp)
        do inod = 1, numnod_psf
          psf_aves%dmax(icomp)                                          &
     &          = max(psf_aves%dmax(icomp), d_nod_psf(inod,icomp))
        end do
      end do
!
      end subroutine cal_minmax_psf
!
!-----------------------------------------------------------------------
!
      subroutine cal_range_rms_ave_4_psf(psf_ele, psf_phys, psf_norm,   &
     &          icomp_ref, iflag_ref, ref_value, area, psf_aves)
!
      type(element_data), intent(in) :: psf_ele
      type(phys_data), intent(in) :: psf_phys
!
      type(psf_normals), intent(in) :: psf_norm
!
      integer(kind = kint), intent(in) :: icomp_ref, iflag_ref
      real(kind = kreal), intent(in) :: ref_value
      real(kind = kreal), intent(inout) :: area
!
      type(psf_averages), intent(inout) :: psf_aves
!
      integer(kind = kint) :: iele, icomp, i1, i2, i3
      real(kind = kreal) :: d_ele, ref_ele
!      real(kind = kreal) :: x_ctr(3)
!
!   Evaluate average and RMS
      area =    0.0d0
      psf_aves%ave = 0.0d0
      psf_aves%rms = 0.0d0
      psf_aves%sdev = 0.0d0
      do iele = 1, psf_ele%numele
        if(psf_norm%rflag_ave(iele).eq.0.0d0) cycle
!
        i1 = psf_ele%ie(iele,1)
        i2 = psf_ele%ie(iele,2)
        i3 = psf_ele%ie(iele,3)
!
        ref_ele =  (psf_phys%d_fld(i1,icomp_ref)                        &
     &            + psf_phys%d_fld(i2,icomp_ref)                        &
     &            + psf_phys%d_fld(i3,icomp_ref) ) / 3.0d0
!
        if(iflag_ref.eq.1 .and. ref_ele.lt.ref_value) cycle
        if(iflag_ref.eq.2 .and. ref_ele.gt.ref_value) cycle
!
!        x_ctr(1:3) = (xx_psf(i1,1:3) + xx_psf(i2,1:3) + xx_psf(i3,1:3))&
!     &              /3.0d0
!
        area = area + psf_norm%area_ele(iele)
        do icomp = 1, psf_phys%ntot_phys
          d_ele = (psf_phys%d_fld(i1,icomp) + psf_phys%d_fld(i2,icomp)  &
     &           + psf_phys%d_fld(i3,icomp) ) / 3.0d0
!
          psf_aves%rms(icomp) = psf_aves%rms(icomp)                     &
     &                    + d_ele**2 * psf_norm%area_ele(iele)
          psf_aves%ave(icomp) = psf_aves%ave(icomp)                     &
     &                    + d_ele * psf_norm%area_ele(iele)
        end do
      end do
!
      do icomp = 1, psf_phys%ntot_phys
        psf_aves%rms(icomp) = sqrt( psf_aves%rms(icomp)/area )
        psf_aves%ave(icomp) = psf_aves%ave(icomp)/area
      end do
!
!   Evaluate standard deviation
      do iele = 1, psf_ele%numele
        if(psf_norm%rflag_ave(iele).eq.0.0d0) cycle
!
        i1 = psf_ele%ie(iele,1)
        i2 = psf_ele%ie(iele,2)
        i3 = psf_ele%ie(iele,3)
!
        ref_ele =  (psf_phys%d_fld(i1,icomp_ref)                        &
     &            + psf_phys%d_fld(i2,icomp_ref)                        &
     &            + psf_phys%d_fld(i3,icomp_ref) ) / 3.0d0
!
        if(iflag_ref.eq.1 .and. ref_ele.lt.ref_value) cycle
        if(iflag_ref.eq.2 .and. ref_ele.gt.ref_value) cycle
!
        area = area + psf_norm%area_ele(iele)
        do icomp = 1, psf_phys%ntot_phys
          d_ele = (psf_phys%d_fld(i1,icomp) + psf_phys%d_fld(i2,icomp)  &
     &           + psf_phys%d_fld(i3,icomp) ) / 3.0d0                   &
     &           - psf_aves%ave(icomp)
!
          psf_aves%sdev(icomp) = psf_aves%sdev(icomp)                   &
     &                     + d_ele**2 * psf_norm%area_ele(iele)
        end do
      end do
!
      do icomp = 1, psf_phys%ntot_phys
        psf_aves%sdev(icomp) = sqrt( psf_aves%sdev(icomp)/area )
      end do
!
      end subroutine cal_range_rms_ave_4_psf
!
!-----------------------------------------------------------------------
!
      end module take_avarages_4_psf
