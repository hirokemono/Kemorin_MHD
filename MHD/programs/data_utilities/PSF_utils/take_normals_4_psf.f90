!
!      module take_normals_4_psf
!
!      Written by H. Matsui
!
!!      subroutine cal_center_ele_4_psf                                 &
!!     &         (numnod_psf, numele_psf, xx_psf, ie_psf)
!!      subroutine cal_norm_area_4_psf                                  &
!!     &         (numnod_psf, numele_psf, xx_psf, ie_psf)
!!      subroutine cal_nod_normal_4_psf                                 &
!!     &         (numnod_psf, numele_psf, xx_psf, ie_psf)
!
      module take_normals_4_psf
!
      use m_precision
      use m_constants
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
      subroutine cal_center_ele_4_psf                                   &
     &         (numnod_psf, numele_psf, xx_psf, ie_psf)
!
      integer(kind = kint), intent(in) :: numnod_psf, numele_psf
      integer(kind = kint), intent(in) :: ie_psf(numele_psf,3)
      real(kind = kreal), intent(in) :: xx_psf(numnod_psf,3)
!
      integer(kind = kint) :: iele, i1, i2, i3
!
!
!$omp parallel do private(i1,i2,i3)
      do iele = 1, numele_psf
        i1 = ie_psf(iele,1)
        i2 = ie_psf(iele,2)
        i3 = ie_psf(iele,3)
!
        center_ele_psf(iele,1)                                          &
     &      = (xx_psf(i2,1) + xx_psf(i2,1) + xx_psf(i2,1)) / 3.0d0
        center_ele_psf(iele,2)                                          &
     &      = (xx_psf(i2,2) + xx_psf(i2,2) + xx_psf(i2,2)) / 3.0d0
        center_ele_psf(iele,3)                                          &
     &      = (xx_psf(i2,3) + xx_psf(i2,3) + xx_psf(i2,3)) / 3.0d0
!
        radius_ele_psf(iele) = sqrt(center_ele_psf(iele,1)**2           &
     &                            + center_ele_psf(iele,2)**2           &
     &                            + center_ele_psf(iele,3)**2)
      end do
!$omp end parallel do
!
      end subroutine cal_center_ele_4_psf
!
!-----------------------------------------------------------------------
!
      subroutine cal_norm_area_4_psf                                    &
     &         (numnod_psf, numele_psf, xx_psf, ie_psf)
!
      integer(kind = kint), intent(in) :: numnod_psf, numele_psf
      integer(kind = kint), intent(in) :: ie_psf(numele_psf,3)
      real(kind = kreal), intent(in) :: xx_psf(numnod_psf,3)
!
      integer(kind = kint) :: iele, i1, i2, i3
      real(kind = kreal) :: ax(3), bx(3)
!
!
!$omp parallel do private(i1,i2,i3,ax,bx)
      do iele = 1, numele_psf
        i1 = ie_psf(iele,1)
        i2 = ie_psf(iele,2)
        i3 = ie_psf(iele,3)
!
        ax(1) = xx_psf(i2,1) - xx_psf(i1,1)
        ax(2) = xx_psf(i2,2) - xx_psf(i1,2)
        ax(3) = xx_psf(i2,3) - xx_psf(i1,3)
!
        bx(1) = xx_psf(i3,1) - xx_psf(i1,1)
        bx(2) = xx_psf(i3,2) - xx_psf(i1,2)
        bx(3) = xx_psf(i3,3) - xx_psf(i1,3)
!
        norm_ele_psf(iele,1) = ax(2)*bx(3) - ax(3)*bx(2)
        norm_ele_psf(iele,2) = ax(3)*bx(1) - ax(1)*bx(3)
        norm_ele_psf(iele,3) = ax(1)*bx(2) - ax(2)*bx(1)
        area_psf(iele)                                                  &
     &       = sqrt( norm_ele_psf(iele,1)*norm_ele_psf(iele,1)          &
     &             + norm_ele_psf(iele,2)*norm_ele_psf(iele,2)          &
     &             + norm_ele_psf(iele,3)*norm_ele_psf(iele,3) )
!
        if ( area_psf(iele) .eq. 0.0d0) then
          norm_ele_psf(iele,1) = 0.0d0
          norm_ele_psf(iele,2) = 0.0d0
          norm_ele_psf(iele,3) = 0.0d0
        else
          norm_ele_psf(iele,1) = norm_ele_psf(iele,1) / area_psf(iele)
          norm_ele_psf(iele,2) = norm_ele_psf(iele,2) / area_psf(iele)
          norm_ele_psf(iele,3) = norm_ele_psf(iele,3) / area_psf(iele)
        end if
      end do
!$omp end parallel do
!
      area_total_psf = 0.0d0
      do iele = 1, numele_psf
        area_total_psf = area_total_psf + area_psf(iele)
      end do
!
      end subroutine cal_norm_area_4_psf
!
!-----------------------------------------------------------------------
!
      subroutine cal_nod_normal_4_psf                                   &
     &         (numnod_psf, numele_psf, xx_psf, ie_psf)
!
      integer(kind = kint), intent(in) :: numnod_psf, numele_psf
      integer(kind = kint), intent(in) :: ie_psf(numele_psf,3)
      real(kind = kreal), intent(in) :: xx_psf(numnod_psf,3)
!
      real(kind = kreal) :: d, xele_psf(3), d2h_psf(3)
!
      integer(kind = kint) :: inod, iele, i1, i2, i3
!
      w_4_norm_nod = 0.0d0
      norm_nod_psf = 0.0d0
      do iele = 1, numele_psf
!
        i1 = ie_psf(iele,1)
        i2 = ie_psf(iele,2)
        i3 = ie_psf(iele,3)
!
        xele_psf(1)                                                     &
     &       = ( xx_psf(i1,1) + xx_psf(i2,1) + xx_psf(i3,1) ) / 3.0d0
        xele_psf(2)                                                     &
     &       = ( xx_psf(i1,2) + xx_psf(i2,2) + xx_psf(i3,2) ) / 3.0d0
        xele_psf(3)                                                     &
     &       = ( xx_psf(i1,3) + xx_psf(i2,3) + xx_psf(i3,3) ) / 3.0d0
!
        d2h_psf(1) = sqrt( (xele_psf(1) - xx_psf(i1,1))**2              &
     &                   + (xele_psf(2) - xx_psf(i1,2))**2              &
     &                   + (xele_psf(3) - xx_psf(i1,3))**2 )
        d2h_psf(2) = sqrt( (xele_psf(1) - xx_psf(i2,1))**2              &
     &                   + (xele_psf(2) - xx_psf(i2,2))**2              &
     &                   + (xele_psf(3) - xx_psf(i2,3))**2 )
        d2h_psf(2) = sqrt( (xele_psf(1) - xx_psf(i3,1))**2              &
     &                   + (xele_psf(2) - xx_psf(i3,2))**2              &
     &                   + (xele_psf(3) - xx_psf(i3,3))**2 )
!
!
        if ( d2h_psf(1).eq.0.0d0 ) then
          w_4_norm_nod(i1) = -1.0d30
          norm_nod_psf(i1,1) = norm_ele_psf(iele,1)
          norm_nod_psf(i1,2) = norm_ele_psf(iele,2)
          norm_nod_psf(i1,3) = norm_ele_psf(iele,3)
        else
          w_4_norm_nod(i1) = w_4_norm_nod(i1) + one/d2h_psf(1)
          norm_nod_psf(i1,1) = norm_nod_psf(i1,1)                       &
     &      + norm_ele_psf(iele,1) * (d2h_psf(2)+d2h_psf(3))
          norm_nod_psf(i1,2) = norm_nod_psf(i1,2)                       &
     &      + norm_ele_psf(iele,2) * (d2h_psf(2)+d2h_psf(3))
          norm_nod_psf(i1,3) = norm_nod_psf(i1,3)                       &
     &      + norm_ele_psf(iele,3) * (d2h_psf(2)+d2h_psf(3))
        end if
!
        if ( d2h_psf(2).eq.0.0d0 ) then
          w_4_norm_nod(i2) = -1.0d30
          norm_nod_psf(i2,1) = norm_ele_psf(iele,1)
          norm_nod_psf(i2,2) = norm_ele_psf(iele,2)
          norm_nod_psf(i2,3) = norm_ele_psf(iele,3)
        else
          w_4_norm_nod(i2) = w_4_norm_nod(i2) + one/d2h_psf(2)
          norm_nod_psf(i2,1) = norm_nod_psf(i2,1)                       &
     &      + norm_ele_psf(iele,1) * (d2h_psf(1)+d2h_psf(3))
          norm_nod_psf(i2,2) = norm_nod_psf(i2,2)                       &
     &      + norm_ele_psf(iele,2) * (d2h_psf(1)+d2h_psf(3))
          norm_nod_psf(i2,3) = norm_nod_psf(i2,3)                       &
     &      + norm_ele_psf(iele,3) * (d2h_psf(1)+d2h_psf(3))
        end if
!
        if ( d2h_psf(3).eq.0.0d0 ) then
          w_4_norm_nod(i3) = -1.0d30
          norm_nod_psf(i3,1) = norm_ele_psf(iele,1)
          norm_nod_psf(i3,2) = norm_ele_psf(iele,2)
          norm_nod_psf(i3,3) = norm_ele_psf(iele,3)
        else
          w_4_norm_nod(i3) = w_4_norm_nod(i3) + one/d2h_psf(3)
          norm_nod_psf(i3,1) = norm_nod_psf(i3,1)                       &
     &      + norm_ele_psf(iele,1) * (d2h_psf(1)+d2h_psf(2))
          norm_nod_psf(i3,2) = norm_nod_psf(i3,2)                       &
     &      + norm_ele_psf(iele,2) * (d2h_psf(1)+d2h_psf(2))
          norm_nod_psf(i3,3) = norm_nod_psf(i3,3)                       &
     &      + norm_ele_psf(iele,3) * (d2h_psf(1)+d2h_psf(2))
        end if
!
      end do
!
!
!
      do inod = 1, numnod_psf
        d = sqrt( norm_nod_psf(inod,1)*norm_nod_psf(inod,1)             &
     &          + norm_nod_psf(inod,2)*norm_nod_psf(inod,2)             &
     &          + norm_nod_psf(inod,3)*norm_nod_psf(inod,3) )
        norm_nod_psf(inod,1) = norm_nod_psf(inod,1) / d
        norm_nod_psf(inod,2) = norm_nod_psf(inod,2) / d
        norm_nod_psf(inod,3) = norm_nod_psf(inod,3) / d
      end do
!
      end subroutine cal_nod_normal_4_psf
!
!-----------------------------------------------------------------------
!
      end module take_normals_4_psf
