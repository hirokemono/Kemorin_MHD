!
!      module take_normals_4_psf
!
!      Written by H. Matsui
!
!!      subroutine cal_center_ele_4_psf(psf_nod, psf_ele, psf_norm)
!!      subroutine cal_norm_area_4_psf(psf_nod, psf_ele, psf_norm)
!!      subroutine cal_nod_normal_4_psf(psf_nod, psf_ele, psf_norm)
!
      module take_normals_4_psf
!
      use m_precision
      use m_constants
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
      subroutine cal_center_ele_4_psf(psf_nod, psf_ele, psf_norm)
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
!
      type(psf_normals), intent(inout) :: psf_norm
!
      integer(kind = kint) :: iele, i1, i2, i3
!
!
!$omp parallel do private(i1,i2,i3)
      do iele = 1, psf_ele%numele
        i1 = psf_ele%ie(iele,1)
        i2 = psf_ele%ie(iele,2)
        i3 = psf_ele%ie(iele,3)
!
        psf_norm%center_ele(iele,1)                                     &
     &      = (psf_nod%xx(i2,1) + psf_nod%xx(i2,1) + psf_nod%xx(i2,1))  &
     &       / 3.0d0
        psf_norm%center_ele(iele,2)                                     &
     &      = (psf_nod%xx(i2,2) + psf_nod%xx(i2,2) + psf_nod%xx(i2,2))  &
     &       / 3.0d0
        psf_norm%center_ele(iele,3)                                     &
     &      = (psf_nod%xx(i2,3) + psf_nod%xx(i2,3) + psf_nod%xx(i2,3))  &
     &       / 3.0d0
!
        psf_norm%r_ele(iele) = sqrt(psf_norm%center_ele(iele,1)**2      &
     &                             + psf_norm%center_ele(iele,2)**2     &
     &                             + psf_norm%center_ele(iele,3)**2)
      end do
!$omp end parallel do
!
      end subroutine cal_center_ele_4_psf
!
!-----------------------------------------------------------------------
!
      subroutine cal_norm_area_4_psf(psf_nod, psf_ele, psf_norm)
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
!
      type(psf_normals), intent(inout) :: psf_norm
!
      integer(kind = kint) :: iele, i1, i2, i3
      real(kind = kreal) :: ax(3), bx(3)
!
!
!$omp parallel do private(i1,i2,i3,ax,bx)
      do iele = 1, psf_ele%numele
        i1 = psf_ele%ie(iele,1)
        i2 = psf_ele%ie(iele,2)
        i3 = psf_ele%ie(iele,3)
!
        ax(1) = psf_nod%xx(i2,1) - psf_nod%xx(i1,1)
        ax(2) = psf_nod%xx(i2,2) - psf_nod%xx(i1,2)
        ax(3) = psf_nod%xx(i2,3) - psf_nod%xx(i1,3)
!
        bx(1) = psf_nod%xx(i3,1) - psf_nod%xx(i1,1)
        bx(2) = psf_nod%xx(i3,2) - psf_nod%xx(i1,2)
        bx(3) = psf_nod%xx(i3,3) - psf_nod%xx(i1,3)
!
        psf_norm%norm_ele(iele,1) = ax(2)*bx(3) - ax(3)*bx(2)
        psf_norm%norm_ele(iele,2) = ax(3)*bx(1) - ax(1)*bx(3)
        psf_norm%norm_ele(iele,3) = ax(1)*bx(2) - ax(2)*bx(1)
        psf_norm%area_ele(iele)                                         &
     &      = sqrt(psf_norm%norm_ele(iele,1)*psf_norm%norm_ele(iele,1)  &
     &           + psf_norm%norm_ele(iele,2)*psf_norm%norm_ele(iele,2)  &
     &           + psf_norm%norm_ele(iele,3)*psf_norm%norm_ele(iele,3))
!
        if ( psf_norm%area_ele(iele) .eq. 0.0d0) then
          psf_norm%norm_ele(iele,1) = 0.0d0
          psf_norm%norm_ele(iele,2) = 0.0d0
          psf_norm%norm_ele(iele,3) = 0.0d0
        else
          psf_norm%norm_ele(iele,1)                                     &
     &          = psf_norm%norm_ele(iele,1) / psf_norm%area_ele(iele)
          psf_norm%norm_ele(iele,2)                                     &
     &          = psf_norm%norm_ele(iele,2) / psf_norm%area_ele(iele)
          psf_norm%norm_ele(iele,3)                                     &
     &          = psf_norm%norm_ele(iele,3) / psf_norm%area_ele(iele)
        end if
      end do
!$omp end parallel do
!
      psf_norm%area = 0.0d0
      do iele = 1, psf_ele%numele
        psf_norm%area = psf_norm%area + psf_norm%area_ele(iele)
      end do
!
      end subroutine cal_norm_area_4_psf
!
!-----------------------------------------------------------------------
!
      subroutine cal_nod_normal_4_psf(psf_nod, psf_ele, psf_norm)
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
!
      type(psf_normals), intent(inout) :: psf_norm
!
      real(kind = kreal) :: d, xele_psf(3), d2h_psf(3)
!
      integer(kind = kint) :: inod, iele, i1, i2, i3
!
      psf_norm%weight_4_nod = 0.0d0
      psf_norm%norm_nod = 0.0d0
      do iele = 1, psf_ele%numele
!
        i1 = psf_ele%ie(iele,1)
        i2 = psf_ele%ie(iele,2)
        i3 = psf_ele%ie(iele,3)
!
        xele_psf(1)                                                     &
     &     = ( psf_nod%xx(i1,1) + psf_nod%xx(i2,1) + psf_nod%xx(i3,1) ) &
     &      / 3.0d0
        xele_psf(2)                                                     &
     &     = ( psf_nod%xx(i1,2) + psf_nod%xx(i2,2) + psf_nod%xx(i3,2) ) &
     &      / 3.0d0
        xele_psf(3)                                                     &
     &     = ( psf_nod%xx(i1,3) + psf_nod%xx(i2,3) + psf_nod%xx(i3,3) ) &
     &      / 3.0d0
!
        d2h_psf(1) = sqrt( (xele_psf(1) - psf_nod%xx(i1,1))**2          &
     &                   + (xele_psf(2) - psf_nod%xx(i1,2))**2          &
     &                   + (xele_psf(3) - psf_nod%xx(i1,3))**2 )
        d2h_psf(2) = sqrt( (xele_psf(1) - psf_nod%xx(i2,1))**2          &
     &                   + (xele_psf(2) - psf_nod%xx(i2,2))**2          &
     &                   + (xele_psf(3) - psf_nod%xx(i2,3))**2 )
        d2h_psf(2) = sqrt( (xele_psf(1) - psf_nod%xx(i3,1))**2          &
     &                   + (xele_psf(2) - psf_nod%xx(i3,2))**2          &
     &                   + (xele_psf(3) - psf_nod%xx(i3,3))**2 )
!
!
        if ( d2h_psf(1).eq.0.0d0 ) then
          psf_norm%weight_4_nod(i1) = -1.0d30
          psf_norm%norm_nod(i1,1) = psf_norm%norm_ele(iele,1)
          psf_norm%norm_nod(i1,2) = psf_norm%norm_ele(iele,2)
          psf_norm%norm_nod(i1,3) = psf_norm%norm_ele(iele,3)
        else
          psf_norm%weight_4_nod(i1) = psf_norm%weight_4_nod(i1)         &
     &                                + one/d2h_psf(1)
          psf_norm%norm_nod(i1,1) = psf_norm%norm_nod(i1,1)             &
     &      + psf_norm%norm_ele(iele,1) * (d2h_psf(2)+d2h_psf(3))
          psf_norm%norm_nod(i1,2) = psf_norm%norm_nod(i1,2)             &
     &      + psf_norm%norm_ele(iele,2) * (d2h_psf(2)+d2h_psf(3))
          psf_norm%norm_nod(i1,3) = psf_norm%norm_nod(i1,3)             &
     &      + psf_norm%norm_ele(iele,3) * (d2h_psf(2)+d2h_psf(3))
        end if
!
        if ( d2h_psf(2).eq.0.0d0 ) then
          psf_norm%weight_4_nod(i2) = -1.0d30
          psf_norm%norm_nod(i2,1) = psf_norm%norm_ele(iele,1)
          psf_norm%norm_nod(i2,2) = psf_norm%norm_ele(iele,2)
          psf_norm%norm_nod(i2,3) = psf_norm%norm_ele(iele,3)
        else
          psf_norm%weight_4_nod(i2) = psf_norm%weight_4_nod(i2)         &
     &                                + one/d2h_psf(2)
          psf_norm%norm_nod(i2,1) = psf_norm%norm_nod(i2,1)             &
     &      + psf_norm%norm_ele(iele,1) * (d2h_psf(1)+d2h_psf(3))
          psf_norm%norm_nod(i2,2) = psf_norm%norm_nod(i2,2)             &
     &      + psf_norm%norm_ele(iele,2) * (d2h_psf(1)+d2h_psf(3))
          psf_norm%norm_nod(i2,3) = psf_norm%norm_nod(i2,3)             &
     &      + psf_norm%norm_ele(iele,3) * (d2h_psf(1)+d2h_psf(3))
        end if
!
        if ( d2h_psf(3).eq.0.0d0 ) then
          psf_norm%weight_4_nod(i3) = -1.0d30
          psf_norm%norm_nod(i3,1) = psf_norm%norm_ele(iele,1)
          psf_norm%norm_nod(i3,2) = psf_norm%norm_ele(iele,2)
          psf_norm%norm_nod(i3,3) = psf_norm%norm_ele(iele,3)
        else
          psf_norm%weight_4_nod(i3) = psf_norm%weight_4_nod(i3)         &
     &                                + one/d2h_psf(3)
          psf_norm%norm_nod(i3,1) = psf_norm%norm_nod(i3,1)             &
     &      + psf_norm%norm_ele(iele,1) * (d2h_psf(1)+d2h_psf(2))
          psf_norm%norm_nod(i3,2) = psf_norm%norm_nod(i3,2)             &
     &      + psf_norm%norm_ele(iele,2) * (d2h_psf(1)+d2h_psf(2))
          psf_norm%norm_nod(i3,3) = psf_norm%norm_nod(i3,3)             &
     &      + psf_norm%norm_ele(iele,3) * (d2h_psf(1)+d2h_psf(2))
        end if
!
      end do
!
!
!
      do inod = 1, psf_nod%numnod
        d = sqrt(psf_norm%norm_nod(inod,1)*psf_norm%norm_nod(inod,1)    &
     &         + psf_norm%norm_nod(inod,2)*psf_norm%norm_nod(inod,2)    &
     &         + psf_norm%norm_nod(inod,3)*psf_norm%norm_nod(inod,3))
        psf_norm%norm_nod(inod,1) = psf_norm%norm_nod(inod,1) / d
        psf_norm%norm_nod(inod,2) = psf_norm%norm_nod(inod,2) / d
        psf_norm%norm_nod(inod,3) = psf_norm%norm_nod(inod,3) / d
      end do
!
      end subroutine cal_nod_normal_4_psf
!
!-----------------------------------------------------------------------
!
      end module take_normals_4_psf
