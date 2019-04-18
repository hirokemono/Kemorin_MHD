!
!      module cal_horizontal_rms_ave
!
!     Written by H. Matsui
!
!!      subroutine s_cal_horizontal_rms_ave                             &
!!     &         (kx_max, ky_max, iz_max, num_crt, num_domain_c,        &
!!     &          phys_d1, phys_d2, ave_data, rms_data, sig_data,       &
!!     &          ave_data2, rms_data2, sig_data2, crt_data, rms_ratio)
!
      module cal_horizontal_rms_ave
!
      use m_precision
!
      implicit none
!
      private :: cal_rms
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_cal_horizontal_rms_ave                               &
     &         (kx_max, ky_max, iz_max, num_crt, num_domain_c,          &
     &          phys_d1, phys_d2, ave_data, rms_data, sig_data,         &
     &          ave_data2, rms_data2, sig_data2, crt_data, rms_ratio)
!
      integer(kind=kint), intent(in) :: kx_max, ky_max, iz_max
      integer(kind=kint), intent(in) :: num_crt, num_domain_c
      real(kind=kreal), intent(in) ::  phys_d1(num_domain_c*num_crt)
      real(kind=kreal), intent(in) ::  phys_d2(num_domain_c*num_crt)
!
      real(kind=kreal), intent(inout)  ::  ave_data(iz_max*num_crt)
      real(kind=kreal), intent(inout)  ::  ave_data2(iz_max*num_crt)
      real(kind=kreal), intent(inout)  ::  rms_data(iz_max*num_crt)
      real(kind=kreal), intent(inout)  ::  rms_data2(iz_max*num_crt)
      real(kind=kreal), intent(inout)  ::  sig_data(iz_max*num_crt)
      real(kind=kreal), intent(inout)  ::  sig_data2(iz_max*num_crt)
      real(kind=kreal), intent(inout)  ::  crt_data(iz_max*num_crt)
      real(kind=kreal), intent(inout)  ::  rms_ratio(iz_max*num_crt)
!
      integer (kind = kint) :: ix, iy, iz, j, ii, i0
!
!
      do j = 1, num_crt
       do iz = 1, iz_max
        ii = iz + (j-1)*iz_max
        ave_data(ii) = 0.0d0
        ave_data2(ii) = 0.0d0
        rms_data(ii) = 0.0d0
        rms_data2(ii) = 0.0d0
        sig_data(ii) = 0.0d0
        sig_data2(ii) = 0.0d0
       end do
      end do
!
      crt_data = 0.0d0
      rms_ratio = 0.0d0
!
      do j = 1, num_crt
       do iz = 1, iz_max
        ii = iz + (j-1)*iz_max
!
        do iy = 1, ky_max
         do ix = 1, kx_max
          i0 = ix + (iy-1)*kx_max + (iz-1)*(kx_max*ky_max)              &
     &          + (j-1)*num_domain_c
          ave_data(ii) = ave_data(ii) + phys_d1(i0)
          ave_data2(ii) = ave_data2(ii) + phys_d2(i0)
          rms_data(ii) = rms_data(ii) + phys_d1(i0)**2
          rms_data2(ii) = rms_data2(ii) + phys_d2(i0)**2
         end do
        end do
!
        ave_data(ii) =  ave_data(ii) / dble(kx_max*ky_max)
        ave_data2(ii) = ave_data2(ii) / dble(kx_max*ky_max)
        rms_data(ii) =  rms_data(ii)
        rms_data2(ii) = rms_data2(ii)
       end do
      end do
!
!      do j = 1, num_crt
!       do iz = 1, iz_max
!        ii = iz + (j-1)*iz_max
!        write(50,*) j, iz, rms_data(ii), rms_data2(ii)
!       end do
!      end do
!
      do j = 1, num_crt
       do iz = 1, iz_max
        ii = iz + (j-1)*iz_max
!
        do iy = 1, ky_max
         do ix = 1, kx_max
          i0 = ix + (iy-1)*kx_max + (iz-1)*(kx_max*ky_max)              &
     &          + (j-1)*num_domain_c
          sig_data(ii) = sig_data(ii)                                   &
     &                + (phys_d1(i0) - ave_data(ii) )**2
          sig_data2(ii) = sig_data2(ii)                                 &
     &                + (phys_d2(i0) - ave_data2(ii) )**2
         end do
        end do
!
        call cal_rms(kx_max, ky_max, sig_data(ii),  rms_data(ii) )
        call cal_rms(kx_max, ky_max, sig_data2(ii), rms_data2(ii) )
!
       end do
      end do
!
!      do j = 1, num_crt
!       do iz = 1, iz_max
!        ii = iz + (j-1)*iz_max
!        write(51,*) j, iz, rms_data(ii), rms_data2(ii)
!       end do
!      end do
!
      do j = 1, num_crt
       do iz = 1, iz_max
        ii = iz + (j-1)*iz_max
        rms_ratio(ii) = rms_data(ii) / rms_data2(ii)
       end do
      end do
!
      end subroutine s_cal_horizontal_rms_ave
!
!  ---------------------------------------------------------------------
!
      subroutine cal_rms(kx_max, ky_max, sig_data, rms_data)
!
      integer(kind = kint), intent(in) :: kx_max, ky_max
      real(kind = kreal), intent(inout) :: sig_data, rms_data
!
!
        if ( sig_data .eq. 0.0d0 ) then
          sig_data = 1.0d-30
        else
          sig_data = sqrt( sig_data )
        end if
!
        if ( rms_data .le. 0.0d0 ) then
          rms_data = 1.0d-30
        else
          rms_data = sqrt( rms_data )/ dble(kx_max*ky_max)
        end if
!
      end subroutine cal_rms
!
!  ---------------------------------------------------------------------
!
      end module cal_horizontal_rms_ave
