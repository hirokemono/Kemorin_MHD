!
!      module cal_x_correlate_4_plane
!
!      Written by Kemorin
!
!!      subroutine s_cal_x_correlate_4_plane                            &
!!     &         (crt_data_code, rms_data_code, istep, num_crt,         &
!!     &          num_domain_c, kx_max, ky_max, iz_max, z_out,          &
!!     &          phys_d1, ave_data, rms_data, sig_data,                &
!!     &          phys_d2, ave_data2, rms_data2, sig_data2,             &
!!     &          crt_data, rms_ratio)
!!      subroutine s_cal_x_correlate_4_snap                             &
!!     &         (crt_data_code, rms_data_code, istep, num_crt,         &
!!     &          num_domain_c, kx_max, ky_max, iz_max, x_out, y_out,   &
!!     &          z_out, phys_d1, ave_data, rms_data, sig_data,         &
!!     &          phys_d2, ave_data2, rms_data2, sig_data2,             &
!!     &          crt_data, rms_ratio)
!
      module cal_x_correlate_4_plane
!
!
      use m_precision
!
      implicit    none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_cal_x_correlate_4_plane                              &
     &         (crt_data_code, rms_data_code, istep, num_crt,           &
     &          num_domain_c, kx_max, ky_max, iz_max, z_out,            &
     &          phys_d1, ave_data, rms_data, sig_data,                  &
     &          phys_d2, ave_data2, rms_data2, sig_data2,               &
     &          crt_data, rms_ratio)
!
      use cal_horizontal_rms_ave
      use output_correlation_data
!
      integer(kind = kint), intent(in) :: crt_data_code
      integer(kind = kint), intent(in) :: rms_data_code
      integer(kind = kint), intent(in) :: istep
      integer(kind=kint ), intent(in) :: num_crt, num_domain_c
      integer(kind=kint ), intent(in) :: kx_max, ky_max, iz_max
      real(kind=kreal), intent(in)  ::  z_out(iz_max)
      real(kind=kreal), intent(in) :: phys_d1(num_domain_c*num_crt) 
      real(kind=kreal), intent(in) :: phys_d2(num_domain_c*num_crt) 
!
      real(kind=kreal), intent(inout) :: ave_data(iz_max*num_crt)
      real(kind=kreal), intent(inout) :: ave_data2(iz_max*num_crt)
      real(kind=kreal), intent(inout) :: rms_data(iz_max*num_crt)
      real(kind=kreal), intent(inout) :: rms_data2(iz_max*num_crt)
      real(kind=kreal), intent(inout) :: sig_data(iz_max*num_crt)
      real(kind=kreal), intent(inout) :: sig_data2(iz_max*num_crt)
      real(kind=kreal), intent(inout)  ::  rms_ratio(iz_max*num_crt)
!
      real   (kind=kreal), intent(inout)  ::  crt_data(iz_max*num_crt)
!
      integer (kind = kint) :: ix, iy, iz, j, ii, i0
!
!
      call s_cal_horizontal_rms_ave                                     &
     &         (kx_max, ky_max, iz_max, num_crt, num_domain_c,          &
     &          phys_d1, phys_d2, ave_data, rms_data, sig_data,         &
     &          ave_data2, rms_data2, sig_data2, crt_data, rms_ratio)
!
      do j = 1, num_crt
       do iz = 1, iz_max
        ii = iz + (j-1)*iz_max
!
        do iy = 1, ky_max
         do ix = 1, kx_max
          i0 = ix + (iy-1)*kx_max + (iz-1)*(kx_max*ky_max)              &
     &          + (j-1)*num_domain_c
          crt_data(ii) = crt_data(ii) + (phys_d1(i0) - ave_data(ii))    &
     &                  * (phys_d2(i0) - ave_data2(ii))
         end do
        end do
!
        crt_data(ii) = crt_data(ii) / (sig_data(ii) * sig_data2(ii))
       end do
      end do
!
!     ======================
!      data output
!     ======================
!
      write(*,*) 'write data (output_correlate_plane)'
      call output_correlate_plane(crt_data_code, rms_data_code,         &
     &    istep, iz_max, num_crt, z_out, crt_data, rms_ratio)
!
      end subroutine s_cal_x_correlate_4_plane
!
!  ---------------------------------------------------------------------
!
      subroutine s_cal_x_correlate_4_snap                               &
     &         (crt_data_code, rms_data_code, istep, num_crt,           &
     &          num_domain_c, kx_max, ky_max, iz_max, x_out, y_out,     &
     &          z_out, phys_d1, ave_data, rms_data, sig_data,           &
     &          phys_d2, ave_data2, rms_data2, sig_data2,               &
     &          crt_data, rms_ratio)
!
      use cal_horizontal_rms_ave
      use output_correlation_data
!
      integer(kind = kint), intent(in) :: crt_data_code
      integer(kind = kint), intent(in) :: rms_data_code
      integer(kind = kint), intent(in) :: istep
      integer(kind=kint ), intent(in) :: num_crt, num_domain_c
      integer(kind=kint ), intent(in) :: kx_max, ky_max, iz_max
      real(kind=kreal), intent(in)  ::  x_out(kx_max)
      real(kind=kreal), intent(in)  ::  y_out(ky_max)
      real(kind=kreal), intent(in)  ::  z_out(iz_max)
      real(kind=kreal), intent(in) :: phys_d1(num_domain_c*num_crt) 
      real(kind=kreal), intent(in) :: phys_d2(num_domain_c*num_crt) 
!
      real(kind=kreal), intent(inout) :: ave_data(iz_max*num_crt)
      real(kind=kreal), intent(inout) :: ave_data2(iz_max*num_crt)
      real(kind=kreal), intent(inout) :: rms_data(iz_max*num_crt)
      real(kind=kreal), intent(inout) :: rms_data2(iz_max*num_crt)
      real(kind=kreal), intent(inout) :: sig_data(iz_max*num_crt)
      real(kind=kreal), intent(inout) :: sig_data2(iz_max*num_crt)
      real(kind=kreal), intent(inout)  ::  rms_ratio(iz_max*num_crt)
!
      real   (kind=kreal), intent(inout)  ::  crt_data(iz_max*num_crt)
!
      integer (kind = kint) :: ix, iy, iz, j, ii, i0, jx, jy, j0
!
!
      call s_cal_horizontal_rms_ave                                     &
     &         (kx_max, ky_max, iz_max, num_crt, num_domain_c,          &
     &          phys_d1, phys_d2, ave_data, rms_data, sig_data,         &
     &          ave_data2, rms_data2, sig_data2, crt_data, rms_ratio)
!
      do jy = 1, ky_max
        write(*,*) 'offset of y: ', jy, ' of ', ky_max
        do jx = 1, kx_max
          crt_data = 0.0d0
!
          do j = 1, num_crt
            do iz = 1, iz_max
              ii = iz + (j-1)*iz_max
!
              j0 = mod((jx+kx_max/2),kx_max)                            &
     &            + (mod((jy+ky_max/2),ky_max)-1)*kx_max                &
     &            + (iz-1)*(kx_max*ky_max) + (j-1)*num_domain_c
              do iy = 1, ky_max
                do ix = 1, kx_max
                  i0 = ix + (iy-1)*kx_max + (iz-1)*(kx_max*ky_max)      &
     &                + (j-1)*num_domain_c
                  crt_data(ii) = crt_data(ii)                           &
     &                          + (phys_d1(i0) - ave_data(ii))          &
     &                           * (phys_d2(j0) - ave_data2(ii))
                end do
              end do
!
              crt_data(ii) = crt_data(ii)                               &
     &                      / (sig_data(ii) * sig_data2(ii))
            end do
          end do
!
!     ======================
!      data output
!     ======================
!
          call output_correlate_snap(crt_data_code, rms_data_code,      &
     &        istep, jx, jy, kx_max, ky_max, iz_max, num_crt,           &
     &        x_out, y_out, z_out, crt_data, rms_ratio)
        end do
      end do
!
      end subroutine s_cal_x_correlate_4_snap
!
!  ---------------------------------------------------------------------
!
      end module cal_x_correlate_4_plane
