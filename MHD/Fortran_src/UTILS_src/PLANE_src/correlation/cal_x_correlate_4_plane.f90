!
!      module cal_x_correlate_4_plane
!
      module cal_x_correlate_4_plane
!
!      Written by Kemorin
!
      use m_precision
!
      implicit    none
!
!      subroutine s_cal_x_correlate_4_plane(istep)
!      subroutine s_cal_x_correlate_4_snap(istep)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_cal_x_correlate_4_plane(istep)
!
      use m_correlate_4_plane
      use cal_horizontal_rms_ave
      use output_correlation_data
!
      integer(kind = kint), intent(in) :: istep
      integer (kind = kint) :: ix, iy, iz, j, ii, i0
!
!
      call s_cal_horizontal_rms_ave
!
      do j = 1, num_crt
       do iz = 1, iz_max
        ii = iz + (j-1)*iz_max
!
        do iy = 1, ky_max
         do ix = 1, kx_max
          i0 = ix + (iy-1)*kx_max + (iz-1)*(kx_max*ky_max)              &
     &          + (j-1)*num_domain
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
      call output_correlate_plane(istep)
!
      end subroutine s_cal_x_correlate_4_plane
!
!  ---------------------------------------------------------------------
!
      subroutine s_cal_x_correlate_4_snap(istep)
!
      use m_correlate_4_plane
      use cal_horizontal_rms_ave
      use output_correlation_data
!
      integer(kind = kint), intent(in) :: istep
      integer (kind = kint) :: ix, iy, iz, j, ii, i0, jx, jy, j0
!
!
      call s_cal_horizontal_rms_ave
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
     &            + (iz-1)*(kx_max*ky_max) + (j-1)*num_domain
              do iy = 1, ky_max
                do ix = 1, kx_max
                  i0 = ix + (iy-1)*kx_max + (iz-1)*(kx_max*ky_max)      &
     &                + (j-1)*num_domain
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
          call output_correlate_snap(istep, jx, jy)
!
        end do
      end do
!
      end subroutine s_cal_x_correlate_4_snap
!
!  ---------------------------------------------------------------------
!
      end module cal_x_correlate_4_plane
