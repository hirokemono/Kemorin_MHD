!
!      module output_correlation_data
!
!      Written by Kemorin
!
!!      subroutine output_correlate_plane(crt_data_code, rms_data_code, &
!!     &          istep, iz_max, num_crt, z_out, crt_data, rms_ratio)
!!      subroutine output_correlate_snap(crt_data_code, rms_data_code,  &
!!     &          istep, ix, iy, kx_max, ky_max, iz_max, num_crt,       &
!!     &           x_out, y_out, z_out, crt_data, rms_ratio)
!!
      module output_correlation_data
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
      subroutine output_correlate_plane(crt_data_code, rms_data_code,   &
     &          istep, iz_max, num_crt, z_out, crt_data, rms_ratio)
!
      integer(kind = kint), intent(in) :: crt_data_code
      integer(kind = kint), intent(in) :: rms_data_code
      integer(kind = kint), intent(in) :: istep
      integer(kind=kint ), intent(in) :: iz_max, num_crt
      real(kind=kreal), intent(in)  ::  z_out(iz_max)
      real(kind=kreal), intent(in)  ::  crt_data(iz_max*num_crt)
      real(kind=kreal), intent(in)  ::  rms_ratio(iz_max*num_crt)
!
      integer(kind = kint) :: iz, j
!
!
      do iz = 1, iz_max
!
        write(crt_data_code,4001) istep, iz, z_out(iz),                 &
     &      ( crt_data(iz+(j-1)*iz_max), j = 1, num_crt)
        write(rms_data_code,4001) istep, iz, z_out(iz),                 &
     &      ( rms_ratio(iz+(j-1)*iz_max), j = 1, num_crt)
!
      end do
!
 4001  format(2i16, 1p255E25.15e3)
!
      end subroutine output_correlate_plane
!
!  ---------------------------------------------------------------------
!
      subroutine output_correlate_snap(crt_data_code, rms_data_code,    &
     &          istep, ix, iy, kx_max, ky_max, iz_max, num_crt,         &
     &           x_out, y_out, z_out, crt_data, rms_ratio)
!
      integer(kind = kint), intent(in) :: crt_data_code
      integer(kind = kint), intent(in) :: rms_data_code
      integer(kind = kint), intent(in) :: istep, ix, iy
!
      integer(kind=kint ), intent(in) :: kx_max, ky_max
      integer(kind=kint ), intent(in) :: iz_max, num_crt
      real(kind=kreal), intent(in)  ::  x_out(kx_max)
      real(kind=kreal), intent(in)  ::  y_out(ky_max)
      real(kind=kreal), intent(in)  ::  z_out(iz_max)
      real(kind=kreal), intent(in)  ::  crt_data(iz_max*num_crt)
      real(kind=kreal), intent(in)  ::  rms_ratio(iz_max*num_crt)
!
      integer(kind = kint) :: iz, j
!
!
      if (ix.eq.1 .and. iy.eq.1) then
      do iz = 1, iz_max
        write(rms_data_code,4001) istep, iz, z_out(iz),                 &
     &      ( rms_ratio(iz+(j-1)*iz_max), j = 1, num_crt)
      end do
      end if
!
      do iz = 1, iz_max
!
        write(crt_data_code,4002) istep, ix, iy, iz,                    &
     &       x_out(ix), y_out(iy), z_out(iz),                           &
     &      ( crt_data(iz+(j-1)*iz_max), j = 1, num_crt)
!
      end do
!
 4001 format(2i16, 1p255E25.15e3)
 4002 format(4i16, 1p255E25.15e3)
!
      end subroutine output_correlate_snap
!
!  ---------------------------------------------------------------------
!
      end module output_correlation_data
