!
!      module output_correlation_data
!
      module output_correlation_data
!
!      Written by Kemorin
!
      use m_precision
!
      implicit    none
!
!      subroutine output_correlate_plane(istep)
!      subroutine output_correlate_snap(istep, ix, iy)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine output_correlate_plane(istep)
!
      use m_correlate_4_plane
      use m_size_4_plane
!
      integer(kind = kint), intent(in) :: istep
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
      subroutine output_correlate_snap(istep, ix, iy)
!
      use m_correlate_4_plane
      use m_size_4_plane
!
      integer(kind = kint), intent(in) :: istep, ix, iy
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
