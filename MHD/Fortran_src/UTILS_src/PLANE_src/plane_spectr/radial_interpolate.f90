!
!     module radial_interpolate
!
      module radial_interpolate
!
!      Written by H.Matsui
!      Modified by H.Matsui on June, 2007
!
      use m_precision
!
      implicit none
!
!      subroutine s_radial_interpolate
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_radial_interpolate
!
      use m_constants
      use m_size_of_cube
      use m_size_4_plane
      use m_spectr_4_ispack
      use m_set_new_spectr
!
      integer(kind=kint) :: i, j, ixy, iz, inod, i1, i2
!
!
      do i = 1, ncomp_nsp
       j = idx_field(i)
!
        do ixy = 1, kx_max*ky_max
         do iz = 1, c_size1%nz_all
          inod = (ixy-1) * c_size1%nz_all + iz
          i1   = (j-1)*num_spectr + iz_max*(ixy-1) + iz_1(iz)
          i2   = (j-1)*num_spectr + iz_max*(ixy-1) + iz_1(iz) - 1
          work_array(inod,i) = z_1(iz)*phys_io(i1)                      &
     &          + (one - z_1(iz))*phys_io(i2)
        end do
       end do
!
      end do
!
      end subroutine s_radial_interpolate
!
!  ---------------------------------------------------------------------
!
      end module radial_interpolate
