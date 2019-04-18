!
!     module radial_interpolate
!
!      Written by H.Matsui
!      Modified by H.Matsui on June, 2007
!
!!      subroutine s_radial_interpolate(c_size, kx_max, ky_max, iz_max,&
!!     &          num_fft, num_io, num_spectr, phys_io, z_1, iz_1,     &
!!     &          nnod_new_k_org_z, ncomp_nsp, idx_field, work_array)
!!        type(size_of_cube), intent(in) :: c_size
!
      module radial_interpolate
!
      use m_precision
      use m_constants
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_radial_interpolate(c_size, kx_max, ky_max, iz_max,  &
     &          num_fft, num_io, num_spectr, phys_io, z_1, iz_1,       &
     &          nnod_new_k_org_z, ncomp_nsp, idx_field, work_array)
!
      use t_size_of_cube
!
      type(size_of_cube), intent(in) :: c_size
      integer(kind=kint ), intent(in) :: kx_max, ky_max, iz_max
      integer(kind=kint ), intent(in) :: num_fft, num_io, num_spectr
      real   (kind=kreal), intent(in) ::  phys_io(num_io*num_fft)
      real   (kind=kreal), intent(in) ::  z_1(c_size%nz_all)
      integer(kind=kint ), intent(in) ::  iz_1(c_size%nz_all)
      integer(kind=kint ), intent(in) :: nnod_new_k_org_z, ncomp_nsp
      integer(kind=kint), intent(in)  ::  idx_field(ncomp_nsp)
!
      real(kind=kreal), intent(inout)                                   &
     &                  :: work_array(nnod_new_k_org_z,ncomp_nsp)
!
      integer(kind=kint) :: i, j, ixy, iz, inod, i1, i2
!
!
      do i = 1, ncomp_nsp
       j = idx_field(i)
!
        do ixy = 1, kx_max*ky_max
         do iz = 1, c_size%nz_all
          inod = (ixy-1) * c_size%nz_all + iz
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
