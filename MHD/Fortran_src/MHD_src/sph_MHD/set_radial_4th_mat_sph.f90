!>@file   set_radial_4th_mat_sph.f90
!!@brief  module set_radial_4th_mat_sph
!!
!!@author H. Matsui
!!@date Programmed in Apr, 2009
!
!>@brief  Construct 4th order matrix for spherical shell dynamo model
!!
!!@verbatim
!!      subroutine set_radial_vp5_mat_sph(nri, jmax, kr_in, kr_out,     &
!!     &          ar_1d_rj, g_sph_rj, poisson_mat)
!!
!!    Format of band matrix
!!               | a(2,1)  a(1,2)  ........     0         0     |
!!               | a(3,1)  a(2,2)  ........     .         .     |
!!               |   0     a(3,2)  ........     .         .     |
!!    a(i,j)  =  |   .       0     ........     0         .     |
!!               | ...... a(3,k-1)  a(2,k)  a(1,k+1) .......... |
!!               |   .       .     ........  a(1,N-2)     0     |
!!               |   .       .     ........  a(2,N-2)  a(1,N-1) |
!!               |   0       0     ........  a(3,N-2)  a(2,N-1) |
!!
!!   Original band matrix
!!      band_a(i-j+iband+1,j) = a(i,j)
!!      band_a(k,j) = a(k+j-iband-1,j)
!!   3-band matrix
!!      band_a(i-j+2,j) = a(i,j)
!!      band_a(k,j) = a(k+j-2,j)
!!   5-band matrix
!!      band_lu(i-j+3,j) = a(i,j)
!!      band_lu(k,j) = a(k+j-3,j)
!!   7-band matrix
!!      band_lu(i-j+4,j) = a(i,j)
!!      band_lu(k,j) = a(k+j-4,j)
!!@endverbatim
!!
!!@n @param nri     Number of radial points
!!@n @param jmax    Number of spherical harmonics modes
!!@n @param kr_st   Start radial address to construct matrix
!!@n @param kr_ed   End radial address to construct matrix
!!@n @param kr_in    Radial address for inner boundary
!!@n @param kr_out   Radial address for outer boundary
!!@n @param coef_imp   Coefficient for contribution of implicit term
!!@n @param coef_d     Coefficient of diffusiotn term
!!@n @param coef_p     Coefficient of pressure gradient
!!
!!@n @param evo_mat(3,nri,jmax)  Band matrix for time evolution
!!@n @param poisson_mat(3,nri,jmax)  Band matrix for Poisson equation
!
      module set_radial_4th_mat_sph
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_t_int_parameter
      use m_fdm_4th_coefs
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_radial_vp5_mat_sph(nri, jmax, kr_in, kr_out,       &
     &          ar_1d_rj, g_sph_rj, poisson_mat)
!
      integer(kind = kint), intent(in) :: jmax, nri
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: ar_1d_rj(nri,3)
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!
      real(kind = kreal), intent(inout) :: poisson_mat(5,nri,jmax)
!
      integer(kind = kint) :: k, j
!
!
!$omp do private (k,j)
      do k = kr_in+2, kr_out-2
        do j = 1, jmax
          poisson_mat(5,k-2,j) = - d2nod_mat_fdm_4(k,-2)
          poisson_mat(4,k-1,j) = - d2nod_mat_fdm_4(k,-1)
          poisson_mat(3,k,  j) = - d2nod_mat_fdm_4(k, 0)                &
     &                            + g_sph_rj(j,3)*ar_1d_rj(k,2)
          poisson_mat(2,k+1,j) = - d2nod_mat_fdm_4(k, 1)
          poisson_mat(1,k+2,j) = - d2nod_mat_fdm_4(k, 2)
        end do
      end do
!$omp end do nowait
!
      end subroutine set_radial_vp5_mat_sph
!
! -----------------------------------------------------------------------
!
      end module set_radial_4th_mat_sph
