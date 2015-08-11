!>@file   m_vpol_evo7_mat_sph.f90
!!@brief  module m_vpol_evo7_mat_sph
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief Radial matrix for time evolution
!!        of poloidal velocity with 7-band matrix
!!
!!@verbatim
!!      subroutine allocate_vpol_evo7_mat_sph
!!      subroutine deallocate_vpol_evo7_mat_sph
!!      subroutine check_vpol_evo7_mat_sph(my_rank)
!!@endverbatim
!
      module m_vpol_evo7_mat_sph
!
      use m_precision
!
      implicit none
!
      real(kind = kreal), allocatable :: vs_evo7_mat(:,:,:)
      real(kind = kreal), allocatable :: vs_evo7_lu(:,:,:)
      real(kind = kreal), allocatable :: vs_evo7_det(:,:)
      integer(kind = kint), allocatable :: i_vs_evo7_pivot(:,:)
!
      real(kind = kreal), allocatable :: vs_poisson5_mat(:,:,:)
      real(kind = kreal), allocatable :: vs_poisson5_lu(:,:,:)
      real(kind = kreal), allocatable :: vs_poisson5_det(:,:)
      integer(kind = kint), allocatable :: i_vs_poisson5_pivot(:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_vpol_evo7_mat_sph
!
      use m_spheric_parameter
!
      integer(kind = kint) :: nri, jmax
!
      nri = nidx_rj(1)
      jmax = nidx_rj(2)
!
!
      allocate( vs_poisson5_mat(5,nri,jmax) )
      allocate( vs_poisson5_lu(7,nri,jmax) )
      allocate( vs_poisson5_det(nri,jmax) )
      allocate( i_vs_poisson5_pivot(nri,jmax) )
!
      allocate( vs_evo7_mat(7,nri,jmax) )
      allocate( vs_evo7_lu(13,nri,jmax) )
      allocate( vs_evo7_det(nri,jmax) )
      allocate( i_vs_evo7_pivot(nri,jmax) )
!
      vs_evo7_mat =   0.0d0
      vs_evo7_lu =    0.0d0
      vs_evo7_det =   0.0d0
      i_vs_evo7_pivot =   0
!
      vs_poisson5_mat =   0.0d0
      vs_poisson5_lu =    0.0d0
      vs_poisson5_det =   0.0d0
      i_vs_poisson5_pivot =   0
!
      vs_evo7_mat(3,1:nri,1:jmax) = 1.0d0
!
      if(nlayer_ICB .gt. 1) then
        vs_poisson5_mat(2,1:nlayer_ICB-1,1:jmax) = 1.0d0
      end if
!
      if(nlayer_CMB .lt. nri) then
        vs_poisson5_mat(2,nlayer_CMB+1:nri,1:jmax) = 1.0d0
      end if
!
      end subroutine allocate_vpol_evo7_mat_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_vpol_evo7_mat_sph
!
!
      deallocate( vs_poisson5_mat, vs_poisson5_lu )
      deallocate( vs_poisson5_det, i_vs_poisson5_pivot )
!
      deallocate( vs_evo7_mat, vs_evo7_lu )
      deallocate( vs_evo7_det, i_vs_evo7_pivot )
!
      end subroutine deallocate_vpol_evo7_mat_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_vpol_evo7_mat_sph(my_rank)
!
      use m_spheric_parameter
      use check_sph_radial_mat
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      write(50+my_rank,'(a)') 'poisson matrix for poloidal velocity'
      call check_radial_5band_mat(my_rank, nidx_rj(1), nidx_rj(2),      &
     &    idx_gl_1d_rj_j, radius_1d_rj_r, vs_poisson5_mat)
!
      write(50+my_rank,'(a)') 'crank matrix for poloidal velocity'
      call check_radial_7band_mat(my_rank, nidx_rj(1), nidx_rj(2),      &
     &    idx_gl_1d_rj_j, radius_1d_rj_r, vs_evo7_mat)
!
      end subroutine check_vpol_evo7_mat_sph
!
! -----------------------------------------------------------------------
!
      end module m_vpol_evo7_mat_sph
