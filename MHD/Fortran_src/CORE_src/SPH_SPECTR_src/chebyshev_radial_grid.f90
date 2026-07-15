!>@file   chebyshev_radial_grid.f90
!!@brief  module chebyshev_radial_grid
!!
!!@author H. Okuda and H. Matsui
!!@date Programmed in Sep., 2009
!
!> @brief Set radial grid data
!!
!!@verbatim
!!      subroutine set_chebyshev_distance_shell(num_layer, nlayer_ICB,  &
!!     &          nlayer_CMB, r_ICB, r_CMB, r_grid)
!!      subroutine adjust_chebyshev_shell                               &
!!     &         (num_layer, nlayer_ICB, nlayer_CMB, increment, r_grid)
!!
!!      subroutine count_chebyshev_ext_layers(nri, r_ICB, r_CMB,        &
!!     &          r_min, r_max, ntot_shell, nlayer_ICB, nlayer_CMB)
!!@endverbatim
!
      module chebyshev_radial_grid
!
      use m_precision
      use m_constants
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine set_chebyshev_distance_shell(num_layer, nlayer_ICB,    &
     &          nlayer_CMB, r_ICB, r_CMB, r_grid)
!
      integer(kind = kint), intent(in) :: num_layer
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
!
      real(kind = kreal), intent(inout) :: r_grid(num_layer)
!
      integer(kind = kint) :: kst, ked, k, nri
      real(kind = kreal) :: pi, dr, shell
!
      write(*,*) 'r_grid start', r_grid
      write(*,*) 'nlayer_ICB', nlayer_ICB
      write(*,*) 'r_ICB', r_ICB
!
      pi = four * atan(one)
      nri = nlayer_CMB - nlayer_ICB
      shell = r_CMB - r_ICB
!
      kst = 1
      ked = nlayer_ICB-nri/2 - 1
      do k = kst, ked
        r_grid(k) = (r_ICB - half * shell) * dble(k) / dble(ked+1)
      end do
!
      kst = max(ione,nlayer_ICB-nri/itwo)
      ked = nlayer_ICB-1
      do k = kst, ked
        r_grid(k) = r_ICB - half * shell * (one - cos( pi               &
     &              * dble(k-nlayer_ICB)/dble(nri)) )
      end do
!
      kst = nlayer_ICB
      ked = nlayer_CMB
      do k = kst, ked
        r_grid(k) = r_ICB + half * shell * (one - cos( pi               &
     &              * dble(k-nlayer_ICB)/dble(nri)) )
      end do
!
      kst = nlayer_CMB + 1
      ked = min(num_layer, nlayer_CMB + nri/2)
      do k = kst, ked
        r_grid(k) = r_CMB + half * shell * (one - cos( pi               &
     &              * dble(k-nlayer_CMB)/dble(nri)) )
      end do
      dr = r_grid(ked) - r_grid(ked-1)
!
      kst = nlayer_CMB + nri/2 + 1
      ked = num_layer
      do k = kst, ked
        r_grid(k) = r_grid(k-1) + dr
      end do
!
      write(*,*) 'r_grid end', r_grid
!
      end subroutine set_chebyshev_distance_shell
!
!  -------------------------------------------------------------------
!
      subroutine adjust_chebyshev_shell                                 &
     &         (num_layer, nlayer_ICB, nlayer_CMB, increment, r_grid)
!
      integer(kind = kint), intent(in) :: num_layer
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: increment
!
      real(kind = kreal), intent(inout) :: r_grid(num_layer)
!
      integer(kind = kint) :: k, kk, kst, ked, nri
      real(kind = kreal) :: r1, r2
!
!
      if(increment .le. 1) return
      nri = nlayer_CMB - nlayer_ICB
!
      do k = nlayer_ICB, nlayer_CMB-increment, increment
        if(nlayer_ICB .eq. 0) then
          r1 = 0.0d0
        else
          r1 = r_grid(k)
        end if
        r2 = r_grid(k+increment)
!
        do kk = 1, increment-1
          r_grid(k+kk) = r1 + (r2 - r1) * dble(kk) / dble(increment)
        end do
      end do
!
      ked = min(num_layer, nlayer_CMB + nri/2)
      do k = nlayer_CMB+1, ked
        kk = 2*nlayer_CMB - k
        r_grid(k) = 2.0d0 * r_grid(nlayer_CMB) - r_grid(kk)
      end do
!
      kst = max(ione, nlayer_ICB-nri/itwo)
      do k = kst, nlayer_ICB-1
        kk = 2*nlayer_ICB - k
        r_grid(k) = 2.0d0 * r_grid(nlayer_ICB) - r_grid(kk)
      end do
!
      end subroutine adjust_chebyshev_shell
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine count_chebyshev_ext_layers(nri, r_ICB, r_CMB,          &
     &          r_min, r_max, ntot_shell, nlayer_ICB, nlayer_CMB)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      real(kind = kreal), intent(in) :: r_min, r_max
!
      integer(kind = kint), intent(inout) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(inout) :: ntot_shell
!
      real(kind = kreal) :: shell
      integer(kind = kint) :: ngrid_icore, ngrid_external
!
!
      shell = r_CMB - r_ICB
      dr =   half * shell * ( one - cos( pi/dble(nri)) )
!
      ngrid_icore = count_chebyshev_inner_shell(shell, nri,             &
     &                                          r_ICB, r_min)
      ngrid_external = count_chebyshev_external(shell, nri,             &
     &                                          r_CMB, r_max)
!
      nlayer_ICB = ngrid_icore + 1
      nlayer_CMB = nlayer_ICB +  nri
      ntot_shell = nlayer_CMB + ngrid_ext
!
      end subroutine count_chebyshev_ext_layers
!
!  -------------------------------------------------------------------
!
      integer(kind = kint) function count_chebyshev_inner_shell         &
     &                                    (shell, nri, r_ICB, r_min)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: shell
      real(kind = kreal), intent(in) :: r_ICB
      real(kind = kreal), intent(in) :: r_min
!
      real(kind = kreal), parameter :: pi = four * atan(one)
      real(kind = kreal) :: dr, r
      integer(kind = kint) :: k
!
!
      dr =   half * shell * (one - cos( pi/dble(nri)))
!
!      Set grid by Chebyshev grid
      r = r_ICB
      k = 0
      do
        if(r .le. zero)  exit
        if(r .le. r_min) exit
        if(k .ge. nri/2) exit
!
        k = k + 1
        r = r_ICB - half * shell * (one - cos( pi*dble(k)/dble(nri)) )
        dr =   half * shell * ( cos( pi*dble(k-1)/dble(nri))            &
     &                        - cos( pi*dble(k  )/dble(nri)) )
!        write(*,*) k, r, dr
      end do
!
!      Set grid by equidistance grid
      if(k .eq. nri/2) k = int(aint((r - r_min)/dr), KIND(k)) + nri/2
      count_chebyshev_inner_shell = max(k-1, 0)
!
      end function count_chebyshev_inner_shell
!
!  -------------------------------------------------------------------
!
      integer(kind = kint) function count_chebyshev_external            &
     &                                     (shell, nri, r_CMB, r_max)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: shell
      real(kind = kreal), intent(in) :: r_CMB
      real(kind = kreal), intent(in) :: r_max
!
      real(kind = kreal), parameter :: pi = four * atan(one)
      real(kind = kreal) :: dr, r, shell
      integer(kind = kint) :: ngrid_icore, ngrid_ext
      integer(kind = kint) :: k
!
!
      dr =   half * shell * ( one - cos( pi/dble(nri)) )
!
      r = r_CMB
      k = 0
      do
        if(r .ge. r_max) exit
        if(k .ge. nri/2) exit
!
        k = k + 1
        r = r_CMB + half * shell * (one - cos( pi*dble(k)/dble(nri)) )
        dr =  half * shell * ( -cos( pi*dble(k  )/dble(nri))            &
     &                        + cos( pi*dble(k-1)/dble(nri)) )
!        write(*,*) k, r, dr
      end do
!
      do
        if(r .ge. r_max) exit
        k = k + 1
        r = r + dr
!        write(*,*) k, r, dr
      end do
!
      count_chebyshev_external = k
      if(k .le. 1) count_chebyshev_external = 0
!
      end function count_chebyshev_external
!
!  -------------------------------------------------------------------
!
      end module chebyshev_radial_grid
