!>@file   cal_sph_filtering_data.f90
!!@brief  module cal_sph_filtering_data
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Evaluate horizontal filtering in spectrunm space
!!
!!@verbatim
!!      subroutine set_sph_gaussian_filter(l_truncation, f_width,       &
!!     &          weight, num_momentum, filter_mom)
!!      subroutine set_sph_cutoff_filter(l_truncation, f_width,         &
!!     &          weight, num_momentum, filter_mom)
!!      subroutine set_sph_recursive_filter                             &
!!     &         (l_truncation, num_momentum, nmom_ref1, nmom_ref2,     &
!!     &          ref1_weight, ref1_mom, ref2_weight, ref2_mom,         &
!!     &          weight, filter_mom)
!!@endverbatim
!!
!
      module cal_sph_filtering_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_sph_gaussian_filter(l_truncation, f_width,         &
     &          weight, num_momentum, filter_mom)
!
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: num_momentum
      real(kind = kreal), intent(in) :: f_width
      real(kind = kreal), intent(inout) :: weight(0:l_truncation)
      real(kind = kreal), intent(inout) :: filter_mom(0:num_momentum-1)
!
      integer(kind = kint) :: i, l, l_rest, k_rev
      real(kind = kreal) :: b, pi
!
!
      pi = four * atan(one)
!      k_rev = 2*l_truncation / f_width
!      b = log(two) / (one                                               &
!     &   - cos(pi*dble(f_width) / (dble(l_truncation+1)*six)))
      b = four * pi * (dble(l_truncation)*f_width)**2 / three 
!
      weight(0) = one
      if(b .gt. 227.0) then
        weight(1) = one
      else
        weight(1) = (one + exp(-two*b)) / (one - exp(-two*b)) - one / b
      end if
!
      l_rest = l_truncation+1
      do l = 2, l_truncation
        weight(l) = - weight(l-1) * dble(2*l - 1) / b + weight(l-2)
        if(weight(l) .le. zero) then
          l_rest = l
          exit
        end if
      end do
!
      do l = l_rest, l_truncation
        weight(l) = zero
      end do
!
      filter_mom(0) = one
      filter_mom(1) = zero
!      filter_mom(2) = pi * f_width**2 / 12.0d0                         &
!     &               * (dble(l_truncation*ithree / itwo))**(-2)
      filter_mom(2) = f_width**2 / 12.0d0 
      do i = 3, num_momentum-2, 2
        filter_mom(i) = zero
        filter_mom(i+1) =   real(2*i-1) * filter_mom(i-1) * b**i
      end do
      filter_mom(0:num_momentum-1) = filter_mom**2
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &        'Gaussian filter moments for for sphere:', filter_mom
!
      end subroutine set_sph_gaussian_filter
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_cutoff_filter(l_truncation, f_width,           &
     &          weight, num_momentum, filter_mom)
!
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: num_momentum
      real(kind = kreal), intent(in) :: f_width
      real(kind = kreal), intent(inout) :: weight(0:l_truncation)
      real(kind = kreal), intent(inout) :: filter_mom(0:num_momentum-1)
!
      integer(kind = kint) :: i, l, l_cutoff
!
!
      l_cutoff = min(l_truncation, int(dble(l_truncation)/f_width))
      do l = 0, l_cutoff
        weight(l) = one
      end do
      do l = l_cutoff+1, l_truncation
        weight(l) = zero
      end do
!
      filter_mom(0) = one
      do i = 1, num_momentum-2, 2
        filter_mom(i) = zero
        filter_mom(i+1) =   real(2*i-1) * filter_mom(i-1)
      end do
      filter_mom(0:num_momentum-1) = filter_mom**2
!
      end subroutine set_sph_cutoff_filter
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_recursive_filter                               &
     &         (l_truncation, num_momentum, nmom_ref1, nmom_ref2,       &
     &          ref1_weight, ref1_mom, ref2_weight, ref2_mom,           &
     &          weight, filter_mom)
!
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: nmom_ref1, nmom_ref2
      integer(kind = kint), intent(in) :: num_momentum
      real(kind = kreal), intent(in) :: ref1_weight(0:l_truncation)
      real(kind = kreal), intent(in) :: ref2_weight(0:l_truncation)
      real(kind = kreal), intent(in) :: ref1_mom(0:num_momentum-1)
      real(kind = kreal), intent(in) :: ref2_mom(0:num_momentum-1)
!
      real(kind = kreal), intent(inout) :: weight(0:l_truncation)
      real(kind = kreal), intent(inout) :: filter_mom(0:num_momentum-1)
!
      integer(kind = kint) :: min_nmom
!
!
      min_nmom = min(nmom_ref1, nmom_ref2)
      weight(0:l_truncation) =   ref1_weight(0:l_truncation)            &
     &                         * ref2_weight(0:l_truncation)
      filter_mom(0:min_nmom-1) = ref1_mom(0:min_nmom-1)                 &
     &                         * ref2_mom(0:min_nmom-1)
!
      end subroutine set_sph_recursive_filter
!
! -----------------------------------------------------------------------
!
      end module cal_sph_filtering_data
