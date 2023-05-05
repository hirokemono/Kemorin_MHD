!>@file   t_circle_transform.f90
!!@brief  module t_circle_transform
!!
!!@author H. Matsui
!!@date Programmed on June., 2013
!
!>@brief  spherical transform at a specific circle at @f$(r, theta)@f$
!!
!!@verbatim
!!      subroutine alloc_circle_transform(ltr, circ_spec)
!!      subroutine dealloc_circle_transform(circ_spec)
!!@endverbatim
!!
!!@n @param  ltr      Truncation of spherical harmonics
!!@n @param  jmax     Number of modes for harmonincs except for 0 degree
!!@n @param  d_rj_circle(0:jmax,3)   Spectr field data
!!@n @param  numdir   Number of components of field
!!@n @param v_rtp_circle(mphi_circle,numdir)  Field along circle
!!@n @param vrtm_mag(0:mphi_circle,numdir)  Amplitude of spectrum data
!!                                        along with the circle
!!@n @param vrtm_phase(0:mphi_circle,numdir)    Phase of spectrum data
!!                                        along with the circle
!
      module t_circle_transform
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_phys_data
!
      implicit none
!
!
      type circle_transform_spetr
!>        Truncation level for spherical transform at equator
        integer(kind = kint) :: ltr_circle
!>        Number of modes for spherical transform at equator
        integer(kind = kint) :: jmax_circle
!>        end address of SMP parallelization for scalar Fourier transform
        integer(kind = kint), allocatable :: istack_circfft_smp(:)
!
!>        Radius for specific circle
        real(kind = kreal) :: r_circle
!>        @f$ 1/ r @f$ for specific circle
        real(kind = kreal) :: ar_circle
!>        @f$ 1/ r^{2} @f$ for specific circle
        real(kind = kreal) :: ar2_circle
!
!>        colatitude for specific circle
        real(kind = kreal) :: theta_circle
!
!>        associated Lagender polynomial at circle
        real(kind = kreal), allocatable :: P_circle(:)
!>         @f$ dP_{l}^{m}/ d\theta @f$ at circle
        real(kind = kreal), allocatable :: dPdt_circle(:)
!
!>        spectr data for Fourier transform at a circle
        real(kind = kreal), allocatable :: vcirc_rtm(:,:)
      end type circle_transform_spetr
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_circle_transform(ltr, circ_spec)
!
      integer(kind = kint), intent(in) :: ltr
      type(circle_transform_spetr), intent(inout) :: circ_spec
!
!
      circ_spec%ltr_circle =  ltr
      circ_spec%jmax_circle = ltr*(ltr+2)
!
      allocate(circ_spec%P_circle(0:circ_spec%jmax_circle))
      allocate(circ_spec%dPdt_circle(0:circ_spec%jmax_circle))
      circ_spec%P_circle =    zero
      circ_spec%dPdt_circle = zero
!
      allocate(circ_spec%vcirc_rtm(-ltr:ltr,3))
      circ_spec%vcirc_rtm = zero
!
      allocate(circ_spec%istack_circfft_smp(0:np_smp))
      circ_spec%istack_circfft_smp(0) =        0
      circ_spec%istack_circfft_smp(1:np_smp) = 1
!
      end subroutine alloc_circle_transform
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_circle_transform(circ_spec)
!
      type(circle_transform_spetr), intent(inout) :: circ_spec
!
!
      deallocate(circ_spec%P_circle, circ_spec%dPdt_circle)
      deallocate(circ_spec%vcirc_rtm)
      deallocate(circ_spec%istack_circfft_smp)
!
      end subroutine dealloc_circle_transform
!
! ----------------------------------------------------------------------
!
     end module t_circle_transform
