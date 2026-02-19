!>@file   init_radial_reference_temp.f90
!!@brief  module init_radial_reference_temp
!!
!!@author H. Matsui
!!@date Programmed in March, 2008
!
!> @brief Set initial data for spectrum dynamos
!!
!!@verbatim
!!     Tempareture at fluid core
!!       T = const_OC + coef_OC / r - (1/6) source_OC r^2
!!     Tempareture at inner core
!!       T = const_IC - (1/6) source_IC r^2
!!
!!      subroutine init_outer_core_ref_temp(kr_in, kr_out, nri_1d, r_1d,&
!!     &          source_OC, const_OC, coef_OC, reference)
!!        integer(kind = kint), intent(in) :: kr_in, kr_out
!!        integer(kind = kint), intent(in) :: nri_1d
!!        real(kind = kreal), intent(in) :: r_1d(0:nri_1d)
!!        real(kind = kreal), intent(in) :: source_OC
!!        real(kind = kreal), intent(in) :: const_OC, coef_OC
!!        real(kind = kreal), intent(inout) :: reference(0:nri_1d)
!!      subroutine init_inner_core_ref_temp(kr_in, nri_1d, r_1d,        &
!!     &          source_IC, const_IC, reference)
!!        integer(kind = kint), intent(in) :: kr_in
!!        integer(kind = kint), intent(in) :: nri_1d
!!        real(kind = kreal), intent(in) :: r_1d(0:nri_1d)
!!        real(kind = kreal), intent(in) :: source_IC
!!        real(kind = kreal), intent(in) :: const_IC
!!        real(kind = kreal), intent(inout) :: reference(0:nri_1d)
!!      subroutine init_constant_reference(kr_in, kr_out, source,       &
!!     &                                   nri_1d, reference)
!!        integer(kind = kint), intent(in) :: kr_in, kr_out
!!        real(kind = kreal), intent(in) :: source
!!        integer(kind = kint), intent(in) :: nri_1d
!!        real(kind = kreal), intent(inout) :: reference(0:nri_1d)
!!@endverbatim
!
      module init_radial_reference_temp
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_outer_core_ref_temp(kr_in, kr_out, nri_1d, r_1d,  &
     &          source_OC, const_OC, coef_OC, reference)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: nri_1d
      real(kind = kreal), intent(in) :: r_1d(0:nri_1d)
      real(kind = kreal), intent(in) :: source_OC
      real(kind = kreal), intent(in) :: const_OC, coef_OC
!
      real(kind = kreal), intent(inout) :: reference(0:nri_1d)
!
      integer(kind = kint) :: k
!
!   set reference temperature (l = m = 0)
      do k = kr_in, kr_out
        reference(k) = const_OC + coef_OC / r_1d(k)                     &
     &                          - source_OC * r_1d(k)**2 / six
      end do
!
      end subroutine init_outer_core_ref_temp
!
!-----------------------------------------------------------------------
!
      subroutine init_inner_core_ref_temp(kr_in, nri_1d, r_1d,          &
     &          source_IC, const_IC, reference)
!
      integer(kind = kint), intent(in) :: kr_in
      integer(kind = kint), intent(in) :: nri_1d
      real(kind = kreal), intent(in) :: r_1d(0:nri_1d)
!
      real(kind = kreal), intent(in) :: source_IC
      real(kind = kreal), intent(in) :: const_IC
!
      real(kind = kreal), intent(inout) :: reference(0:nri_1d)
!
      integer(kind = kint) :: k
!
!   set reference temperature (l = m = 0)
      do k = 0, kr_in
        reference(k) = const_IC - source_IC * r_1d(k)**2 / six
      end do
!
      end subroutine init_inner_core_ref_temp
!
!-----------------------------------------------------------------------
!
      subroutine init_constant_reference(kr_in, kr_out, source,         &
     &                                   nri_1d, reference)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: source
      integer(kind = kint), intent(in) :: nri_1d
!
      real(kind = kreal), intent(inout) :: reference(0:nri_1d)
!
      reference(kr_in:kr_out) = source
!
      end subroutine init_constant_reference
!
!-----------------------------------------------------------------------
!
      end module init_radial_reference_temp
