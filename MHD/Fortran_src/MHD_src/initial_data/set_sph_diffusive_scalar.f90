!>@file   set_sph_diffusive_scalar.f90
!!@brief  module set_sph_diffusive_scalar
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
!!      subroutine init_sph_ref_temp_outer_core(kr_in, kr_out, nri_1d,  &
!!     &          r_1d, source_OC, const_OC, coef_OC, reftemp)
!!      subroutine init_sph_ref_temp_full_sphere(kr_out, nri_1d, r_1d,  &
!!     &          source_OC, const_OC, reftemp)
!!      subroutine init_sph_ref_temp_whole_core                         &
!!     &         (kr_in, kr_out, nri_1d, r_1d, source_IC, source_OC,    &
!!     &          const_OC, coef_OC, const_IC, reftemp)
!!        integer(kind = kint), intent(in) :: kr_in, kr_out
!!        integer(kind = kint), intent(in) :: nri_1d
!!        real(kind = kreal), intent(in) :: r_1d(0:nri_1d)
!!        real(kind = kreal), intent(in) :: source_IC, source_OC
!!        real(kind = kreal), intent(in) :: const_OC, coef_OC
!!        real(kind = kreal), intent(in) :: const_IC
!!        real(kind = kreal), intent(inout) :: reftemp(0:nri_1d)
!!
!!      subroutine init_sph_ref_source_whole_core(kr_in, kr_out, nri_1d,&
!!     &          source_IC, source_OC, ref_src)
!!        integer(kind = kint), intent(in) :: kr_in, kr_out
!!        integer(kind = kint), intent(in) :: nri_1d
!!        real(kind = kreal), intent(in) :: source_IC, source_OC
!!        real(kind = kreal), intent(inout) :: ref_src(0:nri_1d)
!!      subroutine init_sph_ref_source_full_sphere(kr_out, nri_1d,      &
!!     &                                           source_OC, ref_src)
!!        integer(kind = kint), intent(in) :: kr_out
!!        integer(kind = kint), intent(in) :: nri_1d
!!        real(kind = kreal), intent(in) :: source_OC
!!        real(kind = kreal), intent(inout) :: ref_src(0:nri_1d)
!!@endverbatim
!
      module set_sph_diffusive_scalar
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_spheric_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_ref_temp_outer_core(kr_in, kr_out, nri_1d,    &
     &          r_1d, source_OC, const_OC, coef_OC, reftemp)
!
      use init_radial_reference_temp
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: nri_1d
!
      real(kind = kreal), intent(in) :: r_1d(0:nri_1d)
      real(kind = kreal), intent(in) :: source_OC
      real(kind = kreal), intent(in) :: const_OC, coef_OC
!
      real(kind = kreal), intent(inout) :: reftemp(0:nri_1d)
!
!
      call init_outer_core_ref_temp(kr_in, kr_out, nri_1d, r_1d,        &
     &    source_OC, const_OC, coef_OC, reftemp)
      call init_constant_reference(izero, (kr_in-1), reftemp(kr_in),    &
     &                             nri_1d, reftemp)
      call init_constant_reference((kr_out+1), nri_1d, reftemp(kr_out), &
     &                             nri_1d, reftemp)
!
      end subroutine init_sph_ref_temp_outer_core
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_ref_temp_full_sphere(kr_out, nri_1d, r_1d,    &
     &          source_OC, const_OC, reftemp)
!
      use init_radial_reference_temp
!
      integer(kind = kint), intent(in) :: kr_out
      integer(kind = kint), intent(in) :: nri_1d
!
      real(kind = kreal), intent(in) :: r_1d(0:nri_1d)
      real(kind = kreal), intent(in) :: source_OC, const_OC
!
      real(kind = kreal), intent(inout) :: reftemp(0:nri_1d)
!
!
      call init_inner_core_ref_temp(kr_out, nri_1d, r_1d,               &
     &                              source_OC, const_OC, reftemp)
      call init_constant_reference((kr_out+1), nri_1d, reftemp(kr_out), &
     &                           nri_1d, reftemp)
!
      end subroutine init_sph_ref_temp_full_sphere
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_ref_temp_whole_core                           &
     &         (kr_in, kr_out, nri_1d, r_1d, source_IC, source_OC,      &
     &          const_OC, coef_OC, const_IC, reftemp)
!
      use init_radial_reference_temp
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: nri_1d
!
      real(kind = kreal), intent(in) :: r_1d(0:nri_1d)
      real(kind = kreal), intent(in) :: source_IC, source_OC
      real(kind = kreal), intent(in) :: const_OC, coef_OC
      real(kind = kreal), intent(in) :: const_IC
!
      real(kind = kreal), intent(inout) :: reftemp(0:nri_1d)
!
!
      call init_inner_core_ref_temp(kr_in, nri_1d, r_1d,                &
     &                              source_IC, const_IC, reftemp)
      call init_outer_core_ref_temp(kr_in, kr_out, nri_1d, r_1d,        &
     &    source_OC, const_OC, coef_OC, reftemp)
      call init_constant_reference((kr_out+1), nri_1d, reftemp(kr_out), &
     &                             nri_1d, reftemp)
!
      end subroutine init_sph_ref_temp_whole_core
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine init_sph_ref_source_whole_core(kr_in, kr_out, nri_1d,  &
     &          source_IC, source_OC, ref_src)
!
      use init_radial_reference_temp
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: nri_1d
!
      real(kind = kreal), intent(in) :: source_IC, source_OC
!
      real(kind = kreal), intent(inout) :: ref_src(0:nri_1d)
!
!
      call init_constant_reference(izero, (kr_in-1), source_IC,         &
     &                             nri_1d, ref_src)
      call init_constant_reference(kr_in, kr_out, source_OC,            &
     &                             nri_1d, ref_src)
      call init_constant_reference((kr_out+1), nri_1d, zero,            &
     &                             nri_1d, ref_src)
!
      end subroutine init_sph_ref_source_whole_core
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_ref_source_full_sphere(kr_out, nri_1d,        &
     &                                           source_OC, ref_src)
!
      use init_radial_reference_temp
!
      integer(kind = kint), intent(in) :: kr_out
      integer(kind = kint), intent(in) :: nri_1d
      real(kind = kreal), intent(in) :: source_OC
!
      real(kind = kreal), intent(inout) :: ref_src(0:nri_1d)
!
!
      call init_constant_reference(izero, kr_out, source_OC,            &
     &                             nri_1d, ref_src)
      call init_constant_reference((kr_out+1), nri_1d, zero,            &
     &                             nri_1d, ref_src)
!
      end subroutine init_sph_ref_source_full_sphere
!
!-----------------------------------------------------------------------
!
      end module set_sph_diffusive_scalar
