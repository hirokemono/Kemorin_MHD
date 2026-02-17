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
!!      subroutine init_sph_ref_temp_outer_core(sph, kr_in, kr_out,     &
!!     &          source_OC, const_OC, coef_OC, n_point, temp_rj)
!!      subroutine init_sph_ref_temp_full_sphere(sph, kr_out,           &
!!     &          source_OC, const_OC, n_point, temp_rj)
!!      subroutine init_sph_ref_temp_whole_core(sph, kr_in, kr_out,     &
!!     &          source_IC, source_OC, const_OC, coef_OC, const_IC,    &
!!     &          n_point, temp_rj)
!!        type(sph_grids), intent(in) :: sph
!!        integer(kind = kint), intent(in) :: kr_in, kr_out
!!        real(kind = kreal), intent(in) :: source_IC, source_OC
!!        real(kind = kreal), intent(in) :: const_OC, coef_OC
!!        real(kind = kreal), intent(in) :: const_IC
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(inout) :: temp_rj(n_point)
!!
!!      subroutine init_outer_core_ref_temp(sph, kr_in, kr_out,         &
!!     &          source_OC, const_OC, coef_OC, n_point, temp_rj)
!!      subroutine init_inner_core_ref_temp(sph, kr_in,                 &
!!     &          source_IC, const_IC, n_point, temp_rj)
!!      subroutine init_external_ref_temp(sph, kr_out, n_point, temp_rj)
!!        type(sph_grids), intent(in) :: sph
!!        integer(kind = kint), intent(in) :: kr_in
!!        integer(kind = kint), intent(in) :: kr_out
!!        real(kind = kreal), intent(in) :: source_OC
!!        real(kind = kreal), intent(in) :: const_OC, coef_OC
!!        real(kind = kreal), intent(in) :: source_IC
!!        real(kind = kreal), intent(in) :: const_IC
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(inout) :: temp_rj(n_point)
!!
!!      subroutine init_constant_source(sph, kr_in, kr_out, source,     &
!!     &                                n_point, source_rj)
!!        type(sph_grids), intent(in) :: sph
!!        integer(kind = kint), intent(in) :: kr_in, kr_out
!!        real(kind = kreal), intent(in) :: source
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(inout) :: source_rj(n_point)
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
      private :: init_outer_core_ref_temp, init_inner_core_ref_temp
      private :: init_external_ref_temp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_ref_temp_outer_core(sph, kr_in, kr_out,       &
     &          source_OC, const_OC, coef_OC, n_point, temp_rj)
!
      type(sph_grids), intent(in) :: sph
      integer(kind = kint), intent(in) :: kr_in, kr_out
!
      real(kind = kreal), intent(in) :: source_OC
      real(kind = kreal), intent(in) :: const_OC, coef_OC
!
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: temp_rj(n_point)
!
!
      call init_outer_core_ref_temp(sph, kr_in, kr_out,                 &
     &    source_OC, const_OC, coef_OC, n_point, temp_rj)
      call init_external_ref_temp(sph, kr_out, n_point, temp_rj)
!
      end subroutine init_sph_ref_temp_outer_core
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_ref_temp_full_sphere(sph, kr_out,             &
     &          source_OC, const_OC, n_point, temp_rj)
!
      type(sph_grids), intent(in) :: sph
      integer(kind = kint), intent(in) :: kr_out
!
      real(kind = kreal), intent(in) :: source_OC, const_OC
!
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: temp_rj(n_point)
!
!
      call init_inner_core_ref_temp(sph, kr_out, source_OC, const_OC,   &
     &                              n_point, temp_rj)
      call init_external_ref_temp(sph, kr_out, n_point, temp_rj)
!
      end subroutine init_sph_ref_temp_full_sphere
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_ref_temp_whole_core(sph, kr_in, kr_out,       &
     &          source_IC, source_OC, const_OC, coef_OC, const_IC,      &
     &          n_point, temp_rj)
!
      type(sph_grids), intent(in) :: sph
      integer(kind = kint), intent(in) :: kr_in, kr_out
!
      real(kind = kreal), intent(in) :: source_IC, source_OC
      real(kind = kreal), intent(in) :: const_OC, coef_OC
      real(kind = kreal), intent(in) :: const_IC
!
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: temp_rj(n_point)
!
!
      call init_inner_core_ref_temp(sph, kr_in, source_IC, const_IC,    &
     &                              n_point, temp_rj)
      call init_outer_core_ref_temp(sph, kr_in, kr_out,                 &
     &    source_OC, const_OC, coef_OC, n_point, temp_rj)
      call init_external_ref_temp(sph, kr_out, n_point, temp_rj)
!
      end subroutine init_sph_ref_temp_whole_core
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine init_outer_core_ref_temp(sph, kr_in, kr_out,           &
     &          source_OC, const_OC, coef_OC, n_point, temp_rj)
!
      use spherical_indices_picker
!
      type(sph_grids), intent(in) :: sph
      integer(kind = kint), intent(in) :: kr_in, kr_out
!
      real(kind = kreal), intent(in) :: source_OC
      real(kind = kreal), intent(in) :: const_OC, coef_OC
!
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: temp_rj(n_point)
!
      integer(kind = kint) :: k, jj, inod
      real(kind = kreal) :: r
!
!   set reference temperature (l = m = 0)
      jj = idx_rj_degree_zero(sph)
      if(jj .le. 0) return
      do k = kr_in, kr_out
        inod = local_sph_data_address(sph, k, jj)
        r = radius_1d_rj_r(sph,k)
        temp_rj(inod) = const_OC + coef_OC / r - source_OC * r**2 / six
      end do
!
      end subroutine init_outer_core_ref_temp
!
!-----------------------------------------------------------------------
!
      subroutine init_inner_core_ref_temp(sph, kr_in,                   &
     &          source_IC, const_IC, n_point, temp_rj)
!
      use spherical_indices_picker
!
      type(sph_grids), intent(in) :: sph
      integer(kind = kint), intent(in) :: kr_in
!
      real(kind = kreal), intent(in) :: source_IC
      real(kind = kreal), intent(in) :: const_IC
!
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: temp_rj(n_point)
!
      integer(kind = kint) :: k, jj, inod
      real(kind = kreal) :: r
!
!   set reference temperature (l = m = 0)
      jj = idx_rj_degree_zero(sph)
      if(jj .le. 0) return
      if(kr_in .le. 0) return
      do k = 1, kr_in
        inod = local_sph_data_address(sph, k, jj)
        r = radius_1d_rj_r(sph,k)
        temp_rj(inod) = const_IC - source_IC * r*r / six
      end do
!
!    Center
      inod = inod_rj_center(sph)
      if(inod .gt. 0) temp_rj(inod) = const_IC
!
      end subroutine init_inner_core_ref_temp
!
!-----------------------------------------------------------------------
!
      subroutine init_external_ref_temp(sph, kr_out, n_point, temp_rj)
!
      use spherical_indices_picker
!
      type(sph_grids), intent(in) :: sph
      integer(kind = kint), intent(in) :: kr_out
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: temp_rj(n_point)
!
      integer(kind = kint) :: k, jj, inod, i_cmb
!
!   set reference temperature (l = m = 0)
      jj = idx_rj_degree_zero(sph)
      if(jj .le. 0) return
      if(kr_out .ge. num_rj_radial_point(sph)) return
      do k = kr_out+1, num_rj_radial_point(sph)
        inod =  local_sph_data_address(sph, k,      jj)
        i_cmb = local_sph_data_address(sph, kr_out, jj)
        temp_rj(inod) = temp_rj(i_cmb)
      end do
!
      end subroutine init_external_ref_temp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine init_constant_source(sph, kr_in, kr_out, source,       &
     &                                n_point, source_rj)
!
      use spherical_indices_picker
!
      type(sph_grids), intent(in) :: sph
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: source
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: source_rj(n_point)
!
      integer(kind = kint) :: k, jj, inod
!
!   set reference temperature (l = m = 0)
      jj = idx_rj_degree_zero(sph)
      if(jj .le. 0) return
      do k = kr_in, kr_out
        inod =  local_sph_data_address(sph, k, jj)
        source_rj(inod) = source
      end do
!
!    Center
      if(kr_in .gt. 1) return
      inod = inod_rj_center(sph)
      if(inod .gt. 0) source_rj(inod) = source
!
      end subroutine init_constant_source
!
!-----------------------------------------------------------------------
!
      end module set_sph_diffusive_scalar
