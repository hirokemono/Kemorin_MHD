!>@file   t_sph_radial_interpolate.f90
!!@brief  module t_sph_radial_interpolate
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief Interpolate spectr data
!!
!!@verbatim
!!      subroutine alloc_radial_interpolate(nri_source, r_itp)
!!      subroutine alloc_original_sph_data(n_rj_org, r_itp)
!!      subroutine dealloc_radial_interpolate(r_itp)
!!      subroutine dealloc_original_sph_data(r_itp)
!!        integer(kind = kint), intent(in) :: nri_source
!!        integer(kind = kint), intent(in) :: n_rj_org
!!        type(sph_radial_interpolate), intent(inout) :: r_itp
!!@endverbatim
!
      module t_sph_radial_interpolate
!
      use m_precision
      use m_constants
!
!      use calypso_mpi
!      use m_machine_parameter
!
!      use t_spheric_rj_data
!      use t_phys_address
!      use t_phys_data
!
      implicit  none
!
!
!>      Structure for radial interpolation
      type sph_radial_interpolate
!>        Inner boundary address
        integer(kind = kint) :: kr_inside
!>        Outer boundary address
        integer(kind = kint) :: kr_source_outside
!
!>        Radial data number for original data
        integer(kind = kint) :: nri_source
!>        Original radial address for interpolation
        integer(kind = kint), allocatable :: k_inter(:,:)
!>        interpolation coefficients for interpolation
        real(kind = kreal), allocatable :: rcoef_inter(:,:)
!>        Original radius
        real(kind = kreal), allocatable :: source_radius(:)
!
!>        Total data number for original data
        integer(kind = kint) :: n_rj_org
!>        Original work area for fields
        real(kind = kreal), allocatable :: d_rj_org(:,:)
      end type sph_radial_interpolate
!
!  -------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_radial_interpolate(nri_source, r_itp)
!
      integer(kind = kint), intent(in) :: nri_source
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
!
      r_itp%nri_source = nri_source
      allocate(r_itp%source_radius(r_itp%nri_source))
      allocate(r_itp%k_inter(r_itp%nri_source,2))
      allocate(r_itp%rcoef_inter(r_itp%nri_source,2))
!
      if(r_itp%nri_source .gt. 0) then
        r_itp%k_inter = izero
        r_itp%source_radius = zero
        r_itp%rcoef_inter = zero
      end if
!
      end subroutine alloc_radial_interpolate
!
!  -------------------------------------------------------------------
!
      subroutine alloc_original_sph_data(n_rj_org, r_itp)
!
      integer(kind = kint), intent(in) :: n_rj_org
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
      r_itp%n_rj_org = n_rj_org
      allocate(r_itp%d_rj_org(r_itp%n_rj_org,6))
      if(r_itp%n_rj_org .gt. 0) r_itp%d_rj_org = zero
!
      end subroutine alloc_original_sph_data
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine dealloc_radial_interpolate(r_itp)
!
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
      deallocate(r_itp%source_radius)
      deallocate(r_itp%k_inter, r_itp%rcoef_inter)
!
      end subroutine dealloc_radial_interpolate
!
!  -------------------------------------------------------------------
!
      subroutine dealloc_original_sph_data(r_itp)
!
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
      deallocate(r_itp%d_rj_org)
!
      end subroutine dealloc_original_sph_data
!
!  -------------------------------------------------------------------
!
      end module t_sph_radial_interpolate
