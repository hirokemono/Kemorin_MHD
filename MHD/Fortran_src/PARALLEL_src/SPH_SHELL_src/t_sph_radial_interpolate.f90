!>@file   t_sph_radial_interpolate.f90
!!@brief  module t_sph_radial_interpolate
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief Interpolate spectr data
!!
!!@verbatim
!!      subroutine alloc_original_sph_data(num_org, r_itp)
!!      subroutine dealloc_original_sph_data(r_itp)
!!        integer(kind = kint), intent(in) :: num_org
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
        integer(kind = kint) :: kr_outside
!
!>        Total data number for original data
        integer(kind = kint) :: n_rj_org
!>        Radial data number for original data
        integer(kind = kint) :: nri_org
!
!>        Original radial address for interpolation
        integer(kind = kint), allocatable :: k_inter(:,:)
!>        interpolation coefficients for interpolation
        real(kind = kreal), allocatable :: rcoef_inter(:,:)
!>        Original radius
        real(kind = kreal), allocatable :: r_org(:)
!
!>        Number of component of original data
        integer(kind = kint) :: ntot_phys_rj_itp
!>        Original field data
        real(kind = kreal), allocatable :: d_rj_org(:,:)
      end type sph_radial_interpolate
!
!  -------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_original_sph_data(num_org, r_itp)
!
      integer(kind = kint), intent(in) :: num_org
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
      r_itp%n_rj_org = num_org
      allocate(r_itp%d_rj_org(r_itp%n_rj_org,6))
      if(r_itp%n_rj_org .gt. 0) r_itp%d_rj_org = zero
!
      end subroutine alloc_original_sph_data
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
