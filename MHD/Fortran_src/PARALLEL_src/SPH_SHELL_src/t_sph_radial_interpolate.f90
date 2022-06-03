!>@file   t_sph_radial_interpolate.f90
!!@brief  module t_sph_radial_interpolate
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief Interpolate spectr data
!!
!!@verbatim
!!      subroutine copy_cmb_icb_radial_point(nlayer_ICB, nlayer_CMB)
!!      subroutine set_cmb_icb_radial_point                             &
!!     &         (cmb_r_grp, icb_r_grp, radial_rj_grp)
!!        type(group_data), intent(in) :: radial_rj_grp
!!      subroutine set_sph_magne_address(rj_fld, ipol)
!!        type(phys_data), intent(in) :: rj_fld
!!        type(phys_address), intent(inout) :: ipol
!!      subroutine input_old_rj_sph_trans                               &
!!     &         (rj_file_param, l_truncation, sph_rj)
!!
!!      subroutine r_interpolate_sph_rst_from_IO                        &
!!     &         (fld_IO, sph_rj, ipol, rj_fld)
!!      subroutine r_interpolate_sph_fld_from_IO                        &
!!     &         (fld_IO, sph_rj, ipol, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module t_sph_radial_interpolate
!
      use m_precision
!
!      use calypso_mpi
!      use m_constants
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
      end module t_sph_radial_interpolate
