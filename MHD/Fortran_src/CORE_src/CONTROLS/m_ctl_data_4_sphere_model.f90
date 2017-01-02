!>@file   m_ctl_data_4_sphere_model.f90
!!@brief  module m_ctl_data_4_sphere_model
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  control data for resolutions of spherical shell
!!
!!@verbatim
!!      subroutine read_ctl_4_shell_define
!!
!! =======================================================
!!    example of control section
!!
!!  begin num_grid_sph
!!! ----------------------------------------------------------------
!!!   sph_coef_type_ctl:  grid type for spherical harmonics data
!!!         no_pole:      Coefficients on spherical shell only
!!!         with_center:  Add center
!!!   sph_grid_type_ctl:  grid type for mesh data
!!!         no_pole:      Gaussian points only
!!!         with_pole:    Add pole grids
!!!         with_center:  Add center
!!! ----------------------------------------------------------------
!!
!!    sph_coef_type_ctl       no_pole
!!    sph_grid_type_ctl       no_pole
!!    truncation_level_ctl     4
!!    longitude_symmetry_ctl   2
!!    ngrid_meridonal_ctl     12
!!    ngrid_zonal_ctl         24
!!
!!   radial_grid_type_ctl:  Definition for radial grid   
!!         explicit:        Set each radial grid explicitly
!!         Chebyshev:       Set Chebyshev collocation points
!!         equi_distance:   Set equi-diatance grid
!!
!!    radial_grid_type_ctl   explicit
!!    array r_layer       4
!!      r_layer    1  0.3584615384615
!!      r_layer    2  0.5384615384615     ICB
!!      r_layer    3  1.038461538462      Mid
!!      r_layer    4  1.538461538462      CMB
!!    end array r_layer
!!
!!    radial_grid_type_ctl   Chebyshev
!!     num_fluid_grid_ctl  5
!!     fluid_core_size_ctl   0.35
!!     ICB_to_CMB_ratio_ctl  1.0
!!     Min_radius_ctl      0.0
!!     ICB_radius_ctl      0.5384615384615
!!     CMB_radius_ctl      1.538461538462
!!     Max_radius_ctl      2.0
!!
!!    array boundaries_ctl   3
!!      boundaries_ctl  to_Center   1
!!      boundaries_ctl  ICB         2
!!      boundaries_ctl  CMB         4
!!    end array  boundaries_ctl
!!
!!
!!    num_radial_layering_ctl        10
!!    num_meridional_layering_ctl    10
!!
!!    array radial_layering_ctl        2
!!      radial_layering_ctl          1   8
!!      radial_layering_ctl          8  15
!!    end array radial_layering_ctl
!!
!!    array meridional_layering_ctl        2
!!      meridional_layering_ctl          1   6
!!      meridional_layering_ctl          6  13
!!    end array meridional_layering_ctl
!!  end num_grid_sph
!!
!! =======================================================
!!@endverbatim
!
      module m_ctl_data_4_sphere_model
!
      use m_precision
      use t_ctl_data_4_sphere_model
!
      implicit  none
!
      type(sphere_data_control), save :: spctl1
!
!   labels of data field
!
      character(len=kchara), parameter                                  &
     &                     :: hd_sph_def = 'shell_define_ctl'
      character(len=kchara), parameter :: hd_shell_def = 'num_grid_sph'
      integer(kind = kint) :: i_shell_def =   0
!
!
      private :: hd_sph_def, hd_shell_def, i_shell_def
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_4_shell_define
!
!
      call read_control_shell_define(hd_shell_def, i_shell_def, spctl1)
      call read_control_shell_define(hd_sph_def, i_shell_def, spctl1)
!
      end subroutine read_ctl_4_shell_define
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_4_sphere_model
