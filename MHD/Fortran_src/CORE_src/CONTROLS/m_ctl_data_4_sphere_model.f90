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
!!      subroutine read_ctl_ndomain_4_shell
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
!!  ---------------------------------------------------------------------
!!    example of control data
!!
!!  begin num_domain_ctl
!!    num_radial_domain_ctl         2
!!    num_horizontal_domain_ctl     2
!!
!!    array  num_domain_sph_grid   2
!!      num_domain_sph_grid    radial       2   end
!!      num_domain_sph_grid   meridional    3   end
!!    end array num_domain_sph_grid
!!
!!    array num_domain_legendre   2
!!      num_domain_legendre   radial        2   end
!!      num_domain_legendre   zonal         3   end
!!    end array num_domain_legendre
!!
!!    array num_domain_spectr     1
!!      num_domain_spectr     modes         6   end
!!    end array num_domain_spectr
!!  end num_domain_ctl
!!
!!  ---------------------------------------------------------------------
!!@endverbatim
!
      module m_ctl_data_4_sphere_model
!
      use m_precision
      use t_ctl_data_4_sphere_model
      use t_ctl_data_4_divide_sphere
!
      implicit  none
!
      type(sphere_data_control), save :: spctl1
      type(sphere_domain_control), save :: sdctl1
!
!   labels of data field
!
      character(len=kchara), parameter                                  &
     &                     :: hd_sph_def = 'shell_define_ctl'
      character(len=kchara), parameter :: hd_shell_def = 'num_grid_sph'
      integer(kind = kint) :: i_shell_def =   0
!
      character(len=kchara), parameter                                  &
     &                     :: hd_domains_sph = 'num_domain_ctl'
      integer(kind = kint) :: i_domains_sph = 0
!
!
      private :: hd_sph_def, hd_shell_def, i_shell_def
      private :: hd_domains_sph, i_domains_sph
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
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_ndomain_4_shell
!
!
      call read_control_shell_domain                                    &
     &   (hd_domains_sph, i_domains_sph, sdctl1)
!
      end subroutine read_ctl_ndomain_4_shell
!
!  ---------------------------------------------------------------------
!
      end module m_ctl_data_4_sphere_model
