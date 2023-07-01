!>@file   c_link_ctl_data_sph_shell.f90
!!@brief  module c_link_ctl_data_sph_shell
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for sphere_data_control structure
!!@verbatim
!!      type(c_ptr) function c_sphere_data_ctl_block_name(c_ctl)        &
!!     &          bind(C, NAME = 'c_sphere_data_ctl_block_name')
!!      type(c_ptr) function c_sphere_data_ctl_iflag(c_ctl)             &
!!     &          bind(C, NAME = 'c_sphere_data_ctl_iflag')
!!
!!      type(c_ptr) function c_sphere_data_ltr_ctl(c_ctl)               &
!!     &          bind(C, NAME = 'c_sphere_data_ltr_ctl')
!!      type(c_ptr) function c_sphere_data_phi_symmetry_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_sphere_data_phi_symmetry_ctl')
!!      type(c_ptr) function c_sphere_data_sph_grd_tpe_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_sphere_data_sph_grd_tpe_ctl')
!!      type(c_ptr) function c_sphere_data_coef_type_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_sphere_data_coef_type_ctl')
!!      type(c_ptr) function c_sphere_data_n_elevation_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_sphere_data_n_elevation_ctl')
!!      type(c_ptr) function c_sphere_data_ngrid_azmth_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_sphere_data_ngrid_azmth_ctl')
!!      type(c_ptr) function c_sphere_data_radius_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_sphere_data_radius_ctl')
!!      type(c_ptr) function c_sphere_data_radial_grp_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_sphere_data_radial_grp_ctl')
!!      type(c_ptr) function c_sphere_data_add_ext_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_sphere_data_add_ext_ctl')
!!      type(c_ptr) function c_sphere_data_r_grid_type_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_sphere_data_r_grid_type_ctl')
!!      type(c_ptr) function c_sphere_data_num_fld_grid_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_sphere_data_num_fld_grid_ctl')
!!      type(c_ptr) function c_sphere_data_icrmnt_cheby_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_sphere_data_icrmnt_cheby_ctl')
!!      type(c_ptr) function c_sphere_data_Min_radius_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_sphere_data_Min_radius_ctl')
!!      type(c_ptr) function c_sphere_data_ICB_radius_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_sphere_data_ICB_radius_ctl')
!!      type(c_ptr) function c_sphere_data_CMB_radius_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_sphere_data_CMB_radius_ctl')
!!      type(c_ptr) function c_sphere_data_Max_radius_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_sphere_data_Max_radius_ctl')
!!      type(c_ptr) function c_sphere_data_fld_core_sze_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_sphere_data_fld_core_sze_ctl')
!!      type(c_ptr) function c_sphere_data_ICB_CMB_ratio_ctl(c_ctl)     &
!!     &          bind(C, NAME = 'c_sphere_data_ICB_CMB_ratio_ctl')
!!      type(c_ptr) function c_sphere_data_num_r_layer_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_sphere_data_num_r_layer_ctl')
!!      type(c_ptr) function c_sphere_data_n_med_layer_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_sphere_data_n_med_layer_ctl')
!!      type(c_ptr) function c_sphere_data_r_layer_list_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_sphere_data_r_layer_list_ctl')
!!      type(c_ptr) function c_sphere_data_med_list_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_sphere_data_med_list_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_ctl_data_sph_shell
!
      use iso_c_binding
      use t_ctl_data_4_sphere_model
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_ctl_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_sphere_data_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_sphere_data_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_ctl_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_sphere_data_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_ctl_iflag = C_loc(f_ctl%i_shell_def)
      end function c_sphere_data_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_ltr_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_sphere_data_ltr_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_ltr_ctl = C_loc(f_ctl%ltr_ctl)
      end function c_sphere_data_ltr_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_phi_symmetry_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_sphere_data_phi_symmetry_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_phi_symmetry_ctl = C_loc(f_ctl%phi_symmetry_ctl)
      end function c_sphere_data_phi_symmetry_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_sph_grd_tpe_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_sphere_data_sph_grd_tpe_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_sph_grd_tpe_ctl = C_loc(f_ctl%sph_grid_type_ctl)
      end function c_sphere_data_sph_grd_tpe_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_coef_type_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_sphere_data_coef_type_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_coef_type_ctl = C_loc(f_ctl%sph_coef_type_ctl)
      end function c_sphere_data_coef_type_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_n_elevation_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_sphere_data_n_elevation_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_n_elevation_ctl                                     &
     &             = C_loc(f_ctl%ngrid_elevation_ctl)
      end function c_sphere_data_n_elevation_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_ngrid_azmth_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_sphere_data_ngrid_azmth_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_ngrid_azmth_ctl = C_loc(f_ctl%ngrid_azimuth_ctl)
      end function c_sphere_data_ngrid_azmth_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_radius_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_sphere_data_radius_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_radius_ctl = C_loc(f_ctl%radius_ctl)
      end function c_sphere_data_radius_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_radial_grp_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_sphere_data_radial_grp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_radial_grp_ctl = C_loc(f_ctl%radial_grp_ctl)
      end function c_sphere_data_radial_grp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_add_ext_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_sphere_data_add_ext_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_add_ext_ctl = C_loc(f_ctl%add_ext_layer_ctl)
      end function c_sphere_data_add_ext_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_r_grid_type_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_sphere_data_r_grid_type_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_r_grid_type_ctl                                     &
     &             = C_loc(f_ctl%radial_grid_type_ctl)
      end function c_sphere_data_r_grid_type_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_num_fld_grid_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_sphere_data_num_fld_grid_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_num_fld_grid_ctl                                    &
     &             = C_loc(f_ctl%num_fluid_grid_ctl)
      end function c_sphere_data_num_fld_grid_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_icrmnt_cheby_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_sphere_data_icrmnt_cheby_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_icrmnt_cheby_ctl                                    &
     &             = C_loc(f_ctl%increment_cheby_ctl)
      end function c_sphere_data_icrmnt_cheby_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_Min_radius_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_sphere_data_Min_radius_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_Min_radius_ctl = C_loc(f_ctl%Min_radius_ctl)
      end function c_sphere_data_Min_radius_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_ICB_radius_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_sphere_data_ICB_radius_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_ICB_radius_ctl = C_loc(f_ctl%ICB_radius_ctl)
      end function c_sphere_data_ICB_radius_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_CMB_radius_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_sphere_data_CMB_radius_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_CMB_radius_ctl = C_loc(f_ctl%CMB_radius_ctl)
      end function c_sphere_data_CMB_radius_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_Max_radius_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_sphere_data_Max_radius_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_Max_radius_ctl = C_loc(f_ctl%Max_radius_ctl)
      end function c_sphere_data_Max_radius_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_fld_core_sze_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_sphere_data_fld_core_sze_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_fld_core_sze_ctl = C_loc(f_ctl%fluid_core_size_ctl)
      end function c_sphere_data_fld_core_sze_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_ICB_CMB_ratio_ctl(c_ctl)       &
     &          bind(C, NAME = 'c_sphere_data_ICB_CMB_ratio_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_ICB_CMB_ratio_ctl                                   &
     &             = C_loc(f_ctl%ICB_to_CMB_ratio_ctl)
      end function c_sphere_data_ICB_CMB_ratio_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_num_r_layer_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_sphere_data_num_r_layer_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_num_r_layer_ctl                                     &
     &             = C_loc(f_ctl%num_radial_layer_ctl)
      end function c_sphere_data_num_r_layer_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_n_med_layer_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_sphere_data_n_med_layer_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_n_med_layer_ctl = C_loc(f_ctl%num_med_layer_ctl)
      end function c_sphere_data_n_med_layer_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_r_layer_list_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_sphere_data_r_layer_list_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_r_layer_list_ctl                                    &
     &             = C_loc(f_ctl%radial_layer_list_ctl)
      end function c_sphere_data_r_layer_list_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sphere_data_med_list_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_sphere_data_med_list_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sphere_data_med_list_ctl= C_loc(f_ctl%med_layer_list_ctl)
      end function c_sphere_data_med_list_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_ctl_data_sph_shell
