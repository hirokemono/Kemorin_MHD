!>@file   c_link_MHD_force_model_ctl.f90
!!@brief  module c_link_MHD_force_model_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for forces control structure
!!@verbatim
!!      type(c_ptr) function c_MHD_gravity_ctl_block_name(c_ctl)        &
!!     &          bind(C, NAME = 'c_MHD_gravity_ctl_block_name')
!!      type(c_ptr) function c_MHD_gravity_ctl_iflag(c_ctl)             &
!!     &          bind(C, NAME = 'c_MHD_gravity_ctl_iflag')
!!      type(c_ptr) function c_MHD_FEM_gravity_model(c_ctl)             &
!!     &          bind(C, NAME = 'c_MHD_FEM_gravity_model')
!!      type(c_ptr) function c_MHD_gravity_ctl_gravity(c_ctl)           &
!!     &          bind(C, NAME = 'c_MHD_gravity_ctl_gravity')
!!      type(c_ptr) function c_MHD_gravity_ctl_vector(c_ctl)            &
!!     &          bind(C, NAME = 'c_MHD_gravity_ctl_vector')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!!!
!!      type(c_ptr) function c_MHD_coriolis_ctl_block_name(c_ctl)       &
!!     &          bind(C, NAME = 'c_MHD_coriolis_ctl_block_name')
!!      type(c_ptr) function c_MHD_coriolis_ctl_iflag(c_ctl)            &
!!     &          bind(C, NAME = 'c_MHD_coriolis_ctl_iflag')
!!      type(c_ptr) function c_MHD_FEM_coriolis_model(c_ctl)            &
!!     &          bind(C, NAME = 'c_MHD_FEM_coriolis_model')
!!      type(c_ptr) function c_MHD_FEM_coriolis_implicit(c_ctl)         &
!!     &          bind(C, NAME = 'c_MHD_FEM_coriolis_implicit')
!!      type(c_ptr) function c_MHD_system_rotation(c_ctl)               &
!!     &          bind(C, NAME = 'c_MHD_system_rotation')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!!!
!!      type(c_ptr) function c_MHD_mag_cv_ctl_block_name(c_ctl)         &
!!     &          bind(C, NAME = 'c_MHD_mag_cv_ctl_block_name')
!!      type(c_ptr) function c_MHD_mag_cv_ctl_iflag(c_ctl)              &
!!     &          bind(C, NAME = 'c_MHD_mag_cv_ctl_iflag')
!!      type(c_ptr) function c_MHD_mag_cv_filterd_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_MHD_mag_cv_filterd_ctl')
!!      type(c_ptr) function c_MHD_mag_cv_ctl_magneto_cv(c_ctl)         &
!!     &          bind(C, NAME = 'c_MHD_mag_cv_ctl_magneto_cv')
!!      type(c_ptr) function c_MHD_mag_cv_ctl_ext_magne(c_ctl)          &
!!     &          bind(C, NAME = 'c_MHD_mag_cv_ctl_ext_magne')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!!!
!!      type(c_ptr) function c_MHD_B_scale_ctl_block_name(c_ctl)        &
!!     &          bind(C, NAME = 'c_MHD_B_scale_ctl_block_name')
!!      type(c_ptr) function c_MHD_B_scale_ctl_iflag(c_ctl)             &
!!     &          bind(C, NAME = 'c_MHD_B_scale_ctl_iflag')
!!      type(c_ptr) function c_MHD_B_scale_mag_to_kin_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_MHD_B_scale_mag_to_kin_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_MHD_force_model_ctl
!
      use iso_c_binding
      use t_ctl_data_gravity
      use t_ctl_data_coriolis_force
      use t_ctl_data_mhd_magne
      use t_ctl_data_magnetic_scale
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_gravity_ctl_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_gravity_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(gravity_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_gravity_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_gravity_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_gravity_ctl_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_MHD_gravity_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(gravity_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_gravity_ctl_iflag = C_loc(f_ctl%i_gravity_ctl)
      end function c_MHD_gravity_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_FEM_gravity_model(c_ctl)               &
     &          bind(C, NAME = 'c_MHD_FEM_gravity_model')
      type(c_ptr), value, intent(in) :: c_ctl
      type(gravity_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_FEM_gravity_model = C_loc(f_ctl%FEM_gravity_model)
      end function c_MHD_FEM_gravity_model
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_gravity_ctl_gravity(c_ctl)             &
     &          bind(C, NAME = 'c_MHD_gravity_ctl_gravity')
      type(c_ptr), value, intent(in) :: c_ctl
      type(gravity_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_gravity_ctl_gravity = C_loc(f_ctl%gravity)
      end function c_MHD_gravity_ctl_gravity
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_gravity_ctl_vector(c_ctl)              &
     &          bind(C, NAME = 'c_MHD_gravity_ctl_vector')
      type(c_ptr), value, intent(in) :: c_ctl
      type(gravity_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_gravity_ctl_vector = C_loc(f_ctl%gravity_vector)
      end function c_MHD_gravity_ctl_vector
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_coriolis_ctl_block_name(c_ctl)         &
     &          bind(C, NAME = 'c_MHD_coriolis_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(coriolis_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_coriolis_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_coriolis_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_coriolis_ctl_iflag(c_ctl)              &
     &          bind(C, NAME = 'c_MHD_coriolis_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(coriolis_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_coriolis_ctl_iflag = C_loc(f_ctl%i_coriolis_ctl)
      end function c_MHD_coriolis_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_FEM_coriolis_model(c_ctl)              &
     &          bind(C, NAME = 'c_MHD_FEM_coriolis_model')
      type(c_ptr), value, intent(in) :: c_ctl
      type(coriolis_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_FEM_coriolis_model = C_loc(f_ctl%FEM_coriolis_model)
      end function c_MHD_FEM_coriolis_model
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_FEM_coriolis_implicit(c_ctl)           &
     &          bind(C, NAME = 'c_MHD_FEM_coriolis_implicit')
      type(c_ptr), value, intent(in) :: c_ctl
      type(coriolis_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_FEM_coriolis_implicit = C_loc(f_ctl%FEM_coriolis_implicit)
      end function c_MHD_FEM_coriolis_implicit
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_system_rotation(c_ctl)                 &
     &          bind(C, NAME = 'c_MHD_system_rotation')
      type(c_ptr), value, intent(in) :: c_ctl
      type(coriolis_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_system_rotation = C_loc(f_ctl%system_rotation)
      end function c_MHD_system_rotation
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mag_cv_ctl_block_name(c_ctl)           &
     &          bind(C, NAME = 'c_MHD_mag_cv_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(magneto_convection_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mag_cv_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_mag_cv_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mag_cv_ctl_iflag(c_ctl)                &
     &          bind(C, NAME = 'c_MHD_mag_cv_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(magneto_convection_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mag_cv_ctl_iflag = C_loc(f_ctl%i_magneto_ctl)
      end function c_MHD_mag_cv_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mag_cv_filterd_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_MHD_mag_cv_filterd_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(magneto_convection_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mag_cv_filterd_ctl = C_loc(f_ctl%filterd_induction_ctl)
      end function c_MHD_mag_cv_filterd_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mag_cv_ctl_magneto_cv(c_ctl)           &
     &          bind(C, NAME = 'c_MHD_mag_cv_ctl_magneto_cv')
      type(c_ptr), value, intent(in) :: c_ctl
      type(magneto_convection_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mag_cv_ctl_magneto_cv = C_loc(f_ctl%magneto_cv)
      end function c_MHD_mag_cv_ctl_magneto_cv
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mag_cv_ctl_ext_magne(c_ctl)            &
     &          bind(C, NAME = 'c_MHD_mag_cv_ctl_ext_magne')
      type(c_ptr), value, intent(in) :: c_ctl
      type(magneto_convection_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mag_cv_ctl_ext_magne = C_loc(f_ctl%ext_magne)
      end function c_MHD_mag_cv_ctl_ext_magne
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_B_scale_ctl_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_B_scale_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(magnetic_field_scale_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_B_scale_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_B_scale_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_B_scale_ctl_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_MHD_B_scale_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(magnetic_field_scale_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_B_scale_ctl_iflag = C_loc(f_ctl%i_bscale_ctl)
      end function c_MHD_B_scale_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_B_scale_mag_to_kin_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_B_scale_mag_to_kin_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(magnetic_field_scale_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_B_scale_mag_to_kin_ctl = C_loc(f_ctl%mag_to_kin_energy_ctl)
      end function c_MHD_B_scale_mag_to_kin_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_MHD_force_model_ctl
