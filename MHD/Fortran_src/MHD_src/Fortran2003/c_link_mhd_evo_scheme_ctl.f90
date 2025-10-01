!>@file   c_link_mhd_evo_scheme_ctl.f90
!!@brief  module c_link_mhd_evo_scheme_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!!
!>@brief C binding routines for sphere_data_control structure
!!@verbatim
!!      type(c_ptr) function c_mhd_evo_scheme_ctl_block_name(c_ctl)     &
!!     &          bind(C, NAME = 'c_mhd_evo_scheme_ctl_block_name')
!!      type(c_ptr) function c_mhd_evo_scheme_ctl_iflag(c_ctl)          &
!!     &          bind(C, NAME = 'c_mhd_evo_scheme_ctl_iflag')
!!
!!      type(c_ptr) function c_evo_scheme_coef_implicit_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_evo_scheme_coef_implicit_ctl')
!!      type(c_ptr) function c_evo_scheme_coef_imp_v_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_evo_scheme_coef_imp_v_ctl')
!!      type(c_ptr) function c_evo_scheme_coef_imp_t_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_evo_scheme_coef_imp_t_ctl')
!!      type(c_ptr) function c_evo_scheme_coef_imp_b_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_evo_scheme_coef_imp_b_ctl')
!!      type(c_ptr) function c_evo_scheme_coef_imp_c_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_evo_scheme_coef_imp_c_ctl')
!!
!!      type(c_ptr) function c_evo_scheme_iflag_supg_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_evo_scheme_iflag_supg_ctl')
!!      type(c_ptr) function c_evo_scheme_iflag_supg_v_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_evo_scheme_iflag_supg_v_ctl')
!!      type(c_ptr) function c_evo_scheme_iflag_supg_t_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_evo_scheme_iflag_supg_t_ctl')
!!      type(c_ptr) function c_evo_scheme_iflag_supg_b_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_evo_scheme_iflag_supg_b_ctl')
!!      type(c_ptr) function c_evo_scheme_iflag_supg_c_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_evo_scheme_iflag_supg_c_ctl')
!!
!!      type(c_ptr) function c_evo_scheme_num_multi_pass(c_ctl)         &
!!     &          bind(C, NAME = 'c_evo_scheme_num_multi_pass')
!!      type(c_ptr) function c_evo_scheme_maxiter_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_evo_scheme_maxiter_ctl')
!!      type(c_ptr) function c_evo_scheme_eps_4_velo_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_evo_scheme_eps_4_velo_ctl')
!!      type(c_ptr) function c_evo_scheme_eps_4_magne_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_evo_scheme_eps_4_magne_ctl')
!!      type(c_ptr) function c_evo_scheme_eps_crank_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_evo_scheme_eps_crank_ctl')
!!      type(c_ptr) function c_evo_scheme_eps_B_crank_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_evo_scheme_eps_B_crank_ctl')
!!      type(c_ptr) function c_evo_scheme_scheme_ctl(c_ctl)             &
!!     &          bind(C, NAME = 'c_evo_scheme_scheme_ctl')
!!      type(c_ptr) function c_evo_scheme_diffuse_correct(c_ctl)        &
!!     &          bind(C, NAME = 'c_evo_scheme_diffuse_correct')
!!      type(c_ptr) function c_evo_scheme_method_4_CN(c_ctl)            &
!!     &          bind(C, NAME = 'c_evo_scheme_method_4_CN')
!!      type(c_ptr) function c_evo_scheme_precond_4_CN(c_ctl)           &
!!     &          bind(C, NAME = 'c_evo_scheme_precond_4_CN')
!!
!!      type(c_ptr) function c_evo_scheme_Leg_trans_type(c_ctl)         &
!!     &          bind(C, NAME = 'c_evo_scheme_Leg_trans_type')
!!      type(c_ptr) function c_evo_scheme_FFT_library(c_ctl)            &
!!     &          bind(C, NAME = 'c_evo_scheme_FFT_library')
!!      type(c_ptr) function c_evo_scheme_import_mode(c_ctl)            &
!!     &          bind(C, NAME = 'c_evo_scheme_import_mode')
!!
!!      type(c_ptr) function c_evo_scheme_leg_vector_len(c_ctl)         &
!!     &          bind(C, NAME = 'c_evo_scheme_leg_vector_len')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_mhd_evo_scheme_ctl
!
      use iso_c_binding
      use t_ctl_data_mhd_evo_scheme
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_mhd_evo_scheme_ctl_block_name(c_ctl)       &
     &          bind(C, NAME = 'c_mhd_evo_scheme_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_mhd_evo_scheme_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_mhd_evo_scheme_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_mhd_evo_scheme_ctl_iflag(c_ctl)            &
     &          bind(C, NAME = 'c_mhd_evo_scheme_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_mhd_evo_scheme_ctl_iflag = C_loc(f_ctl%i_time_loop)
      end function c_mhd_evo_scheme_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_coef_implicit_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_evo_scheme_coef_implicit_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_coef_implicit_ctl = C_loc(f_ctl%coef_implicit_ctl)
      end function c_evo_scheme_coef_implicit_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_coef_imp_v_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_evo_scheme_coef_imp_v_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_coef_imp_v_ctl = C_loc(f_ctl%coef_imp_v_ctl)
      end function c_evo_scheme_coef_imp_v_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_coef_imp_t_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_evo_scheme_coef_imp_t_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_coef_imp_t_ctl = C_loc(f_ctl%coef_imp_t_ctl)
      end function c_evo_scheme_coef_imp_t_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_coef_imp_b_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_evo_scheme_coef_imp_b_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_coef_imp_b_ctl = C_loc(f_ctl%coef_imp_b_ctl)
      end function c_evo_scheme_coef_imp_b_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_coef_imp_c_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_evo_scheme_coef_imp_c_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_coef_imp_c_ctl = C_loc(f_ctl%coef_imp_c_ctl)
      end function c_evo_scheme_coef_imp_c_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_iflag_supg_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_evo_scheme_iflag_supg_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_iflag_supg_ctl = C_loc(f_ctl%iflag_supg_ctl)
      end function c_evo_scheme_iflag_supg_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_iflag_supg_v_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_evo_scheme_iflag_supg_v_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_iflag_supg_v_ctl = C_loc(f_ctl%iflag_supg_v_ctl)
      end function c_evo_scheme_iflag_supg_v_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_iflag_supg_t_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_evo_scheme_iflag_supg_t_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_iflag_supg_t_ctl = C_loc(f_ctl%iflag_supg_t_ctl)
      end function c_evo_scheme_iflag_supg_t_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_iflag_supg_b_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_evo_scheme_iflag_supg_b_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_iflag_supg_b_ctl = C_loc(f_ctl%iflag_supg_b_ctl)
      end function c_evo_scheme_iflag_supg_b_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_iflag_supg_c_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_evo_scheme_iflag_supg_c_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_iflag_supg_c_ctl = C_loc(f_ctl%iflag_supg_c_ctl)
      end function c_evo_scheme_iflag_supg_c_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_num_multi_pass(c_ctl)           &
     &          bind(C, NAME = 'c_evo_scheme_num_multi_pass')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_num_multi_pass = C_loc(f_ctl%num_multi_pass_ctl)
      end function c_evo_scheme_num_multi_pass
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_maxiter_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_evo_scheme_maxiter_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_maxiter_ctl = C_loc(f_ctl%maxiter_ctl)
      end function c_evo_scheme_maxiter_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_eps_4_velo_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_evo_scheme_eps_4_velo_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_eps_4_velo_ctl = C_loc(f_ctl%eps_4_velo_ctl)
      end function c_evo_scheme_eps_4_velo_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_eps_4_magne_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_evo_scheme_eps_4_magne_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_eps_4_magne_ctl = C_loc(f_ctl%eps_4_magne_ctl)
      end function c_evo_scheme_eps_4_magne_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_eps_crank_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_evo_scheme_eps_crank_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_eps_crank_ctl = C_loc(f_ctl%eps_crank_ctl)
      end function c_evo_scheme_eps_crank_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_eps_B_crank_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_evo_scheme_eps_B_crank_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_eps_B_crank_ctl = C_loc(f_ctl%eps_B_crank_ctl)
      end function c_evo_scheme_eps_B_crank_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_scheme_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_evo_scheme_scheme_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_scheme_ctl = C_loc(f_ctl%scheme_ctl)
      end function c_evo_scheme_scheme_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_diffuse_correct(c_ctl)          &
     &          bind(C, NAME = 'c_evo_scheme_diffuse_correct')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_diffuse_correct = C_loc(f_ctl%diffuse_correct)
      end function c_evo_scheme_diffuse_correct
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_method_4_CN(c_ctl)              &
     &          bind(C, NAME = 'c_evo_scheme_method_4_CN')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_method_4_CN = C_loc(f_ctl%method_4_CN)
      end function c_evo_scheme_method_4_CN
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_precond_4_CN(c_ctl)             &
     &          bind(C, NAME = 'c_evo_scheme_precond_4_CN')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_precond_4_CN = C_loc(f_ctl%precond_4_CN)
      end function c_evo_scheme_precond_4_CN
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_Leg_trans_type(c_ctl)           &
     &          bind(C, NAME = 'c_evo_scheme_Leg_trans_type')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_Leg_trans_type = C_loc(f_ctl%Legendre_trans_type)
      end function c_evo_scheme_Leg_trans_type
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_FFT_library(c_ctl)              &
     &          bind(C, NAME = 'c_evo_scheme_FFT_library')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_FFT_library = C_loc(f_ctl%FFT_library)
      end function c_evo_scheme_FFT_library
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_import_mode(c_ctl)              &
     &          bind(C, NAME = 'c_evo_scheme_import_mode')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_import_mode = C_loc(f_ctl%import_mode)
      end function c_evo_scheme_import_mode
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_evo_scheme_leg_vector_len(c_ctl)           &
     &          bind(C, NAME = 'c_evo_scheme_leg_vector_len')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_scheme_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_evo_scheme_leg_vector_len = C_loc(f_ctl%leg_vector_len)
      end function c_evo_scheme_leg_vector_len
!
!  ---------------------------------------------------------------------
!
      end module c_link_mhd_evo_scheme_ctl
