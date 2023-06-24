!>@file   c_link_ctl_data_divide_sph.f90
!!@brief  module c_link_ctl_data_divide_sph
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for sphere_domain_control structure
!!@verbatim
!!      type(c_ptr) function c_sph_domain_ctl_block_name(c_ctl)         &
!!     &          bind(C, NAME = 'c_sph_domain_ctl_block_name')
!!      type(c_ptr) function c_sph_domain_ctl_iflag(c_ctl)              &
!!     &          bind(C, NAME = 'c_sph_domain_ctl_iflag')
!!
!!      type(c_ptr) function c_sph_inner_decomp_ctl(c_ctl)              &
!!     &          bind(C, NAME = 'c_sph_inner_decomp_ctl')
!!      type(c_ptr) function c_sph_rj_inner_loop_ctl(c_ctl)             &
!!     &          bind(C, NAME = 'c_sph_rj_inner_loop_ctl')
!!      type(c_ptr) function c_sph_rlm_inner_loop_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_sph_rlm_inner_loop_ctl')
!!      type(c_ptr) function c_sph_rtm_inner_loop_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_sph_rtm_inner_loop_ctl')
!!      type(c_ptr) function c_sph_rtp_inner_loop_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_sph_rtp_inner_loop_ctl')
!!      type(c_ptr) function c_sph_domain_rlm_distr_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_sph_domain_rlm_distr_ctl')
!!      type(c_ptr) function c_sph_domain_smpl_r_decomp_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_sph_domain_smpl_r_decomp_ctl')
!!      type(c_ptr) function c_sph_num_radial_domain_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_sph_num_radial_domain_ctl')
!!      type(c_ptr) function c_sph_num_horiz_domain_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_sph_num_horiz_domain_ctl')
!!      type(c_ptr) function c_sph_ndomain_sph_grid_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_sph_ndomain_sph_grid_ctl')
!!      type(c_ptr) function c_sph_ndomain_legendre_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_sph_ndomain_legendre_ctl')
!!      type(c_ptr) function c_sph_ndomain_spectr_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_sph_ndomain_spectr_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_FEM_mesh_FILE_ctl_block_name(c_ctl)      &
!!     &          bind(C, NAME = 'c_FEM_mesh_FILE_ctl_block_name')
!!      type(c_ptr) function c_FEM_mesh_FILE_ctl_iflag(c_ctl)           &
!!     &          bind(C, NAME = 'c_FEM_mesh_FILE_ctl_iflag')
!!      type(c_ptr) function c_FEM_mesh_mem_conserve_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_FEM_mesh_mem_conserve_ctl')
!!      type(c_ptr) function c_FEM_mesh_output_switch(c_ctl)            &
!!     &          bind(C, NAME = 'c_FEM_mesh_output_switch')
!!      type(c_ptr) function c_FEM_surface_output_switch(c_ctl)         &
!!     &          bind(C, NAME = 'c_FEM_surface_output_switch')
!!      type(c_ptr) function c_FEM_viewer_output_switch(c_ctl)          &
!!     &          bind(C, NAME = 'c_FEM_viewer_output_switch')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_ctl_data_divide_sph
!
      use iso_c_binding
      use t_ctl_data_4_divide_sphere
      use t_ctl_data_4_FEM_mesh
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_domain_ctl_block_name(c_ctl)           &
     &          bind(C, NAME = 'c_sph_domain_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_domain_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_domain_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_sph_domain_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_domain_ctl_iflag(c_ctl)                &
     &          bind(C, NAME = 'c_sph_domain_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_domain_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_domain_ctl_iflag = C_loc(f_ctl%i_domains_sph)
      end function c_sph_domain_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_inner_decomp_ctl(c_ctl)                &
     &          bind(C, NAME = 'c_sph_inner_decomp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_domain_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_inner_decomp_ctl = C_loc(f_ctl%inner_decomp_ctl)
      end function c_sph_inner_decomp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_rj_inner_loop_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_sph_rj_inner_loop_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_domain_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_rj_inner_loop_ctl = C_loc(f_ctl%rj_inner_loop_ctl)
      end function c_sph_rj_inner_loop_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_rlm_inner_loop_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_sph_rlm_inner_loop_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_domain_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_rlm_inner_loop_ctl = C_loc(f_ctl%rlm_inner_loop_ctl)
      end function c_sph_rlm_inner_loop_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_rtm_inner_loop_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_sph_rtm_inner_loop_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_domain_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_rtm_inner_loop_ctl = C_loc(f_ctl%rtm_inner_loop_ctl)
      end function c_sph_rtm_inner_loop_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_rtp_inner_loop_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_sph_rtp_inner_loop_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_domain_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_rtp_inner_loop_ctl = C_loc(f_ctl%rtp_inner_loop_ctl)
      end function c_sph_rtp_inner_loop_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_domain_rlm_distr_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_sph_domain_rlm_distr_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_domain_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_domain_rlm_distr_ctl = C_loc(f_ctl%rlm_distibution_ctl)
      end function c_sph_domain_rlm_distr_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_domain_smpl_r_decomp_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_sph_domain_smpl_r_decomp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_domain_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_domain_smpl_r_decomp_ctl = C_loc(f_ctl%simple_r_decomp_ctl)
      end function c_sph_domain_smpl_r_decomp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_num_radial_domain_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_sph_num_radial_domain_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_domain_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_num_radial_domain_ctl = C_loc(f_ctl%num_radial_domain_ctl)
      end function c_sph_num_radial_domain_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_num_horiz_domain_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_sph_num_horiz_domain_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_domain_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_num_horiz_domain_ctl = C_loc(f_ctl%num_horiz_domain_ctl)
      end function c_sph_num_horiz_domain_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_ndomain_sph_grid_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_sph_ndomain_sph_grid_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_domain_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_ndomain_sph_grid_ctl = C_loc(f_ctl%ndomain_sph_grid_ctl)
      end function c_sph_ndomain_sph_grid_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_ndomain_legendre_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_sph_ndomain_legendre_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_domain_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_ndomain_legendre_ctl = C_loc(f_ctl%ndomain_legendre_ctl)
      end function c_sph_ndomain_legendre_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_ndomain_spectr_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_sph_ndomain_spectr_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sphere_domain_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_ndomain_spectr_ctl = C_loc(f_ctl%ndomain_spectr_ctl)
      end function c_sph_ndomain_spectr_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_FEM_mesh_FILE_ctl_block_name(c_ctl)        &
     &          bind(C, NAME = 'c_FEM_mesh_FILE_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(FEM_mesh_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_FEM_mesh_FILE_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_FEM_mesh_FILE_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_FEM_mesh_FILE_ctl_iflag(c_ctl)             &
     &          bind(C, NAME = 'c_FEM_mesh_FILE_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(FEM_mesh_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_FEM_mesh_FILE_ctl_iflag = C_loc(f_ctl%i_FEM_mesh)
      end function c_FEM_mesh_FILE_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_FEM_mesh_mem_conserve_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_FEM_mesh_mem_conserve_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(FEM_mesh_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_FEM_mesh_mem_conserve_ctl                                       &
     &             = C_loc(f_ctl%memory_conservation_ctl)
      end function c_FEM_mesh_mem_conserve_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_FEM_mesh_output_switch(c_ctl)              &
     &          bind(C, NAME = 'c_FEM_mesh_output_switch')
      type(c_ptr), value, intent(in) :: c_ctl
      type(FEM_mesh_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_FEM_mesh_output_switch = C_loc(f_ctl%FEM_mesh_output_switch)
      end function c_FEM_mesh_output_switch
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_FEM_surface_output_switch(c_ctl)           &
     &          bind(C, NAME = 'c_FEM_surface_output_switch')
      type(c_ptr), value, intent(in) :: c_ctl
      type(FEM_mesh_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_FEM_surface_output_switch                                       &
     &             = C_loc(f_ctl%FEM_surface_output_switch)
      end function c_FEM_surface_output_switch
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_FEM_viewer_output_switch(c_ctl)            &
     &          bind(C, NAME = 'c_FEM_viewer_output_switch')
      type(c_ptr), value, intent(in) :: c_ctl
      type(FEM_mesh_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_FEM_viewer_output_switch= C_loc(f_ctl%FEM_viewer_output_switch)
      end function c_FEM_viewer_output_switch
!
!  ---------------------------------------------------------------------
!
      end module c_link_ctl_data_divide_sph
