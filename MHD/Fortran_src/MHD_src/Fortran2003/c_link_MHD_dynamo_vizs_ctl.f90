!>@file   c_link_MHD_dynamo_vizs_ctl.f90
!!@brief  module c_link_MHD_dynamo_vizs_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for forces control structure
!!@verbatim
!!      type(c_ptr) function c_MHD_dynamo_viz_ctl_block_name(c_ctl)     &
!!     &          bind(C, NAME = 'c_MHD_dynamo_viz_ctl_block_name')
!!      type(c_ptr) function c_MHD_dynamo_viz_ctl_iflag(c_ctl)          &
!!     &          bind(C, NAME = 'c_MHD_dynamo_viz_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_MHD_dynamo_viz_crust_fil_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_MHD_dynamo_viz_crust_fil_ctl')
!!      type(c_ptr) function c_MHD_dynamo_viz_zm_psf_ctls(c_ctl)        &
!!     &          bind(C, NAME = 'c_MHD_dynamo_viz_zm_psf_ctls')
!!      type(c_ptr) function c_MHD_dynamo_viz_zRMS_psf_ctls(c_ctl)      &
!!     &          bind(C, NAME = 'c_MHD_dynamo_viz_zRMS_psf_ctls')
!!      type(c_ptr) function c_MHD_dynamo_viz_zm_map_ctls(c_ctl)        &
!!     &          bind(C, NAME = 'c_MHD_dynamo_viz_zm_map_ctls')
!!      type(c_ptr) function c_MHD_dynamo_viz_zRMS_map_ctls(c_ctl)      &
!!     &          bind(C, NAME = 'c_MHD_dynamo_viz_zRMS_map_ctls')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!!!
!!      type(c_ptr) function c_clust_filter_ctl_block_name(c_ctl)       &
!!     &          bind(C, NAME = 'c_clust_filter_ctl_block_name')
!!      type(c_ptr) function c_clust_filter_ctl_iflag(c_ctl)            &
!!     &          bind(C, NAME = 'c_clust_filter_ctl_iflag')
!!      type(c_ptr) function c_clust_filter_ltr_ctl(c_ctl)              &
!!     &          bind(C, NAME = 'c_clust_filter_ltr_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_MHD_dynamo_vizs_ctl
!
      use iso_c_binding
      use t_control_data_dynamo_vizs
      use t_ctl_data_crust_filter
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_dynamo_viz_ctl_block_name(c_ctl)       &
     &          bind(C, NAME = 'c_MHD_dynamo_viz_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_dynamo_viz_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_dynamo_viz_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_dynamo_viz_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_dynamo_viz_ctl_iflag(c_ctl)            &
     &          bind(C, NAME = 'c_MHD_dynamo_viz_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_dynamo_viz_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_dynamo_viz_ctl_iflag = C_loc(f_ctl%i_viz_ctl)
      end function c_MHD_dynamo_viz_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_dynamo_viz_crust_fil_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_MHD_dynamo_viz_crust_fil_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_dynamo_viz_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_dynamo_viz_crust_fil_ctl = C_loc(f_ctl%crust_filter_ctl)
      end function c_MHD_dynamo_viz_crust_fil_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_dynamo_viz_zm_psf_ctls(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_dynamo_viz_zm_psf_ctls')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_dynamo_viz_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_dynamo_viz_zm_psf_ctls = C_loc(f_ctl%zm_psf_ctls)
      end function c_MHD_dynamo_viz_zm_psf_ctls
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_dynamo_viz_zRMS_psf_ctls(c_ctl)        &
     &          bind(C, NAME = 'c_MHD_dynamo_viz_zRMS_psf_ctls')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_dynamo_viz_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_dynamo_viz_zRMS_psf_ctls = C_loc(f_ctl%zRMS_psf_ctls)
      end function c_MHD_dynamo_viz_zRMS_psf_ctls
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_dynamo_viz_zm_map_ctls(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_dynamo_viz_zm_map_ctls')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_dynamo_viz_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_dynamo_viz_zm_map_ctls = C_loc(f_ctl%zm_map_ctls)
      end function c_MHD_dynamo_viz_zm_map_ctls
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_dynamo_viz_zRMS_map_ctls(c_ctl)        &
     &          bind(C, NAME = 'c_MHD_dynamo_viz_zRMS_map_ctls')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_dynamo_viz_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_dynamo_viz_zRMS_map_ctls = C_loc(f_ctl%zRMS_map_ctls)
      end function c_MHD_dynamo_viz_zRMS_map_ctls
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_clust_filter_ctl_block_name(c_ctl)         &
     &          bind(C, NAME = 'c_clust_filter_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(clust_filtering_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_clust_filter_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_clust_filter_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_clust_filter_ctl_iflag(c_ctl)              &
     &          bind(C, NAME = 'c_clust_filter_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(clust_filtering_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_clust_filter_ctl_iflag = C_loc(f_ctl%i_crustal_filtering)
      end function c_clust_filter_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_clust_filter_ltr_ctl(c_ctl)                &
     &          bind(C, NAME = 'c_clust_filter_ltr_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(clust_filtering_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_clust_filter_ltr_ctl = C_loc(f_ctl%crust_truncation_ctl)
      end function c_clust_filter_ltr_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_MHD_dynamo_vizs_ctl
