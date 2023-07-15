!>@file   c_link_new_patition_ctl.f90
!!@brief  module c_link_new_patition_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!!
!>@brief C binding routines for sphere_data_control structure
!!@verbatim
!!      type(c_ptr) function c_new_repart_ctl_block_name(c_ctl)         &
!!     &          bind(C, NAME = 'c_new_repart_ctl_block_name')
!!      type(c_ptr) function c_new_repart_ctl_iflag(c_ctl)              &
!!     &          bind(C, NAME = 'c_new_repart_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_new_repart_table_head_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_new_repart_table_head_ctl')
!!      type(c_ptr) function c_new_repart_table_fmt_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_new_repart_table_fmt_ctl')
!!      type(c_ptr) function c_new_repart_partition_ref_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_new_repart_partition_ref_ctl')
!!      type(c_ptr) function c_new_repart_trace_cnt_head_ctl(c_ctl)     &
!!     &          bind(C, NAME = 'c_new_repart_trace_cnt_head_ctl')
!!      type(c_ptr) function c_new_repart_trace_cnt_fmt_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_new_repart_trace_cnt_fmt_ctl')
!!      type(c_ptr) function c_new_repart_ndomain_sect_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_new_repart_ndomain_sect_ctl')
!!      type(c_ptr) function c_new_repart_ratio_group_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_new_repart_ratio_group_ctl')
!!      type(c_ptr) function c_new_repart_sleeve_level_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_new_repart_sleeve_level_ctl')
!!      type(c_ptr) function c_new_repart_weight_to_prev_ctl(c_ctl)     &
!!     &          bind(C, NAME = 'c_new_repart_weight_to_prev_ctl')
!!      type(c_ptr) function c_new_repart_mask_switch_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_new_repart_mask_switch_ctl')
!!      type(c_ptr) function c_new_repart_mask_weight_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_new_repart_mask_weight_ctl')
!!      type(c_ptr) function c_new_repart_pwr_of_vol_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_new_repart_pwr_of_vol_ctl')
!!      integer(c_int) function c_new_repart_num_masking_ctl(c_ctl)     &
!!     &          bind(C, NAME = 'c_new_repart_num_masking_ctl')
!!      type(c_ptr) function c_new_repart_mask_ctl(idx_in, c_ctl)       &
!!     &          bind(C, NAME = 'c_new_repart_mask_ctl')
!!        integer(c_int), value, intent(in) :: idx_in
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_new_patition_ctl
!
      use iso_c_binding
      use t_ctl_data_volume_grouping
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_new_repart_ctl_block_name(c_ctl)           &
     &          bind(C, NAME = 'c_new_repart_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(new_patition_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_new_repart_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_new_repart_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_new_repart_ctl_iflag(c_ctl)                &
     &          bind(C, NAME = 'c_new_repart_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(new_patition_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_new_repart_ctl_iflag = C_loc(f_ctl%i_new_patition_ctl)
      end function c_new_repart_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_new_repart_table_head_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_new_repart_table_head_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(new_patition_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_new_repart_table_head_ctl = C_loc(f_ctl%repart_table_head_ctl)
      end function c_new_repart_table_head_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_new_repart_table_fmt_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_new_repart_table_fmt_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(new_patition_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_new_repart_table_fmt_ctl = C_loc(f_ctl%repart_table_fmt_ctl)
      end function c_new_repart_table_fmt_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_new_repart_partition_ref_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_new_repart_partition_ref_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(new_patition_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_new_repart_partition_ref_ctl                                    &
     &            = C_loc(f_ctl%partition_reference_ctl)
      end function c_new_repart_partition_ref_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_new_repart_trace_cnt_head_ctl(c_ctl)       &
     &          bind(C, NAME = 'c_new_repart_trace_cnt_head_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(new_patition_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_new_repart_trace_cnt_head_ctl                                   &
     &            = C_loc(f_ctl%trace_count_head_ctl)
      end function c_new_repart_trace_cnt_head_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_new_repart_trace_cnt_fmt_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_new_repart_trace_cnt_fmt_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(new_patition_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_new_repart_trace_cnt_fmt_ctl = C_loc(f_ctl%trace_count_fmt_ctl)
      end function c_new_repart_trace_cnt_fmt_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_new_repart_ndomain_sect_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_new_repart_ndomain_sect_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(new_patition_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_new_repart_ndomain_sect_ctl = C_loc(f_ctl%ndomain_section_ctl)
      end function c_new_repart_ndomain_sect_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_new_repart_ratio_group_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_new_repart_ratio_group_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(new_patition_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_new_repart_ratio_group_ctl = C_loc(f_ctl%ratio_of_grouping_ctl)
      end function c_new_repart_ratio_group_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_new_repart_sleeve_level_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_new_repart_sleeve_level_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(new_patition_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_new_repart_sleeve_level_ctl = C_loc(f_ctl%sleeve_level_ctl)
      end function c_new_repart_sleeve_level_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_new_repart_weight_to_prev_ctl(c_ctl)       &
     &          bind(C, NAME = 'c_new_repart_weight_to_prev_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(new_patition_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_new_repart_weight_to_prev_ctl                                   &
     &            = C_loc(f_ctl%weight_to_previous_ctl)
      end function c_new_repart_weight_to_prev_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_new_repart_mask_switch_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_new_repart_mask_switch_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(new_patition_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_new_repart_mask_switch_ctl = C_loc(f_ctl%masking_switch_ctl)
      end function c_new_repart_mask_switch_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_new_repart_mask_weight_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_new_repart_mask_weight_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(new_patition_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_new_repart_mask_weight_ctl = C_loc(f_ctl%masking_weight_ctl)
      end function c_new_repart_mask_weight_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_new_repart_pwr_of_vol_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_new_repart_pwr_of_vol_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(new_patition_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_new_repart_pwr_of_vol_ctl = C_loc(f_ctl%power_of_volume_ctl)
      end function c_new_repart_pwr_of_vol_ctl
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_new_repart_num_masking_ctl(c_ctl)       &
     &          bind(C, NAME = 'c_new_repart_num_masking_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(new_patition_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_new_repart_num_masking_ctl = f_ctl%num_masking_ctl
      end function c_new_repart_num_masking_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_new_repart_mask_ctl(idx_in, c_ctl)         &
     &          bind(C, NAME = 'c_new_repart_mask_ctl')
      integer(c_int), value, intent(in) :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(new_patition_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_new_repart_mask_ctl = C_loc(f_ctl%mask_ctl(idx_in+1))
      end function c_new_repart_mask_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_new_patition_ctl
