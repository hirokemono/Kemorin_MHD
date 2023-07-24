!>@file   t_ctl_data_volume_grouping.f90
!!@brief  module t_ctl_data_volume_grouping
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine dealloc_ctl_data_new_decomp(new_part_ctl)
!!        type(new_patition_control), intent(inout) :: new_part_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine dup_ctl_data_new_decomp(org_new_part_c,              &
!!     &                                   new_new_part_c)
!!        type(new_patition_control), intent(in) :: org_new_part_c
!!        type(new_patition_control), intent(inout) :: new_new_part_c
!!
!! --------------------------------------------------------------------
!!    Example of control block
!!
!!    begin new_partitioning_ctl
!!      repartition_table_prefix      'mesh_new/transfer_table'
!!      repartition_table_format      'merged_bin_gz'
!!
!!      array dir_domain_ctl
!!        dir_domain_ctl  x     3
!!        dir_domain_ctl  y     4
!!        dir_domain_ctl  z     8
!!      end array dir_domain_ctl
!!      group_ratio_to_domain_ctl    100
!!
!!      sleeve_level_ctl             2
!!
!!!!      partition_reference_ctl:
!!!!         SAVED_COUNT, PREDICTED_COUNT, STACKED_COUNT, AVERAGE_COUNT
!!!!         VOLUME_BASED, NUMBER_BASED, or NO_REPARTITION
!!      partition_reference_ctl         VOLUME_BASED
!!
!!      trace_count_file_prefix         'line_integration_count'
!!      trace_count_file_format         'merged_bin_gz'
!!
!!      weight_to_previous          0.6
!!      power_of_volume_ctl         0.5
!!
!!      masking_switch_ctl          On
!!      masking_weight_ctl          0.1
!!      array masking_control
!!        begin masking_control
!!          masking_field        magnetic_field
!!          masking_component    amplitude
!!          array masking_range      1
!!            masking_range       0.5    0.8
!!            ...
!!          end array masking_range
!!        end masking_control
!!        ...
!!      end array masking_control
!!    end new_partitioning_ctl
!! -------------------------------------------------------------------
!!@endverbatim
      module t_ctl_data_volume_grouping
!
      use m_precision
      use m_machine_parameter
!
      use t_read_control_elements
      use t_control_array_real
      use t_control_array_integer
      use t_control_array_character
      use t_control_array_charaint
      use t_control_data_maskings
      use skip_comment_f
!
      implicit  none
!
      type new_patition_control
!>        Block name
        character(len=kchara) :: block_name = 'LIC_ctl'
!>        Data transfer table file prefix
        type(read_character_item) :: repart_table_head_ctl
!>        Data transfer table file format
        type(read_character_item) :: repart_table_fmt_ctl
!
!>        Flag for new patitioning method
        type(read_character_item) :: partition_reference_ctl
!>        Data file prefix for trace counding data
        type(read_character_item) :: trace_count_head_ctl
!>        DAta format for trace counding data
        type(read_character_item) :: trace_count_fmt_ctl
!
!>        Structure for number of subdomains
!!@n        ndomain_section_ctl%c_tbl:  Direction of sectioning
!!@n        ndomain_section_ctl%ivect:  Number of domains
        type(ctl_array_ci) :: ndomain_section_ctl
!>        Ratio of reference division to subdomain
        type(read_integer_item) :: ratio_of_grouping_ctl
!
!>        Number of sleeve level
        type(read_integer_item) :: sleeve_level_ctl
!>        Weight for previous elapsed time
        type(read_real_item) :: weight_to_previous_ctl
!
!>        Flag for masking
        type(read_character_item) :: masking_switch_ctl
!>        Weight for masked data
        type(read_real_item) :: masking_weight_ctl
!>        Power of volume for re-paritiong reference
        type(read_real_item) :: power_of_volume_ctl
!
!>        field masking list
        type(multi_masking_ctl) :: mul_mask_c
!
        integer(kind = kint) :: i_new_patition_ctl = 0
      end type new_patition_control
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_ctl_data_new_decomp(new_part_ctl)
!
      type(new_patition_control), intent(inout) :: new_part_ctl
!
!
      call dealloc_control_array_c_i(new_part_ctl%ndomain_section_ctl)
!
      new_part_ctl%repart_table_head_ctl%iflag = 0
      new_part_ctl%repart_table_fmt_ctl%iflag =  0
!
      new_part_ctl%partition_reference_ctl%iflag = 0
      new_part_ctl%trace_count_head_ctl%iflag =    0
      new_part_ctl%trace_count_fmt_ctl%iflag =     0
      new_part_ctl%weight_to_previous_ctl%iflag =  0
      new_part_ctl%masking_switch_ctl%iflag =  0
      new_part_ctl%power_of_volume_ctl%iflag = 0
      new_part_ctl%masking_weight_ctl%iflag =  0
      new_part_ctl%sleeve_level_ctl%iflag =    0
!
      new_part_ctl%ratio_of_grouping_ctl%iflag = 0
!
      call dealloc_mul_masking_ctl(new_part_ctl%mul_mask_c)
!
      new_part_ctl%i_new_patition_ctl = 0
!
      end subroutine dealloc_ctl_data_new_decomp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dup_ctl_data_new_decomp(org_new_part_c,                &
     &                                   new_new_part_c)
!
      type(new_patition_control), intent(in) :: org_new_part_c
      type(new_patition_control), intent(inout) :: new_new_part_c
!
!
      call copy_chara_ctl(org_new_part_c%repart_table_head_ctl,         &
     &                    new_new_part_c%repart_table_head_ctl)
      call copy_chara_ctl(org_new_part_c%repart_table_fmt_ctl,          &
     &                    new_new_part_c%repart_table_fmt_ctl)
!
      call copy_chara_ctl(org_new_part_c%partition_reference_ctl,       &
     &                    new_new_part_c%partition_reference_ctl)
      call copy_chara_ctl(org_new_part_c%trace_count_head_ctl,          &
     &                    new_new_part_c%trace_count_head_ctl)
      call copy_chara_ctl(org_new_part_c%trace_count_fmt_ctl,           &
     &                    new_new_part_c%trace_count_fmt_ctl)
      call copy_chara_ctl(org_new_part_c%masking_switch_ctl,            &
     &                    new_new_part_c%masking_switch_ctl)
      call copy_real_ctl(org_new_part_c%power_of_volume_ctl,            &
     &                   new_new_part_c%power_of_volume_ctl)
      call copy_real_ctl(org_new_part_c%masking_weight_ctl,             &
     &                   new_new_part_c%masking_weight_ctl)
      call copy_real_ctl(org_new_part_c%weight_to_previous_ctl,         &
     &                   new_new_part_c%weight_to_previous_ctl)
!
      call dup_control_array_c_i(org_new_part_c%ndomain_section_ctl,    &
     &                           new_new_part_c%ndomain_section_ctl)
      call copy_integer_ctl(org_new_part_c%sleeve_level_ctl,            &
     &                      new_new_part_c%sleeve_level_ctl)
      call copy_integer_ctl(org_new_part_c%ratio_of_grouping_ctl,       &
     &                      new_new_part_c%ratio_of_grouping_ctl)
!
      new_new_part_c%block_name = org_new_part_c%block_name
      new_new_part_c%i_new_patition_ctl                                 &
     &      = org_new_part_c%i_new_patition_ctl
!
      end subroutine dup_ctl_data_new_decomp
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_volume_grouping
