!>@file   ctl_data_volume_grouping_IO.f90
!!@brief  module ctl_data_volume_grouping_IO
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine read_ctl_data_new_partition                          &
!!     &         (id_control, hd_block, new_part_ctl, c_buf)
!!         integer(kind = kint), intent(in) :: id_control
!!         character(len=kchara), intent(in) :: hd_block
!!         type(new_patition_control), intent(inout) :: new_part_ctl
!!         type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_ctl_data_new_partition                         &
!!     &         (id_control, hd_block, new_part_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(new_patition_control), intent(in) :: new_part_ctl
!!        integer(kind = kint), intent(inout) :: level
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
      module ctl_data_volume_grouping_IO
!
      use m_precision
      use m_machine_parameter
!
      use t_read_control_elements
      use t_control_array_real
      use t_control_array_integer
      use t_control_array_character
      use t_control_array_charaint
      use t_control_data_masking
      use t_ctl_data_volume_grouping
      use skip_comment_f
!
      implicit  none
!
!     Labels
!
      character(len=kchara), parameter, private                         &
     &       :: hd_repart_table_head = 'repartition_table_prefix'
      character(len=kchara), parameter, private                         &
     &       :: hd_repart_table_fmt =  'repartition_table_format'
!
      character(len=kchara), parameter, private                         &
     &               :: hd_num_es =       'dir_domain_ctl'
      character(len=kchara), parameter, private                         &
     &               :: hd_ratio_divide = 'group_ratio_to_domain_ctl'
!
      character(len=kchara), parameter, private                         &
     &               :: hd_sleeve_level = 'sleeve_level_ctl'
!
      character(len=kchara), parameter, private                         &
     &               :: hd_part_ref =         'partition_reference_ctl'
      character(len=kchara), parameter, private                         &
     &               :: hd_trace_cnt_prefix = 'trace_count_file_prefix'
      character(len=kchara), parameter, private                         &
     &               :: hd_trace_count_fmt =  'trace_count_file_format'
!
      character(len=kchara), parameter, private                         &
     &               :: hd_weight_to_prev = 'weight_to_previous'
      character(len=kchara), parameter, private                         &
     &               :: hd_power_of_volume = 'power_of_volume_ctl'
!
      character(len=kchara), parameter, private                         &
     &              :: hd_masking_switch = 'masking_switch_ctl'
      character(len=kchara), parameter, private                         &
     &              :: hd_masking_weight = 'masking_weight_ctl'
      character(len=kchara), parameter, private                         &
     &              :: hd_masking_ctl = 'masking_control'
!

      private :: read_repart_masking_ctl_array
      private :: write_repart_masking_ctl_array
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_ctl_data_new_partition                            &
     &         (id_control, hd_block, new_part_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(new_patition_control), intent(inout) :: new_part_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(new_part_ctl%i_new_patition_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c_i(id_control,                         &
     &      hd_num_es, new_part_ctl%ndomain_section_ctl, c_buf)
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_sleeve_level, new_part_ctl%sleeve_level_ctl)
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_ratio_divide, new_part_ctl%ratio_of_grouping_ctl)
!
        call read_chara_ctl_type(c_buf, hd_repart_table_head,           &
     &                           new_part_ctl%repart_table_head_ctl)
        call read_chara_ctl_type(c_buf, hd_repart_table_fmt,            &
     &                           new_part_ctl%repart_table_fmt_ctl)
!
        call read_chara_ctl_type(c_buf, hd_part_ref,                    &
     &                           new_part_ctl%partition_reference_ctl)
        call read_chara_ctl_type(c_buf, hd_trace_cnt_prefix,            &
     &                           new_part_ctl%trace_count_head_ctl)
        call read_chara_ctl_type(c_buf, hd_trace_count_fmt,             &
     &                           new_part_ctl%trace_count_fmt_ctl)
!
        call read_chara_ctl_type(c_buf, hd_masking_switch,              &
     &                           new_part_ctl%masking_switch_ctl)
!
        call read_real_ctl_type(c_buf, hd_power_of_volume,              &
     &                          new_part_ctl%power_of_volume_ctl)
        call read_real_ctl_type(c_buf, hd_masking_weight,               &
     &                          new_part_ctl%masking_weight_ctl)
        call read_real_ctl_type(c_buf, hd_weight_to_prev,               &
     &      new_part_ctl%weight_to_previous_ctl)
!
        call read_repart_masking_ctl_array                              &
     &     (id_control, hd_masking_ctl, new_part_ctl, c_buf)
      end do
      new_part_ctl%i_new_patition_ctl = 1
!
      end subroutine read_ctl_data_new_partition
!
! -----------------------------------------------------------------------
!
      subroutine write_ctl_data_new_partition                           &
     &         (id_control, hd_block, new_part_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(new_patition_control), intent(in) :: new_part_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(new_part_ctl%i_new_patition_ctl .le. 0) return
!
      maxlen = len_trim(hd_repart_table_head)
      maxlen = max(maxlen, len_trim(hd_repart_table_fmt))
      maxlen = max(maxlen, len_trim(hd_ratio_divide))
      maxlen = max(maxlen, len_trim(hd_sleeve_level))
      maxlen = max(maxlen, len_trim(hd_part_ref))
      maxlen = max(maxlen, len_trim(hd_trace_cnt_prefix))
      maxlen = max(maxlen, len_trim(hd_trace_count_fmt))
      maxlen = max(maxlen, len_trim(hd_weight_to_prev))
      maxlen = max(maxlen, len_trim(hd_power_of_volume))
      maxlen = max(maxlen, len_trim(hd_masking_switch))
      maxlen = max(maxlen, len_trim(hd_masking_weight))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    new_part_ctl%repart_table_head_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    new_part_ctl%repart_table_fmt_ctl)
!
      call write_control_array_c_i(id_control, level,                   &
     &    hd_num_es, new_part_ctl%ndomain_section_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    new_part_ctl%ratio_of_grouping_ctl)
!
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    new_part_ctl%sleeve_level_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    new_part_ctl%partition_reference_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    new_part_ctl%trace_count_head_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    new_part_ctl%trace_count_fmt_ctl)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_weight_to_prev, new_part_ctl%weight_to_previous_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_power_of_volume, new_part_ctl%power_of_volume_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    new_part_ctl%masking_switch_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_masking_weight, new_part_ctl%masking_weight_ctl)
!
      call write_repart_masking_ctl_array(id_control,                   &
     &    hd_masking_ctl, new_part_ctl, level)
!
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_ctl_data_new_partition
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_repart_masking_ctl_array                          &
     &         (id_control, hd_block, new_part_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_block
!
      type(new_patition_control), intent(inout) :: new_part_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_array_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(allocated(new_part_ctl%mask_ctl)) return
      new_part_ctl%num_masking_ctl = 0
      call alloc_repart_masking_ctl(new_part_ctl)
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if (check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_begin_flag(c_buf, hd_block)) then
          call append_repart_masking_ctl(new_part_ctl)
          call read_masking_ctl_data(id_control, hd_block,              &
     &        new_part_ctl%mask_ctl(new_part_ctl%num_masking_ctl),      &
     &        c_buf)
        end if
      end do
!
      end subroutine read_repart_masking_ctl_array
!
!  ---------------------------------------------------------------------
!
      subroutine write_repart_masking_ctl_array                         &
     &         (id_control, hd_block, new_part_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_block
      type(new_patition_control), intent(in) :: new_part_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      level = write_array_flag_for_ctl(id_control, level, hd_block)
      do i = 1, new_part_ctl%num_masking_ctl
        call write_masking_ctl_data(id_control, hd_block,               &
     &      new_part_ctl%mask_ctl(i), level)
      end do
      level = write_end_array_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_repart_masking_ctl_array
!
!  ---------------------------------------------------------------------
!
      end module ctl_data_volume_grouping_IO
