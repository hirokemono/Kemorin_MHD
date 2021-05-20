!>@file   t_ctl_data_volume_grouping.f90
!!@brief  module t_ctl_data_volume_grouping
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine read_ctl_data_new_partition                          &
!!     &         (id_control, hd_block, new_part_ctl, c_buf)
!!      subroutine dealloc_ctl_data_new_decomp(new_part_ctl)
!!      subroutine bcast_ctl_data_new_decomp(new_part_ctl)
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
!!      partitioning_method_ctl
!!      array dir_domain_ctl
!!        dir_domain_ctl  x     3
!!        dir_domain_ctl  y     4
!!        dir_domain_ctl  z     8
!!      end array dir_domain_ctl
!!      group_ratio_to_domain_ctl    100
!!
!!      sleeve_level_ctl             2
!!    end new_partitioning_ctl
!! -------------------------------------------------------------------
!!@endverbatim
      module t_ctl_data_volume_grouping
!
      use m_precision
      use m_machine_parameter
!
      use t_read_control_elements
      use t_control_array_integer
      use t_control_array_character
      use t_control_array_charaint
      use skip_comment_f
!
      implicit  none
!
      type new_patition_control
!>        Data transfer table file prefix
        type(read_character_item) :: repart_table_head_ctl
!>        Data transfer table file format
        type(read_character_item) :: repart_table_fmt_ctl
!
!>        Flag for new patitioning method
        type(read_character_item) :: new_part_method_ctl
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
!
        integer(kind = kint) :: i_new_patition_ctl = 0
      end type new_patition_control
!
!     Labels
!
      character(len=kchara), parameter, private                         &
     &       :: hd_repart_table_head = 'repartition_table_prefix'
      character(len=kchara), parameter, private                         &
     &       :: hd_repart_table_fmt =  'repartition_table_format'
!
      character(len=kchara), parameter, private                         &
     &                 :: hd_part_method =  'partitioning_method_ctl'
      character(len=kchara), parameter, private                         &
     &                 :: hd_sleeve_level = 'sleeve_level_ctl'
      character(len=kchara), parameter, private                         &
     &                 :: hd_num_es =       'dir_domain_ctl'
      character(len=kchara), parameter, private                         &
     &                 :: hd_ratio_divide = 'group_ratio_to_domain_ctl'
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
        call load_one_line_from_control(id_control, c_buf)
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
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_part_method, new_part_ctl%new_part_method_ctl)
      end do
      new_part_ctl%i_new_patition_ctl = 1
!
      end subroutine read_ctl_data_new_partition
!
! -----------------------------------------------------------------------
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
      new_part_ctl%new_part_method_ctl%iflag = 0
      new_part_ctl%sleeve_level_ctl%iflag = 0
!
      new_part_ctl%ratio_of_grouping_ctl%iflag = 0
!
      new_part_ctl%i_new_patition_ctl = 0
!
      end subroutine dealloc_ctl_data_new_decomp
!
! -----------------------------------------------------------------------
!
      subroutine bcast_ctl_data_new_decomp(new_part_ctl)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(new_patition_control), intent(inout) :: new_part_ctl
!
!
      call bcast_ctl_type_c1(new_part_ctl%repart_table_head_ctl)
      call bcast_ctl_type_c1(new_part_ctl%repart_table_fmt_ctl)
!
      call bcast_ctl_array_ci(new_part_ctl%ndomain_section_ctl)
      call bcast_ctl_type_c1(new_part_ctl%new_part_method_ctl)
      call bcast_ctl_type_i1(new_part_ctl%sleeve_level_ctl)
      call bcast_ctl_type_i1(new_part_ctl%ratio_of_grouping_ctl)
!
      call calypso_mpi_bcast_one_int                                    &
     &   (new_part_ctl%i_new_patition_ctl, 0)
!
      end subroutine bcast_ctl_data_new_decomp
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
      call copy_chara_ctl(org_new_part_c%new_part_method_ctl,           &
     &                    new_new_part_c%new_part_method_ctl)
!
      call dup_control_array_c_i(org_new_part_c%ndomain_section_ctl,    &
     &                           new_new_part_c%ndomain_section_ctl)
      call copy_integer_ctl(org_new_part_c%sleeve_level_ctl,            &
     &                      new_new_part_c%sleeve_level_ctl)
      call copy_integer_ctl(org_new_part_c%ratio_of_grouping_ctl,       &
     &                      new_new_part_c%ratio_of_grouping_ctl)
!
      new_new_part_c%i_new_patition_ctl                                 &
     &      = org_new_part_c%i_new_patition_ctl
!
      end subroutine dup_ctl_data_new_decomp
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_volume_grouping
