!>@file   ctl_file_volume_repart_IO.f90
!!@brief  module ctl_file_volume_repart_IO
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Control data for repartitioning by volume
!!
!!@verbatim
!!      subroutine sel_read_ctl_file_vol_repart(id_control, hd_block,   &
!!     &          file_name, viz_repart_c, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        character(len=kchara), intent(inout) :: file_name
!!        type(viz_repartition_ctl), intent(inout) :: viz_repart_c
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine sel_write_ctl_file_vol_repart(id_control, hd_block,  &
!!     &          file_name, viz_repart_c, level)
!!      subroutine write_ctl_file_vol_repart(id_control, file_name,     &
!!     &                                     hd_block, viz_repart_c)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(viz_repartition_ctl), intent(inout) :: viz_repart_c
!!        integer(kind = kint), intent(inout) :: level
!!
!!   --------------------------------------------------------------------
!!    Example of control block
!!
!!  begin viz_repartition_ctl
!!    begin viz_data_files_def
!!      num_subdomain_ctl    96
!!      mesh_file_prefix         'mesh_new/in'
!!      mesh_file_fmt_ctl    'merged_bin_gz'
!!    end viz_data_files_def
!!
!!    begin FEM_mesh_ctl
!!      ...
!!    end FEM_mesh_ctl
!!
!!    begin new_partitioning_ctl
!!      ...
!!    end new_partitioning_ctl
!!
!!    begin FEM_sleeve_ctl
!!      ...
!!    end FEM_sleeve_ctl
!!  end viz_repartition_ctl
!!
!! -------------------------------------------------------------------
!!@endverbatim
      module ctl_file_volume_repart_IO
!
      use m_precision
      use m_machine_parameter
!
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
      use t_ctl_data_FEM_sleeve_size
      use t_ctl_data_volume_grouping
      use t_ctl_data_volume_repart
!
      implicit  none
!
      private :: read_ctl_file_vol_repart
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine sel_read_ctl_file_vol_repart(id_control, hd_block,     &
     &          file_name, viz_repart_c, c_buf)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      character(len=kchara), intent(inout) :: file_name
      type(viz_repartition_ctl), intent(inout) :: viz_repart_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_file_flag(c_buf, hd_block)) then
        file_name = third_word(c_buf)
!
        call write_one_ctl_file_message                                 &
     &     (hd_block, c_buf%level, file_name)
        call read_ctl_file_vol_repart((id_control+2), file_name,        &
     &                                hd_block, viz_repart_c, c_buf)
      else if(check_begin_flag(c_buf, hd_block)) then
        file_name = 'NO_FILE'
!
        call write_included_message(hd_block, c_buf%level)
        call read_control_vol_repart(id_control, hd_block,              &
     &                               viz_repart_c, c_buf)
      end if
!
      end subroutine sel_read_ctl_file_vol_repart
!
!   --------------------------------------------------------------------
!
      subroutine read_ctl_file_vol_repart(id_control, file_name,        &
     &          hd_block, viz_repart_c, c_buf)
!
      use skip_comment_f
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(viz_repartition_ctl), intent(inout) :: viz_repart_c
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      c_buf%level = c_buf%level + 1
      open(id_control, file=file_name, status='old')
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
!
        call read_control_vol_repart                                    &
     &     (id_control, hd_block, viz_repart_c, c_buf)
        if(viz_repart_c%i_viz_repartition_ctl .gt. 0) exit
      end do
      close(id_control)
!
      c_buf%level = c_buf%level - 1
!
      end subroutine read_ctl_file_vol_repart
!
!   --------------------------------------------------------------------
!
      subroutine sel_write_ctl_file_vol_repart(id_control, hd_block,    &
     &          file_name, viz_repart_c, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(viz_repartition_ctl), intent(in) :: viz_repart_c
      integer(kind = kint), intent(inout) :: level
!
!
      if(no_file_flag(file_name)) then
        call write_control_vol_repart(id_control, hd_block,             &
     &                                viz_repart_c, level)
      else if(id_control .eq. id_monitor) then
        write(*,'(4a)') '!  ', trim(hd_block),                          &
     &       ' should be written to file ... ', trim(file_name)
        call write_control_vol_repart(id_control, hd_block,             &
     &                                viz_repart_c, level)
      else
        write(*,'(3a)') trim(hd_block),                                 &
     &       ' is written to file ... ', trim(file_name)
        call write_file_name_for_ctl_line(id_control, level,            &
     &                                    hd_block, file_name)
        call write_ctl_file_vol_repart((id_control+2), file_name,       &
     &                                 hd_block, viz_repart_c)
      end if
!
      end subroutine sel_write_ctl_file_vol_repart
!
!   --------------------------------------------------------------------
!
      subroutine write_ctl_file_vol_repart(id_control, file_name,       &
     &                                     hd_block, viz_repart_c)
!
      use skip_comment_f
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(viz_repartition_ctl), intent(in) :: viz_repart_c
!
      integer(kind = kint) :: level
!
      level = 0
      open(id_control, file=file_name)
      call write_control_vol_repart                                     &
     &   (id_control, hd_block, viz_repart_c, level)
      close(id_control)
!
      end subroutine write_ctl_file_vol_repart
!
!   --------------------------------------------------------------------
!
      end module ctl_file_volume_repart_IO
