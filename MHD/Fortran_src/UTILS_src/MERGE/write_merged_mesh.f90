!
!      module write_merged_mesh
!
!      Written by H. Matsui on Jan., 2007
!
!      subroutine s_write_merged_mesh
!
      module write_merged_mesh
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_write_merged_mesh
!
      use m_constants
      use m_control_param_merge
      use m_comm_data_IO
      use m_read_mesh_data
      use copy_merge_mesh_2_IO
      use mesh_IO_select
      use set_parallel_file_name
!
!
      my_rank_IO = izero
      call s_copy_merge_mesh_2_IO
!
      mesh_file_head = new_mesh_head
      iflag_mesh_file_fmt = inew_mesh_file_fmt
      call sel_write_mesh_file(izero)
!
      end subroutine s_write_merged_mesh
!
! ----------------------------------------------------------------------
!
      end module write_merged_mesh
