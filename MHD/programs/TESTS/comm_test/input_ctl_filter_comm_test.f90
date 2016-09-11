!input_ctl_filter_comm_test.f90
!      module input_ctl_filter_comm_test
!
!     Written by H. Matsui in May, 2008
!
!      subroutine s_input_ctl_filter_comm_test(filtering, wk_filter)
!
      module input_ctl_filter_comm_test
!
      use m_precision
!
      implicit none
!
      private :: set_ctl_param_filter_comm_test
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_input_ctl_filter_comm_test(filtering, wk_filter)
!
      use calypso_mpi
      use t_filtering_data
      use m_machine_parameter
      use m_ctl_data_filter_comm_test
      use m_comm_data_IO
      use m_nod_filter_comm_table
      use m_filter_file_names
!
      use filter_coefs_file_IO
      use set_filter_geometry_4_IO
      use set_comm_table_4_IO
!
      type(filtering_data_type), intent(inout) :: filtering
      type(filtering_work_type), intent(inout) :: wk_filter
      character(len=kchara) :: file_name
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_filter_comm_test'
      call read_control_filter_comm_test
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_4_comm_test'
      call set_ctl_param_filter_comm_test
!
!  --  read filter geometry
!
      if (iflag_debug.eq.1) write(*,*) 'read_filter_geometry_file'
      filter_file_head = filter_3d_head
      call read_filter_geometry_file(file_name, my_rank)
!
      if (iflag_debug.eq.1) write(*,*) 'copy_filter_comm_tbl_from_IO'
      call copy_filtering_geometry_from_IO
      call copy_comm_tbl_type(comm_IO, filtering%comm)
      call deallocate_type_comm_tbl(comm_IO)
!
      call alloc_nod_data_4_filter(nnod_filtering, wk_filter)
!
!
      end subroutine s_input_ctl_filter_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_ctl_param_filter_comm_test
!
      use m_filter_file_names
      use m_file_format_switch
      use m_ctl_data_filter_files
!
!
      if (filter_head_ctl%iflag .gt. 0) then
        filter_3d_head = filter_head_ctl%charavalue
      end if
      ifmt_filter_file = id_ascii_file_fmt
!
      end subroutine set_ctl_param_filter_comm_test
!
!   --------------------------------------------------------------------
!
      end module input_ctl_filter_comm_test
