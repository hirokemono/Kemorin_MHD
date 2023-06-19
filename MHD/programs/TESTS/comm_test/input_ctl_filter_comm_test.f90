!input_ctl_filter_comm_test.f90
!      module input_ctl_filter_comm_test
!
!     Written by H. Matsui in May, 2008
!
!!      subroutine s_input_ctl_filter_comm_test                         &
!!     &         (filtering, filter_node, wk_filter)
!!       type(filtering_data_type), intent(inout) :: filtering
!!       type(filtering_work_type), intent(inout) :: wk_filter
!!       type(node_data), intent(inout) :: filter_node
!
      module input_ctl_filter_comm_test
!
      use m_precision
      use calypso_mpi
!
      implicit none
!
      character(len = kchara), parameter, private                       &
     &               :: fname_test_mesh_ctl = "ctl_filter_comm_test"
!
      private :: set_ctl_param_filter_comm_test
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_input_ctl_filter_comm_test                           &
     &         (filtering, filter_node, wk_filter)
!
      use t_geometry_data
      use t_filter_file_data
      use t_filtering_data
      use t_ctl_data_filter_comm_test
      use t_read_control_elements
      use m_machine_parameter
      use m_filter_file_names
!
      use filter_coefs_file_IO
      use copy_mesh_structures
!
      type(filtering_data_type), intent(inout) :: filtering
      type(filtering_work_type), intent(inout) :: wk_filter
      type(node_data), intent(inout) :: filter_node
!
      type(ctl_data_filter_comm_test) :: fc_test_ctl
      type(filter_file_data) :: filter_IO_t
      character(len=kchara) :: file_name
      integer(kind = kint) :: ierr
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      if (iflag_debug.eq.1) write(*,*) 'read_control_filter_comm_test'
      if(my_rank .eq. 0) then
        call read_control_filter_comm_test(fname_test_mesh_ctl,         &
     &                                     fc_test_ctl, c_buf1)
      end if
      call bcast_filter_comm_test_data(fc_test_ctl)
!
      if(c_buf1%iend .gt. 0) then
        call calypso_MPI_abort(fc_test_ctl%i_filter_test_ctl,           &
     &                             'control file is broken')
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_4_comm_test'
      call set_ctl_param_filter_comm_test(fc_test_ctl%ffile_ctest_ctl)
!
!  --  read filter geometry
!
      if (iflag_debug.eq.1) write(*,*) 'read_filter_geometry_file'
      filter_file_head = filter_3d_head
      call read_filter_geometry_file                                    &
     &   (file_name, my_rank, filter_IO_t, ierr)
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr, 'Filter geometry data is wrong!!')
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'copy_filter_comm_tbl_from_IO'
      call copy_node_geometry(filter_IO_t%node, filter_node)
      call copy_comm_tbl_type(filter_IO_t%nod_comm, filtering%comm)
      call dealloc_filter_geometry_data(filter_IO_t)
!
      call alloc_nod_data_4_filter(filter_node%numnod, wk_filter)
!
!
      end subroutine s_input_ctl_filter_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_ctl_param_filter_comm_test(ffile_ctl)
!
      use m_filter_file_names
      use m_file_format_switch
      use t_ctl_data_filter_files
!
      type(filter_file_control), intent(inout) :: ffile_ctl
!
!
      if (ffile_ctl%filter_head_ctl%iflag .gt. 0) then
        filter_3d_head = ffile_ctl%filter_head_ctl%charavalue
      end if
      ifmt_filter_file = id_ascii_file_fmt
!
      end subroutine set_ctl_param_filter_comm_test
!
!   --------------------------------------------------------------------
!
      subroutine bcast_filter_comm_test_data(fc_test_ctl)
!
      use t_ctl_data_filter_comm_test
      use calypso_mpi_int
      use bcast_4_platform_ctl
      use bcast_4_filter_files_ctl
!
      type(ctl_data_filter_comm_test), intent(inout) :: fc_test_ctl
!
!
      call bcast_ctl_data_4_platform(fc_test_ctl%f_comm_plt)
      call bcast_filter_fnames_control(fc_test_ctl%ffile_ctest_ctl)
!
      call calypso_mpi_bcast_one_int(fc_test_ctl%i_filter_test_ctl, 0)
!
      end subroutine bcast_filter_comm_test_data
!
!   --------------------------------------------------------------------
!
      end module input_ctl_filter_comm_test
