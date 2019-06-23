!>@file   read_ctl_data_gen_table.f90
!!@brief  module read_ctl_data_gen_table
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief Structure for reading parameters to generate interpolate table
!!
!!@verbatim
!!      subroutine read_control_4_gen_itp_table(gtbl_ctl)
!!      subroutine read_control_4_interpolate(gtbl_ctl)
!!        type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!!@endverbatim
!
      module read_ctl_data_gen_table
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_gen_table
!
      implicit  none
!
!
      integer(kind = kint), parameter :: table_ctl_file_code = 11
!
      character(len = kchara), parameter                                &
     &                 :: fname_table_ctl = "ctl_gen_table"
      character(len = kchara), parameter                                &
     &                 :: fname_itp_ctl = "ctl_interpolate"
!
!     Top level
!
      character(len=kchara), parameter :: hd_table_control              &
     &                   = 'construct_table'
      private :: hd_table_control
!
      private :: bcast_const_itp_tbl_ctl_data
      private :: bcast_itp_files_ctl, bcast_itaration_model_ctl
      private :: bcast_itaration_param_ctl, bcast_element_hash_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_gen_itp_table(gtbl_ctl)
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open(table_ctl_file_code, file=fname_table_ctl, status='old')
        do
          call load_one_line_from_control(table_ctl_file_code, c_buf1)
          call read_const_itp_tbl_ctl_data                              &
     &       (table_ctl_file_code, hd_table_control, gtbl_ctl, c_buf1)
          if(gtbl_ctl%i_table_control .gt. 0) exit
        end do
        close(table_ctl_file_code)
      end if
!
      call bcast_const_itp_tbl_ctl_data(gtbl_ctl)
!
      end subroutine read_control_4_gen_itp_table
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_interpolate(gtbl_ctl)
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open(table_ctl_file_code, file=fname_itp_ctl, status='old')
        do
          call load_one_line_from_control(table_ctl_file_code, c_buf1)
          call read_const_itp_tbl_ctl_data                              &
     &       (table_ctl_file_code, hd_table_control, gtbl_ctl, c_buf1)
          if(gtbl_ctl%i_table_control .gt. 0) exit
        end do
        close(table_ctl_file_code)
      end if
!
      call bcast_const_itp_tbl_ctl_data(gtbl_ctl)
!
      end subroutine read_control_4_interpolate
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_const_itp_tbl_ctl_data(gtbl_ctl)
!
      use bcast_control_arrays
      use bcast_4_platform_ctl
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
!
      call bcast_ctl_data_4_platform(gtbl_ctl%src_plt)
      call bcast_ctl_data_4_platform(gtbl_ctl%dst_plt)
!
      call bcast_itp_files_ctl(gtbl_ctl)
      call bcast_itaration_model_ctl(gtbl_ctl)
      call bcast_itaration_param_ctl(gtbl_ctl)
      call bcast_element_hash_ctl(gtbl_ctl)
!
      call MPI_BCAST(gtbl_ctl%i_table_control, 1,                       &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_const_itp_tbl_ctl_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_control_dist_itp_data(gtbl_ctl)
!
      use bcast_4_platform_ctl
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
!
      call bcast_ctl_data_4_platform(gtbl_ctl%src_plt)
      call bcast_ctl_data_4_platform(gtbl_ctl%dst_plt)
      call bcast_itp_files_ctl(gtbl_ctl)
!
      call MPI_BCAST(gtbl_ctl%i_distribute_itp, 1,                      &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_control_dist_itp_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_itp_files_ctl(gtbl_ctl)
!
      use bcast_control_arrays
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
!
      call bcast_ctl_type_c1(gtbl_ctl%table_head_ctl)
      call bcast_ctl_type_c1(gtbl_ctl%fmt_itp_table_file_ctl)
      call bcast_ctl_type_c1(gtbl_ctl%itp_node_head_ctl)
      call bcast_ctl_type_c1(gtbl_ctl%reverse_element_table_ctl)
      call bcast_ctl_type_c1(gtbl_ctl%single_itp_tbl_head_ctl)
!
      call MPI_BCAST(gtbl_ctl%i_itp_files, 1,                           &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_itp_files_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_itaration_model_ctl(gtbl_ctl)
!
      use bcast_4_field_ctl
      use bcast_4_time_step_ctl
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
!
      call bcast_phys_data_ctl(gtbl_ctl%fld_gt_ctl)
      call bcast_ctl_data_4_time_step(gtbl_ctl%t_gt_ctl)
      call MPI_BCAST(gtbl_ctl%i_itp_model, 1,                           &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_itaration_model_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_itaration_param_ctl(gtbl_ctl)
!
      use bcast_control_arrays
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
!
      call bcast_ctl_array_ir(gtbl_ctl%eps_4_itp_ctl)
      call bcast_ctl_type_i1(gtbl_ctl%itr_refine_ctl)
      call bcast_ctl_type_r1(gtbl_ctl%eps_refine_ctl)
!
      call MPI_BCAST(gtbl_ctl%i_iteration_ctl, 1,                       &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_itaration_param_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_element_hash_ctl(gtbl_ctl)
!
      use bcast_control_arrays
!
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!
!
      call bcast_ctl_type_c1(gtbl_ctl%ele_hash_type_ctl)
!
      call bcast_ctl_type_i1(gtbl_ctl%num_radial_divide_ctl)
      call bcast_ctl_type_i1(gtbl_ctl%num_theta_divide_ctl)
      call bcast_ctl_type_i1(gtbl_ctl%num_phi_divide_ctl)
      call bcast_ctl_type_i1(gtbl_ctl%num_x_divide_ctl)
      call bcast_ctl_type_i1(gtbl_ctl%num_y_divide_ctl)
      call bcast_ctl_type_i1(gtbl_ctl%num_z_divide_ctl)
!
      call MPI_BCAST(gtbl_ctl%i_element_hash, 1,                        &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_element_hash_ctl
!
!   --------------------------------------------------------------------
!
      end module read_ctl_data_gen_table
