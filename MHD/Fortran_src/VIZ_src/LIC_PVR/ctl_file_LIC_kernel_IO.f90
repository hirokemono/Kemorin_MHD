!>@file   ctl_file_LIC_kernel_IO.f90
!!@brief  module ctl_file_LIC_kernel_IO
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for noise configration for LIC
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine sel_read_LIC_kernel_ctl_file                         &
!!     &         (id_control, hd_block, kernel_ctl, c_buf)
!!      subroutine read_LIC_kernel_control_file(id_control, hd_block,   &
!!     &                                        kernel_ctl)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(lic_kernel_ctl), intent(inout) :: kernel_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine sel_write_LIC_kernel_ctl_file                        &
!!     &         (id_control, hd_block, kernel_ctl, level)
!!      subroutine write_LIC_kernel_control_file(id_control, hd_block,  &
!!     &                                         kernel_ctl)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: hd_block
!!        type(lic_kernel_ctl), intent(in) :: kernel_ctl
!!        integer(kind = kint), intent(inout) :: level
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      List of flags  (Not used currently)
!!    kernel_type:             'gaussian' or 'triangle'
!!    trace_length_mode:       'length'  or  'element_count'
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin kernel_ctl
!!    kernel_type            'Gaussian'
!!
!!    kernel_resolution          256
!!    peak_position_ctl          0.4
!!    gaussian_width_ctl         0.25
!!
!!    trace_length_mode   'length'
!!    half_length_ctl           0.3
!!    max_trace_count             8
!!  end kernel_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module ctl_file_LIC_kernel_IO
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
      use t_control_data_LIC_kernel
      use skip_comment_f
!
      implicit  none
!
!      character(len=kchara) :: hd_kernel =      'kernel_ctl'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sel_read_LIC_kernel_ctl_file                           &
     &         (id_control, hd_block, kernel_ctl, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(lic_kernel_ctl), intent(inout) :: kernel_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_file_flag(c_buf, hd_block)) then
        kernel_ctl%LIC_kernel_ctl_fname = third_word(c_buf)
!
        write(*,'(a)', ADVANCE='NO') ' is read file from ... '
        call read_LIC_kernel_control_file((id_control+2), hd_block,     &
     &                                    kernel_ctl)
      else if(check_begin_flag(c_buf, hd_block)) then
        kernel_ctl%LIC_kernel_ctl_fname = 'NO_FILE'
!
        write(*,*) ' is included'
        call read_kernel_control_data(id_control, hd_block,             &
     &                                kernel_ctl, c_buf)
      end if
!
      end subroutine sel_read_LIC_kernel_ctl_file
!
!   --------------------------------------------------------------------
!
      subroutine read_LIC_kernel_control_file(id_control, hd_block,     &
     &                                        kernel_ctl)
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_block
      type(lic_kernel_ctl), intent(inout) :: kernel_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      if(kernel_ctl%LIC_kernel_ctl_fname .eq. 'NO_FILE') return
!
      write(*,*) 'LIC noise control file: ',                            &
     &          trim(kernel_ctl%LIC_kernel_ctl_fname)
!
      open(id_control, file=kernel_ctl%LIC_kernel_ctl_fname,            &
     &     status='old')
      do
        call load_one_line_from_control(id_control, c_buf1)
        call read_kernel_control_data                                   &
     &     (id_control, hd_block, kernel_ctl, c_buf1)
        if(kernel_ctl%i_kernel_control .gt. 0) exit
      end do
      close(id_control)
!
      end subroutine read_LIC_kernel_control_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sel_write_LIC_kernel_ctl_file                          &
     &         (id_control, hd_block, kernel_ctl, level)
!
      use ctl_file_LIC_noise_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_block
      type(lic_kernel_ctl), intent(in) :: kernel_ctl
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(cmp_no_case(kernel_ctl%LIC_kernel_ctl_fname, 'NO_FILE')) then
        call write_kernel_control_data(id_control, hd_block,            &
     &                                  kernel_ctl, level)
      else
        write(*,'(a)', ADVANCE='NO') ' is write file to ... '
        call write_file_name_for_ctl_line(id_control, level,            &
     &      hd_block, kernel_ctl%LIC_kernel_ctl_fname)
        call write_LIC_kernel_control_file((id_control+2), hd_block,    &
     &                                     kernel_ctl)
      end if
!
      end subroutine sel_write_LIC_kernel_ctl_file
!
!   --------------------------------------------------------------------
!
      subroutine write_LIC_kernel_control_file(id_control, hd_block,    &
     &                                         kernel_ctl)
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_block
      type(lic_kernel_ctl), intent(in) :: kernel_ctl
!
      integer(kind = kint) :: level
!
      level = 0
      write(*,*) 'Write LIC noise control file: ',                      &
     &          trim(kernel_ctl%LIC_kernel_ctl_fname)
      open(id_control, file=kernel_ctl%LIC_kernel_ctl_fname)
      call write_kernel_control_data                                    &
     &     (id_control, hd_block, kernel_ctl, level)
      close(id_control)
!
      end subroutine write_LIC_kernel_control_file
!
!  ---------------------------------------------------------------------
!
      end module ctl_file_LIC_kernel_IO
