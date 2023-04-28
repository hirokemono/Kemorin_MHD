!>@file   read_ctl_data_flines.f90
!!@brief  module read_ctl_data_flines
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for cross sections
!!
!!@verbatim
!!      subroutine read_files_4_fline_ctl                               &
!!     &         (id_control, hd_block, fline_ctls, c_buf)
!!      subroutine sel_read_fline_control                               &
!!     &         (id_control, hd_block, fline_ctl_struct, c_buf)
!!      subroutine read_fline_control_file(id_control,                  &
!!     &                                   hd_block, fline_ctl_struct)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(fieldline_controls), intent(inout) :: fline_ctls
!!        type(fline_ctl), intent(inout)  :: fline_ctl_struct
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      subroutine write_files_4_fline_ctl                              &
!!     &         (id_control, hd_block, fline_ctls, level)
!!      subroutine sel_write_fline_control                              &
!!     &         (id_control, hd_block, fline_ctl_struct, level)
!!      subroutine write_fline_control_file(id_control,                 &
!!     &                                    hd_block, fline_ctl_struct)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(fieldline_controls), intent(in) :: fline_ctls
!!        type(fline_ctl), intent(in)  :: fline_ctl_struct
!!        integer(kind = kint), intent(inout) :: level
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array  fieldline
!!      file  fieldline  'ctl_fline_magne'
!!    end array fieldline
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module read_ctl_data_flines
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use t_ctl_data_4_fline
      use t_control_data_flines
!
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_files_4_fline_ctl                                 &
     &         (id_control, hd_block, fline_ctls, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
      use read_field_line_ctl
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(fieldline_controls), intent(inout) :: fline_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_array_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(allocated(fline_ctls%fline_ctl_struct)) return
      fline_ctls%num_fline_ctl = 0
      call alloc_fline_ctl_struct(fline_ctls)
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_file_flag(c_buf, hd_block)                             &
     &        .or. check_begin_flag(c_buf, hd_block)) then
          call append_new_fline_control(fline_ctls)
!
          write(*,'(2a,i4,a)', ADVANCE='NO')                            &
     &        trim(hd_block), ' No. ', fline_ctls%num_fline_ctl, '... '
          call sel_read_fline_control(id_control, hd_block,             &
     &        fline_ctls%fline_ctl_struct(fline_ctls%num_fline_ctl),    &
     &        c_buf)
        end if
      end do
!
      end subroutine read_files_4_fline_ctl
!
!   --------------------------------------------------------------------
!
      subroutine sel_read_fline_control                                 &
     &         (id_control, hd_block, fline_ctl_struct, c_buf)
!
      use read_field_line_ctl
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(fline_ctl), intent(inout)  :: fline_ctl_struct
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_file_flag(c_buf, hd_block)) then
        fline_ctl_struct%fline_ctl_fname = third_word(c_buf)
!
        write(*,'(3a,i4,a)', ADVANCE='NO') 'is read from '
        call read_fline_control_file((id_control+2), hd_block,          &
     &      fline_ctl_struct)
      else if(check_begin_flag(c_buf, hd_block)) then
        fline_ctl_struct%fline_ctl_fname = 'NO_FILE'
!
        write(*,*) ' is included'
        call s_read_field_line_ctl(id_control, hd_block,                &
     &                             fline_ctl_struct, c_buf)
      end if
!
      end subroutine sel_read_fline_control
!
!  ---------------------------------------------------------------------
!
      subroutine read_fline_control_file(id_control,                    &
     &                                   hd_block, fline_ctl_struct)
!
      use read_field_line_ctl
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(fline_ctl), intent(inout)  :: fline_ctl_struct
!
      type(buffer_for_control) :: c_buf1
!
!
      write(*,*) 'Control file: ',                                      &
     &          trim(fline_ctl_struct%fline_ctl_fname)
      call reset_fline_control_flags(fline_ctl_struct)
      open(id_control, file=fline_ctl_struct%fline_ctl_fname,           &
     &     status='old')
!
      do
        call load_one_line_from_control(id_control, c_buf1)
        call s_read_field_line_ctl(id_control, hd_block,                &
     &      fline_ctl_struct, c_buf1)
        if(fline_ctl_struct%i_vr_fline_ctl .gt. 0) exit
      end do
      close(id_control)
!
      end subroutine read_fline_control_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_files_4_fline_ctl                                &
     &         (id_control, hd_block, fline_ctls, level)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(fieldline_controls), intent(in) :: fline_ctls
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      do i = 1, fline_ctls%num_fline_ctl
        write(*,'(2a,i4,a)', ADVANCE='NO')                              &
     &        trim(hd_block), ' No. ', fline_ctls%num_fline_ctl, '... '
        call sel_write_fline_control(id_control, hd_block,              &
     &      fline_ctls%fline_ctl_struct(i), level)
      end do
!
      end subroutine write_files_4_fline_ctl
!
!   --------------------------------------------------------------------
!
      subroutine sel_write_fline_control                                &
     &         (id_control, hd_block, fline_ctl_struct, level)
!
      use read_field_line_ctl
      use write_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(fline_ctl), intent(in)  :: fline_ctl_struct
      integer(kind = kint), intent(inout) :: level
!
!
      if(cmp_no_case(fline_ctl_struct%fline_ctl_fname, 'NO_FILE')) then
        write(*,*) ' is included'
        call write_field_line_ctl(id_control, hd_block,                 &
     &                            fline_ctl_struct, level)
      else
        write(*,'(3a,i4,a)', ADVANCE='NO') 'is write to '
        call write_file_name_for_ctl_line(id_control, level,            &
     &      hd_block, fline_ctl_struct%fline_ctl_fname)
        call write_fline_control_file((id_control+2), hd_block,         &
     &      fline_ctl_struct)
      end if
!
      end subroutine sel_write_fline_control
!
!  ---------------------------------------------------------------------
!
      subroutine write_fline_control_file(id_control,                   &
     &                                    hd_block, fline_ctl_struct)
!
      use read_field_line_ctl
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(fline_ctl), intent(in)  :: fline_ctl_struct
!
      integer(kind = kint) :: level
!
!
      level = 0
      write(*,*) 'write fieldline control file: ',                      &
     &          trim(fline_ctl_struct%fline_ctl_fname)
      open(id_control, file=fline_ctl_struct%fline_ctl_fname)
      call write_field_line_ctl(id_control, hd_block,                   &
     &                          fline_ctl_struct, level)
      close(id_control)
!
      end subroutine write_fline_control_file
!
!  ---------------------------------------------------------------------
!
      end module read_ctl_data_flines
