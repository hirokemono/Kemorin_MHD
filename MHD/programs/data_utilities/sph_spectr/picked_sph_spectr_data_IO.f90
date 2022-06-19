!>@file   picked_sph_spectr_data_IO.f90
!!@brief  module picked_sph_spectr_data_IO
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine write_tave_sph_spec_monitor                          &
!!     &         (file_name, i_step, time, true_start, picked_IO)
!!        integer(kind = kint), intent(in) :: n_step
!!        type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!!
!!      subroutine load_picked_sph_spectr_series                        &
!!     &         (flag_log, file_name, start_time, end_time,            &
!!     &          true_start, true_end, picked_IO)
!!        logical, intent(in) :: flag_log
!!        character(len=kchara), intent(in) :: file_name
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!        real(kind = kreal), intent(inout) :: true_start, true_end
!!        type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!!      subroutine read_pick_series_head(id_pick, picked_IO)
!!      subroutine read_sph_spec_monitor                                &
!!     &         (id_pick, i_step, time, picked_IO, ierr)
!!@endverbatim
!!
!!@n @param  id_rank   Process ID
!!@n @param  i_step    time step
!!@n @param  time      time
!!@n @param  id_pick   file ID
!!@n @param  ierr      Error flag (0:success, 1:error)
!
      module picked_sph_spectr_data_IO
!
      use m_precision
      use m_constants
      use t_pickup_sph_spectr_data
      use t_picked_sph_spectr_data_IO
!
      implicit  none
!
!>      File ID for spectrum monitor file
      integer(kind = kint), parameter :: id_pick_mode = 22
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine write_tave_sph_spec_monitor                            &
     &         (file_name, i_step, time, true_start, picked_IO)
!
      use m_monitor_file_labels
      use write_field_labels
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time, true_start
!
      type(picked_spectrum_data_IO), intent(in) :: picked_IO
!
      integer(kind = kint) :: ipick, i_fld, ist
!
!
      if(picked_IO%num_mode .eq. izero) return
!
      open(id_pick_mode, file = file_name, form='formatted',            &
     &     position='append')
!
      write(*,'(a,3i16)') hd_pick_sph_head(), picked_IO%num_layer,      &
     &        picked_IO%num_mode, picked_IO%ntot_pick_spectr
      write(*,'(a,i16)') hd_pick_sph_num(), picked_IO%ntot_comp
!
      write(*,'(a)')  '#   Start and end time'
      write(*,'(1p2e25.12)')  true_Start, time
!
      write(id_pick_mode,'(a)')    '#'
      write(id_pick_mode,'(a)')    '# num_layers, num_spectr'
      write(id_pick_mode,'(3i16)') picked_IO%num_layer,                 &
     &           picked_IO%num_mode, picked_IO%ntot_pick_spectr
      write(id_pick_mode,'(a)')    '# number of component'
      write(id_pick_mode,'(i16)') picked_IO%ntot_comp
!
      write(id_pick_mode,'(a)',advance='NO')  't_step    time    '
      write(id_pick_mode,'(a)',advance='NO')  'radius_ID    radius    '
      write(id_pick_mode,'(a)',advance='NO')  'degree    order    '
!
      call write_multi_labels(id_pick_mode, picked_IO%ntot_comp,        &
     &    picked_IO%spectr_name)
!
      do ipick = 1, picked_IO%ntot_pick_spectr
          ist = (ipick-1) * picked_IO%ntot_comp
          write(id_pick_mode,'(i16,1pe25.14e3)', advance='NO')          &
     &               i_step, time
          write(id_pick_mode,'(i16,1pe25.14e3,2i16)', advance='NO')     &
     &            picked_IO%idx_sph(ipick,1), picked_IO%radius(ipick),  &
     &            picked_IO%idx_sph(ipick,3:4)
          do i_fld = 1, picked_IO%ntot_comp
            write(id_pick_mode,'(1pe25.14e3)', advance='NO')            &
     &            picked_IO%d_pk(ist+i_fld)
          end do
          write(id_pick_mode,'(a)') ''
      end do
!
      close(id_pick_mode)
!
      end subroutine write_tave_sph_spec_monitor
!
! -----------------------------------------------------------------------
!
      subroutine load_picked_sph_spectr_series                          &
     &         (flag_log, file_name, start_time, end_time,              &
     &          true_start, true_end, picked_IO)
!
      logical, intent(in) :: flag_log
      character(len=kchara), intent(in) :: file_name
      real(kind = kreal), intent(in) :: start_time, end_time
!
      real(kind = kreal), intent(inout) :: true_start, true_end
      type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!
      integer(kind = kint) :: i_step, ierr, icou, i
      real(kind = kreal) :: time
!
!
      write(*,*) 'Open file: ', trim(file_name)
      open(id_pick_mode, file = file_name)
      backspace(id_pick_mode)
      read(id_pick_mode,*) i_step, time
      write(*,*) 'End time: ', i_step, time
      rewind(id_pick_mode)
      call read_pick_series_head(id_pick_mode, picked_IO)
      read(id_pick_mode,*) i_step, time
      write(*,*) 'Start time: ', i_step, time
      backspace(id_pick_mode)
!
      icou = 0
      true_start = start_time
      true_end = true_start
      do
        call read_sph_spec_time                                         &
     &     (id_pick_mode, picked_IO, i_step, time, ierr)
        if(ierr .gt. 0) exit
!
        if(time .ge. start_time) then
!          call append_picked_sph_series(i_step, time, picked_IO)
          if(icou .eq. 0) true_start = time
!
          icou = icou + 1
          if(flag_log) then
            write(*,'(69a1,a5,i12,a4,1pe16.8e3,a20,i12)',advance="NO")  &
     &        (char(8),i=1,69), 'step ', i_step,                        &
     &        ' at ', time, ' is read. count is  ', icou
          end if
        end if
!
        if(time .ge. end_time) exit
      end do
      true_end = time
      rewind(id_pick_mode)
      write(*,*)
!
      call dealloc_pick_sph_monitor_IO(picked_IO)
!
      call read_pick_series_head(id_pick_mode, picked_IO)
      call alloc_pick_sph_series(icou, picked_IO)
!
!       Evaluate time average
      icou = 0
      do
        call read_sph_spec_monitor                                      &
     &     (id_pick_mode, i_step, time, picked_IO, ierr)
        if(ierr .gt. 0) exit
!
        if(time .ge. start_time) then
          icou = icou + 1
          call copy_to_pick_sph_series(icou, i_step, time, picked_IO)
!
          if(flag_log) then
            write(*,'(69a1,a5,i12,a4,1pe16.8e3,a20,i12)',advance="NO")  &
     &          (char(8),i=1,69), 'step ', i_step,                      &
     &          ' at ', time, ' is read. count is  ', icou
          end if
        end if
!
        if(time .ge. end_time) exit
      end do
      close(id_pick_mode)
      write(*,*)
!
      write(*,*) 'Saved hermonics mode:'
      do i = 1, picked_IO%ntot_pick_spectr, picked_IO%num_layer
        write(*,*) i, picked_IO%idx_sph(i,3:4)
      end do
      write(*,*) 'Saved radial points:'
      do i = 1, picked_IO%num_layer
        write(*,*) i, picked_IO%idx_sph(i,1), picked_IO%radius(i)
      end do
      write(*,*) 'Saved field names:'
      do i = 1, picked_IO%ntot_comp
        write(*,*) i, trim(picked_IO%spectr_name(i))
      end do
!
      end subroutine load_picked_sph_spectr_series
!
! -----------------------------------------------------------------------
!
      subroutine read_pick_series_head(id_pick, picked_IO)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_pick
      type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!
      integer(kind = kint) :: i
      character(len=kchara) :: tmpchara
!
!
      call skip_comment(tmpchara,id_pick)
      read(tmpchara,*,err=89) picked_IO%num_layer,                      &
     &               picked_IO%num_mode, picked_IO%ntot_pick_spectr
      go to 10
!
  89  continue
         picked_IO%ntot_pick_spectr                                     &
     &      = picked_IO%num_mode * picked_IO%num_layer
  10  continue
!
      call skip_comment(tmpchara,id_pick)
      read(tmpchara,*) picked_IO%ntot_comp
!
      call alloc_pick_sph_monitor_IO(picked_IO)
!
      read(id_pick,*) (tmpchara,i=1,6),                                 &
     &                 picked_IO%spectr_name(1:picked_IO%ntot_comp)
!
      end subroutine read_pick_series_head
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_spec_time                                     &
     &         (id_pick, picked_IO, i_step, time, ierr)
!
      type(picked_spectrum_data_IO), intent(in) :: picked_IO
      integer(kind = kint), intent(in) :: id_pick
      integer(kind = kint), intent(inout) :: i_step, ierr
      real(kind = kreal), intent(inout) :: time
!
      integer(kind = kint) :: ipick
!
!
      ierr = 0
      do ipick = 1, picked_IO%ntot_pick_spectr
        read(id_pick,*,err=99,end=99) i_step, time
      end do
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine read_sph_spec_time
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_spec_monitor                                  &
     &         (id_pick, i_step, time, picked_IO, ierr)
!
      use spherical_harmonics
!
      integer(kind = kint), intent(in) :: id_pick
      integer(kind = kint), intent(inout) :: i_step, ierr
      real(kind = kreal), intent(inout) :: time
!
      type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!
      integer(kind = kint) :: l, m, ipick, ist
!
!
      ierr = 0
      do ipick = 1, picked_IO%ntot_pick_spectr
        ist = (ipick-1) * picked_IO%ntot_comp
        read(id_pick,*,err=99,end=99) i_step, time,                     &
     &     picked_IO%idx_sph(ipick,1), picked_IO%radius(ipick),         &
     &     l, m, picked_IO%d_pk(ist+1:ist+picked_IO%ntot_comp)
        picked_IO%idx_sph(ipick,2) = get_idx_by_full_degree_order(l,m)
        picked_IO%idx_sph(ipick,3) = l
        picked_IO%idx_sph(ipick,4) = m
      end do
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine read_sph_spec_monitor
!
! -----------------------------------------------------------------------
!
      end module picked_sph_spectr_data_IO
