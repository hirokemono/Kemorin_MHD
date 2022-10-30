!>@file   t_append_gauss_coef_file.f90
!!        program t_append_gauss_coef_file
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2020
!!
!!
!> @brief Append mean square data file
!!
!!@verbatim
!!      subroutine append_sph_mean_sq_file(flag_spectr, flag_vol_ave,   &
!!     &          append_file_name, target_file_name)
!!        logical, intent(in) :: flag_spectr, flag_vol_ave
!!        character(len=kchara), intent(in) :: append_file_name
!!        character(len=kchara), intent(in) :: target_file_name
!!@endverbatim
      module t_append_gauss_coef_file
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_read_sph_spectra
      use t_pick_copy_monitor_data
      use t_buffer_4_gzip
!
      implicit none
!
      private :: pick_copy_sph_pwr_data_to_end
      private :: sel_num_sph_mean_sq_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine append_gauss_coef_file                                 &
     &         (append_file_name, target_file_name)
!
      use t_pick_copy_monitor_data
      use t_gauss_coefs_monitor_IO
      use select_gz_stream_file_IO
      use gz_gauss_coefs_monitor_IO
      use gz_spl_sph_spectr_data_IO
!
      implicit none
!
      character(len=kchara), intent(in) :: append_file_name
      character(len=kchara), intent(in) :: target_file_name
!
!
      type(picked_gauss_coefs_IO), save :: gauss_IN1
      type(picked_gauss_coefs_IO), save :: gauss_OUT1
!
      integer(kind = kint), parameter :: id_append_file = 15
      integer(kind = kint), parameter :: id_write_file = 16
!
      integer(kind = kint) :: istep_start
      real(kind = kreal) :: start_time
!
      integer(kind = kint) :: nd
!
      logical :: flag_gzip1
      type(buffer_4_gzip), save :: zbuf1
      character, pointer, save  :: FPz_f1
!
!
      write(*,*) 'Open file to append.'
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_append_file, append_file_name, flag_gzip1, zbuf1)
      call read_gauss_coefs_header(FPz_f1, id_append_file,              &
     &                             flag_gzip1, gauss_IN1, zbuf1)
      call alloc_gauss_coef_monitor(gauss_IN1)
      call read_gauss_coefs_labels(FPz_f1, id_append_file,              &
     &                             flag_gzip1, gauss_IN1, zbuf1)
!
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f1, id_append_file, flag_gzip1, zbuf1)
      read(zbuf1%fixbuf(1),*) istep_start, start_time
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_append_file, flag_gzip1, zbuf1)
!
!
      write(*,*) 'Open file to write', ': ', trim(target_file_name)
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_write_file, target_file_name, flag_gzip1, zbuf1)
      call read_gauss_coefs_header(FPz_f1, id_write_file,               &
     &                             flag_gzip1, gauss_OUT1, zbuf1)
      call alloc_gauss_coef_monitor(gauss_OUT1)
      call read_gauss_coefs_labels(FPz_f1, id_write_file,               &
     &                             flag_gzip1, gauss_OUT1, zbuf1)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_write_file, flag_gzip1, zbuf1)
!
      if(gauss_IN1%radius_gauss .ne. gauss_OUT1%radius_gauss) then
        write(*,*) 'Radius does not match',                             &
     &     gauss_IN1%radius_gauss, gauss_OUT1%radius_gauss
        stop
      end if
!
      call init_pick_copy_sph_pwr_list                                  &
     &   (gauss_IN1%num_mode, gauss_OUT1%num_mode,                      &
     &    gauss_IN1%gauss_coef_name, gauss_OUT1%gauss_coef_name,        &
     &    comp_tbl1)
!
      call open_bwd_serch_to_append(target_file_name, id_write_file,    &
     &    istep_start, start_time, ione)

      write(*,*) 'Start Append'
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_append_file, append_file_name, flag_gzip1, zbuf1)
      call read_gauss_coefs_header(FPz_f1, id_append_file,              &
     &                             flag_gzip1, gauss_IN1, zbuf1)
      call read_gauss_coefs_labels(FPz_f1, id_append_file,              &
     &                             flag_gzip1, gauss_IN1, zbuf1)
!
      if(comp_tbl1%fast_flag) then
        write(*,*) 'Copy data as text'
        call copy_sph_monitor_to_end                                    &
     &     (FPz_f1, id_append_file, flag_gzip1,                         &
     &      id_write_file, ione, zbuf1)
      else
        do
          call read_gauss_coefs_4_monitor(FPz_f, id_stream, flag_gzip,  &
     &        i_step, time, gauss_IN1, zbuf, ierr)
          if(ierr .gt. 0) exit
!
          call pick_copy_monitor_data                                   &
     &       (comp_tbl, gauss_IN1%num_mode, gauss_OUT1%num_mode,        &
     &        ione, gauss_IN1%gauss_coef(1), gauss_OUT1%gauss_coef(1))
!
          write(id_write_file,'(i16,1pe23.15e3)', advance='NO')         &
     &              i_step, time
          do inum = 1, gauss_OUT1%num_mode
            write(id_write_file,'(1pe23.15e3)', advance='NO')           &
     &       gauss_OUT1%gauss_coef(inum)
          end do
          write(id_gauss_coef,'(a)') ''
        end do
      end if
!
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_append_file, flag_gzip1, zbuf1)
      close(id_write_file)
!
      call dealloc_gauss_coef_monitor(gauss_IN1)
      call dealloc_gauss_coef_monitor(gauss_OUT1)
!
      end subroutine append_gauss_coef_file
!
! -----------------------------------------------------------------------
!
      end module t_append_gauss_coef_file
