!>@file   write_picked_sph_spectr.f90
!!@brief  module write_picked_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine write_each_picked_specr_file                         &
!!     &         (time_d, sph_params, sph_rj, rj_fld, picked)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(picked_spectrum_data), intent(in) :: picked
!!      subroutine open_each_picked_spectr                              &
!!     &         (id_pick, nlayer_ICB, nlayer_CMB, picked, l, m)
!!      logical function error_eack_picked_spectr                       &
!!     &               (id_file, nlayer_ICB, nlayer_CMB, picked, l, m)
!!        integer(kind = kint), intent(in) :: id_pick, l, m
!!        integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!!        type(picked_spectrum_data), intent(in) :: picked
!!
!!@endverbatim
!!
      module write_picked_sph_spectr
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_spheric_parameter
      use t_pickup_sph_spectr_data
      use t_phys_data
      use t_time_data
      use t_buffer_4_gzip
!
      implicit  none
!
      integer(kind = kint), parameter, private :: id_pick = 17
!
      private :: each_picked_mode_file_name
!
! -----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_each_picked_specr_file                           &
     &         (time_d, sph_params, sph_rj, rj_fld, picked)
!
      use pickup_sph_spectr_data
      use sph_monitor_data_text
      use select_gz_stream_file_IO
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
!
      integer(kind = kint) :: inum, knum
      integer(kind = kint_gl) :: num
!
      real(kind=kreal), allocatable :: d_rj_out(:,:)
      type(buffer_4_gzip) :: zbuf_p
      logical :: flag_gzip_p = .FALSE.
!
!
      if(picked%num_sph_mode_lc .le. 0) return
!
      num = picked%istack_picked_spec_lc(my_rank+1)                     &
     &     - picked%istack_picked_spec_lc(my_rank)
      if(num .le. 0) return
!
      allocate(d_rj_out(picked%ntot_comp_rj,picked%num_layer))
!
      if(picked%idx_out(0,4) .gt. 0) then
        call open_each_picked_spectr(flag_gzip_p, id_pick,              &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB, picked,       &
     &      izero, izero, zbuf_p)
        call pick_center_spectrum_monitor                               &
     &     (rj_fld, picked, picked%ntot_comp_rj, d_rj_out(1,1))
        call sel_gz_write_text_stream(flag_gzip_p, id_pick,             &
     &      picked_each_mode_data_text(time_d%i_time_step, time_d%time, &
     &                                 zero, izero, izero, izero,       &
     &                                 picked%ntot_comp_rj,             &
     &                                 d_rj_out(1,1)),                  &
     &      zbuf_p)
        close(id_pick)
      end if
!
      do inum = 1, picked%num_sph_mode_lc
        do knum = 1, picked%num_layer
          call pick_single_sph_spec_4_monitor(inum, knum, sph_rj,       &
     &        rj_fld, picked, picked%ntot_comp_rj, d_rj_out(1,knum))
        end do
!
        call open_each_picked_spectr(flag_gzip_p, id_pick,              &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB, picked,       &
     &      picked%idx_out(inum,1), picked%idx_out(inum,2), zbuf_p)
!
        call sel_gz_write_picked_spec_data(flag_gzip_p, id_pick,        &
     &      time_d, picked, inum, d_rj_out, zbuf_p)
        close(id_pick)
      end do
      deallocate(d_rj_out)
!
      end subroutine write_each_picked_specr_file
!
! -----------------------------------------------------------------------
!
      subroutine open_each_picked_spectr(flag_gzip, id_file,            &
     &          nlayer_ICB, nlayer_CMB, picked, l, m, zbuf)
!
      use write_field_labels
!
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: id_file, l, m
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(picked_spectrum_data), intent(in) :: picked
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      character(len = kchara) :: file_name
!
!
      file_name = each_picked_mode_file_name                            &
     &          (flag_gzip, picked%file_prefix, l, m)
      open(id_file, file=file_name, status='old', position='append',    &
     &     form='unformatted', ACCESS='stream', err = 99)
      return
!
!
   99 continue
      open(id_file, file=file_name, status='replace',                   &
     &     form='unformatted', ACCESS='stream')
!
      call write_each_pick_sph_file_header                              &
     &    (flag_gzip, id_file, nlayer_ICB, nlayer_CMB, picked, zbuf)
!
      end subroutine open_each_picked_spectr
!
! -----------------------------------------------------------------------
!
      logical function error_eack_picked_spectr(flag_gzip, id_file,     &
     &                nlayer_ICB, nlayer_CMB, picked, l, m)
!
      use set_parallel_file_name
      use write_field_labels
      use delete_data_files
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use compare_sph_monitor_header
!
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: id_file, l, m
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(picked_spectrum_data), intent(in) :: picked
!
      logical :: flag_gzip1
      character, pointer :: FPz_fp
      type(buffer_4_gzip) :: zbuf_p
      character(len = kchara) :: file_name
      type(read_sph_spectr_data) :: sph_IN_p, sph_OUT_p
      type(sph_spectr_head_labels) :: sph_lbl_IN_p, sph_lbl_OUT_p
!
!
      error_eack_picked_spectr = .FALSE.
      file_name = each_picked_mode_file_name                            &
     &          (flag_gzip, picked%file_prefix, l, m)
      if(check_file_exist(file_name) .eqv. .FALSE.) go to 99

      call sel_open_read_gz_stream_file(FPz_fp, id_file,                &
     &                                  file_name, flag_gzip1, zbuf_p)
      call s_select_input_picked_sph_head(FPz_fp, id_file, flag_gzip,   &
     &    sph_lbl_IN_p, sph_IN_p, zbuf_p)
      call sel_close_read_gz_stream_file(FPz_fp, id_file,               &
     &                                  flag_gzip, zbuf_p)
!
      call dup_each_pick_sph_file_header(nlayer_ICB, nlayer_CMB,        &
     &                                   picked, sph_OUT_p)
!
      error_eack_picked_spectr                                          &
     &    = .not. cmp_sph_layer_monitor_heads(sph_lbl_IN_p, sph_IN_p,   &
     &                                        pick_spectr_labels,       &
     &                                        sph_OUT_p)
      if(error_eack_picked_spectr) go to 98
      error_eack_picked_spectr                                          &
     &    = .not. cmp_sph_monitor_field_labels(sph_lbl_IN_p, sph_IN_p,  &
     &                                        pick_spectr_labels,       &
     &                                        sph_OUT_p)
!
   98 continue
      call dealloc_sph_espec_data(sph_IN_p)
      call dealloc_sph_espec_data(sph_OUT_p)
!
      return
!
!       Make new file with header
   99 continue
      open(id_file, file=file_name, status='replace',                   &
     &     form='unformatted', ACCESS='stream')
!
      call write_each_pick_sph_file_header                              &
     &   (flag_gzip, id_file, nlayer_ICB, nlayer_CMB, picked, zbuf_p)
      close(id_file)
      error_eack_picked_spectr = .FALSE.
!
      end function error_eack_picked_spectr
!
! -----------------------------------------------------------------------
!
      character(len=kchara) function each_picked_mode_file_name         &
     &                             (flag_gzip, file_prefix, l, m)
!
      use set_parallel_file_name
!
      logical, intent(in) :: flag_gzip
      character(len = kchara), intent(in) :: file_prefix
      integer(kind = kint), intent(in) :: l, m
!
      character(len = kchara) :: file_name, fname_tmp
      integer(kind = kint) :: mm
!
      mm = abs(m)
      write(fname_tmp,'(a,a2)') trim(file_prefix), '_l'
      call add_index_after_name(l, fname_tmp, file_name)
      write(fname_tmp,'(a,a2)') trim(file_name), '_m'
      call add_index_after_name(mm, fname_tmp, file_name)
      if(m .lt. 0) then
        write(fname_tmp,'(a,a1)') trim(file_name), 's'
      else
        write(fname_tmp,'(a,a1)') trim(file_name), 'c'
      end if
!
      file_name = add_dat_extension(fname_tmp)
      if(flag_gzip) then
        each_picked_mode_file_name = add_gzip_extension(file_name)
      else
        each_picked_mode_file_name = file_name
      end if
!
      end function each_picked_mode_file_name
!
! -----------------------------------------------------------------------
!
      subroutine sel_gz_write_picked_spec_data(flag_gzip, id_file,      &
     &          time_d, picked, inum, d_rj_out, zbuf)
!
      use sph_monitor_data_text
      use gzip_defleate
!
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: inum
      type(time_data), intent(in) :: time_d
      type(picked_spectrum_data), intent(in) :: picked
!
      real(kind = kreal), intent(in)                                    &
     &              :: d_rj_out(picked%ntot_comp_rj,picked%num_layer)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: knum, line_len
!
!
      if(flag_gzip) then
        line_len = len(picked_each_mode_data_text                       &
     &                           (time_d%i_time_step, time_d%time,      &
     &                            picked%radius_gl(1),                  &
     &                            picked%id_radius(1),                  &
     &                            picked%idx_out(1,1),                  &
     &                            picked%idx_out(1,2),                  &
     &                            picked%ntot_comp_rj, d_rj_out(1,1)))
        zbuf%ilen_gz = int(dble(picked%num_layer*line_len)*1.01 + 24,   &
     &                     KIND(zbuf%ilen_gz))
        call alloc_zip_buffer(zbuf)
        zbuf%ilen_gzipped = 0
        if(picked%num_layer .eq. 1) then
          knum = 1
          call gzip_defleat_char_once(line_len,                         &
     &        picked_each_mode_data_text                                &
     &                 (time_d%i_time_step, time_d%time,                &
     &                  picked%radius_gl(knum), picked%id_radius(knum), &
     &                  picked%idx_out(inum,1), picked%idx_out(inum,2), &
     &                  picked%ntot_comp_rj, d_rj_out(1,knum)),         &
     &        int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
        else
          knum = 1
          call gzip_defleat_char_begin(line_len,                        &
     &        picked_each_mode_data_text                                &
     &                 (time_d%i_time_step, time_d%time,                &
     &                  picked%radius_gl(knum), picked%id_radius(knum), &
     &                  picked%idx_out(inum,1), picked%idx_out(inum,2), &
     &                  picked%ntot_comp_rj, d_rj_out(1,knum)),         &
     &        int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
          do knum = 2, picked%num_layer - 1
            call gzip_defleat_char_cont(line_len,                       &
     &          picked_each_mode_data_text                              &
     &                 (time_d%i_time_step, time_d%time,                &
     &                  picked%radius_gl(knum), picked%id_radius(knum), &
     &                  picked%idx_out(inum,1), picked%idx_out(inum,2), &
     &                  picked%ntot_comp_rj, d_rj_out(1,knum)),         &
     &          zbuf)
          end do
          knum = picked%num_layer
          call gzip_defleat_char_last(line_len,                         &
     &        picked_each_mode_data_text                                &
     &                             (time_d%i_time_step, time_d%time,    &
     &                              picked%radius_gl(knum),             &
     &                              picked%id_radius(knum),             &
     &                              picked%idx_out(inum,1),             &
     &                              picked%idx_out(inum,2),             &
     &                              picked%ntot_comp_rj,                &
     &                              d_rj_out(1,knum)),                  &
     &        zbuf)
        end if
!
        write(id_file) zbuf%gzip_buf(1:zbuf%ilen_gzipped)
        call dealloc_zip_buffer(zbuf)
        return
      end if
!
      do knum = 1, picked%num_layer
        write(id_file) picked_each_mode_data_text                       &
     &               (time_d%i_time_step, time_d%time,                  &
     &                picked%radius_gl(knum), picked%id_radius(knum),   &
     &                picked%idx_out(inum,1), picked%idx_out(inum,2),   &
     &                picked%ntot_comp_rj, d_rj_out(1,knum))
      end do
!
      end subroutine sel_gz_write_picked_spec_data
!
! -----------------------------------------------------------------------
!
      end module write_picked_sph_spectr
