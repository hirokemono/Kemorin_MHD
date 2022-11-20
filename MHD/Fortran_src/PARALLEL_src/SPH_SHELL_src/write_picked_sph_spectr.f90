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
!!      subroutine open_eack_picked_spectr                              &
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
      use gzip_defleate
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
!
      integer(kind = kint) :: inum, knum, line_len
      integer(kind = kint_gl) :: num
!
      real(kind=kreal), allocatable :: d_rj_out(:,:)
      type(buffer_4_gzip) :: zbuf_p
      logical :: zlib_flag_p = .FALSE.
!
!
      if(picked%num_sph_mode_lc .le. 0) return
!
      num = picked%istack_picked_spec_lc(my_rank+1)                     &
     &     - picked%istack_picked_spec_lc(my_rank)
      if(num .le. 0) return
!
      allocate(d_rj_out(picked%ntot_comp_rj,picked%num_layer))
      line_len = len(picked_each_mode_data_text                         &
     &                           (time_d%i_time_step, time_d%time,      &
     &                            picked%radius_gl(1),                  &
     &                            picked%id_radius(1),                  &
     &                            picked%idx_out(1,1),                  &
     &                            picked%idx_out(1,2),                  &
     &                            picked%ntot_comp_rj, d_rj_out(1,1)))
!
      if(picked%idx_out(0,4) .gt. 0) then
        call open_eack_picked_spectr(zlib_flag_p, id_pick,              &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB, picked,       &
     &      izero, izero, zbuf_p)
        call pick_center_spectrum_monitor                               &
     &     (rj_fld, picked, picked%ntot_comp_rj, d_rj_out(1,1))
        call sel_gz_write_text_buffer(zlib_flag_p, id_pick, line_len,   &
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
        call open_eack_picked_spectr(zlib_flag_p, id_pick,              &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB, picked,         &
     &      picked%idx_out(inum,1), picked%idx_out(inum,2), zbuf_p)
!
        if(zlib_flag_p) then
          zbuf_p%ilen_gz = int(dble(picked%num_layer*line_len)*1.01+24, &
     &                       KIND(zbuf_p%ilen_gz))
          call alloc_zip_buffer(zbuf_p)
          zbuf_p%ilen_gzipped = 0
          if(picked%num_layer .eq. 1) then
            knum = 1
            call gzip_defleat_char_once(line_len,                       &
     &          picked_each_mode_data_text                              &
     &                 (time_d%i_time_step, time_d%time,                &
     &                  picked%radius_gl(knum), picked%id_radius(knum), &
     &                  picked%idx_out(inum,1), picked%idx_out(inum,2), &
     &                  picked%ntot_comp_rj, d_rj_out(1,knum)),         &
     &          int(zbuf_p%ilen_gz), zbuf_p, zbuf_p%gzip_buf(1))
          else
            knum = 1
            call gzip_defleat_char_begin(line_len,                      &
     &          picked_each_mode_data_text                              &
     &                 (time_d%i_time_step, time_d%time,                &
     &                  picked%radius_gl(knum), picked%id_radius(knum), &
     &                  picked%idx_out(inum,1), picked%idx_out(inum,2), &
     &                  picked%ntot_comp_rj, d_rj_out(1,knum)),         &
     &          int(zbuf_p%ilen_gz), zbuf_p, zbuf_p%gzip_buf(1))
            do knum = 2, picked%num_layer - 1
              call gzip_defleat_char_cont(line_len,                     &
     &            picked_each_mode_data_text                            &
     &                 (time_d%i_time_step, time_d%time,                &
     &                  picked%radius_gl(knum), picked%id_radius(knum), &
     &                  picked%idx_out(inum,1), picked%idx_out(inum,2), &
     &                  picked%ntot_comp_rj, d_rj_out(1,knum)),         &
     &            zbuf_p)
            end do
            knum = picked%num_layer
            call gzip_defleat_char_last(line_len,                       &
     &          picked_each_mode_data_text                              &
     &                             (time_d%i_time_step, time_d%time,    &
     &                              picked%radius_gl(knum),             &
     &                              picked%id_radius(knum),             &
     &                              picked%idx_out(inum,1),             &
     &                              picked%idx_out(inum,2),             &
     &                              picked%ntot_comp_rj,                &
     &                              d_rj_out(1,knum)),                  &
     &          zbuf_p)
          end if
!
          write(id_pick) zbuf_p%gzip_buf(zbuf_p%ilen_gzipped)
          call dealloc_zip_buffer(zbuf_p)
        else
          do knum = 1, picked%num_layer
            write(id_pick) picked_each_mode_data_text                   &
     &                 (time_d%i_time_step, time_d%time,                &
     &                  picked%radius_gl(knum), picked%id_radius(knum), &
     &                  picked%idx_out(inum,1), picked%idx_out(inum,2), &
     &                  picked%ntot_comp_rj, d_rj_out(1,knum))
          end do
        end if
        close(id_pick)
      end do
      deallocate(d_rj_out)
!
      end subroutine write_each_picked_specr_file
!
! -----------------------------------------------------------------------
!
      subroutine open_eack_picked_spectr(zlib_flag, id_file,            &
     &          nlayer_ICB, nlayer_CMB, picked, l, m, zbuf)
!
      use write_field_labels
!
      logical, intent(in) :: zlib_flag
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
     &          (zlib_flag, picked%file_prefix, l, m)
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
     &    (zlib_flag, id_file, nlayer_ICB, nlayer_CMB, picked, zbuf)
!
      end subroutine open_eack_picked_spectr
!
! -----------------------------------------------------------------------
!
      logical function error_eack_picked_spectr(zlib_flag, id_file,     &
     &                nlayer_ICB, nlayer_CMB, picked, l, m)
!
      use set_parallel_file_name
      use write_field_labels
!
      logical, intent(in) :: zlib_flag
      integer(kind = kint), intent(in) :: id_file, l, m
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(picked_spectrum_data), intent(in) :: picked
!
      type(buffer_4_gzip) :: zbuf_p
      logical :: zlib_flag_p = .FALSE.
      character(len = kchara) :: file_name
!
!
      error_eack_picked_spectr = .TRUE.
      file_name = each_picked_mode_file_name                            &
     &          (zlib_flag, picked%file_prefix, l, m)
      open(id_file, file=file_name, status='old',                       &
     &     form='unformatted', ACCESS='stream', err = 99)
      error_eack_picked_spectr = .FALSE.
      return
!
!
   99 continue
      open(id_file, file=file_name, status='replace',                   &
     &     form='unformatted', ACCESS='stream')
!
      call write_each_pick_sph_file_header                              &
     &   (zlib_flag_p, id_file, nlayer_ICB, nlayer_CMB, picked, zbuf_p)
      close(id_file)
      error_eack_picked_spectr = .FALSE.
!
      end function error_eack_picked_spectr
!
! -----------------------------------------------------------------------
!
      character(len=kchara) function each_picked_mode_file_name         &
     &                             (zlib_flag, file_prefix, l, m)
!
      use set_parallel_file_name
!
      logical, intent(in) :: zlib_flag
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
      if(zlib_flag) then
        each_picked_mode_file_name = add_gzip_extension(file_name)
      else
        each_picked_mode_file_name = file_name
      end if
!
      end function each_picked_mode_file_name
!
! -----------------------------------------------------------------------
!
      end module write_picked_sph_spectr
