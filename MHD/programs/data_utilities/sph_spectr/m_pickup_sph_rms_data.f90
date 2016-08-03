!m_pickup_sph_rms_data.f90
!      module m_pickup_sph_rms_data
!
!        programmed by H.Matsui on Dec., 2012
!
!>@file   m_pickup_sph_rms_data.f90
!!@brief  module m_pickup_sph_rms_data
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data IO routines for monitoring mean square spectrum data
!!
!!@verbatim
!!      subroutine allocate_pick_sph_rms
!!      subroutine deallocate_pick_sph_rms
!!
!!      subroutine write_sph_rms_4_monitor(my_rank, i_step, time)
!!
!!      subroutine open_sph_rms_read_monitor(id_pick)
!!      subroutine read_sph_rms_4_monitor(id_pick, i_step, time, ierr)
!!@endverbatim
!!
!!@n @param my_rank     Procdess ID
!!@n @param i_step      Time step
!!@n @param time        Time
!!@n @param ierr        Error flag (No error: 0)
!
      module m_pickup_sph_rms_data
!
      use m_precision
      use m_constants
!
      use t_pickup_sph_spectr_data
      use m_pickup_sph_spectr_data
!
      implicit  none
!
!
!>        Structure for pickup list
      type(picked_spectrum_data), save :: pick_rms1
!
      character(len = kchara) :: pickup_sph_rms_head =  'picked_ene_spec'
!
      private :: open_sph_rms_4_monitor
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pick_sph_rms
!
      integer(kind = kint) :: num
!
!
      pick_rms1%num_layer = pick1%num_layer
      call alloc_pick_sph_monitor(pick_rms1)
!
      end subroutine allocate_pick_sph_rms
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_pick_sph_rms
!
!
      deallocate(pick_rms1%idx_gl, pick_rms1%d_rj_gl)
      deallocate(pick_rms1%idx_lc, pick_rms1%d_rj_lc)
!
      end subroutine deallocate_pick_sph_rms
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_sph_rms_4_monitor
!
      use set_parallel_file_name
      use write_field_labels
!
      character(len = kchara) :: pick_sph_rms_name
!
!
      call add_dat_extension(pickup_sph_rms_head, pick_sph_rms_name)
      open(id_pick_mode, file = pick_sph_rms_name,                      &
     &    form='formatted', status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_pick_mode, file = pick_sph_rms_name,                      &
     &    form='formatted', status='replace')
!
      write(id_pick_mode,'(a)')
      write(id_pick_mode,'(a)')    '# num_layers, num_spectr'
      write(id_pick_mode,'(2i16)')                                      &
     &        pick1%num_layer, pick_rms1%num_sph_mode
      write(id_pick_mode,'(a)')    '# number of component'
      write(id_pick_mode,'(i16)') pick_rms1%ntot_comp_rj
!
!
      write(id_pick_mode,'(a)',advance='NO') 't_step    time    '
      write(id_pick_mode,'(a)',advance='NO') 'radius_ID    radius    '
      write(id_pick_mode,'(a)',advance='NO') 'degree    order    '
!
      call write_multi_labels(id_pick_mode, pick_rms1%ntot_comp_rj,     &
     &    pick_rms1%spectr_name)
      write(id_pick_mode,'(a)') ''
!
      end subroutine open_sph_rms_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_rms_4_monitor(my_rank, i_step, time)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint) :: inum, knum, ipick, i_fld
!
!
      if(pick_rms1%num_sph_mode .eq. izero) return
      if(my_rank .gt. izero) return
!
      call open_sph_rms_4_monitor
!
      do inum = 1, pick_rms1%num_sph_mode
        do knum = 1, pick1%num_layer
          ipick = knum + (inum-1) * pick1%num_layer
          write(id_pick_mode,'(i16,1pe23.14e3)', advance='NO')          &
     &           i_step, time
          write(id_pick_mode,'(i16,1pe23.14e3,2i16)', advance='NO')     &
     &          pick1%id_radius(knum), pick1%radius_gl(knum),           &
     &          pick_rms1%idx_gl(inum,2:3)
          do i_fld = 1, pick_rms1%ntot_comp_rj
            write(id_pick_mode,'(1pe23.14e3)', advance='NO')            &
     &              pick_rms1%d_rj_gl(i_fld,ipick)
          end do
          write(id_pick_mode,'(a)') ''
        end do
      end do
!
      close(id_pick_mode)
!
      end subroutine write_sph_rms_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_sph_rms_read_monitor(id_pick)
!
      use skip_comment_f
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: id_pick
!
      integer(kind = kint) :: i
      character(len=255) :: tmpchara
      character(len = kchara) :: pick_sph_rms_name
!
!
      call add_dat_extension(pickup_sph_rms_head, pick_sph_rms_name)
      open(id_pick, file = pick_sph_rms_name)
!
!
      call skip_comment(tmpchara,id_pick)
      read(tmpchara,*) pick1%num_layer, pick_rms1%num_sph_mode
      call skip_comment(tmpchara,id_pick)
      read(tmpchara,*) pick_rms1%ntot_comp_rj
!
      call allocate_pick_sph_rms
!
      read(id_pick,*) (tmpchara,i=1,6),                                 &
     &                 pick_rms1%spectr_name(1:pick_rms1%ntot_comp_rj)
!
      end subroutine open_sph_rms_read_monitor
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_rms_4_monitor(id_pick, i_step, time, ierr)
!
      use spherical_harmonics
!
      integer(kind = kint), intent(in) :: id_pick
      integer(kind = kint), intent(inout) :: i_step, ierr
      real(kind = kreal), intent(inout) :: time
!
      integer(kind = kint) :: inum, knum, l, m, ipick
!
!
      ierr = 0
      do inum = 1, pick_rms1%num_sph_mode
        do knum = 1, pick1%num_layer
          ipick = knum + (inum-1) * pick1%num_layer
          read(id_pick,*,err=99,end=99) i_step, time,                   &
     &          pick1%id_radius(knum), pick1%radius_gl(knum),           &
     &          l, m, pick_rms1%d_rj_gl(1:pick_rms1%ntot_comp_rj,ipick)
          pick_rms1%idx_gl(inum,1)                                      &
     &              = get_idx_by_full_degree_order(l,m)
          pick_rms1%idx_gl(inum,2) = l
          pick_rms1%idx_gl(inum,3) = m
        end do
      end do
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine read_sph_rms_4_monitor
!
! -----------------------------------------------------------------------
!
      end module m_pickup_sph_rms_data
