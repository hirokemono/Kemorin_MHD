!>@file   m_pickup_sph_spectr_data.f90
!!@brief  module m_pickup_sph_spectr_data
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine allocate_num_pick_layer
!!      subroutine allocate_pick_sph_mode
!!      subroutine allocate_pick_sph_l
!!      subroutine allocate_pick_sph_m
!!      subroutine allocate_pick_sph_monitor
!!
!!      subroutine deallocate_num_pick_layer
!!      subroutine deallocate_pick_sph_mode
!!      subroutine deallocate_pick_sph_monitor
!!
!!      subroutine write_sph_spec_4_monitor(my_rank, i_step, time)
!!
!!      subroutine open_sph_spec_read_monitor(id_pick)
!!      subroutine read_sph_spec_4_monitor(id_pick, i_step, time, ierr)
!!@endverbatim
!!
!!@n @param  my_rank   Process ID
!!@n @param  i_step    time step
!!@n @param  time      time
!!@n @param  id_pick   file ID
!!@n @param  ierr      Error flag (0:success, 1:error)
!
      module m_pickup_sph_spectr_data
!
      use m_precision
      use m_constants
      use t_pickup_sph_spectr_data
!
      implicit  none
!
!
!>        Structure for pickup list
      type(pickup_mode_list), save :: pick_list1
!pick_list1%num_modes
!
!>        Structure for pickup list
      type(picked_spectrum_data), save :: pick1
!pick1%ifield_monitor_rj
!
!>      File ID for spectrum monitor file
!      integer(kind = kint), parameter :: id_pick_mode = 22
!>      File prefix for spectr monitoring file
      character(len = kchara) :: pickup_sph_head =  'picked_ene_spec'
!
!
!>      Number of modes of  monitoring spectrum to be evaluated
!      integer(kind = kint) :: num_pick_sph_mode =  0
!>      Global spherical harmonics ID to evaluate  monitoring spectrum
!      integer(kind = kint), allocatable :: idx_pick_sph_gl(:,:)
!>      Local spherical harmonics ID to evaluate  monitoring spectrum
!      integer(kind = kint), allocatable :: idx_pick_sph_lc(:)
!
!>      Number of fields for monitoring output
!!       @f$ f(r,\theta,\phi) @f$
!      integer (kind=kint) ::  num_fld_pick_sph =    0
!>      Total number of component for monitoring spectrum
!      integer(kind = kint) :: ntot_comp_pick_sph =  0
!>      Number of component for monitoring spectrum
!      integer (kind=kint), allocatable :: istack_comp_pick_sph(:)
!>       Field  address for monitoring of @f$ f(r,j) @f$
!      integer (kind=kint), allocatable :: ifield_monitor_rj(:)
!>      monitoring spectrum
!      real(kind = kreal), allocatable :: d_rj_pick_sph_gl(:,:)
!>      Localy evaluated  monitoring spectrum
!      real(kind = kreal), allocatable :: d_rj_pick_sph_lc(:,:)
!>      Scale factor for vector at l=m=0
      real(kind = kreal), allocatable :: scale_for_zelo(:)
!>      Name of  monitoring spectrum
!      character(len=kchara), allocatable :: pick_sph_spec_name(:)
!
      private :: open_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_num_pick_layer
!
!
      call alloc_num_pick_layer(pick1)
!
      end subroutine allocate_num_pick_layer
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pick_sph_mode
!
      call alloc_pick_sph_mode(pick_list1)
!
      end subroutine allocate_pick_sph_mode
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pick_sph_l
!
      call alloc_pick_sph_l(pick_list1)
!
      end subroutine allocate_pick_sph_l
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pick_sph_m
!
      call alloc_pick_sph_m(pick_list1)
!
      end subroutine allocate_pick_sph_m
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pick_sph_monitor
!
!
      call alloc_pick_sph_monitor(pick1)
!
      allocate( scale_for_zelo(pick1%num_sph_mode) )
      if(pick1%num_sph_mode .gt. 0) scale_for_zelo   = 1.0d0
!
      end subroutine allocate_pick_sph_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_num_pick_layer
!
      call dealloc_num_pick_layer(pick1)
!
      end subroutine deallocate_num_pick_layer
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_pick_sph_mode
!
!
      call dealloc_pick_sph_mode(pick_list1)
!
      end subroutine deallocate_pick_sph_mode
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_pick_sph_monitor
!
!
      deallocate(pick1%idx_gl, pick1%d_rj_gl)
      deallocate(pick1%idx_lc, pick1%d_rj_lc, scale_for_zelo)
      deallocate(pick1%spectr_name)
      deallocate(pick1%istack_comp_rj, pick1%ifield_monitor_rj)
!
      end subroutine deallocate_pick_sph_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_sph_spec_4_monitor
!
      use set_parallel_file_name
      use write_field_labels
!
      character(len = kchara) :: pickup_sph_name
!
!
      call add_dat_extension(pickup_sph_head, pickup_sph_name)
      open(id_pick_mode, file = pickup_sph_name, form='formatted',      &
     &    status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_pick_mode, file = pickup_sph_name, form='formatted',      &
     &    status='replace')
!
      write(id_pick_mode,'(a)')
      write(id_pick_mode,'(a)')    '# num_layers, num_spectr'
      write(id_pick_mode,'(2i16)')                                      &
     &         pick1%num_layer, pick1%num_sph_mode
      write(id_pick_mode,'(a)')    '# number of component'
      write(id_pick_mode,'(i16)') pick1%ntot_comp_rj
!
      write(id_pick_mode,'(a)',advance='NO')    't_step    time    '
      write(id_pick_mode,'(a)',advance='NO')    'radius_ID    radius    '
      write(id_pick_mode,'(a)',advance='NO')    'degree    order    '
!
      call write_multi_labels(id_pick_mode, pick1%ntot_comp_rj,         &
     &    pick1%spectr_name)

      write(id_pick_mode,'(a)') ''
!
      end subroutine open_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_sph_spec_4_monitor(my_rank, i_step, time)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint) :: inum, knum, ipick, i_fld
!
!
      if(pick1%num_sph_mode .eq. izero) return
      if(my_rank .gt. izero) return
!
      call open_sph_spec_4_monitor
!
      do inum = 1, pick1%num_sph_mode
        do knum = 1, pick1%num_layer
          ipick = knum + (inum-1) * pick1%num_layer
          write(id_pick_mode,'(i16,1pe23.14e3)', advance='NO')          &
     &               i_step, time
          write(id_pick_mode,'(i16,1pe23.14e3,2i16)', advance='NO')     &
     &          pick1%id_radius(knum), pick1%radius_gl(knum),           &
     &          pick1%idx_gl(inum,2:3)
          do i_fld = 1, pick1%ntot_comp_rj
            write(id_pick_mode,'(1pe23.14e3)', advance='NO')            &
     &              pick1%d_rj_gl(i_fld,ipick)
          end do
          write(id_pick_mode,'(a)') ''
        end do
      end do
!
      close(id_pick_mode)
!
      end subroutine write_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_sph_spec_read_monitor(id_pick)
!
      use set_parallel_file_name
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_pick
      integer(kind = kint) :: i
!
      character(len = kchara) :: pickup_sph_name
      character(len=255) :: tmpchara
!
!
      call add_dat_extension(pickup_sph_head, pickup_sph_name)
      open(id_pick, file = pickup_sph_name)
!
      call skip_comment(tmpchara,id_pick)
      read(tmpchara,*) pick1%num_layer, pick1%num_sph_mode
      call skip_comment(tmpchara,id_pick)
      read(tmpchara,*) pick1%ntot_comp_rj
!
      call allocate_num_pick_layer
      call allocate_pick_sph_monitor
!
      read(id_pick,*) (tmpchara,i=1,6),                                 &
     &                 pick1%spectr_name(1:pick1%ntot_comp_rj)
!
      end subroutine open_sph_spec_read_monitor
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_spec_4_monitor(id_pick, i_step, time, ierr)
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
      do inum = 1, pick1%num_sph_mode
        do knum = 1, pick1%num_layer
          ipick = knum + (inum-1) * pick1%num_layer
          read(id_pick,*,err=99,end=99) i_step, time,                   &
     &          pick1%id_radius(knum), pick1%radius_gl(knum),           &
     &          l, m, pick1%d_rj_gl(1:pick1%ntot_comp_rj,ipick)
          pick1%idx_gl(inum,1) = get_idx_by_full_degree_order(l, m)
          pick1%idx_gl(inum,2) = l
          pick1%idx_gl(inum,3) = m
        end do
      end do
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine read_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
!
      end module m_pickup_sph_spectr_data
