!>@file   t_pickup_sph_spectr_data.f90
!!@brief  module t_pickup_sph_spectr_data
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine alloc_pick_sph_mode(list_pick)
!!      subroutine alloc_pick_sph_l(list_pick)
!!      subroutine alloc_pick_sph_m(list_pick)
!!      subroutine alloc_num_pick_layer(picked)
!!      subroutine alloc_pick_sph_monitor(picked)
!!
!!      subroutine dealloc_pick_sph_mode(list_pick)
!!      subroutine dealloc_num_pick_layer(picked)
!!      subroutine dealloc_pick_sph_monitor(picked)
!!
!!      subroutine write_sph_spec_monitor                               &
!!     &         (my_rank, i_step, time, picked)
!!
!!      subroutine open_sph_spec_read(id_pick, picked)
!!      subroutine read_sph_spec_monitor                                &
!!     &         (id_pick, i_step, time, picked, ierr)
!!@endverbatim
!!
!!@n @param  my_rank   Process ID
!!@n @param  i_step    time step
!!@n @param  time      time
!!@n @param  id_pick   file ID
!!@n @param  ierr      Error flag (0:success, 1:error)
!
      module t_pickup_sph_spectr_data
!
      use m_precision
      use m_constants
!
      implicit  none
!
!>      File ID for spectrum monitor file
      integer(kind = kint), parameter :: id_pick_mode = 22
!
!
!>        Structure for pickup list
      type pickup_mode_list
!>        Number of modes of monitoring spectrum to be evaluated
        integer(kind = kint) :: num_modes = 0
!>        Degree and Order ID of  monitoring spectrum to be evaluated
        integer(kind = kint), pointer :: idx_pick_mode(:,:)
!>        Number of degrees of  monitoring spectrum to be evaluated
        integer(kind = kint) :: num_degree = 0
!>        Degree ID of  monitoring spectrum to be evaluated
        integer(kind = kint), pointer :: idx_pick_l(:)
!>        Number of orders of  monitoring spectrum to be evaluated
        integer(kind = kint) :: num_order = 0
!>        Order ID of  monitoring spectrum to be evaluated
        integer(kind = kint), pointer :: idx_pick_m(:)
      end type pickup_mode_list
!
!
!>        Structure for picked spectr data
      type picked_spectrum_data
!>        File prefix for spectr monitoring file
        character(len = kchara) :: file_prefix =  'picked_ene_spec'
!
!>        Number of radial layer for monitoring spectrum
        integer(kind = kint) :: num_layer = 0
!>        Radial ID for monitoring spectrum
        integer(kind = kint), pointer :: id_radius(:)
!>        Radius for monitoring spectrum
        real(kind = kreal), pointer :: radius_gl(:)
!
!>        Number of modes of  monitoring spectrum to be evaluated
        integer(kind = kint) :: num_sph_mode =  0
!>        Global spherical harmonics ID to evaluate  monitoring spectrum
        integer(kind = kint), pointer :: idx_gl(:,:)
!>        Local spherical harmonics ID to evaluate  monitoring spectrum
        integer(kind = kint), pointer :: idx_lc(:)
!
!>        Number of fields for monitoring output
!!         @f$ f(r,\theta,\phi) @f$
        integer (kind=kint) ::  num_field_rj =  0
!>        Total number of component for monitoring spectrum
        integer(kind = kint) :: ntot_comp_rj =  0
!>        Number of component for monitoring spectrum
        integer (kind=kint), pointer :: istack_comp_rj(:)
!>        Field  address for monitoring of @f$ f(r,j) @f$
        integer (kind=kint), pointer :: ifield_monitor_rj(:)
!>        monitoring spectrum
        real(kind = kreal), pointer :: d_rj_gl(:,:)
!>        Localy evaluated  monitoring spectrum
        real(kind = kreal), pointer :: d_rj_lc(:,:)
!>        Name of  monitoring spectrum
        character(len=kchara), pointer :: spectr_name(:)
      end type picked_spectrum_data
!
      private :: open_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pick_sph_mode(list_pick)
!
      type(pickup_mode_list), intent(inout) :: list_pick
!
!
      allocate(list_pick%idx_pick_mode(list_pick%num_modes,2) )
      if(list_pick%num_modes .gt. 0) list_pick%idx_pick_mode = 0
!
      end subroutine alloc_pick_sph_mode
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pick_sph_l(list_pick)
!
      type(pickup_mode_list), intent(inout) :: list_pick
!
!
      allocate( list_pick%idx_pick_l(list_pick%num_degree) )
      if(list_pick%num_degree .gt. 0) list_pick%idx_pick_l = 0
!
      end subroutine alloc_pick_sph_l
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pick_sph_m(list_pick)
!
      type(pickup_mode_list), intent(inout) :: list_pick
!
!
      allocate( list_pick%idx_pick_m(list_pick%num_order) )
      if(list_pick%num_order .gt. 0) list_pick%idx_pick_m = 0
!
      end subroutine alloc_pick_sph_m
!
! -----------------------------------------------------------------------
!
      subroutine alloc_num_pick_layer(picked)
!
      type(picked_spectrum_data), intent(inout) :: picked
!
!
      allocate( picked%id_radius(picked%num_layer) )
      allocate( picked%radius_gl(picked%num_layer) )
      if(picked%num_layer .gt. 0) then
        picked%id_radius = 0
        picked%radius_gl = 0.0d0
      end if
!
      end subroutine alloc_num_pick_layer
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pick_sph_monitor(picked)
!
      type(picked_spectrum_data), intent(inout) :: picked
!
      integer(kind = kint) :: num
!
!
      num = picked%num_sph_mode*picked%num_layer
!
      allocate( picked%idx_gl(picked%num_sph_mode,3) )
      allocate( picked%idx_lc(picked%num_sph_mode) )
      allocate( picked%d_rj_lc(picked%ntot_comp_rj,num) )
      allocate( picked%d_rj_gl(picked%ntot_comp_rj,num) )
      allocate( picked%spectr_name(picked%ntot_comp_rj) )
      allocate( picked%istack_comp_rj(0:picked%num_field_rj) )
      allocate( picked%ifield_monitor_rj(picked%num_field_rj) )
!
      if(picked%num_field_rj .gt. 0) then
        picked%ifield_monitor_rj = 0
        picked%istack_comp_rj =    0
      end if
      if(num .gt. 0) then
        picked%idx_gl = -1
        picked%idx_lc =  0
        picked%d_rj_lc = 0.0d0
        picked%d_rj_gl = 0.0d0
      end if
!
      end subroutine alloc_pick_sph_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_pick_sph_mode(list_pick)
!
      type(pickup_mode_list), intent(inout) :: list_pick
!
!
      deallocate( list_pick%idx_pick_mode )
      deallocate( list_pick%idx_pick_l, list_pick%idx_pick_m )
!
      end subroutine dealloc_pick_sph_mode
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_num_pick_layer(picked)
!
      type(picked_spectrum_data), intent(inout) :: picked
!
!
      deallocate( picked%id_radius, picked%radius_gl)
!
      end subroutine dealloc_num_pick_layer
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pick_sph_monitor(picked)
!
      type(picked_spectrum_data), intent(inout) :: picked
!
!
      deallocate(picked%idx_gl, picked%d_rj_gl)
      deallocate(picked%idx_lc, picked%d_rj_lc)
      deallocate(picked%spectr_name)
      deallocate(picked%istack_comp_rj, picked%ifield_monitor_rj)
!
      end subroutine dealloc_pick_sph_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_sph_spec_4_monitor(picked)
!
      use set_parallel_file_name
      use write_field_labels
!
      type(picked_spectrum_data), intent(in) :: picked
!
      character(len = kchara) :: pickup_sph_name
!
!
      call add_dat_extension(picked%file_prefix, pickup_sph_name)
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
     &                           picked%num_layer, picked%num_sph_mode
      write(id_pick_mode,'(a)')    '# number of component'
      write(id_pick_mode,'(i16)') picked%ntot_comp_rj
!
      write(id_pick_mode,'(a)',advance='NO')  't_step    time    '
      write(id_pick_mode,'(a)',advance='NO')  'radius_ID    radius    '
      write(id_pick_mode,'(a)',advance='NO')  'degree    order    '
!
      call write_multi_labels(id_pick_mode, picked%ntot_comp_rj,        &
     &    picked%spectr_name)

      write(id_pick_mode,'(a)') ''
!
      end subroutine open_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_sph_spec_monitor                                 &
     &         (my_rank, i_step, time, picked)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
!
      type(picked_spectrum_data), intent(in) :: picked
!
      integer(kind = kint) :: inum, knum, ipick, i_fld
!
!
      if(picked%num_sph_mode .eq. izero) return
      if(my_rank .gt. izero) return
!
      call open_sph_spec_4_monitor(picked)
!
      do inum = 1, picked%num_sph_mode
        do knum = 1, picked%num_layer
          ipick = knum + (inum-1) * picked%num_layer
          write(id_pick_mode,'(i16,1pe23.14e3)', advance='NO')          &
     &               i_step, time
          write(id_pick_mode,'(i16,1pe23.14e3,2i16)', advance='NO')     &
     &               picked%id_radius(knum), picked%radius_gl(knum),    &
     &               picked%idx_gl(inum,2:3)
          do i_fld = 1, picked%ntot_comp_rj
            write(id_pick_mode,'(1pe23.14e3)', advance='NO')            &
     &              picked%d_rj_gl(i_fld,ipick)
          end do
          write(id_pick_mode,'(a)') ''
        end do
      end do
!
      close(id_pick_mode)
!
      end subroutine write_sph_spec_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_sph_spec_read(id_pick, picked)
!
      use set_parallel_file_name
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_pick
      type(picked_spectrum_data), intent(inout) :: picked
!
!
      integer(kind = kint) :: i
!
      character(len = kchara) :: pickup_sph_name
      character(len=255) :: tmpchara
!
!
      call add_dat_extension(picked%file_prefix, pickup_sph_name)
      open(id_pick, file = pickup_sph_name)
!
      call skip_comment(tmpchara,id_pick)
      read(tmpchara,*) picked%num_layer, picked%num_sph_mode
      call skip_comment(tmpchara,id_pick)
      read(tmpchara,*) picked%ntot_comp_rj
!
      picked%num_sph_mode = picked%num_sph_mode
      call alloc_num_pick_layer(picked)
      call alloc_pick_sph_monitor(picked)
!
      read(id_pick,*) (tmpchara,i=1,6),                                 &
     &                 picked%spectr_name(1:picked%ntot_comp_rj)
!
      end subroutine open_sph_spec_read
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_spec_monitor                                  &
     &         (id_pick, i_step, time, picked, ierr)
!
      use spherical_harmonics
!
      integer(kind = kint), intent(in) :: id_pick
      integer(kind = kint), intent(inout) :: i_step, ierr
      real(kind = kreal), intent(inout) :: time
!
      type(picked_spectrum_data), intent(inout) :: picked
!
      integer(kind = kint) :: inum, knum, l, m, ipick
!
!
      ierr = 0
      do inum = 1, picked%num_sph_mode
        do knum = 1, picked%num_layer
          ipick = knum + (inum-1) * picked%num_layer
          read(id_pick,*,err=99,end=99) i_step, time,                   &
     &               picked%id_radius(knum), picked%radius_gl(knum),    &
     &               l, m, picked%d_rj_gl(1:picked%ntot_comp_rj,ipick)
          picked%idx_gl(inum,1) = get_idx_by_full_degree_order(l, m)
          picked%idx_gl(inum,2) = l
          picked%idx_gl(inum,3) = m
        end do
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
      end module t_pickup_sph_spectr_data
