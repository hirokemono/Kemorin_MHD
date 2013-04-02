!m_pickup_sph_rms_data.f90
!      module m_pickup_sph_rms_data
!
!        programmed by H.Matsui on Dec., 2012
!
!      subroutine allocate_pick_sph_rms
!      subroutine deallocate_pick_sph_rms
!
!      subroutine open_sph_rms_4_monitor(my_rank, id_pick)
!      subroutine close_sph_rms_4_monitor(my_rank, id_pick)
!      subroutine write_sph_rms_4_monitor(my_rank, id_pick,             &
!     &          i_step, time)
!
!      subroutine open_sph_rms_read_monitor(id_pick)
!      subroutine read_sph_rms_4_monitor(id_pick, i_step, time, ierr)
!
      module m_pickup_sph_rms_data
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
      character(len = kchara) :: pickup_sph_rms_head =  'picked_ene_spec'
!
      integer(kind = kint) :: num_pick_rms_layer = 0
      integer(kind = kint), allocatable :: id_pick_rms_layer(:)
      real(kind = kreal), allocatable :: r_pick_rms_layer(:)
!
      integer(kind = kint) :: ntot_pick_sph_rms_mode = 0
      integer(kind = kint) :: num_pick_sph_rms_mode =  0
      integer(kind = kint), allocatable :: idx_pick_sph_rms_gl(:)
      integer(kind = kint), allocatable :: idx_pick_sph_rms_lc(:)
!
      integer(kind = kint) :: ncomp_pick_sph_rms =  0
      real(kind = kreal), allocatable :: d_rms_pick_sph_gl(:,:)
      real(kind = kreal), allocatable :: d_rms_pick_sph_lc(:,:)
      character(len=kchara), allocatable :: rms_pick_sph_name(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pick_sph_rms
!
      use m_spheric_parameter
!
      integer(kind = kint) :: num
!
!
      num = ntot_pick_sph_rms_mode*num_pick_rms_layer
!
      allocate( id_pick_rms_layer(num_pick_rms_layer) )
      allocate( r_pick_rms_layer(num_pick_rms_layer) )
!
      allocate( idx_pick_sph_rms_gl(ntot_pick_sph_rms_mode) )
      allocate( idx_pick_sph_rms_lc(ntot_pick_sph_rms_mode) )
      allocate( d_rms_pick_sph_lc(ncomp_pick_sph_rms,num) )
      allocate( d_rms_pick_sph_gl(ncomp_pick_sph_rms,num) )
      allocate( rms_pick_sph_name(ncomp_pick_sph_rms) )
!
      if(num_pick_rms_layer .gt. 0) id_pick_rms_layer = 0
      if(num .gt. 0) then
        idx_pick_sph_rms_gl = -1
        idx_pick_sph_rms_lc =  0
        d_rms_pick_sph_lc = 0.0d0
        d_rms_pick_sph_gl = 0.0d0
      end if
!
      end subroutine allocate_pick_sph_rms
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_pick_sph_rms
!
!
      deallocate(id_pick_rms_layer, r_pick_rms_layer)
      deallocate(idx_pick_sph_rms_gl, d_rms_pick_sph_gl)
      deallocate(idx_pick_sph_rms_lc, d_rms_pick_sph_lc)
!
      end subroutine deallocate_pick_sph_rms
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_sph_rms_4_monitor(my_rank, id_pick)
!
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: my_rank, id_pick
!
      integer(kind = kint) :: i_fld
      character(len = kchara) :: pick_sph_rms_name
!
!
      if(num_pick_sph_rms_mode .eq. izero) return
      if(my_rank .gt. izero) return
!
      call add_dat_extension(pickup_sph_rms_head, pick_sph_rms_name)
      open(id_pick, file = pick_sph_rms_name)
!
!
      write(id_pick,'(a)')
      write(id_pick,'(a)')    '# num_layers, num_spectr'
      write(id_pick,'(2i10)') num_pick_rms_layer, num_pick_sph_rms_mode
      write(id_pick,'(a)')    '# number of component'
      write(id_pick,'(i10)') ncomp_pick_sph_rms
!
!
      write(id_pick,'(a)',advance='NO')    't_step, time, '
      write(id_pick,'(a)',advance='NO')    'radius_ID, radius, '
      write(id_pick,'(a)',advance='NO')    'degree, order, '
!
      do i_fld = 1, ncomp_pick_sph_rms-1
        write(id_pick,'(a,a2)',advance='NO')                            &
     &           trim(rms_pick_sph_name(i_fld)), ', '
      end do
      write(id_pick,'(a)') trim(rms_pick_sph_name(ncomp_pick_sph_rms))
!
      end subroutine open_sph_rms_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine close_sph_rms_4_monitor(my_rank, id_pick)
!
      integer(kind = kint), intent(in) :: my_rank, id_pick
!
!
      if(num_pick_sph_rms_mode .eq. izero) return
      if(my_rank .gt. izero) return
      close(id_pick)
!
      end subroutine close_sph_rms_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_rms_4_monitor(my_rank, id_pick,              &
     &          i_step, time)
!
      integer(kind = kint), intent(in) :: my_rank, id_pick
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint) :: inum, knum, j, l, m, ipick, i_fld
!
!
      if(num_pick_sph_rms_mode .eq. izero) return
      if(my_rank .gt. izero) return
!
      do inum = 1, num_pick_sph_rms_mode
        j = idx_pick_sph_rms_gl(inum)
        l = int( aint(sqrt(dble(j))) )
        m = j - l*(l+1)
        do knum = 1, num_pick_rms_layer
          ipick = knum + (inum-1) * num_pick_rms_layer
          write(id_pick,'(i10,1pe23.14e3)', advance='NO') i_step, time
          write(id_pick,'(i10,1pe23.14e3,2i10)', advance='NO')          &
     &          id_pick_rms_layer(knum), r_pick_rms_layer(knum), l, m
          do i_fld = 1, ncomp_pick_sph_rms
            write(id_pick,'(1pe23.14e3)', advance='NO')                 &
     &              d_rms_pick_sph_gl(i_fld,ipick)
          end do
          write(id_pick,'(a)') ''
        end do
      end do
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
      read(tmpchara,*) num_pick_rms_layer, num_pick_sph_rms_mode
      call skip_comment(tmpchara,id_pick)
      read(tmpchara,*) ncomp_pick_sph_rms
!
      ntot_pick_sph_rms_mode = num_pick_sph_rms_mode
      call allocate_pick_sph_rms
!
      read(id_pick,*) (tmpchara,i=1,6),                                 &
     &                 rms_pick_sph_name(1:ncomp_pick_sph_rms)
!
      end subroutine open_sph_rms_read_monitor
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_rms_4_monitor(id_pick, i_step, time, ierr)
!
      integer(kind = kint), intent(in) :: id_pick
      integer(kind = kint), intent(inout) :: i_step, ierr
      real(kind = kreal), intent(inout) :: time
!
      integer(kind = kint) :: inum, knum, l, m, ipick
!
!
      ierr = 0
      do inum = 1, num_pick_sph_rms_mode
        do knum = 1, num_pick_rms_layer
          ipick = knum + (inum-1) * num_pick_rms_layer
          read(id_pick,*,err=99,end=99) i_step, time,                   &
     &          id_pick_rms_layer(knum), r_pick_rms_layer(knum), l, m,  &
     &          d_rms_pick_sph_gl(1:ncomp_pick_sph_rms,ipick)
          idx_pick_sph_rms_gl(inum) = l*(l+1) + m
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
