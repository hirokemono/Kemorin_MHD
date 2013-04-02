!m_gauss_coefs_monitor_data.f90
!      module m_gauss_coefs_monitor_data
!
!        programmed by H.Matsui on Dec., 2012
!
!      subroutine allocate_pick_gauss
!      subroutine allocate_pick_gauss_l
!      subroutine allocate_pick_gauss_m
!      subroutine allocate_gauss_coef_monitor
!
!      subroutine deallocate_pick_gauss
!      subroutine deallocate_gauss_coef_monitor
!
!      subroutine open_gauss_coefs_4_monitor(my_rank, id_pick)
!      subroutine close_gauss_coefs_4_monitor(my_rank, id_pick)
!      subroutine write_gauss_coefs_4_monitor(my_rank, id_pick,         &
!     &          i_step, time)
!
!      subroutine open_gauss_coefs_read_monitor(id_pick)
!      subroutine read_gauss_coefs_4_monitor(id_pick, i_step, time,     &
!     &          ierr)
!
      module m_gauss_coefs_monitor_data
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
      character(len = kchara) :: gauss_coefs_file_head
      character(len = kchara) :: gauss_coefs_file_name
!
      integer(kind = kint) :: num_pick_gauss_coefs = 0
      integer(kind = kint), allocatable :: idx_pick_gauss_mode(:)
      integer(kind = kint) :: num_pick_gauss_l = 0
      integer(kind = kint), allocatable :: idx_pick_gauss_l(:)
      integer(kind = kint) :: num_pick_gauss_m = 0
      integer(kind = kint), allocatable :: idx_pick_gauss_m(:)
!
      integer(kind = kint) :: ntot_pick_gauss_mode
      integer(kind = kint) :: num_pick_gauss_mode
      integer(kind = kint), allocatable :: idx_pick_gauss_coef_gl(:)
      integer(kind = kint), allocatable :: idx_pick_gauss_coef_lc(:)
      real(kind = kreal), allocatable :: gauss_coef_gl(:)
      real(kind = kreal), allocatable :: gauss_coef_lc(:)
      character(len=kchara), allocatable :: gauss_mode_name(:)
!
      real(kind = kreal) :: r_4_gauss_coefs = 2.91
      real(kind = kreal) :: rcmb_to_Re = 3.5d3 / 6.4d3
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pick_gauss
!
      allocate( idx_pick_gauss_mode(num_pick_gauss_coefs) )
      if(num_pick_gauss_coefs .gt. 0) idx_pick_gauss_mode = -1
!
      end subroutine allocate_pick_gauss
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pick_gauss_l
!
      allocate( idx_pick_gauss_l(num_pick_gauss_l) )
      if(num_pick_gauss_l .gt. 0) idx_pick_gauss_l = -1
!
      end subroutine allocate_pick_gauss_l
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pick_gauss_m
!
      allocate( idx_pick_gauss_m(num_pick_gauss_m) )
      if(num_pick_gauss_m .gt. 0) idx_pick_gauss_m = -1
!
      end subroutine allocate_pick_gauss_m
!
! -----------------------------------------------------------------------
!
      subroutine allocate_gauss_coef_monitor
!
!
      allocate( idx_pick_gauss_coef_gl(ntot_pick_gauss_mode) )
      allocate( idx_pick_gauss_coef_lc(ntot_pick_gauss_mode) )
      allocate( gauss_coef_lc(ntot_pick_gauss_mode) )
      allocate( gauss_coef_gl(ntot_pick_gauss_mode) )
      allocate( gauss_mode_name(ntot_pick_gauss_mode) )
!
      if(ntot_pick_gauss_mode .gt. 0) then
        idx_pick_gauss_coef_gl = -1
        idx_pick_gauss_coef_lc =  0
        gauss_coef_lc = 0.0d0
        gauss_coef_gl = 0.0d0
      end if
!
      end subroutine allocate_gauss_coef_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_pick_gauss
!
      deallocate( idx_pick_gauss_mode )
      deallocate( idx_pick_gauss_l, idx_pick_gauss_m )
!
      end subroutine deallocate_pick_gauss
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_gauss_coef_monitor
!
!
      deallocate(idx_pick_gauss_coef_gl, gauss_coef_gl)
      deallocate(idx_pick_gauss_coef_lc, gauss_coef_lc)
      deallocate(gauss_mode_name)
!
      end subroutine deallocate_gauss_coef_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_gauss_coefs_4_monitor(my_rank, id_pick)
!
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: my_rank, id_pick
!
      integer(kind = kint) :: inum
!
!
      if(num_pick_gauss_mode .eq. izero) return
      if(my_rank .gt. izero) return
!
      call add_dat_extension(gauss_coefs_file_head,                     &
     &    gauss_coefs_file_name)
      open(id_pick, file = gauss_coefs_file_name)
!
      write(id_pick,'(a)')    'num_spectr, reference_radius'
      write(id_pick,'(i10,1pe25.15e3)')                                 &
     &     num_pick_gauss_mode, r_4_gauss_coefs
!
      write(id_pick,'(a)',advance='NO')    't_step, time, '
!
      do inum = 1, num_pick_gauss_mode-1
        write(id_pick,'(a,a2)',advance='no')                            &
     &            trim(gauss_mode_name(inum)), ', '
      end do
      write(id_pick,'(a)')                                              &
     &            trim(gauss_mode_name(num_pick_gauss_mode))
!
      end subroutine open_gauss_coefs_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine close_gauss_coefs_4_monitor(my_rank, id_pick)
!
      integer(kind = kint), intent(in) :: my_rank, id_pick
!
!
      if(num_pick_gauss_mode .eq. izero) return
      if(my_rank .gt. izero) return
      close(id_pick)
!
      end subroutine close_gauss_coefs_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine write_gauss_coefs_4_monitor(my_rank, id_pick,          &
     &          i_step, time)
!
      integer(kind = kint), intent(in) :: my_rank, id_pick
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint) :: inum
!
!
      if(num_pick_gauss_mode .eq. izero) return
      if(my_rank .gt. izero) return
!
      write(id_pick,'(i10,1pe23.14e3)', advance='NO') i_step, time
      do inum = 1, num_pick_gauss_mode
        write(id_pick,'(1pe23.14e3)', advance='NO') gauss_coef_gl(inum)
      end do
      write(id_pick,'(a)') ''
!
      end subroutine write_gauss_coefs_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_gauss_coefs_read_monitor(id_pick)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_pick
!
      integer(kind = kint) :: i
      character(len=255) :: tmpchara
!
!
      open(id_pick, file = gauss_coefs_file_name)
!
      call skip_comment(tmpchara,id_pick)
      read(tmpchara,*) num_pick_gauss_mode
!
      ntot_pick_gauss_mode = num_pick_gauss_mode
      call allocate_gauss_coef_monitor
!
      read(id_pick,*) (tmpchara,i=1,2),                                 &
     &                 gauss_mode_name(1:num_pick_gauss_mode)
!
      end subroutine open_gauss_coefs_read_monitor
!
! -----------------------------------------------------------------------
!
      subroutine read_gauss_coefs_4_monitor(id_pick, i_step, time,      &
     &          ierr)
!
      integer(kind = kint), intent(in) :: id_pick
      integer(kind = kint), intent(inout) :: i_step, ierr
      real(kind = kreal), intent(inout) :: time
!
!
      ierr = 0
      read(id_pick,*,err=99,end=99) i_step, time,                       &
     &       gauss_coef_gl(1:num_pick_gauss_mode)
!
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine read_gauss_coefs_4_monitor
!
! -----------------------------------------------------------------------
!
      end module m_gauss_coefs_monitor_data
