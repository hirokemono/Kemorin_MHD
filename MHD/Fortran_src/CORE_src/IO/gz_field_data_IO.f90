!gz_field_data_IO.f90
!      module gz_field_data_IO
!
!      Written by H. Matsui on Oct., 2007
!
!       zlib and kemo_zlib_io_c are required
!
!      subroutine write_gz_step_data(my_rank)
!      subroutine read_gz_step_data
!
!      subroutine write_gz_field_data(nnod, num_field, ntot_comp,       &
!     &          ncomp_field, field_name, field_data)
!      subroutine read_gz_field_data(nnod, num_field, ntot_comp,        &
!     &          ncomp_field, field_name, field_data)
!
      module gz_field_data_IO
!
      use m_precision
!
      use skip_gz_comment
!
      implicit none
!
      private :: write_gz_field_vect, read_gz_field_vect
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_gz_step_data(my_rank)
!
      use m_time_data_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      write(textbuf,'(a,a1)'   )   '!  domain ID', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(i10,a1)') my_rank, char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)'   )   '!  time step number', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(i10,a1)') i_time_step_IO, char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)'   )   '!  time, Delta t', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(1p2E25.15e3,a1)') time_IO, delta_t_IO, char(0)
      call write_compress_txt(nbuf, textbuf)
!
      end subroutine write_gz_step_data
!
!-------------------------------------------------------------------
!
      subroutine read_gz_step_data
!
      use m_time_data_IO
!
      character(len = kchara) :: tmpchara
      integer(kind = kint) :: itmp
!
!
      call skip_gz_comment_int(itmp)
      call skip_gz_comment_int(i_time_step_IO)
      call skip_gz_comment_chara(tmpchara)
      read(tmpchara,*,err=99, end=99) time_IO, delta_t_IO
!
      go to 10
  99    write(*,*) 'no delta t data... continue'
        delta_t_IO = 0.0d0
  10  continue
!
      end subroutine read_gz_step_data
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine write_gz_field_data(nnod, num_field, ntot_comp,        &
     &          ncomp_field, field_name, field_data)
!
      integer(kind=kint), intent(in)  :: nnod
      integer(kind=kint), intent(in)  :: num_field, ntot_comp
      integer(kind=kint), intent(in)  :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: field_data(nnod,ntot_comp)
!
      integer(kind=kint)  :: i_fld, icou
      character(len=kchara) :: fmt_txt
!
!
      write(textbuf,'(a,a1)'   )                                        &
     &          '! number of field and component', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(2i10,a1)')                                        &
     &           nnod, num_field, char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(fmt_txt,'(a1,i3,a6)') '(', num_field, 'i4,a1)'
      write(textbuf,fmt_txt) ncomp_field(1:num_field), char(0)
      call write_compress_txt(nbuf, textbuf)
!
      icou = 0
      do i_fld = 1, num_field
!
        call write_gz_field_vect(field_name(i_fld),                     &
     &      nnod, ncomp_field(i_fld), field_data(1,icou+1) )
        icou = icou + ncomp_field(i_fld)
      end do
!
      end subroutine write_gz_field_data
!
!------------------------------------------------------------------
!
      subroutine read_gz_field_data(nnod, num_field, ntot_comp,         &
     &          ncomp_field, field_name, field_data)
!
      integer(kind=kint), intent(in)  :: nnod
      integer(kind=kint), intent(in)  :: num_field, ntot_comp
      integer(kind=kint), intent(in)  :: ncomp_field(num_field)
      character(len=kchara), intent(inout) :: field_name(num_field)
      real(kind = kreal), intent(inout) :: field_data(nnod,ntot_comp)
!
      integer(kind=kint)  :: i_fld, icou
!
!
      icou = 0
      do i_fld = 1, num_field
!
        call read_gz_field_vect(field_name(i_fld),                      &
     &      nnod, ncomp_field(i_fld), field_data(1,icou+1) )
        icou = icou + ncomp_field(i_fld)
      end do
!
      end subroutine read_gz_field_data
!
!------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine write_gz_field_vect(d_name, nnod, ndir, vector)
!
      integer(kind=kint), intent(in) :: nnod, ndir
      real(kind = kreal), intent(in) :: vector(nnod,ndir)
      character(len=kchara), intent(in) :: d_name
!
      integer(kind=kint)  :: i
      character(len=kchara) :: fmt_txt
!
!
      write(textbuf,'(a,a1)') trim(d_name), char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(fmt_txt,'(a1,i3,a16)') '(', ndir, '(1pE25.15e3),a1)'
      do i = 1, nnod
        write(textbuf,fmt_txt) vector(i,1:ndir), char(0)
        call write_compress_txt(nbuf, textbuf)
      end do
!
      end subroutine write_gz_field_vect
!
!------------------------------------------------------------------
!
      subroutine read_gz_field_vect(d_name, nnod, ndir, vector)
!
      integer(kind=kint), intent(in) :: nnod, ndir
      character(len=kchara), intent(inout) :: d_name
      real(kind = kreal), intent(inout) :: vector(nnod,ndir)
!
      integer(kind=kint)  :: i, nchara
!
!
      call skip_gz_comment_chara(d_name)
!
      do i = 1, nnod
        call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
        read(textbuf,*)  vector(i,1:ndir)
!        if(mod(i,10000) .eq. 0) write(*,*) 'read ', i
      end do
!
      end subroutine read_gz_field_vect
!
!------------------------------------------------------------------
!
      end module gz_field_data_IO
