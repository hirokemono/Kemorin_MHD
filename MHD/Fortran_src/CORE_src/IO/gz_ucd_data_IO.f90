!gz_ucd_data_IO.f90
!      module gz_ucd_data_IO
!
!      Written by H. Matsui on July, 2006
!
!      subroutine write_gz_udt_field_header(num_output,                 &
!     &          ncomp_out, name_out)
!      subroutine write_gz_single_udt_data(nnod_output,                 &
!     &          ncomp_dat, inod_out, dat_out)
!      subroutine write_gz_multi_udt_data(ntot_out, ist, ied,           &
!     &          ncomp_dat, inod_out, dat_out)
!
!      subroutine read_gz_udt_field_num(num_input)
!      subroutine read_gz_udt_field_name(num_input, ncomp_in, name_in)
!      subroutine read_gz_udt_field_header(num_input, ncomp_in, name_in)
!      subroutine read_gz_single_udt_data(nnod_in, ncomp_dat, dat_in)
!
!
!      subroutine write_gz_udt_mesh_header(nnod_output,                 &
!     &          nele_out, ncomp_output)
!      subroutine write_gz_single_grd_connect(nnod_4_ele, nele_out,     &
!     &          iele_gl, ie_out)
!      subroutine write_gz_multi_grd_connect(nnod_4_ele, ntot_ele,      &
!     &          ist, ied, iele_gl, ie_out)
!
      module gz_ucd_data_IO
!
      use m_precision
!
      use m_constants
      use skip_gz_comment
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine write_gz_udt_field_header(num_output,                  &
     &          ncomp_out, name_out)
!
      integer(kind = kint), intent(in) :: num_output
      integer(kind = kint), intent(in) :: ncomp_out(num_output)
      character(len = kchara), intent(in) :: name_out(num_output)
!
      integer(kind = kint) :: j
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a7,i3,a8)')                                       &
     &                    '(i8,a2,', num_output, '(i4),a1)'
!
      write(textbuf,fmt_txt) num_output,'  ', ncomp_out(1:num_output),  &
     &                      char(0)
      call write_compress_txt(nbuf, textbuf)
!
      do j = 1, num_output
        write(textbuf,'(a,a1,a1)') trim(name_out(j)), ",", char(0)
        call write_compress_txt(nbuf, textbuf)
      end do
!
      end subroutine write_gz_udt_field_header
!
! ----------------------------------------------------------------------
!
      subroutine write_gz_single_udt_data(nnod_output,                  &
     &          ncomp_dat, inod_out, dat_out)
!
      integer(kind = kint), intent(in) :: nnod_output, ncomp_dat
      integer(kind = kint), intent(in) :: inod_out(nnod_output)
      real(kind = kreal), intent(in) :: dat_out(nnod_output, ncomp_dat)
!
      call write_gz_multi_udt_data(nnod_output,                         &
     &    ione, nnod_output, ncomp_dat, inod_out, dat_out)
!
      end subroutine  write_gz_single_udt_data
!
! ----------------------------------------------------------------------
!
      subroutine write_gz_multi_udt_data(ntot_out, ist, ied,            &
     &          ncomp_dat, inod_out, dat_out)
!
      integer(kind = kint), intent(in) :: ntot_out, ncomp_dat
      integer(kind = kint), intent(in) :: ist, ied
      integer(kind = kint), intent(in) :: inod_out(ntot_out)
      real(kind = kreal), intent(in) :: dat_out(ntot_out, ncomp_dat)
!
      integer(kind = kint) :: inod
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a5,i3,a16)')                                      &
     &                '(i10,', ncomp_dat, '(1pE25.15e3),a1)'
      do inod = ist, ied
        write(textbuf,fmt_txt)                                          &
     &             inod_out(inod), dat_out(inod,1:ncomp_dat), char(0)
        call write_compress_txt(nbuf, textbuf)
      end do
!
      end subroutine  write_gz_multi_udt_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_gz_udt_field_num(num_input)
!
      integer(kind = kint), intent(inout) :: num_input
!
      integer(kind = kint) :: nchara
!
!
      call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
      read(textbuf,*) num_input
!
      end subroutine read_gz_udt_field_num
!
! ----------------------------------------------------------------------
!
      subroutine read_gz_udt_field_name(num_input, ncomp_in, name_in)
!
      integer(kind = kint), intent(inout) :: num_input
      integer(kind = kint), intent(inout) :: ncomp_in(num_input)
      character(len = kchara), intent(inout) :: name_in(num_input)
!
      integer (kind =kint) :: i, ist
      integer(kind = kint) :: nchara
!
!
      read(textbuf,*) num_input, ncomp_in(1:num_word-1)
!
      if(num_input .gt. num_word-1) then
        ist = num_word-1
        do
          call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
          read(textbuf,*) ncomp_in(ist+1:ist+num_word)
          ist = ist + num_word
          if(ist .gt. num_input) exit
        end do
      end if
!
      do i = 1, num_input
        call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
        read(textbuf,*) name_in(i)
      end do
!
      end subroutine read_gz_udt_field_name
!
! ----------------------------------------------------------------------
!
      subroutine read_gz_udt_field_header(num_input, ncomp_in, name_in)
!
      integer(kind = kint), intent(inout) :: num_input
      integer(kind = kint), intent(inout) :: ncomp_in(num_input)
      character(len = kchara), intent(inout) :: name_in(num_input)
!
      integer(kind = kint) :: nchara
!
!
      call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
      call read_gz_udt_field_name(num_input, ncomp_in, name_in)
!
      end subroutine read_gz_udt_field_header
!
! ----------------------------------------------------------------------
!
      subroutine read_gz_single_udt_data(nnod_in, ncomp_dat, dat_in)
!
      integer(kind = kint), intent(in) :: nnod_in, ncomp_dat
      real(kind = kreal), intent(inout) :: dat_in(nnod_in, ncomp_dat)
!
      integer(kind = kint) :: inod, itmp, ist
      integer(kind = kint) :: nchara
!
!
      do inod = 1, nnod_in
        call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
        read(textbuf,*) itmp, dat_in(inod,1:num_word-1)
!
        if(ncomp_dat .gt. num_word-1) then
          ist = num_word-1
          do
            call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
            read(textbuf,*) dat_in(inod,ist+1:ist+num_word)
            ist = ist + num_word
            if(ist .gt. ncomp_dat) exit
          end do
        end if
      end do
!
      end subroutine read_gz_single_udt_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_gz_udt_mesh_header( nnod_output,                 &
     &          nele_out, ncomp_output)
!
      integer(kind = kint), intent(in) :: nnod_output, nele_out
      integer(kind = kint), intent(in) :: ncomp_output
!
!
      write(textbuf,'(3i10,2i5,a1)') nnod_output, nele_out,             &
     &         ncomp_output, izero, izero, char(0)
      call write_compress_txt(nbuf, textbuf)
!
      end subroutine write_gz_udt_mesh_header
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_gz_single_grd_connect(nnod_4_ele, nele_out,      &
     &          iele_gl, ie_out)
!
      integer(kind = kint), intent(in) :: nele_out, nnod_4_ele
      integer(kind = kint), intent(in) :: iele_gl(nele_out)
      integer(kind = kint), intent(in) :: ie_out(nele_out,nnod_4_ele)
!
!
      call write_gz_multi_grd_connect(nnod_4_ele, nele_out,             &
     &    ione, nele_out, iele_gl, ie_out)
!
      end subroutine  write_gz_single_grd_connect
!
! ----------------------------------------------------------------------
!
      subroutine write_gz_multi_grd_connect(nnod_4_ele, ntot_ele,       &
     &          ist, ied, iele_gl, ie_out)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: ntot_ele, nnod_4_ele
      integer(kind = kint), intent(in) :: ist, ied
      integer(kind = kint), intent(in) :: iele_gl(ntot_ele)
      integer(kind = kint), intent(in) :: ie_out(ntot_ele,nnod_4_ele)
!
      integer(kind = kint) :: iele
      character(len=6) :: eleflag
      character(len=kchara) :: fmt_txt
!
!
      if(nnod_4_ele.eq.num_t_linear)    write(eleflag,'(a6)') '  hex '
      if(nnod_4_ele.eq.num_triangle)    write(eleflag,'(a6)') '  tri '
      if(nnod_4_ele.eq.num_linear_edge) write(eleflag,'(a6)') ' line '
!
      write(fmt_txt,'(a11,i3,a9)')                                      &
     &                '(i10,i3,a6,', nnod_4_ele, '(i10),a1)'
!
      do iele = ist, ied
        write(textbuf,fmt_txt) iele_gl(iele), ione, eleflag,            &
     &                         ie_out(iele,1:nnod_4_ele), char(0)
        call write_compress_txt(nbuf, textbuf)
      end do
!
      end subroutine  write_gz_multi_grd_connect
!
! ----------------------------------------------------------------------
!
      end module gz_ucd_data_IO
