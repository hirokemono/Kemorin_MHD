!>@file  gz_ucd_data_IO.f90
!!       module gz_ucd_data_IO
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in Feb., 2007
!
!> @brief Output routine for gzipped UCD data segments
!!
!!@verbatim
!!      subroutine write_gz_udt_field_header(num_output,                &
!!     &          ncomp_out, name_out)
!!      subroutine write_gz_ucd_field_data(ntot_nod, ncomp_dat, nnod,   &
!!     &          inod_out, dat_out)
!!
!!      subroutine read_gz_udt_field_num(num_input)
!!      subroutine read_gz_udt_field_name(num_input, ncomp_in, name_in)
!!      subroutine read_gz_udt_field_header(num_input, ncomp_in, name_in)
!!      subroutine read_gz_udt_field_data(nnod_in, ncomp_dat, dat_in)
!!      subroutine read_gz_udt_mesh_header(nnod_input, nele_in,         &
!!     &          ncomptot_in)
!!      subroutine read_gz_ucd_mesh_connect(nele_in, nnod_4_ele,        &
!!     &          iele_gl, ie_out)
!!
!!      subroutine write_gz_udt_mesh_header(nnod_output,                &
!!     &          nele_out, ncomp_output)
!!      subroutine write_gz_ucd_mesh_connect(ntot_ele, nnod_4_ele,      &
!!     &          nele, iele_gl, ie_out)
!!@endverbatim
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
      subroutine write_gz_ucd_field_data(ntot_nod, ncomp_dat, nnod,     &
     &          inod_out, dat_out)
!
      integer(kind = kint), intent(in) :: ntot_nod, ncomp_dat
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: inod_out(ntot_nod)
      real(kind = kreal), intent(in) :: dat_out(ntot_nod, ncomp_dat)
!
      integer(kind = kint) :: inod
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a5,i3,a16)')                                      &
     &                '(i10,', ncomp_dat, '(1pE25.15e3),a1)'
      do inod = 1, nnod
        write(textbuf,fmt_txt)                                          &
     &             inod_out(inod), dat_out(inod,1:ncomp_dat), char(0)
        call write_compress_txt(nbuf, textbuf)
      end do
!
      end subroutine  write_gz_ucd_field_data
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
      subroutine read_gz_udt_field_data(nnod_in, ncomp_dat, dat_in)
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
      end subroutine read_gz_udt_field_data
!
! ----------------------------------------------------------------------
!
      subroutine read_gz_udt_mesh_header(nnod_input, nele_in,           &
     &          ncomptot_in)
!
      integer(kind = kint), intent(inout) :: nnod_input, nele_in
      integer(kind = kint), intent(inout) :: ncomptot_in
!
      integer(kind = kint) :: nchara
      integer(kind = kint) :: itmp
!
!
      call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
      read(textbuf,*) nnod_input, nele_in, ncomptot_in, itmp, itmp
!
      end subroutine read_gz_udt_mesh_header
!
! ----------------------------------------------------------------------
!
      subroutine read_gz_ucd_mesh_connect(nele_in, nnod_4_ele,         &
     &          iele_gl, ie_out)
!
      integer(kind = kint), intent(in) :: nele_in, nnod_4_ele
      integer(kind = kint), intent(inout) :: iele_gl(nele_in)
      integer(kind = kint), intent(inout) :: ie_out(nele_in,nnod_4_ele)
!
      integer(kind = kint) :: nchara
      integer(kind = kint) :: iele
      integer(kind = kint) :: itmp
      character(len=kchara) :: tmpchara
!
!
      do iele = 1, nele_in
        call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
        read(textbuf,*) iele_gl(iele), itmp, tmpchara,                  &
     &                  ie_out(iele,1:nnod_4_ele)
      end do
!
      end subroutine  read_gz_ucd_mesh_connect
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
      subroutine write_gz_ucd_mesh_connect(ntot_ele, nnod_4_ele,        &
     &          nele, iele_gl, ie_out)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: ntot_ele, nnod_4_ele, nele
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
      do iele = 1, nele
        write(textbuf,fmt_txt) iele_gl(iele), ione, eleflag,            &
     &                         ie_out(iele,1:nnod_4_ele), char(0)
        call write_compress_txt(nbuf, textbuf)
      end do
!
      end subroutine  write_gz_ucd_mesh_connect
!
! ----------------------------------------------------------------------
!
      end module gz_ucd_data_IO
