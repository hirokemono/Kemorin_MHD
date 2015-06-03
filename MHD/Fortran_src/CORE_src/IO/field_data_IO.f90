!>@file  field_data_IO.f90
!!       module field_data_IO
!!
!!@author H. Matsui
!!@date   Programmed 2007
!!@date   modified in May, 2015
!
!> @brief Data IO rountines for field data IO
!!
!!@verbatim
!!      function field_num_buffer(nnod, num_field)
!!      function field_comp_buffer(num_field, ncomp_field)
!!      function each_field_name_buffer(field_name)
!!      function each_field_data_buffer(ncomp, vect)
!!
!!      subroutine read_field_num_buffer(textbuf, nnod, num_field)
!!      subroutine read_field_comp_buffer                               &
!!     &         (textbuf, num_field, ncomp_field)
!!      integer(kind = kint) function read_each_field_name_buffer       &
!!     &                            (textbuf, field_name)
!!      function read_each_field_name_buffer(ncomp, vect)
!!      subroutine read_each_field_data_buffer(textbuf ncomp, vect)
!!
!!      subroutine write_field_data(id_file, nnod, num_field, ntot_comp,&
!!     &          ncomp_field, field_name, field_data)
!!      subroutine read_field_data(id_file, nnod, num_field, ntot_comp, &
!!     &          ncomp_field, field_name, field_data)
!!
!!      subroutine write_field_data_b(id_file, nnod, num_field,         &
!!     &          ntot_comp, ncomp_field, field_name, field_data)
!!      subroutine read_field_data_b(id_file,                           &
!!     &          nnod, num_field, ntot_comp, field_name, field_data)
!!@endverbatim
!
      module field_data_IO
!
      use m_precision
!
      implicit none
!
      character(len=31), parameter                                      &
     &            :: FLD_HD1 = '! number of field and component'
!
      private :: FLD_HD1
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      function field_num_buffer(nnod, num_field)
!
      integer(kind = kint), intent(in) :: nnod, num_field
      character(len=31+1+2*16+1) :: field_num_buffer
!
      character(len=2*16) :: buf_nfld
!
!
      write(buf_nfld,'(2i16,a1)') nnod, num_field
!
      field_num_buffer = FLD_HD1  // char(10)                           &
     &               //  buf_nfld // char(10)
!
      end function field_num_buffer
!
! -------------------------------------------------------------------
!
      function field_comp_buffer(num_field, ncomp_field)
!
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(in) :: ncomp_field(num_field)
      character(len=num_field*5+1) :: field_comp_buffer
!
      character(len=kchara) :: fmt_txt
!
      write(fmt_txt,'(a1,i5,a8)') '(', num_field, '(i5),a1)'
!
      write(field_comp_buffer,fmt_txt)                                  &
     &            ncomp_field(1:num_field), char(10)
!
      end function field_comp_buffer
!
! -------------------------------------------------------------------
!
      function each_field_name_buffer(field_name)
!
      character(len=kchara), intent(in) :: field_name
!
      character(len=len_trim(field_name)+1) :: each_field_name_buffer
!
!
      each_field_name_buffer = trim(field_name) // char(10)
!
      end function each_field_name_buffer
!
! -------------------------------------------------------------------
!
      function each_field_data_buffer(ncomp, vect)
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in) :: vect(ncomp)
!
      character(ncomp*25+1) :: each_field_data_buffer
!
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a1,i1,a16)') '(', ncomp, '(1pE25.15e3),a1)'
!
      write(each_field_data_buffer,fmt_txt) vect(1:ncomp), char(10)
!
      end function each_field_data_buffer
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine read_field_num_buffer(textbuf, nnod, num_field)
!
      character(len=65), intent(in) :: textbuf
      integer(kind = kint), intent(inout) :: nnod, num_field
!
      character(len=kchara) :: tmpchara(2)
!
!
      read(textbuf,'(a32,a33)') tmpchara(1:2)
      read(tmpchara(2),*) nnod, num_field
!
      end subroutine read_field_num_buffer
!
! -------------------------------------------------------------------
!
      subroutine read_field_comp_buffer                                 &
     &         (textbuf, num_field, ncomp_field)
!
      character(len=*), intent(in) :: textbuf
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(inout) :: ncomp_field(num_field)
!
!
      read(textbuf,*) ncomp_field(1:num_field)
!
      end subroutine read_field_comp_buffer
!
! -------------------------------------------------------------------
!
      integer(kind = kint) function read_each_field_name_buffer         &
     &                            (textbuf, field_name)
!
      character(len=*), intent(in) :: textbuf
      character(len=kchara), intent(inout) :: field_name
!
!
      read(textbuf,*) field_name
      read_each_field_name_buffer = len_trim(field_name)
!
      end function read_each_field_name_buffer
!
! -------------------------------------------------------------------
!
      subroutine read_each_field_data_buffer(textbuf, ncomp, vect)
!
      character(len=*), intent(in) :: textbuf
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(inout) :: vect(ncomp)
!
!
      read(textbuf,*) vect(1:ncomp)
!
      end subroutine read_each_field_data_buffer
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine write_field_data(id_file, nnod, num_field, ntot_comp,  &
     &          ncomp_field, field_name, field_data)
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(in) :: ntot_comp
      integer(kind = kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: field_data(nnod, ntot_comp)
!
      integer(kind = kint) :: i_fld, icou, ist, inod
      character(len=kchara) :: fmt_txt
!
!
      write(id_file,'(a)'   ) FLD_HD1
      write(id_file,'(2i16)') nnod, num_field
      write(id_file,'(10i5)') ncomp_field(1:num_field)
!
      icou = 0
      do i_fld = 1, num_field
        write(id_file,'(a)') trim(field_name(i_fld))
!
        ist = icou + 1
        icou = icou + ncomp_field(i_fld)
        write(fmt_txt,'(a1,i1,a16)')                                    &
     &                   '(', ncomp_field(i_fld), '(1pE25.15e3),a1)'
        do inod = 1, nnod
          write(id_file,fmt_txt) field_data(inod,ist:icou)
        end do
      end do
!
      end subroutine write_field_data
!
! -------------------------------------------------------------------
!
      subroutine read_field_data(id_file, nnod, num_field, ntot_comp,   &
     &          ncomp_field, field_name, field_data)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(in) :: ntot_comp
      integer(kind = kint), intent(in) :: ncomp_field(num_field)
!
      character(len=kchara), intent(inout) :: field_name(num_field)
      real(kind = kreal), intent(inout) :: field_data(nnod, ntot_comp)
!
      character(len=255) :: character_4_read
      integer(kind = kint) :: i_fld, icou, ist, inod
!
!
      icou = 0
      do i_fld = 1, num_field
        call skip_comment(character_4_read,id_file)
        read(character_4_read,*) field_name(i_fld)
!
        ist = icou + 1
        icou = icou + ncomp_field(i_fld)
        do inod = 1, nnod
          read(id_file,*)  field_data(inod,ist:icou)
        end do
      end do
!
      end subroutine read_field_data
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine write_field_data_b(id_file, nnod, num_field,           &
     &          ntot_comp, ncomp_field, field_name, field_data)
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(in) :: ntot_comp
      integer(kind = kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: field_data(nnod, ntot_comp)
!
      integer(kind = kint) :: i
!
!
      write(id_file) nnod, num_field
      write(id_file) ncomp_field(1:num_field)
      write(id_file) field_name(1:num_field)
!
      do i = 1, ntot_comp
        write(id_file) field_data(1:nnod,i)
      end do
!
      end subroutine write_field_data_b
!
! -------------------------------------------------------------------
!
      subroutine read_field_data_b(id_file,                             &
     &          nnod, num_field, ntot_comp, field_name, field_data)
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(in) :: ntot_comp
!
      character(len=kchara), intent(inout) :: field_name(num_field)
      real(kind = kreal), intent(inout) :: field_data(nnod, ntot_comp)
!
      integer(kind = kint) :: i
!
!
      read(id_file) field_name(1:num_field)
!
      do i = 1, ntot_comp
        read(id_file) field_data(1:nnod,i)
      end do
!
      end subroutine read_field_data_b
!
! -------------------------------------------------------------------
!
      end module field_data_IO
