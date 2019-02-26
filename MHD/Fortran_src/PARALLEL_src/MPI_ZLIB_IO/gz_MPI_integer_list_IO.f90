!>@file  gz_MPI_integer_list_IO.f90
!!       module gz_MPI_integer_list_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief  Data IO routines for spectrum data
!!
!!@verbatim
!!      subroutine gz_mpi_read_ele_connect                              &
!!     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!!      subroutine gz_mpi_read_element_type                             &
!!     &         (IO_param, ncolumn, num, int_dat)
!!      subroutine gz_mpi_read_int_list(IO_param, nele, ncomp, ivect)
!!
!!      subroutine gz_mpi_write_ele_connect                             &
!!     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!!      subroutine gz_mpi_write_element_type                            &
!!     &         (IO_param, ncolumn, num, int_dat)
!!      subroutine gz_mpi_write_int_list(IO_param, nele, ncomp, ivect)
!!@endverbatim
!
      module gz_MPI_integer_list_IO
!
      use m_precision
!
      use t_calypso_mpi_IO_param
      use gz_MPI_ascii_data_IO
      use gz_MPI_domain_data_IO
      use data_IO_to_textline
!
      implicit none
!
      integer(kind = kint), parameter, private :: maxline = 1000
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_ele_connect                               &
     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nele, nnod_4_ele
      integer(kind=kint_gl), intent(inout) :: id_global(nele)
      integer(kind=kint), intent(inout) :: ie(nele, nnod_4_ele)
!
      integer(kind = kint_gl) :: ie_tmp(nnod_4_ele)
      integer(kind = kint) :: i, ist
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: ilen_gz, ilen_gzipped, ilen_tmp
      integer(kind = kint) :: ilen_line, ilen_used, ilen_in
      integer(kind = kint) :: nline
!
      character(len=1), allocatable :: gzip_buf(:)
      character(len=1), allocatable :: textbuf(:)
!
!
      call read_int8_stack_textline                                     &
         (gz_mpi_read_charahead(IO_param,                               &
     &      len_multi_int_textline(IO_param%nprocs_in)),                &
     &    IO_param%nprocs_in, IO_param%istack_merged)
!
      ilen_gz = IO_param%istack_merged(IO_param%id_rank+1)              &
     &         - IO_param%istack_merged(IO_param%id_rank)
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(ilen_gz .le. 0) return
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
!
      ilen_line = len_int8_and_mul_int8_textline(nnod_4_ele)
      allocate(textbuf(ilen_line))
      allocate(gzip_buf(ilen_gz))
      call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset,          &
     &    int(ilen_gz), gzip_buf(1))
!
      if(nele .le. 0) then
        ilen_in = int(ilen_gz)
        call gzip_infleat_once                                          &
     &    (ilen_in, gzip_buf(1), ione, textbuf(1), ilen_used)
        ilen_gzipped = ilen_used
      else if(nele .eq. 1) then
        ilen_in = int(ilen_gz)
        call gzip_infleat_once                                          &
     &    (ilen_in, gzip_buf(1), ilen_line, textbuf(1), ilen_used)
        call read_int8_and_mul_int8_textline                            &
     &     (textbuf(1), id_global(1), nnod_4_ele, ie_tmp)
        ie(1,1:nnod_4_ele) = ie_tmp(1:nnod_4_ele)
        ilen_gzipped = ilen_used
      else if(nele .gt. 0) then
        ist = 0
        ilen_gzipped = 0
        ilen_tmp = dble(maxline*ilen_line) * 1.01 + 24
!        if(my_rank .eq. 0) write(*,*) 'all start ',                    &
!     &      nele, ilen_line, ilen_gz, ilen_tmp
        do
          nline = int(min((nele - ist), maxline))
          ilen_in = int(min(ilen_gz-ilen_gzipped, ilen_tmp))
!
!          if(my_rank .eq. 0) write(*,*) 'start ',                      &
!     &      ist+1, ist+nline, nline, ilen_gzipped+1,  ilen_in
          call gzip_infleat_begin(ilen_in, gzip_buf(ilen_gzipped+1),    &
     &        ilen_line, textbuf(1), ilen_used)
          call read_int8_and_mul_int8_textline                          &
     &       (textbuf(1), id_global(ist+1), nnod_4_ele, ie_tmp)
          ie(ist+1,1:nnod_4_ele) = ie_tmp(1:nnod_4_ele)
!          if(my_rank .eq. 0) write(*,*) 'gzip_defleat_begin', ilen_used
!
          do i = ist+2, ist+nline-1
            call gzip_infleat_cont                                      &
     &         (ilen_in, ilen_line, textbuf(1), ilen_used)
            call read_int8_and_mul_int8_textline                        &
     &         (textbuf(1), id_global(i), nnod_4_ele, ie_tmp)
            ie(i,1:nnod_4_ele) = ie_tmp(1:nnod_4_ele)
          end do
!          if(my_rank .eq. 0) write(*,*) 'gzip_defleat_cont', ilen_used
!
          call gzip_infleat_last                                        &
     &       (ilen_in, ilen_line, textbuf(1), ilen_used)
          call read_int8_and_mul_int8_textline                          &
     &       (textbuf(1), id_global(ist+nline), nnod_4_ele, ie_tmp)
          ie(ist+nline,1:nnod_4_ele) = ie_tmp(1:nnod_4_ele)
!          if(my_rank .eq. 0) write(*,*) 'gzip_defleat_last',           &
!     &        ilen_used, ist + nline, nele
!
          ioffset = ioffset + nline * ilen_line
          ilen_gzipped = ilen_gzipped + ilen_used
          ist = ist + nline
          if(ist .ge. nele) exit
        end do
!        if(my_rank .eq. 0) write(*,*) 'all done ', ilen_gzipped
      end if
!
      deallocate(gzip_buf, textbuf)
!
      end subroutine gz_mpi_read_ele_connect
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_element_type                               &
     &         (IO_param, ncolumn, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: ncolumn
      integer(kind=kint), intent(in) :: num
      integer(kind=kint), intent(inout) :: int_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: ilen_gz
      character(len=1), allocatable :: gzip_buf(:)
!
!
      call read_int8_stack_textline                                     &
         (gz_mpi_read_charahead(IO_param,                               &
     &      len_multi_int_textline(IO_param%nprocs_in)),                &
     &    IO_param%nprocs_in, IO_param%istack_merged)
!
      ilen_gz = IO_param%istack_merged(IO_param%id_rank+1)              &
     &         - IO_param%istack_merged(IO_param%id_rank)
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(ilen_gz .le. 0) return
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
!
      allocate(gzip_buf(ilen_gz))
      call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset,          &
     &    int(ilen_gz), gzip_buf(1))
!
      call infleate_element_type                                        &
     &   (ncolumn, ilen_gz, gzip_buf, num, int_dat)
!
      deallocate(gzip_buf)
!
      end subroutine gz_mpi_read_element_type
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_int_list(IO_param, nele, ncomp, ivect)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nele, ncomp
      integer(kind=kint), intent(inout) :: ivect(nele, ncomp)
!
      integer(kind = kint) :: ie_tmp(ncomp)
      integer(kind = kint) :: i
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_line, ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
      character(len=1), allocatable :: textbuf(:)
!
!
      call read_int8_stack_textline                                     &
         (gz_mpi_read_charahead(IO_param,                               &
     &      len_multi_int_textline(IO_param%nprocs_in)),                &
     &    IO_param%nprocs_in, IO_param%istack_merged)
!
      ilen_gz = int(IO_param%istack_merged(IO_param%id_rank+1)          &
     &            - IO_param%istack_merged(IO_param%id_rank))
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(ilen_gz .le. 0) return
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
!
      ilen_line = len_multi_int_textline(ncomp)
      allocate(textbuf(ilen_line))
      allocate(gzip_buf(ilen_gz))
      call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset,          &
     &   ilen_gz, gzip_buf(1))
!
      if(nele .le. 0) then
        call gzip_infleat_once                                          &
     &    (ilen_gz, gzip_buf(1), ione, textbuf(1), ilen_gzipped)
      else if(nele .eq. 1) then
        call gzip_infleat_once                                          &
     &    (ilen_gz, gzip_buf(1), ilen_line, textbuf(1), ilen_gzipped)
        call read_multi_int_textline(textbuf(1), ncomp, ivect(1,1))
      else if(nele .gt. 0) then
        call gzip_infleat_begin                                         &
     &   (ilen_gz, gzip_buf(1), ilen_line, textbuf(1), ilen_gzipped)
        call read_multi_int_textline(textbuf(1), ncomp, ie_tmp)
        ivect(1,1:ncomp) = ie_tmp(1:ncomp)
!
        do i = 2, nele-1
          call gzip_infleat_cont                                        &
     &       (ilen_gz, ilen_line, textbuf(1), ilen_gzipped)
          call read_multi_int_textline(textbuf(1), ncomp, ie_tmp)
          ivect(i,1:ncomp) = ie_tmp(1:ncomp)
        end do
!
        call gzip_infleat_last                                          &
     &     (ilen_gz, ilen_line, textbuf(1), ilen_gzipped)
        call read_multi_int_textline(textbuf(1), ncomp, ie_tmp)
        ivect(nele,1:ncomp) = ie_tmp(1:ncomp)
      end if
!
      deallocate(gzip_buf, textbuf)
!
      end subroutine gz_mpi_read_int_list
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_ele_connect                               &
     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nele
      integer(kind=kint), intent(in) :: nnod_4_ele
      integer(kind=kint_gl), intent(in) :: id_global(nele)
      integer(kind=kint), intent(in) :: ie(nele,nnod_4_ele)
!
      integer(kind = kint) :: i, ist
      integer(kind = kint_gl) :: ie_tmp(nnod_4_ele)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: ilen_gz, ilen_gzipped, ilen_tmp
      integer(kind = kint) :: ilen_line, ilen_used, ilen_in
      integer(kind = kint) :: nline
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      ilen_line = len_int8_and_mul_int8_textline(nnod_4_ele)
      ilen_gz = dble(nele*ilen_line) *1.01 + 24
      allocate(gzip_buf(ilen_gz))
!
      if(nele .le. 0) then
        ilen_in = int(ilen_gz)
        call gzip_defleat_once(ione, char(10),                          &
     &      ilen_in, ilen_used, gzip_buf(1))
        ilen_gzipped = ilen_used
      else if(nele .eq. 1) then
        ilen_in = int(ilen_gz)
        ie_tmp(1:nnod_4_ele) = ie(1,1:nnod_4_ele)
        call gzip_defleat_once(ilen_line,                               &
     &      int8_and_mul_int8_textline                                  &
     &         (id_global(1), nnod_4_ele, ie_tmp),                      &
     &      ilen_in, ilen_used, gzip_buf(1))
        ilen_gzipped = ilen_used
      else if(nele .gt. 0) then
        ist = 0
        ilen_gzipped = 0
        ilen_tmp = dble(maxline*ilen_line) * 1.01 + 24
!        if(my_rank .eq. 0) write(*,*) 'all start ',                    &
!     &      nele, ilen_line, ilen_gz, ilen_tmp
        do
          nline = int(min((nele - ist), maxline))
          ilen_in = int(min(ilen_gz-ilen_gzipped, ilen_tmp))
!
!          if(my_rank .eq. 0) write(*,*) 'start ',                      &
!     &      ist+1, ist+nline, nline, ilen_gzipped+1,  ilen_in
          ie_tmp(1:nnod_4_ele) = ie(ist+1,1:nnod_4_ele)
          call gzip_defleat_begin(ilen_line,                            &
     &        int8_and_mul_int8_textline                                &
     &             (id_global(ist+1), nnod_4_ele, ie_tmp),              &
     &        ilen_in, ilen_used, gzip_buf(ilen_gzipped+1))
!          if(my_rank .eq. 0) write(*,*) 'gzip_defleat_begin', ilen_used
!
          do i = ist+2, ist+nline-1
            ie_tmp(1:nnod_4_ele) = ie(i,1:nnod_4_ele)
            call gzip_defleat_cont(ilen_line,                           &
     &          int8_and_mul_int8_textline                              &
     &              (id_global(i), nnod_4_ele, ie_tmp),                 &
     &          ilen_in, ilen_used)
          end do
!          if(my_rank .eq. 0) write(*,*) 'gzip_defleat_cont', ilen_used
!
          ie_tmp(1:nnod_4_ele) = ie(ist+nline,1:nnod_4_ele)
          call gzip_defleat_last(ilen_line,                             &
     &        int8_and_mul_int8_textline                                &
     &             (id_global(ist+nline), nnod_4_ele, ie_tmp),          &
     &        ilen_in, ilen_used)
!          if(my_rank .eq. 0) write(*,*) 'gzip_defleat_last',           &
!     &        ilen_used, ist + nline, nele
!
          ioffset = ioffset + nline * ilen_line
          ilen_gzipped = ilen_gzipped + ilen_used
          ist = ist + nline
          if(ist .ge. nele) exit
        end do
!        if(my_rank .eq. 0) write(*,*) 'all done ', ilen_gzipped
      end if
!
      call gz_mpi_write_stack_over_domain(IO_param, int(ilen_gzipped))
!
      if(ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &      int(ilen_gzipped), gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine gz_mpi_write_ele_connect
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_element_type                              &
     &         (IO_param, ncolumn, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: i, ist, nrest
      integer(kind = kint) :: nitem_1, nitem_2, nitem_c
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: ilen_gz, ilen_gzipped, ilen_tmp
      integer(kind = kint) :: ilen_line, ilen_used, ilen_in
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      ilen_gz = int(real(num*len_6digit_txt) *1.01) + 24
      allocate(gzip_buf(ilen_gz))
!
      if(num .le. 0) then
        ilen_in = int(ilen_gz)
        call gzip_defleat_once(ione, char(10),                          &
     &      ilen_in, ilen_used, gzip_buf(1))
        ilen_gzipped = ilen_used
      else
        ist = 0
        ilen_gzipped = 0
        ilen_line = len_multi_6digit_line(ncolumn)
        ilen_tmp = dble(maxline*ilen_line) * 1.01 + 24
        if(my_rank .eq. 0) write(*,*)                                   &
     &     'gz_mpi_write_element_type start ',                          &
     &      num, ilen_line, ilen_gz, ilen_tmp
!
        do
          nitem_1 = num-ist
          nitem_2 = min(num-ist,ncolumn*maxline)
          nitem_c = nitem_2 - (mod(nitem_2-1,ncolumn)+1)
          ilen_in = int(min(ilen_gz-ilen_gzipped, ilen_tmp))
!
          if(my_rank .eq. 0) write(*,*) 'start loop',                   &
     &         ist+1, ist+nitem_1, ist+nitem_2, ist+nitem_c,            &
     &         ilen_gzipped+1, ilen_in
          if(nitem_1 .le. ncolumn) then
            call gzip_defleat_once(len_multi_6digit_line(nitem_1),      &
     &          mul_6digit_int_line(nitem_1, int_dat(ist+1)),           &
     &          ilen_in, ilen_used, gzip_buf(ilen_gzipped+1))
            ilen_gzipped = ilen_gzipped + ilen_used
            exit
          else
            call gzip_defleat_begin(ilen_line,                          &
     &          mul_6digit_int_line(ncolumn, int_dat(ist+1)),           &
     &          ilen_in, ilen_used, gzip_buf(ilen_gzipped+1))
            if(my_rank .eq. 0) write(*,*) 'gzip_defleat_begin',         &
     &                       ist+ncolumn, ilen_used
!
            do i = ist+ncolumn+1, ist+nitem_c, ncolumn
              call gzip_defleat_cont(ilen_line,                         &
     &            mul_6digit_int_line(ncolumn, int_dat(i)),             &
     &            ilen_in, ilen_used)
            end do
            if(my_rank .eq. 0) write(*,*) 'gzip_defleat_cont',          &
     &                       ist+nitem_c, ilen_used
!
            nrest = nitem_2 - nitem_c
            call gzip_defleat_last(len_multi_6digit_line(nrest),        &
     &          mul_6digit_int_line(nrest, int_dat(ist+nitem_c+1)),     &
     &          ilen_in, ilen_used)
            if(my_rank .eq. 0) write(*,*) 'gzip_defleat_last',          &
     &                       ilen_used, ist + nitem_2, num
!
            ilen_gzipped = ilen_gzipped + ilen_used
            ist = ist + nitem_2
            if(ist .ge. num) exit
          end if
        end do
        if(my_rank .eq. 0) write(*,*) 'all done ', ilen_gzipped
      end if
!
      call gz_mpi_write_stack_over_domain(IO_param, int(ilen_gzipped))
!
      if(ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &     int(ilen_gzipped), gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine gz_mpi_write_element_type
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_int_list(IO_param, nele, ncomp, ivect)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nele, ncomp
      integer(kind=kint), intent(in) :: ivect(nele,ncomp)
!
      integer(kind = kint) :: i
      integer(kind = kint) :: ie_tmp(ncomp)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_line, ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      ilen_line = len_multi_int_textline(ncomp)
      ilen_gz = int(real(nele*ilen_line *1.01)) + 24
      allocate(gzip_buf(ilen_gz))
!
      if(nele .le. 0) then
        call gzip_defleat_once(ione, char(10),                          &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(nele .eq. 1) then
        call gzip_defleat_once(ilen_line,                               &
     &      multi_int_textline(ncomp, ivect(1,1)),                      &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(nele .gt. 0) then
        ie_tmp(1:ncomp) = ivect(1,1:ncomp)
        call gzip_defleat_begin(ilen_line,                              &
     &     multi_int_textline(ncomp, ie_tmp),                           &
     &     ilen_gz, ilen_gzipped, gzip_buf(1))
        do i = 2, nele - 1
          ie_tmp(1:ncomp) = ivect(i,1:ncomp)
          call gzip_defleat_cont(ilen_line,                             &
     &     multi_int_textline(ncomp, ie_tmp), ilen_gz, ilen_gzipped)
        end do
        ie_tmp(1:ncomp) = ivect(nele,1:ncomp)
        call gzip_defleat_last(ilen_line,                               &
     &     multi_int_textline(ncomp, ie_tmp), ilen_gz, ilen_gzipped)
      end if
!
      call gz_mpi_write_stack_over_domain(IO_param, ilen_gzipped)
!
      if(ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &     ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine gz_mpi_write_int_list
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine infleate_element_type                                  &
     &         (ncolumn, ilen_gz, gzip_buf, num, int_dat)
!
      integer(kind=kint), intent(in) :: ncolumn
      integer(kind = kint_gl), intent(in) :: ilen_gz
      character(len=1), intent(in) :: gzip_buf(ilen_gz)
!
      integer(kind=kint), intent(in) :: num
      integer(kind=kint), intent(inout) :: int_dat(num)
!
      integer(kind = kint) :: i, ist, nrest
      integer(kind = kint) :: nitem_1, nitem_2, nitem_c
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: ilen_gzipped, ilen_tmp
      integer(kind = kint) :: ilen_line, ilen_used, ilen_in
!
      character(len=1), allocatable :: textbuf(:)
!
!
      ilen_line = len_multi_6digit_line(ncolumn)
      allocate(textbuf(ilen_line))
!
      if(num .le. 0) then
        ilen_in = int(ilen_gz)
        call gzip_infleat_once                                          &
     &    (ilen_in, gzip_buf(1), ione, textbuf(1), ilen_used)
        ilen_gzipped = ilen_used
      else
        ist = 0
        ilen_gzipped = 0
        ilen_tmp = dble(maxline*ilen_line) * 1.01 + 24
        if(my_rank .eq. 0) write(*,*) 'infleate_element_type  start ',  &
     &      num, ilen_line, ilen_gz, ilen_tmp
!
        do
          nitem_1 = num-ist
          nitem_2 = min(num-ist,ncolumn*maxline)
          nitem_c = nitem_2 - (mod(nitem_2-1,ncolumn)+1)
          ilen_in = int(min(ilen_gz-ilen_gzipped, ilen_tmp))
!
          if(my_rank .eq. 0) write(*,*) 'start loop',                   &
     &         ist+1, ist+nitem_1, ist+nitem_2, ist+nitem_c,            &
     &         ilen_gzipped+1, ilen_in
          if(nitem_1 .le. ncolumn) then
            call gzip_infleat_once(ilen_in, gzip_buf(ilen_gzipped+1),   &
     &          len_multi_6digit_line(nitem_1), textbuf(1), ilen_used)
            call read_mul_6digit_int_line                               &
     &         (textbuf(1), nitem_1, int_dat(ist+1))
            ilen_gzipped = ilen_gzipped + ilen_used
            exit
          else
            call gzip_infleat_begin                                     &
     &         (ilen_in, gzip_buf(ilen_gzipped+1), ilen_line,           &
     &          textbuf(1), ilen_used)
            call read_mul_6digit_int_line                               &
     &         (textbuf(1), ncolumn, int_dat(ist+1))
!
            do i = ist+ncolumn+1, ist+nitem_c, ncolumn
              call gzip_infleat_cont(ilen_in, ilen_line,                &
     &            textbuf(1), ilen_used)
              call read_mul_6digit_int_line                             &
     &           (textbuf(1), ncolumn, int_dat(i))
            end do
            if(my_rank .eq. 0) write(*,*) 'gzip_infleat_cont',          &
     &                       ist+nitem_c, ilen_used
!
            nrest = nitem_2 - nitem_c
            call gzip_infleat_last                                      &
     &         (ilen_in, len_multi_6digit_line(nrest),                  &
     &          textbuf(1), ilen_used)
            call read_mul_6digit_int_line                               &
     &         (textbuf(1), nrest, int_dat(ist+nitem_c+1))
            if(my_rank .eq. 0) write(*,*) 'gzip_infleat_last',          &
     &                       ist+nitem_2, ilen_used
!
            ilen_gzipped = ilen_gzipped + ilen_used
            ist = ist + nitem_2
            if(ist .ge. num) exit
          end if
        end do
        if(my_rank .eq. 0) write(*,*) 'all done ', ilen_gzipped
      end if
!
      deallocate(textbuf)
!
      end subroutine infleate_element_type
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_integer_list_IO
