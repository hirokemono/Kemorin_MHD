!>@file  gz_field_data_MPI_IO.f90
!!       module gz_field_data_MPI_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged VTK file usgin MPI-IO
!!
!!@verbatim
!!      subroutine gz_write_fld_vecotr_mpi                              &
!!     &         (id_fld, ioff_gl, nnod, ndir, vector)
!!      subroutine gz_write_fld_header_mpi(id_fld, ioff_gl, header_txt)
!!
!!      subroutine gz_read_fld_charhead_mpi(id_fld,                     &
!!     &         ioff_gl, ilength, chara_dat)
!!      subroutine gz_read_fld_1word_mpi(id_fld, ioff_gl, field_name)
!!      subroutine gz_read_each_field_mpi(id_fld, nprocs_in, id_rank,   &
!!     &          ioff_gl, nnod, ndir, vector)
!!      subroutine gz_skip_each_field_mpi(id_fld, nprocs_in, ioff_gl)
!!@endverbatim
!
      module gz_field_data_MPI_IO
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use t_buffer_4_gzip
!
      implicit none
!
      type(buffer_4_gzip) :: zbuf
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_fld_vecotr_mpi                                &
     &         (id_fld, ioff_gl, nnod, ndir, vector)
!
      use field_data_IO
      use data_IO_to_textline
!
      use zlib_convert_ascii_vector
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nnod, ndir
      real(kind = kreal), intent(in) :: vector(nnod,ndir)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint) :: ip
      integer(kind = kint_gl) :: ilen_gzipped_gl(nprocs)
      integer(kind = kint_gl) :: istack_buffer(0:nprocs)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: nnod64
!
!
      nnod64 = nnod
      call defleate_vector_txt(nnod64, ndir, vector, zbuf)
!
      call MPI_Allgather(zbuf%ilen_gzipped, ione, CALYPSO_GLOBAL_INT,   &
     &    ilen_gzipped_gl, ione, CALYPSO_GLOBAL_INT, CALYPSO_COMM,      &
     &    ierr_MPI)
!
      istack_buffer(0) = 0
      do ip = 1, nprocs
        istack_buffer(ip) = istack_buffer(ip-1) + ilen_gzipped_gl(ip)
      end do
      call gz_write_fld_header_mpi(id_fld, ioff_gl,                     &
     &    buffer_istack_nod_buffer(nprocs, istack_buffer))
!
      if(zbuf%ilen_gzipped .gt. 0) then
        ioffset = ioff_gl + istack_buffer(my_rank)
        call calypso_mpi_seek_long_write_gz                             &
     &     (id_fld, ioffset, zbuf%ilen_gzipped, zbuf%gzip_buf(1))
      end if
      ioff_gl = ioff_gl + istack_buffer(nprocs)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine gz_write_fld_vecotr_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_fld_header_mpi(id_fld, ioff_gl, header_txt)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      character(len=*), intent(in) :: header_txt
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint) :: ilen_gz32, ilen_gzipped32, ilength
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      character(len=1), allocatable :: gzip_buf(:)
!
!
      if(my_rank .eq. 0) then
        ilength = len(header_txt)
        ilen_gz32 = int(real(ilength) * 1.01 + 24)
        allocate(gzip_buf(ilen_gz32))
        call gzip_defleat_once (ilength, header_txt,                    &
     &     ilen_gz32, ilen_gzipped32, gzip_buf(1))
!
        ioffset = ioff_gl
        call calypso_mpi_seek_write_chara                               &
     &         (id_fld, ioffset, ilen_gzipped32, gzip_buf(1))
        deallocate(gzip_buf)
      end if
      call MPI_BCAST(ilen_gzipped32, ione, CALYPSO_INTEGER, izero,      &
     &    CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + ilen_gzipped32
!
      end subroutine gz_write_fld_header_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_fld_charhead_mpi(id_fld,                       &
     &         ioff_gl, ilength, chara_dat)
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind=kint), intent(in) :: ilength
      character(len=ilength), intent(inout) :: chara_dat
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call calypso_mpi_seek_read_gz                                   &
     &         (id_fld, ioffset, ilen_gz, gzip_buf(1))
!
        call gzip_infleat_once                                          &
     &     (ilen_gz, gzip_buf(1), ilength, chara_dat, ilen_gzipped)
        deallocate(gzip_buf)
      end if
!
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + ilen_gzipped
!
      end subroutine gz_read_fld_charhead_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_fld_1word_mpi(id_fld, ioff_gl, field_name)
!
      use field_data_IO
      use field_data_MPI_IO
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      character(len=kchara), intent(inout) :: field_name
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength, i
!
      character(len=1), allocatable :: gzip_buf(:), textbuf(:)
      character(len=kchara) :: textbuf_c
!
!
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        ilen_gz = int(real(kchara) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call calypso_mpi_seek_read_gz                                   &
     &     (id_fld, ioffset, ilen_gz, gzip_buf(1))
!
        call gzip_infleat_once                                          &
     &     (ilen_gz, gzip_buf(1), kchara, textbuf_c, ilen_gzipped)
!
        call read_each_field_name_buffer                                &
     &     (textbuf_c, field_name, ilength)
        ilength = ilength + 1
!        do i = 1, kchara
!          write(*,*) ilength, i, field_name(i:i),                      &
!     &         iachar(textbuf_c(i:i)), iachar(field_name(i:i))
!        end do
!
!        write(*,*) 'field_name', ilength, trim(field_name)
!
        allocate(textbuf(ilength))
        call gzip_infleat_once                                          &
     &     (ilen_gz, gzip_buf(1), ilength, textbuf(1), ilen_gzipped)
!
        deallocate(gzip_buf, textbuf)
      end if
!
      call sync_field_name_mpi(ilength, field_name)
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + ilen_gzipped
!
      end subroutine gz_read_fld_1word_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_each_field_mpi(id_fld, nprocs_in, id_rank,     &
     &          ioff_gl, nnod, ndir, vector)
!
      use field_data_IO
      use field_data_MPI_IO
      use data_IO_to_textline
!
      use zlib_convert_ascii_vector
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
!
      integer(kind = kint), intent(in) :: nnod, ndir
      real(kind = kreal), intent(inout) :: vector(nnod,ndir)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      character(len=nprocs_in*16+1) :: textbuf_p
!
      integer(kind = kint_gl) :: istack_buf(0:nprocs_in)
      integer(kind = kint_gl) :: nnod64
!
!
      call gz_read_fld_charhead_mpi                                     &
     &   (id_fld, ioff_gl, len(textbuf_p), textbuf_p)

      if(my_rank .eq. 0) call read_bufer_istack_nod_buffer              &
     &                      (textbuf_p, nprocs_in, istack_buf)
!
      call MPI_BCAST(istack_buf, (nprocs_in+1), CALYPSO_GLOBAL_INT,     &
     &    izero, CALYPSO_COMM, ierr_MPI)
!
!
      if(id_rank .ge. nprocs_in) then
        ioff_gl = ioff_gl + istack_buf(nprocs_in)
        return
      end if
!
      zbuf%ilen_gz = istack_buf(id_rank+1) - istack_buf(id_rank)
      call alloc_zip_buffer(zbuf)
!
      ioffset = ioff_gl + istack_buf(id_rank)
      ioff_gl = ioff_gl + istack_buf(nprocs_in)
      call calypso_mpi_seek_long_read_gz                                &
     &   (id_fld, ioffset, zbuf%ilen_gz, zbuf%gzip_buf(1))
!
      nnod64 = nnod
      call infleate_vector_txt(nnod64, ndir, vector, zbuf)
!
      end subroutine gz_read_each_field_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_skip_each_field_mpi(id_fld, nprocs_in, ioff_gl)
!
      use field_data_IO
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in
!
      integer(kind = kint) :: ilength
!
      character(nprocs_in*16+1) :: textbuf_c
!
      integer(kind = kint_gl) :: istack_buf(0:nprocs_in)
!
!
      ilength = len(buffer_istack_nod_buffer(nprocs_in,istack_buf))
!
      call gz_read_fld_charhead_mpi                                     &
     &   (id_fld, ioff_gl, ilength, textbuf_c)
      call read_bufer_istack_nod_buffer                                 &
     &   (textbuf_c, nprocs_in, istack_buf)
!
      ioff_gl = ioff_gl + istack_buf(nprocs_in)
!
      end subroutine gz_skip_each_field_mpi
!
! -----------------------------------------------------------------------
!
      end module gz_field_data_MPI_IO
