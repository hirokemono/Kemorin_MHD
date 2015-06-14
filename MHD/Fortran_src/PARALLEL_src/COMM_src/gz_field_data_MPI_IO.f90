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
!!      subroutine gz_read_fld_vecotr_mpi                               &
!!     &         (nprocs_in, id_rank, nnod, ndir, vector, istack_merged)
!!
!!      integer(kind = kint) function  gz_defleat_vector_txt            &
!!     &                   (nnod, ndir, vector, ilen_gz, buffer)
!!@endverbatim
!
      module gz_field_data_MPI_IO
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_calypso_mpi_IO
!
      implicit none
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
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nnod, ndir
      real(kind = kreal), intent(in) :: vector(nnod,ndir)
!
      integer, intent(in) ::  id_fld
!
      real(kind = kreal) :: v1(ndir)
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength, ip
      integer(kind = kint) :: ilen_gzipped_gl(nprocs)
      integer(kind = kint_gl) :: istack_buffer(0:nprocs)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      character(len=1), allocatable :: gzip_buf(:)
!
!
      v1(1:ndir) = 0.0d0
      ilength = len(each_field_data_buffer(ndir, v1))
      ilen_gz = int(real(nnod*ilength) * 1.01) + 24
      allocate(gzip_buf(ilen_gz))
      ilen_gzipped = gz_defleat_vector_txt(nnod, ndir, vector, ilength, &
     &                                     ilen_gz, gzip_buf(1))
!
      call MPI_Allgather(ilen_gzipped, ione, CALYPSO_INTEGER,           &
     &    ilen_gzipped_gl, ione, CALYPSO_INTEGER, CALYPSO_COMM,         &
     &    ierr_MPI)
!
      istack_buffer(0) = 0
      do ip = 1, nprocs
        istack_buffer(ip) = istack_buffer(ip-1) + ilen_gzipped_gl(ip)
      end do
      call gz_write_fld_header_mpi(id_fld, ioff_gl,                     &
     &    buffer_istack_nod_buffer(nprocs, istack_buffer))
!
      if(ilen_gzipped .gt. 0) then
        ioffset = ioff_gl + istack_buffer(my_rank)
        call calypso_mpi_seek_write_chara                               &
     &     (id_fld, ioffset, ilen_gzipped, gzip_buf(1))
      end if
      deallocate(gzip_buf)
      ioff_gl = ioff_gl + istack_buffer(nprocs)
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
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      character(len=1), allocatable :: gzip_buf(:)
!
!
      if(my_rank .eq. 0) then
        ilength = len(header_txt)
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call gzip_defleat_once                                          &
     &     (ilength, header_txt, ilen_gz, ilen_gzipped, gzip_buf(1))
        ilength = ilen_gzipped
!
        ioffset = int(ioff_gl)
        call calypso_mpi_seek_write_chara                               &
     &         (id_fld, ioffset, ilen_gzipped, gzip_buf(1))
        deallocate(gzip_buf)
      end if
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + ilen_gzipped
!
      end subroutine gz_write_fld_header_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_fld_vecotr_mpi                                 &
     &         (nprocs_in, id_rank, nnod, ndir, vector, istack_merged)
!
      use gz_field_data_IO
      use skip_gz_comment
!
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      integer(kind = kint), intent(in) :: nnod, ndir
      real(kind = kreal), intent(inout) :: vector(nnod,ndir)
!
      integer(kind = kint_gl) :: i
!
!
      do i = 1, istack_merged(id_rank)
        call get_one_line_from_gz_f
      end do
!
      call read_gz_field_vect(nnod, ndir, vector)
!
      do i = istack_merged(id_rank+1)+1, istack_merged(nprocs_in)
        call get_one_line_from_gz_f
      end do
!
      end subroutine gz_read_fld_vecotr_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint) function  gz_defleat_vector_txt              &
     &                   (nnod, ndir, vector, ilength, ilen_gz, buffer)
!
      use field_data_IO
!
      integer(kind = kint), intent(in) :: nnod, ndir
      real(kind = kreal), intent(in) :: vector(nnod,ndir)
!
      integer(kind = kint), intent(in) :: ilength, ilen_gz
      character(len=1), intent(inout) :: buffer(ilen_gz)
!
      real(kind = kreal) :: v1(ndir)
      integer(kind = kint) :: ilen_gzipped
      integer(kind = kint_gl) :: inod
!
!
      if(nnod .eq. 1) then
        v1(1:ndir) = vector(1,1:ndir)
        call gzip_defleat_once(ilength,                                 &
     &      each_field_data_buffer(ndir, v1),                           &
     &      ilen_gz, ilen_gzipped, buffer(1))
!
      else if(nnod .gt. 1) then
        v1(1:ndir) = vector(1,1:ndir)
        call gzip_defleat_begin(ilength,                                &
     &      each_field_data_buffer(ndir, v1),                           &
     &      ilen_gz, ilen_gzipped, buffer(1))
        do inod = 2, nnod-1
          v1(1:ndir) = vector(inod,1:ndir)
          call gzip_defleat_cont(ilength,                               &
     &      each_field_data_buffer(ndir, v1), ilen_gz, ilen_gzipped)
        end do
        v1(1:ndir) = vector(nnod,1:ndir)
        call gzip_defleat_last(ilength,                                 &
     &      each_field_data_buffer(ndir, v1), ilen_gz, ilen_gzipped)
      else
        ilen_gzipped = 0
      end if
      gz_defleat_vector_txt = ilen_gzipped
!
      end function gz_defleat_vector_txt
!
! -----------------------------------------------------------------------
!
      end module gz_field_data_MPI_IO
