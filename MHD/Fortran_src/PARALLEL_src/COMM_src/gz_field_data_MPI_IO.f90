!>@file  gz_field_data_MPI_IO.f90
!!       module gz_field_data_MPI_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged VTK file usgin MPI-IO
!!
!!@verbatim
!!      subroutine gz_write_fld_header_mpi(id_fld, ioff_gl, header_txt)
!!
!!      subroutine gz_write_fld_vecotr_mpi(id_fld, ioff_gl,             &
!!     &          nnod, ndir, vector)
!!      subroutine read_fld_vecotr_gz_mpi                               &
!!     &         (nnod, ndir, vector, istack_merged)
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
      character(len=1), allocatable :: gzip_buf(:)
!
      private :: gzip_buf
!
!  ---------------------------------------------------------------------
!
      contains
!
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
!
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
! -----------------------------------------------------------------------
!
      subroutine gz_write_fld_vecotr_mpi(id_fld, ioff_gl,               &
     &          nnod, ndir, vector)
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
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
      integer(kind = kint_gl) :: inod
!
!
      v1(1:ndir) = 0.0d0
      ilength = len(each_field_data_buffer(ndir, v1))
      ilen_gz = int(real(nnod*ilength) * 1.01) + 24
      allocate(gzip_buf(ilen_gz))
      if(nnod .eq. 1) then
        v1(1:ndir) = vector(1,1:ndir)
        call gzip_defleat_once(ilength,                                 &
     &      each_field_data_buffer(ndir, v1),                           &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
!
      else if(nnod .gt. 1) then
        v1(1:ndir) = vector(1,1:ndir)
        call gzip_defleat_begin(ilength,                                &
     &      each_field_data_buffer(ndir, v1),                           &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
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
!
      call calypso_gz_mpi_seek_write                                    &
     &         (id_fld, ioff_gl, ilen_gzipped, gzip_buf(1))
      deallocate(gzip_buf)
!
      end subroutine gz_write_fld_vecotr_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_fld_vecotr_gz_mpi                                 &
     &         (nnod, ndir, vector, istack_merged)
!
      use gz_field_data_IO
      use skip_gz_comment
!
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs)
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ndir
      real(kind = kreal), intent(inout) :: vector(nnod,ndir)
!
      integer(kind = kint_gl) :: i
!
!
      do i = 1, istack_merged(my_rank)
        call get_one_line_from_gz_f
      end do
!
      call read_gz_field_vect(nnod, ndir, vector)
!
      do i = istack_merged(my_rank+1)+1, istack_merged(nprocs)
        call get_one_line_from_gz_f
      end do
!
      end subroutine read_fld_vecotr_gz_mpi
!
! -----------------------------------------------------------------------
!
      end module gz_field_data_MPI_IO
