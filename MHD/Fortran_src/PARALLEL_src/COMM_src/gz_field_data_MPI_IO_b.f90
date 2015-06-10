!>@file  gz_field_data_MPI_IO_b.f90
!!       module gz_field_data_MPI_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief IO routines for field data IO for merged gzipped binary file
!!
!!@verbatim
!!      subroutine allocate_mpi_status_gz_mpi_b
!!      subroutine deallocate_mpi_status_gz_mpi_b
!!
!!      subroutine gz_write_fld_inthead_mpi_b(id_fld, ioff_gl, int_dat)
!!      subroutine gz_write_fld_realhead_mpi_b(id_fld, ioff_gl, real_dat)
!!
!!      subroutine gz_write_fld_mul_i8head_mpi_b                        &
!!     &         (id_fld, ioff_gl, num, int_gl_dat)
!!      subroutine gz_write_fld_mul_inthead_mpi_b                       &
!!     &         (id_fld, ioff_gl, num, int_dat)
!!      subroutine gz_write_fld_mul_charhead_mpi_b                      &
!!     &         (id_fld, ioff_gl, num, chara_dat)
!!
!!      subroutine gz_write_fld_realarray2_mpi_b                        &
!!     &         (id_fld, ioff_gl, n1, n2, real_dat)
!!
!!      subroutine gz_read_fld_inthead_mpi_b(int_dat)
!!      subroutine gz_read_fld_realhead_mpi_b(real_dat)
!!
!!      subroutine gz_read_fld_mul_i8head_mpi_b(num, int_gl_dat)
!!      subroutine gz_read_fld_mul_inthead_mpi_b(num, int_dat)
!!      subroutine gz_read_fld_mul_charhead_mpi_b(num, chara_dat)
!!
!!      subroutine gz_read_fld_realarray2_mpi_b                         &
!!     &         (n1, n2, real_dat, istack_merged)
!!@endverbatim
!
      module gz_field_data_MPI_IO_b
!
      use m_precision
      use m_constants
!
      use calypso_mpi
!
      implicit none
!
      character(len=1), allocatable :: gzip_buf(:)
!
!>       status flag for sending
      integer, allocatable :: sta1(:)
!
      integer, external :: gzread_f
!
      private :: gzip_buf, sta1
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_mpi_status_gz_mpi_b
!
      allocate(sta1(MPI_STATUS_SIZE))
!
      end subroutine allocate_mpi_status_gz_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_mpi_status_gz_mpi_b
!
      deallocate(sta1)
!
      end subroutine deallocate_mpi_status_gz_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_fld_inthead_mpi_b(id_fld, ioff_gl, int_dat)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: int_dat
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint) :: ilen_gz, ilen_gzipped
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
!
!
!
      if(my_rank .eq. 0) then
        ilength = ione * kint
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call gzip_defleat_once                                          &
     &     (ilength, int_dat, ilen_gz, ilen_gzipped, gzip_buf(1))
        ilength = ilen_gzipped
!
        ioffset = int(ioff_gl)
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_fld, gzip_buf(1), ilen_gzipped,          &
     &                      CALYPSO_CHARACTER, sta1, ierr_MPI)
        deallocate(gzip_buf)
      end if
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + ilen_gzipped
!
      end subroutine gz_write_fld_inthead_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_fld_realhead_mpi_b(id_fld, ioff_gl, real_dat)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      real(kind = kreal), intent(in) :: real_dat
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint) :: ilen_gz, ilen_gzipped
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
!
!
!
      if(my_rank .eq. 0) then
        ilength = ione * kreal
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call gzip_defleat_once                                          &
     &     (ilength, real_dat, ilen_gz, ilen_gzipped, gzip_buf(1))
        ilength = ilen_gzipped
!
        ioffset = int(ioff_gl)
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_fld, gzip_buf(1), ilen_gzipped,          &
     &                      CALYPSO_CHARACTER, sta1, ierr_MPI)
        deallocate(gzip_buf)
      end if
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + ilen_gzipped
!
      end subroutine gz_write_fld_realhead_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_fld_mul_i8head_mpi_b                          &
     &         (id_fld, ioff_gl, num, int_gl_dat)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int_gl_dat(num)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint) :: ilen_gz, ilen_gzipped
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
!
!
!
      if(my_rank .eq. 0) then
        ilength = num * kint_gl
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call gzip_defleat_once                                          &
     &     (ilength, int_gl_dat, ilen_gz, ilen_gzipped, gzip_buf(1))
        ilength = ilen_gzipped
!
        ioffset = int(ioff_gl)
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_fld, gzip_buf(1), ilen_gzipped,          &
     &                      CALYPSO_CHARACTER, sta1, ierr_MPI)
        deallocate(gzip_buf)
      end if
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + ilen_gzipped
!
      end subroutine gz_write_fld_mul_i8head_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_fld_mul_inthead_mpi_b                         &
     &         (id_fld, ioff_gl, num, int_dat)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint) :: ilen_gz, ilen_gzipped
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
!
!
!
      if(my_rank .eq. 0) then
        ilength = num * kint
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call gzip_defleat_once                                          &
     &     (ilength, int_dat, ilen_gz, ilen_gzipped, gzip_buf(1))
        ilength = ilen_gzipped
!
        ioffset = int(ioff_gl)
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_fld, gzip_buf(1), ilen_gzipped,          &
     &                      CALYPSO_CHARACTER, sta1, ierr_MPI)
        deallocate(gzip_buf)
      end if
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + ilen_gzipped
!
      end subroutine gz_write_fld_mul_inthead_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_fld_mul_charhead_mpi_b                        &
     &         (id_fld, ioff_gl, num, chara_dat)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(in) :: chara_dat(num)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint) :: ilen_gz, ilen_gzipped
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
!
!
!
      if(my_rank .eq. 0) then
        ilength = num * kchara
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call gzip_defleat_once                                          &
     &     (ilength, chara_dat, ilen_gz, ilen_gzipped, gzip_buf(1))
        ilength = ilen_gzipped
!
        ioffset = int(ioff_gl)
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_fld, gzip_buf(1), ilen_gzipped,          &
     &                      CALYPSO_CHARACTER, sta1, ierr_MPI)
        deallocate(gzip_buf)
      end if
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + ilen_gzipped
!
      end subroutine gz_write_fld_mul_charhead_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_fld_realarray2_mpi_b                          &
     &         (id_fld, ioff_gl, n1, n2, real_dat)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: n1, n2
      real(kind = kreal), intent(in) :: real_dat(n1,n2)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ip
      integer(kind = kint) :: ilen_gzipped_list(nprocs)
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
!
!
!
      ilength =  n1 * n2 * kreal
      ilen_gz = int(real(ilength) *1.01) + 24
      allocate(gzip_buf(ilen_gz))
      call gzip_defleat_once                                            &
     &   (ilength, real_dat, ilen_gz, ilen_gzipped, gzip_buf(1))
      ilength = ilen_gzipped
!
      call MPI_Allgather(ilen_gzipped, ione, CALYPSO_INTEGER,           &
     &    ilen_gzipped_list(1), ione, CALYPSO_INTEGER,                  &
     &    CALYPSO_COMM, ierr_MPI)
      ioffset = int(ioff_gl)
!
      if(ilen_gzipped .gt. 0) then
        do ip = 1, my_rank
          ioffset = ioffset + ilen_gzipped_list(ip)
        end do
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_fld, gzip_buf(1), ilen_gzipped,          &
     &                      CALYPSO_CHARACTER, sta1, ierr_MPI)
      end if
      do ip = 1, nprocs
        ioff_gl = ioff_gl + ilen_gzipped_list(ip)
      end do
      deallocate(gzip_buf)
!
      end subroutine gz_write_fld_realarray2_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_fld_inthead_mpi_b(int_dat)
!
      integer(kind = kint), intent(inout) :: int_dat
!
      integer(kind = kint) :: ierr
!
!
      if(my_rank .eq. 0) then
        ierr = gzread_f(kint, int_dat)
      else
        call gzseek_go_fwd_f(kint, ierr)
      end if
      call MPI_BCAST(int_dat, ione, CALYPSO_INTEGER, izero,             &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine gz_read_fld_inthead_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_fld_realhead_mpi_b(real_dat)
!
      real(kind = kreal), intent(inout) :: real_dat
!
      integer(kind = kint) :: ierr
!
      if(my_rank .eq. 0) then
        ierr = gzread_f(kreal, real_dat)
      else
        call gzseek_go_fwd_f(kreal, ierr)
      end if
      call MPI_BCAST(real_dat, ione, CALYPSO_REAL, izero,               &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine gz_read_fld_realhead_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_fld_mul_i8head_mpi_b(num, int_gl_dat)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int_gl_dat(num)
!
      integer(kind = kint) :: ilength, ierr
!
!
      ilength = num * kint_gl
      if(my_rank .eq. 0) then
        ierr = gzread_f(ilength, int_gl_dat)
      else
        call gzseek_go_fwd_f(ilength, ierr)
      end if
      call MPI_BCAST(int_gl_dat, num, CALYPSO_GLOBAL_INT, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine gz_read_fld_mul_i8head_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_fld_mul_inthead_mpi_b(num, int_dat)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_dat(num)
!
      integer(kind = kint) :: ilength, ierr
!
!
      ilength = num * kint
      if(my_rank .eq. 0) then
        ierr = gzread_f(ilength, int_dat)
      else
        call gzseek_go_fwd_f(ilength, ierr)
      end if
      call MPI_BCAST(int_dat, num, CALYPSO_INTEGER, izero,              &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine gz_read_fld_mul_inthead_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_fld_mul_charhead_mpi_b(num, chara_dat)
!
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(inout) :: chara_dat(num)
!
      integer(kind = kint) :: ilength, ierr
!
!
      ilength = num * kchara
      if(my_rank .eq. 0) then
        ierr = gzread_f(ilength, chara_dat)
      else
        call gzseek_go_fwd_f(ilength, ierr)
      end if
      call MPI_BCAST(chara_dat, ilength, CALYPSO_CHARACTER, izero,      &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine gz_read_fld_mul_charhead_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_fld_realarray2_mpi_b                           &
     &         (n1, n2, real_dat, istack_merged)
!
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs)
      integer(kind = kint), intent(in) :: n1, n2
      real(kind = kreal), intent(inout) :: real_dat(n1,n2)
!
      integer(kind = kint) :: ilength, ierr
!
!
      ilength =  int(istack_merged(my_rank)) * n2 * kreal
      call gzseek_go_fwd_f(ilength, ierr)
!
      ilength =  n1 * n2 * kreal
      ierr = gzread_f(ilength, real_dat)
!
      ilength = int(istack_merged(nprocs) - istack_merged(my_rank+1))   &
     &          * n2 * kreal
      call gzseek_go_fwd_f(ilength, ierr)
!
      end subroutine gz_read_fld_realarray2_mpi_b
!
! -----------------------------------------------------------------------
!
      end module gz_field_data_MPI_IO_b
