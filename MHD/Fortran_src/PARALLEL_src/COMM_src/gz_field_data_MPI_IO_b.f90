!>@file  gz_field_data_MPI_IO_b.f90
!!       module gz_field_data_MPI_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief IO routines for field data IO for merged gzipped binary file
!!
!!@verbatim
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
!!     &         (nprocs_in, id_rank, n1, n2, real_dat, istack_merged)
!!@endverbatim
!
      module gz_field_data_MPI_IO_b
!
      use m_precision
      use m_constants
      use m_calypso_mpi_IO
!
      use calypso_mpi
!
      implicit none
!
      integer, external :: gzread_f
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_fld_inthead_mpi_b(id_fld, ioff_gl, int_dat)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: int_dat
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      character(len=1), allocatable :: gzip_buf(:)
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
        call calypso_mpi_seek_write_chara                               &
     &     (id_fld, ioffset, ilen_gzipped, gzip_buf(1))
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
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      character(len=1), allocatable :: gzip_buf(:)
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
        call calypso_mpi_seek_write_chara                               &
     &     (id_fld, ioffset, ilen_gzipped, gzip_buf(1))
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
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      character(len=1), allocatable :: gzip_buf(:)
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
        call calypso_mpi_seek_write_chara                               &
     &     (id_fld, ioffset, ilen_gzipped, gzip_buf(1))
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
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      character(len=1), allocatable :: gzip_buf(:)
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
        call calypso_mpi_seek_write_chara                               &
     &     (id_fld, ioffset, ilen_gzipped, gzip_buf(1))
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
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      character(len=1), allocatable :: gzip_buf(:)
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
        call calypso_mpi_seek_write_chara                               &
     &     (id_fld, ioffset, ilen_gzipped, gzip_buf(1))
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
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength, ip
      integer(kind = kint) :: ilen_gzipped_gl(nprocs)
      integer(kind = kint_gl) :: istack_buffer(0:nprocs)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      character(len=1), allocatable :: gzip_buf(:)
!
!
      ilength =  n1 * n2 * kreal
      ilen_gz = int(real(ilength) *1.01) + 24
      allocate(gzip_buf(ilen_gz))
      call gzip_defleat_once                                            &
     &   (ilength, real_dat, ilen_gz, ilen_gzipped, gzip_buf(1))
!
      call MPI_Allgather(ilen_gzipped, ione, CALYPSO_INTEGER,           &
     &    ilen_gzipped_gl, ione, CALYPSO_INTEGER, CALYPSO_COMM,         &
     &    ierr_MPI)
!
      istack_buffer(0) = 0
      do ip = 1, my_rank
        istack_buffer(ip) = istack_buffer(ip-1) + ilen_gzipped_gl(ip)
      end do
!
      call gz_write_fld_mul_i8head_mpi_b                                &
     &   (id_fld, ioff_gl, nprocs, istack_buffer(1))
!
      if(ilen_gzipped .gt. 0) then
        ioffset = ioff_gl + istack_buffer(my_rank)
        call calypso_mpi_seek_write_chara                               &
     &    (id_fld, ioffset, ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      ioff_gl = ioff_gl + istack_buffer(nprocs)
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
        ierr = gzread_f(ilength, int_gl_dat(1))
      else
        call gzseek_go_fwd_f(ilength, ierr)
      end if
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
        ierr = gzread_f(ilength, int_dat(1))
      else
        call gzseek_go_fwd_f(ilength, ierr)
      end if
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
        ierr = gzread_f(ilength, chara_dat(1))
      else
        call gzseek_go_fwd_f(ilength, ierr)
      end if
!
      end subroutine gz_read_fld_mul_charhead_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_fld_realarray2_mpi_b                           &
     &         (nprocs_in, id_rank, n1, n2, real_dat, istack_merged)
!
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      integer(kind = kint), intent(in) :: n1, n2
      real(kind = kreal), intent(inout) :: real_dat(n1,n2)
!
      integer(kind = kint) :: ilength, ierr
!
!
      ilength =  int(istack_merged(id_rank)) * n2 * kreal
      write(*,*) 'gzseek_go_fwd_f', id_rank, ilength
      call gzseek_go_fwd_f(ilength, ierr)
!
      ilength =  n1 * n2 * kreal
      write(*,*) 'gzread_f', id_rank, ilength
      ierr = gzread_f(ilength, real_dat)
!
!      ilength = int(istack_merged(nprocs_in)                           &
!     &            - istack_merged(id_rank+1))  * n2 * kreal
!      call gzseek_go_fwd_f(ilength, ierr)
!
      end subroutine gz_read_fld_realarray2_mpi_b
!
! -----------------------------------------------------------------------
!
      end module gz_field_data_MPI_IO_b
