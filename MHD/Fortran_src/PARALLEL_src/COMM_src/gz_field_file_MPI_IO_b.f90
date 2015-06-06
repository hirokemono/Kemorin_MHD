!>@file  gz_field_file_MPI_IO_b.f90
!!       module gz_field_file_MPI_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine gz_write_step_fld_file_mpi_b(file_name, fld_IO)
!!
!!      subroutine gz_rd_alloc_st_fld_file_mpi_b(file_name, fld_IO)
!!@endverbatim
!
      module gz_field_file_MPI_IO_b
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_time_data_IO
      use t_field_data_IO
!
      implicit none
!
      character(len=1), allocatable :: gzip_buf(:)
!
      integer(kind = kint), allocatable ::    nnod_merged(:)
      integer(kind = kint_gl), allocatable :: istack_merged_nod(:)
!
!>       status flag for sending
      integer, allocatable :: sta1(:)
!
      integer, external :: gzread_f, gzseek_go_fwd_f
!
      private :: nnod_merged, istack_merged_nod, sta1
      private :: gz_write_field_data_mpi_b
      private :: set_istack_merged_nod, deallocate_istack_merged_nod
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gz_write_step_fld_file_mpi_b(file_name, fld_IO)
!
      character(len=kchara), intent(in) :: file_name
!
      type(field_IO), intent(in) :: fld_IO
!
      integer :: id_fld
      integer(kind = kint_gl) :: ioff_gl
!
!
      call set_istack_merged_nod(fld_IO%nnod_IO)
!
      call MPI_FILE_OPEN                                                &
     &   (CALYPSO_COMM, file_name, MPI_MODE_WRONLY+MPI_MODE_CREATE,     &
     &    MPI_INFO_NULL, id_fld, ierr_MPI)
!
      ioff_gl = 0
      call gz_write_field_data_mpi_b(id_fld, ioff_gl,                   &
     &    fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO,             &
     &    istack_merged_nod)
!
      call MPI_FILE_CLOSE(id_fld, ierr_MPI)
      call deallocate_istack_merged_nod
!
      end subroutine gz_write_step_fld_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_rd_alloc_st_fld_file_mpi_b(file_name, fld_IO)
!
      character(len=kchara), intent(in) :: file_name
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      call set_istack_merged_nod(fld_IO%nnod_IO)
!
      call open_rd_gzfile(file_name)
!
      call read_step_data_mpi_b(my_rank)
      call gz_read_fld_mul_i8head_mpi_b(nprocs, istack_merged_nod(1))
      call gz_read_fld_inthead_mpi_b(fld_IO%num_field_IO)
!
      call alloc_phys_name_IO(fld_IO)
      call gz_read_fld_mul_inthead_mpi_b                                &
     &   (fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      fld_IO%nnod_IO = int(istack_merged_nod(my_rank+1)                 &
     &                   - istack_merged_nod(my_rank)  )
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
      call read_field_data_mpi_b                                        &
     &   (fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%fld_name, fld_IO%d_IO, istack_merged_nod)
!
      call close_gzfile()
!
      call deallocate_istack_merged_nod
!
      end subroutine gz_rd_alloc_st_fld_file_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_field_data_mpi_b(id_fld, ioff_gl,             &
     &          nnod, num_field, ntot_comp, ncomp_field,                &
     &          field_name, d_nod, istack_merged)
!
      use m_phys_constants
      use field_data_IO
      use vtk_file_MPI_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs)
      integer(kind=kint), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer, intent(in) ::  id_fld
!
!
      call write_step_data_mpi_b(id_fld, ioff_gl, nprocs-1)
!
      call gz_write_fld_mul_i8head_mpi_b                                &
     &         (id_fld, ioff_gl, nprocs, istack_merged(1))
      call gz_write_fld_inthead_mpi_b(id_fld, ioff_gl, num_field)
      call gz_write_fld_mul_inthead_mpi_b                               &
     &         (id_fld, ioff_gl, num_field, ncomp_field)
!
      call gz_write_fld_mul_charhead_mpi_b                              &
     &         (id_fld, ioff_gl, num_field, field_name)
      call gz_write_fld_realarray2_mpi_b                                &
     &         (id_fld, ioff_gl, nnod, ntot_comp, d_nod)
!
      end subroutine gz_write_field_data_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_step_data_mpi_b(id_fld, ioff_gl, id_rank)
!
      integer(kind=kint), intent(in) :: id_rank
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer, intent(in) ::  id_fld
!
!
      call gz_write_fld_inthead_mpi_b(id_fld, ioff_gl, id_rank)
      call gz_write_fld_inthead_mpi_b(id_fld, ioff_gl, i_time_step_IO)
!
      call gz_write_fld_realhead_mpi_b(id_fld, ioff_gl, time_IO)
      call gz_write_fld_realhead_mpi_b(id_fld, ioff_gl, delta_t_IO)
!
      end subroutine write_step_data_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_step_data_mpi_b(my_rank)
!
      integer(kind=kint), intent(in) :: my_rank
!
      integer(kind = kint) :: id_rank
!
!
      call gz_read_fld_inthead_mpi_b(id_rank)
      call gz_read_fld_inthead_mpi_b(i_time_step_IO)
      call gz_read_fld_realhead_mpi_b(time_IO)
      call gz_read_fld_realhead_mpi_b(delta_t_IO)
!
      end subroutine read_step_data_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_data_mpi_b(nnod, num_field, ncomp,          &
     &          field_name, vect, istack_merged)
!
      use m_phys_constants
!
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs)
      integer(kind=kint), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ncomp
      character(len=kchara), intent(inout) :: field_name(num_field)
      real(kind = kreal), intent(inout) :: vect(nnod,ncomp)
!
!
      call gz_read_fld_mul_charhead_mpi_b(num_field, field_name)
      call gz_read_fld_realarray2_mpi_b                                 &
     &         (nnod, ncomp, vect, istack_merged)
!
      end subroutine read_field_data_mpi_b
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
        ierr = gzseek_go_fwd_f(kint)
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
        ierr = gzseek_go_fwd_f(kreal)
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
        ierr = gzseek_go_fwd_f(ilength)
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
        ierr = gzseek_go_fwd_f(ilength)
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
        ierr = gzseek_go_fwd_f(ilength)
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
      ierr = gzseek_go_fwd_f(ilength)
!
      ilength =  n1 * n2 * kreal
      ierr = gzread_f(ilength, real_dat)
!
      ilength = int(istack_merged(nprocs) - istack_merged(my_rank+1))   &
     &          * n2 * kreal
      ierr = gzseek_go_fwd_f(ilength)
!
      end subroutine gz_read_fld_realarray2_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_istack_merged_nod(nnod)
!
      integer(kind=kint), intent(in) :: nnod
!
      integer(kind = kint) :: ip
!
!
      allocate(nnod_merged(nprocs))
      allocate(istack_merged_nod(0:nprocs))
      allocate(sta1(MPI_STATUS_SIZE))
!
      call MPI_Allgather(nnod, ione, CALYPSO_INTEGER,                   &
     &    nnod_merged, ione, CALYPSO_INTEGER, CALYPSO_COMM, ierr_MPI)
      istack_merged_nod(0) = 0
      do ip = 1, nprocs
        istack_merged_nod(ip)                                           &
     &       =  istack_merged_nod(ip-1) + nnod_merged(ip)
      end do
!
      end subroutine set_istack_merged_nod
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_istack_merged_nod
!
      deallocate(sta1)
      deallocate(istack_merged_nod, nnod_merged)
!
      end subroutine deallocate_istack_merged_nod
!
!  ---------------------------------------------------------------------
!
      end module gz_field_file_MPI_IO_b
