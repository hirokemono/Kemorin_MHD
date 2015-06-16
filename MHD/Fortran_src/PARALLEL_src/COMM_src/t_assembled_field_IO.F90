!>@file  t_assembled_field_IO.f90
!!       module t_assembled_field_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief gzipped data IO for 
!!
!!@verbatim
!!      subroutine sel_write_SPH_assemble_field                         &
!!     &         (nprocs_in, istep_fld, nloop, fld_IO, gz_bufs)
!!
!!   Data format for the merged ascii field data
!!     1.   Number of process
!!     2.   Time step
!!     3.   Time, Delta t
!!     4.   Stacks of numbe of data points
!!     5.   Number of fields
!!     6.   List of number of components
!!     7.   Each field data  (Itarate 7.1 - 7.3)
!!      7.1   Field name
!!      7.2   List of data size (Byte)
!!      7.3   Field data
!!
!!   Data format for the merged binary field data
!!     1.   Number of process
!!     2.   Time step
!!     3.   Time, Delta t
!!     4.   Stacks of numbe of data points
!!     5.   Number of fields
!!     6.   List of number of components
!!     7.   Field names
!!     8.   List of data size (Byte)
!!     9.   All Field data
!!@endverbatim
!
      module t_assembled_field_IO
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_field_data_IO
      use m_calypso_mpi_IO
!
      implicit none
!
!>  Structure for zlib data IO
      type mul_zlib_buffers
!>  Original data size
        integer(kind = kint) :: ilen_gz
!>  Data size for compressed data
        integer(kind = kint) :: len_gzipped
!>  Data buffer for zlib IO
        character(len = 1), pointer :: buffer(:)
      end type mul_zlib_buffers
!
      private :: gz_write_step_asbl_fld_mpi_b
      private :: gz_write_step_asbl_fld_mpi
      private :: gz_write_asmbl_fld_mpi, gz_write_asmbl_fld_mpi_b
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sel_write_SPH_assemble_field                           &
     &         (nprocs_in, istep_fld, nloop, fld_IO, gz_bufs)
!
      use field_IO_select
      use set_field_file_names
!
      integer(kind = kint), intent(in) :: istep_fld
      integer(kind = kint), intent(in) :: nloop, nprocs_in
!
      type(field_IO), intent(inout) :: fld_IO(nloop)
      type(mul_zlib_buffers), intent(inout) :: gz_bufs(nloop)
!
      integer(kind = kint) :: iloop, id_rank
      character(len=kchara) :: file_name
!
!
      if(nprocs_in .ne. nprocs) then
        do iloop = 1, nloop
          id_rank = my_rank + (iloop-1) * nprocs
!
          call set_SPH_fld_file_name(fld_IO(iloop)%file_prefix,         &
     &       fld_IO(iloop)%iflag_file_fmt, id_rank, istep_fld,          &
     &       file_name)
        end do 
!
        if(fld_IO(1)%iflag_file_fmt                                     &
     &       .eq. iflag_single+id_gzip_bin_file_fmt) then
          call gz_write_step_asbl_fld_mpi_b                             &
     &         (file_name, nprocs_in, nloop, fld_IO, gz_bufs)
          return
        else if(fld_IO(1)%iflag_file_fmt                                &
     &       .eq. iflag_single+id_gzip_txt_file_fmt) then
          call gz_write_step_asbl_fld_mpi                               &
     &         (file_name, nprocs_in, nloop, fld_IO, gz_bufs)
          return
        end if
      end if
!
      do iloop = 1, nloop
        id_rank = my_rank + (iloop-1) * nprocs
!
        call sel_write_step_SPH_field_file                              &
     &     (nprocs_in, id_rank, istep_fld, fld_IO(iloop))
      end do 
!
      end subroutine sel_write_SPH_assemble_field
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_step_asbl_fld_mpi                             &
     &         (file_name, nprocs_in, nloop, fld_IO, gz_bufs)
!
      use field_data_IO
      use gz_field_file_MPI_IO
      use gz_field_data_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
!
      integer(kind = kint), intent(in) :: nprocs_in
!
      integer(kind = kint), intent(in) :: nloop
      type(field_IO), intent(in) :: fld_IO(nloop)
      type(mul_zlib_buffers), intent(inout) :: gz_bufs(nloop)
!
      integer ::  id_fld
!
      integer(kind = kint_gl) :: ioff_gl
      integer(kind = kint) :: icou, j
!
!
      if(my_rank .eq. 0) write(*,*) 'Write merged compressed data: ',   &
     &                               trim(file_name)
      call calypso_mpi_write_file_open(file_name, nprocs_in, id_fld)
!
      ioff_gl = 0
      call write_field_head_gz_mpi                                      &
     &   (id_fld, nprocs_in, ioff_gl, fld_IO(1)%num_field_IO,           &
     &    fld_IO(1)%num_comp_IO, fld_IO(1)%istack_numnod_IO)
!
      icou = 1
      do j = 1, fld_IO(1)%num_field_IO
        call gz_write_fld_header_mpi(id_fld, ioff_gl,                   &
     &     each_field_name_buffer(fld_IO(1)%fld_name(j)))
        call gz_write_asmbl_fld_mpi(id_fld, nprocs_in, ioff_gl,         &
     &      icou, fld_IO(1)%num_comp_IO(j), nloop, fld_IO, gz_bufs)
        icou = icou + fld_IO(1)%num_comp_IO(j)
      end do
!
      call calypso_close_mpi_file(id_fld)
!
      end subroutine gz_write_step_asbl_fld_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_step_asbl_fld_mpi_b                           &
     &         (file_name, nprocs_in, nloop, fld_IO, gz_bufs)
!
      use gz_field_file_MPI_IO_b
      use gz_field_data_MPI_IO_b
!
      character(len=kchara), intent(in) :: file_name
!
      integer(kind = kint), intent(in) :: nprocs_in
!
      integer(kind = kint), intent(in) :: nloop
      type(field_IO), intent(in) :: fld_IO(nloop)
      type(mul_zlib_buffers), intent(inout) :: gz_bufs(nloop)
!
      integer ::  id_fld
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &     'Write mergend compressed binary data: ', trim(file_name)
      call calypso_mpi_write_file_open(file_name, nprocs_in, id_fld)
!
!
      ioff_gl = 0
      call gz_write_field_head_mpi_b(id_fld, nprocs_in, ioff_gl,        &
     &      fld_IO(1)%num_field_IO, fld_IO(1)%num_comp_IO,              &
     &      fld_IO(1)%istack_numnod_IO)
!
      call gz_write_fld_mul_charhead_mpi_b(id_fld, ioff_gl,             &
     &   fld_IO(1)%num_field_IO, fld_IO(1)%fld_name)
      call gz_write_asmbl_fld_mpi_b(id_fld, nprocs_in,                  &
     &    ioff_gl, nloop, fld_IO, gz_bufs)
!
      call calypso_close_mpi_file(id_fld)
!
      end subroutine gz_write_step_asbl_fld_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_asmbl_fld_mpi(id_mpi_file, nprocs_in,         &
     &          ioff_gl, ist_fld, ndir, nloop, fld_IO, gz_bufs)
!
      use field_data_IO
      use m_calypso_mpi_IO
      use gz_field_file_MPI_IO
      use gz_field_data_MPI_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in
      integer(kind = kint), intent(in) :: ist_fld, ndir
!
      integer(kind = kint), intent(in) :: nloop
      type(field_IO), intent(in) :: fld_IO(nloop)
      type(mul_zlib_buffers), intent(inout) :: gz_bufs(nloop)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      integer, intent(in) ::  id_mpi_file
!
      real(kind = kreal), pointer :: vector(:,:)
      real(kind = kreal) :: v1(ndir)
      integer(kind = kint_gl) :: istack_gz_pe(0:nprocs_in)
      integer(kind = kint) :: len_gz_pe(nprocs_in)
      integer(kind = kint) :: len_gz_lc(nprocs_in)
!
      integer(kind = kint) :: id_rank
      integer(kind = kint) :: iloop, ip
      integer(kind = kint) :: ilength
!
!
      len_gz_lc(1:nprocs_in) = 0
      v1(1:ndir) = 0.0d0
      ilength = len(each_field_data_buffer(ndir, v1))
!
!        deflate data
      do iloop = 1, nloop
        id_rank = my_rank + (iloop-1) * nprocs
 !
        if(id_rank .lt. nprocs_in) then
          gz_bufs(iloop)%ilen_gz                                        &
     &       = int(real(fld_IO(iloop)%nnod_IO*ilength) * 1.01) + 24
          allocate(gz_bufs(iloop)%buffer(gz_bufs(iloop)%ilen_gz))
 !
          vector => fld_IO(iloop)%d_IO(:,ist_fld:ist_fld+ndir-1)
          gz_bufs(iloop)%len_gzipped = gz_defleat_vector_txt            &
     &                 (fld_IO(iloop)%nnod_IO, ndir, vector, ilength,   &
     &                  gz_bufs(iloop)%ilen_gz, gz_bufs(iloop)%buffer)
          len_gz_lc(id_rank+1) =       gz_bufs(iloop)%len_gzipped
        else
          gz_bufs(iloop)%ilen_gz = 0
          gz_bufs(iloop)%len_gzipped = 0
          allocate(gz_bufs(iloop)%buffer(gz_bufs(iloop)%ilen_gz))
        end if
      end do
!
!        Count data size
      len_gz_pe(1:nprocs_in) = 0
      call MPI_allREDUCE(len_gz_lc, len_gz_pe, nprocs_in,               &
     &    CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      istack_gz_pe(0) = 0
      do ip = 1, nprocs_in
        istack_gz_pe(ip) = istack_gz_pe(ip-1) + len_gz_pe(ip)
      end do
!
!       Write buffer size
      call gz_write_fld_header_mpi(id_mpi_file, ioff_gl,                &
     &     buffer_istack_nod_buffer(nprocs_in, istack_gz_pe))
!
!       Write to file
      do iloop = 1, nloop
        id_rank = my_rank + (iloop-1) * nprocs
        if(id_rank .lt. nprocs_in) then
          ioffset = int(ioff_gl) + istack_gz_pe(id_rank)
          call calypso_mpi_seek_write_chara (id_mpi_file, ioffset,      &
     &       gz_bufs(iloop)%len_gzipped, gz_bufs(iloop)%buffer(1))
        end if
        deallocate(gz_bufs(iloop)%buffer)
      end do
      ioff_gl = ioff_gl + istack_gz_pe(nprocs_in)
!
      end subroutine gz_write_asmbl_fld_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_asmbl_fld_mpi_b(id_mpi_file, nprocs_in,       &
     &          ioff_gl, nloop, fld_IO, gz_bufs)
!
      use field_data_IO
      use m_calypso_mpi_IO
      use gz_field_file_MPI_IO_b
      use gz_field_data_MPI_IO_b
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in
!
      integer(kind = kint), intent(in) :: nloop
      type(field_IO), intent(in) :: fld_IO(nloop)
      type(mul_zlib_buffers), intent(inout) :: gz_bufs(nloop)
!
      integer, intent(in) ::  id_mpi_file
!
      integer(kind = kint_gl) :: istack_gz_pe(0:nprocs_in)
      integer(kind = kint) :: len_gz_pe(nprocs_in)
      integer(kind = kint) :: len_gz_lc(nprocs_in)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      integer(kind = kint) :: id_rank
      integer(kind = kint) :: iloop, ip, ilength
!
!
      len_gz_lc(1:nprocs_in) = 0
!
!        deflate data
      do iloop = 1, nloop
        id_rank = my_rank + (iloop-1) * nprocs
 !
        ilength = fld_IO(iloop)%ntot_comp_IO * fld_IO(iloop)%nnod_IO    &
     &            * kreal
        if(id_rank .lt. nprocs_in) then
          gz_bufs(iloop)%ilen_gz                                        &
     &       = int(real(ilength) * 1.01) + 24
          allocate(gz_bufs(iloop)%buffer(gz_bufs(iloop)%ilen_gz))
 !
          call gzip_defleat_once                                        &
     &       (ilength, fld_IO(iloop)%d_IO, gz_bufs(iloop)%ilen_gz,      &
     &        gz_bufs(iloop)%len_gzipped, gz_bufs(iloop)%buffer)
          len_gz_lc(id_rank+1) =       gz_bufs(iloop)%len_gzipped
        else
          gz_bufs(iloop)%ilen_gz = 0
          gz_bufs(iloop)%len_gzipped = 0
          allocate(gz_bufs(iloop)%buffer(gz_bufs(iloop)%ilen_gz))
        end if
      end do
!
!        Count data size
      len_gz_pe(1:nprocs_in) = 0
      call MPI_allREDUCE(len_gz_lc, len_gz_pe, nprocs_in,               &
     &    CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      istack_gz_pe(0) = 0
      do ip = 1, nprocs_in
        istack_gz_pe(ip) = istack_gz_pe(ip-1) + len_gz_pe(ip)
      end do
!
!       Write data size
      call gz_write_fld_mul_i8head_mpi_b                                &
     &   (id_mpi_file, ioff_gl, nprocs_in, istack_gz_pe(1))
!
!       Write to file
      do iloop = 1, nloop
        id_rank = my_rank + (iloop-1) * nprocs
        if(id_rank .lt. nprocs_in) then
          ioffset = int(ioff_gl) + istack_gz_pe(id_rank)
          call calypso_mpi_seek_write_chara (id_mpi_file, ioffset,      &
     &       gz_bufs(iloop)%len_gzipped, gz_bufs(iloop)%buffer(1))
        end if
        deallocate(gz_bufs(iloop)%buffer)
      end do
      ioff_gl = ioff_gl + istack_gz_pe(nprocs_in)
!
      end subroutine gz_write_asmbl_fld_mpi_b
!
! -----------------------------------------------------------------------
!
      end module t_assembled_field_IO
