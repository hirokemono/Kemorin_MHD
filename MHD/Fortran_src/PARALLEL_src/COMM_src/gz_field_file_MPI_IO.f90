!>@file  gz_field_file_MPI_IO.f90
!!       module gz_field_file_MPI_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged VTK file usgin MPI-IO
!!
!!@verbatim
!!      subroutine write_gz_step_field_file_mpi                         &
!!     &         (file_name, nprocs_in, id_rank, fld_IO)
!!
!!      subroutine write_field_head_gz_mpi(id_fld, nprocs_in,           &
!!     &          ioff_gl, num_field, ncomp_field, istack_merged)
!!
!!      subroutine read_step_field_file_gz_mpi                          &
!!     &         (file_name, nprocs_in, id_rank, fld_IO)
!!      subroutine read_alloc_stp_fld_file_gz_mpi                       &
!!     &         (file_name, nprocs_in, id_rank, fld_IO)
!!
!!      subroutine read_alloc_stp_fld_head_gz_mpi                       &
!!     &         (file_name, nprocs_in, id_rank, fld_IO)
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
!!      7.2   List of data size (Byte, after compressed)
!!      7.3   Field data
!!@endverbatim
!
      module gz_field_file_MPI_IO
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use m_time_data_IO
      use t_field_data_IO
!
      implicit none
!
      private :: write_field_data_gz_mpi, read_field_num_gz_mpi
      private :: read_field_header_gz_mpi, read_field_data_gz_mpi
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_gz_step_field_file_mpi                           &
     &         (file_name, nprocs_in, id_rank, fld_IO)
!
      character(len=kchara), intent(in) :: file_name
!
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      type(field_IO), intent(in) :: fld_IO
!
      integer :: id_fld
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'Write mergend gzipped ascii data: ', trim(file_name)
      call calypso_mpi_write_file_open(file_name, nprocs_in, id_fld)
!
      if(id_rank .lt. nprocs_in) then
        ioff_gl = 0
        call write_field_head_gz_mpi                                    &
     &     (id_fld, nprocs_in, ioff_gl, fld_IO%num_field_IO,            &
     &      fld_IO%num_comp_IO,  fld_IO%istack_numnod_IO)
!
        call write_field_data_gz_mpi(id_fld, ioff_gl,                   &
     &      fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,   &
     &      fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO)
      end if
!
      call calypso_close_mpi_file(id_fld)
!
      end subroutine write_gz_step_field_file_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_step_field_file_gz_mpi                            &
     &         (file_name, nprocs_in, id_rank, fld_IO)
!
      use gz_field_data_IO
      use skip_gz_comment
      use field_file_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
      integer(kind=kint), intent(in) :: id_rank
      integer(kind=kint), intent(in) :: nprocs_in
      type(field_IO), intent(inout) :: fld_IO
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'Read mergend gzipped ascii data: ', trim(file_name)
      call open_rd_gzfile(file_name)
!
      call read_field_step_gz_mpi(nprocs_in, id_rank)
      call sync_field_time_mpi
!
      call alloc_merged_field_stack(nprocs_in, fld_IO)
!
      call read_field_header_gz_mpi(nprocs_in, id_rank,                 &
     &    fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
      call sync_field_header_mpi(nprocs_in, id_rank, fld_IO%nnod_IO,    &
     &      fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
!
      call read_field_num_gz_mpi(nprocs_in, id_rank,                    &
     &    fld_IO%num_field_IO, fld_IO%num_comp_IO)
      call sync_field_comp_mpi(fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call read_field_data_gz_mpi                                       &
     &     (nprocs_in, id_rank, fld_IO%nnod_IO, fld_IO%num_field_IO,    &
     &      fld_IO%ntot_comp_IO, fld_IO%num_comp_IO, fld_IO%fld_name,   &
     &      fld_IO%d_IO, fld_IO%istack_numnod_IO)
      call sync_field_names_mpi(fld_IO%num_field_IO, fld_IO%fld_name)
!
      call dealloc_merged_field_stack(fld_IO)
      call close_gzfile
!
      end subroutine read_step_field_file_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_stp_fld_file_gz_mpi                         &
     &         (file_name, nprocs_in, id_rank, fld_IO)
!
      use gz_field_data_IO
      use skip_gz_comment
      use field_file_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
      integer(kind=kint), intent(in) :: id_rank
      integer(kind=kint), intent(in) :: nprocs_in
      type(field_IO), intent(inout) :: fld_IO
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'Read mergend gzipped ascii data: ', trim(file_name)
      call open_rd_gzfile(file_name)
!
      call read_field_step_gz_mpi(nprocs_in, id_rank)
      call sync_field_time_mpi
!
      call alloc_merged_field_stack(nprocs_in, fld_IO)
!
      call read_field_header_gz_mpi(nprocs_in, id_rank,                 &
     &      fld_IO%nnod_IO, fld_IO%num_field_IO,                        &
     &      fld_IO%istack_numnod_IO)
      call sync_field_header_mpi(nprocs_in, id_rank, fld_IO%nnod_IO,    &
     &      fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
!
      call alloc_phys_name_IO(fld_IO)
!
      call read_field_num_gz_mpi(nprocs_in, id_rank,                    &
     &    fld_IO%num_field_IO, fld_IO%num_comp_IO)
      call sync_field_comp_mpi(fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      call read_field_data_gz_mpi                                       &
     &     (nprocs_in, id_rank, fld_IO%nnod_IO, fld_IO%num_field_IO,    &
     &      fld_IO%ntot_comp_IO, fld_IO%num_comp_IO, fld_IO%fld_name,   &
     &      fld_IO%d_IO, fld_IO%istack_numnod_IO)
      call sync_field_names_mpi(fld_IO%num_field_IO, fld_IO%fld_name)
!
      call close_gzfile
!
      call dealloc_merged_field_stack(fld_IO)
      if(id_rank .ge. nprocs_in) then
        call dealloc_phys_data_IO(fld_IO)
        call dealloc_phys_name_IO(fld_IO)
      end if
!
      end subroutine read_alloc_stp_fld_file_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_stp_fld_head_gz_mpi                         &
     &         (file_name, nprocs_in, id_rank, fld_IO)
!
      use gz_field_data_IO
      use skip_gz_comment
      use field_file_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
      integer(kind=kint), intent(in) :: id_rank
      integer(kind=kint), intent(in) :: nprocs_in
      type(field_IO), intent(inout) :: fld_IO
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'Read mergend gzipped ascii data: ', trim(file_name)
      call open_rd_gzfile(file_name)
!
      call read_field_step_gz_mpi(nprocs_in, id_rank)
      call sync_field_time_mpi
!
      call alloc_merged_field_stack(nprocs_in, fld_IO)
!
      call read_field_header_gz_mpi(nprocs_in, id_rank,                 &
     &      fld_IO%nnod_IO, fld_IO%num_field_IO,                        &
     &      fld_IO%istack_numnod_IO)
      call sync_field_header_mpi(nprocs_in, id_rank, fld_IO%nnod_IO,    &
     &      fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
!
      call alloc_phys_name_IO(fld_IO)
!
      call read_field_num_gz_mpi(nprocs_in, id_rank,                    &
     &    fld_IO%num_field_IO, fld_IO%num_comp_IO)
      call sync_field_comp_mpi(fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
!
      call close_gzfile
!
      call dealloc_merged_field_stack(fld_IO)
      if(id_rank .ge. nprocs_in) then
        call dealloc_phys_name_IO(fld_IO)
      end if
!
      end subroutine read_alloc_stp_fld_head_gz_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_field_head_gz_mpi(id_fld, nprocs_in,             &
     &          ioff_gl, num_field, ncomp_field, istack_merged)
!
      use m_phys_constants
      use field_data_IO
      use gz_field_data_MPI_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      integer(kind = kint), intent(in) :: nprocs_in
      integer(kind=kint), intent(in) :: num_field
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
!
      integer, intent(in) ::  id_fld
!
!
      call gz_write_fld_header_mpi                                      &
     &   (id_fld, ioff_gl, step_data_buffer(nprocs_in-1))
      call gz_write_fld_header_mpi(id_fld, ioff_gl,                     &
     &    field_istack_nod_buffer(nprocs_in, istack_merged))
      call gz_write_fld_header_mpi(id_fld, ioff_gl,                     &
     &    field_num_buffer(num_field))
      call gz_write_fld_header_mpi                                      &
     &   (id_fld, ioff_gl, field_comp_buffer(num_field, ncomp_field))
!
      end subroutine write_field_head_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine write_field_data_gz_mpi(id_fld, ioff_gl,               &
     &           nnod, num_field, ntot_comp, ncomp_field,               &
     &           field_name, d_nod)
!
      use m_phys_constants
      use field_data_IO
      use gz_field_data_MPI_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind=kint), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint) :: j, icou
!
!
      icou = 1
      do j = 1, num_field
        call gz_write_fld_header_mpi                                    &
     &     (id_fld, ioff_gl, each_field_name_buffer(field_name(j)))
        call gz_write_fld_vecotr_mpi(id_fld, ioff_gl,                   &
     &      nnod, ncomp_field(j), d_nod(1,icou))
        icou = icou + ncomp_field(j)
      end do
!
      end subroutine write_field_data_gz_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_step_gz_mpi(nprocs_in, id_rank)
!
      use gz_field_data_IO
      use m_error_IDs
!
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      integer(kind=kint) :: iread
!
!
      if(id_rank .ge. nprocs_in) return
      call read_gz_step_data(iread)
      if(my_rank.eq.0 .and. nprocs_in .ne.(iread + 1)) then
        call calypso_mpi_abort(ierr_fld, 'Set correct field data file')
      end if
!
      end subroutine read_field_step_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_field_header_gz_mpi                               &
     &         (nprocs_in, id_rank, nnod, num_field, istack_merged)
!
      use m_phys_constants
      use field_data_IO
      use gz_field_data_MPI_IO
      use skip_gz_comment
!
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      integer(kind=kint), intent(inout) :: nnod, num_field
      integer(kind = kint_gl), intent(inout)                            &
     &                       :: istack_merged(0:nprocs_in)
!
!
      if(id_rank .ge. nprocs_in) return
      istack_merged(0)  = 0
      call get_one_line_from_gz_f
      call read_gz_multi_int8(nprocs_in, istack_merged(1))
      nnod = int(istack_merged(id_rank+1) - istack_merged(id_rank))
!
      call skip_gz_comment_int(num_field)
!
      end subroutine read_field_header_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_field_num_gz_mpi(nprocs_in, id_rank,              &
     &          num_field, ncomp_field)
!
      use skip_gz_comment
!
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      integer(kind=kint), intent(in) :: num_field
      integer(kind=kint), intent(inout) :: ncomp_field(num_field)
!
!
      if(id_rank .ge. nprocs_in) return
      call read_gz_multi_int(num_field, ncomp_field)
!
      end subroutine read_field_num_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_field_data_gz_mpi                                 &
     &         (nprocs_in, id_rank, nnod, num_field, ntot_comp,         &
     &          ncomp_field, field_name, d_nod, istack_merged)
!
      use m_phys_constants
      use gz_field_data_MPI_IO
      use skip_gz_comment
!
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      integer(kind=kint), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(inout) :: field_name(num_field)
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_comp)
!
      integer(kind = kint) :: j, icou, itmp
!
!
      if(id_rank .ge. nprocs_in) return
      icou = 1
      do j = 1, num_field
        call skip_gz_comment_chara(field_name(j))
        call skip_gz_comment_int(itmp)
        call gz_read_fld_vecotr_mpi(nprocs_in, id_rank,                 &
     &      nnod, ncomp_field(j), d_nod(1,icou), istack_merged)
        icou = icou + ncomp_field(j)
      end do
!
      end subroutine read_field_data_gz_mpi
!
! -----------------------------------------------------------------------
!
      end module gz_field_file_MPI_IO
