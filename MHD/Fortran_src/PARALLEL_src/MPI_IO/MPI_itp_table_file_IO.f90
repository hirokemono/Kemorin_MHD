!>@file   MPI_itp_table_file_IO.f90
!!@brief  module MPI_itp_table_file_IO
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2006
!
!>@brief ASCII Interpolation table file IO
!!
!!@verbatim
!!      subroutine mpi_write_itp_table_file_a                           &
!!     &         (file_name, my_rankt, itp_tbl_IO)
!!      subroutine mpi_read_itp_table_file_a                            &
!!     &         (file_name, id_rank, num_pe, itp_tbl_IO, ierr)
!!        type(interpolate_table), intent(inout) :: itp_tbl_IO
!!
!!      subroutine mpi_wrt_itp_coefs_dest_file_a                        &
!!     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest)
!!      subroutine mpi_read_itp_coefs_dest_file_a                       &
!!     &         (file_name, id_rank, num_pe,                           &
!!     &          IO_itp_dest, IO_itp_c_dest, ierr)
!!      subroutine mpi_read_itp_table_dest_file_a                       &
!!     &         (file_name, id_rank, num_pe, IO_itp_dest, ierr)
!!      subroutine mpi_read_itp_domain_dest_file_a                      &
!!     &         (file_name, id_rank, num_pe, IO_itp_dest, ierr)
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!!@endverbatim
!
      module MPI_itp_table_file_IO
!
      use m_precision
      use m_error_IDs
!
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
      use t_calypso_mpi_IO_param
!
      implicit none
!
      type(calypso_MPI_IO_params), save, private :: IO_param
      integer(kind = kint), parameter :: id_tbl_file = 19
      private :: id_tbl_file
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine mpi_write_itp_table_file_a                             &
     &         (file_name, id_rank, itp_tbl_IO)
!
      use MPI_itp_table_data_IO
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(interpolate_table), intent(inout) :: itp_tbl_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write ascii interpolation file: ', trim(file_name)
!
      call open_write_mpi_file(file_name, IO_param)
      call mpi_write_itp_domain_dest(IO_param, itp_tbl_IO%tbl_dest)
!      call mpi_write_itp_table_dest(id_tbl_file, itp_tbl_IO%tbl_dest)
!
!      call mpi_write_itp_domain_org(IO_param, itp_tbl_IO%tbl_org)
!      call mpi_write_itp_table_org(IO_param, itp_tbl_IO%tbl_org)
!      call mpi_write_itp_coefs_org(IO_param, itp_tbl_IO%tbl_org)
!
      call close_mpi_file(IO_param)
!
      if (itp_tbl_IO%tbl_org%num_dest_domain .gt. 0) then
        call dealloc_itp_table_org(itp_tbl_IO%tbl_org)
        call dealloc_itp_num_org(itp_tbl_IO%tbl_org)
      end if
!
      if (itp_tbl_IO%tbl_dest%num_org_domain .gt. 0) then
        call dealloc_itp_table_dest(itp_tbl_IO%tbl_dest)
      end if
      call dealloc_itp_num_dest(itp_tbl_IO%tbl_dest)
!
      end subroutine mpi_write_itp_table_file_a
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_table_file_a                              &
     &         (file_name, id_rank, num_pe, itp_tbl_IO, ierr)
!
      use MPI_itp_table_data_IO
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank, num_pe
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: n_rank_file
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read ascii interpolation file: ', trim(file_name)
!
      call open_read_mpi_file(file_name, num_pe, id_rank, IO_param)
!        write(*,*) 'mpi_read_itp_domain_dest', trim(file_name)
      call mpi_read_itp_domain_dest(IO_param, itp_tbl_IO%tbl_dest)
!        write(*,*) 'mpi_read_itp_table_dest'
!      call mpi_read_itp_table_dest(IO_param, itp_tbl_IO%tbl_dest)
!
!        write(*,*) 'mpi_read_itp_domain_org'
!      call mpi_read_itp_domain_org                                     &
!     &   (IO_param, n_rank_file, itp_tbl_IO%tbl_org)
!        write(*,*) 'mpi_read_itp_table_org'
!      call mpi_read_itp_table_org(IO_param, itp_tbl_IO%tbl_org)
!        write(*,*) 'mpi_read_itp_coefs_org'
!      call mpi_read_itp_coefs_org(IO_param, itp_tbl_IO%tbl_org)
!
      call close_mpi_file(IO_param)
!
      ierr = 0
!
      end subroutine mpi_read_itp_table_file_a
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine mpi_wrt_itp_coefs_dest_file_a                          &
     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest)
!
      use MPI_itp_table_data_IO
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write ascii export coefs file: ', trim(file_name)
!
      call open_write_mpi_file(file_name, IO_param)
      call mpi_write_itp_domain_dest(IO_param, IO_itp_dest)
!      call mpi_write_itp_table_dest(IO_param, id_rank, IO_itp_dest)
!      call mpi_write_itp_coefs_dest                                    &
!     &   (IO_param, IO_itp_dest, IO_itp_c_dest)
      call close_mpi_file(IO_param)
!
      if (IO_itp_dest%num_org_domain .gt. 0) then
        call dealloc_itp_coef_dest(IO_itp_c_dest)
        call dealloc_itp_coef_stack(IO_itp_c_dest)
        call dealloc_itp_table_dest(IO_itp_dest)
      end if
      call dealloc_itp_num_dest(IO_itp_dest)
!
      end subroutine mpi_wrt_itp_coefs_dest_file_a
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_coefs_dest_file_a                         &
     &         (file_name, id_rank, num_pe,                             &
     &          IO_itp_dest, IO_itp_c_dest, ierr)
!
      use MPI_itp_table_data_IO
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank, num_pe
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read ascii export coefs file: ', trim(file_name)
!
      call open_read_mpi_file(file_name, num_pe, id_rank, IO_param)
      call mpi_read_itp_domain_dest(IO_param, IO_itp_dest)
!      call mpi_read_itp_table_dest(IO_param, IO_itp_dest)
!      call mpi_read_itp_coefs_dest                                     &
!     &   (IO_param, IO_itp_dest, IO_itp_c_dest)
      call close_mpi_file(IO_param)
!
      ierr = 0
!
      end subroutine mpi_read_itp_coefs_dest_file_a
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_table_dest_file_a                         &
     &         (file_name, id_rank, num_pe, IO_itp_dest, ierr)
!
      use MPI_itp_table_data_IO
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank, num_pe
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read ascii interapolate export file: ', trim(file_name)
!
      call open_read_mpi_file(file_name, num_pe, id_rank, IO_param)
      call mpi_read_itp_domain_dest(IO_param, IO_itp_dest)
!      call mpi_read_itp_table_dest(IO_param, IO_itp_dest)
      call close_mpi_file(IO_param)
!
      ierr = 0
!
      end subroutine mpi_read_itp_table_dest_file_a
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_domain_dest_file_a                        &
     &         (file_name, id_rank, num_pe, IO_itp_dest, ierr)
!
      use MPI_itp_table_data_IO
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank, num_pe
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read ascii export domain file: ', trim(file_name)
!
      call open_read_mpi_file(file_name, num_pe, id_rank, IO_param)
      call mpi_read_itp_domain_dest(IO_param, IO_itp_dest)
      call close_mpi_file(IO_param)
!
      ierr = 0
!
      end subroutine mpi_read_itp_domain_dest_file_a
!
!-----------------------------------------------------------------------
!
      end module MPI_itp_table_file_IO
