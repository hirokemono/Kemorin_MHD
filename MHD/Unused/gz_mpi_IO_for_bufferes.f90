!>@file  gz_mpi_IO_for_bufferes.f90
!!       module gz_mpi_IO_for_bufferes
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2016
!
!> @brief gzipped binary data IO to MPI-IO buffer
!!
!!@verbatim
!!      subroutine gz_mpi_write_int8_vector_mul(IO_param)
!!      subroutine gz_mpi_write_int_vector_mul(IO_param)
!!      subroutine gz_mpi_write_int2d_vector_mul(IO_param)
!!      subroutine gz_mpi_write_1d_vector_mul(IO_param)
!!      subroutine gz_mpi_write_2d_vector_mul(IO_param)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!
!!      subroutine gz_mpi_read_int8_vector_mul(IO_param)
!!      subroutine gz_mpi_read_int_vector_mul(IO_param)
!!      subroutine gz_mpi_read_int2d_vector_mul(IO_param)
!!      subroutine gz_mpi_read_1d_vector_mul(IO_param)
!!      subroutine gz_mpi_read_2d_vector_mul(IO_param)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!@endverbatim
!
      module gz_mpi_IO_for_bufferes
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
!
      use t_calypso_mpi_IO_param
      use t_buffer_4_gzip
      use gz_MPI_binary_head_IO
      use defleat_4_merged_arrays
      use transfer_to_long_integers
!
      type(buffer_4_gzip), allocatable, private :: zbufs(:)
!
      private :: gz_mpi_write_gzip_buffers, gz_mpi_read_gzip_buffers
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gz_mpi_write_int8_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      allocate(zbufs(IO_param%nloop))
      call defleat_int8_vector_mul                                      &
     &   (IO_param%nloop, IO_param%i8_array, zbufs)
      call gz_mpi_write_gzip_buffers(IO_param, zbufs)
      deallocate(zbufs)
!
      end subroutine gz_mpi_write_int8_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_int_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      allocate(zbufs(IO_param%nloop))
      call defleat_int_vector_mul                                       &
     &   (IO_param%nloop, IO_param%i_array, zbufs)
      call gz_mpi_write_gzip_buffers(IO_param, zbufs)
      deallocate(zbufs)
!
      end subroutine gz_mpi_write_int_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_int2d_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      allocate(zbufs(IO_param%nloop))
      call defleat_int2d_vector_mul                                     &
     &   (IO_param%nloop, IO_param%iv_array, zbufs)
      call gz_mpi_write_gzip_buffers(IO_param, zbufs)
      deallocate(zbufs)
!
      end subroutine gz_mpi_write_int2d_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_1d_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      allocate(zbufs(IO_param%nloop))
      call defleat_1d_vector_mul                                        &
     &   (IO_param%nloop, IO_param%r_array, zbufs)
      call gz_mpi_write_gzip_buffers(IO_param, zbufs)
      deallocate(zbufs)
!
      end subroutine gz_mpi_write_1d_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_2d_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      allocate(zbufs(IO_param%nloop))
      call defleat_2d_vector_mul                                        &
     &   (IO_param%nloop, IO_param%v_array, zbufs)
      call gz_mpi_write_gzip_buffers(IO_param, zbufs)
      deallocate(zbufs)
!
      end subroutine gz_mpi_write_2d_vector_mul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_int8_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      allocate(zbufs(IO_param%nloop))
      call gz_mpi_read_gzip_buffers(IO_param, zbufs)
      call infleat_int8_vector_mul(IO_param%iflag_bin_swap,             &
     &    IO_param%nloop, IO_param%i8_array, zbufs)
      deallocate(zbufs)
!
      end subroutine gz_mpi_read_int8_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_int_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      allocate(zbufs(IO_param%nloop))
      call gz_mpi_read_gzip_buffers(IO_param, zbufs)
      call infleat_int_vector_mul(IO_param%iflag_bin_swap,              &
     &    IO_param%nloop, IO_param%i_array, zbufs)
      deallocate(zbufs)
!
      end subroutine gz_mpi_read_int_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_int2d_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      allocate(zbufs(IO_param%nloop))
      call gz_mpi_read_gzip_buffers(IO_param, zbufs)
      call infleat_int2d_vector_mul(IO_param%iflag_bin_swap,            &
     &    IO_param%nloop, IO_param%iv_array, zbufs)
      deallocate(zbufs)
!
      end subroutine gz_mpi_read_int2d_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_1d_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      allocate(zbufs(IO_param%nloop))
      call gz_mpi_read_gzip_buffers(IO_param, zbufs)
      call infleat_1d_vector_mul(IO_param%iflag_bin_swap,               &
     &    IO_param%nloop, IO_param%r_array, zbufs)
      deallocate(zbufs)
!
      end subroutine gz_mpi_read_1d_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_2d_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      allocate(zbufs(IO_param%nloop))
      call gz_mpi_read_gzip_buffers(IO_param, zbufs)
      call infleat_2d_vector_mul(IO_param%iflag_bin_swap,               &
     &    IO_param%nloop, IO_param%v_array, zbufs)
      deallocate(zbufs)
!
      end subroutine gz_mpi_read_2d_vector_mul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_gzip_buffers(IO_param, zbuffers)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(buffer_4_gzip), intent(inout) :: zbuffers(IO_param%nloop)
!
!
      call set_istack_by_gzip_length(IO_param%nprocs_in,                &
     &   IO_param%nloop, zbuffers, IO_param%istack_merged)
!
      call gz_mpi_write_merged_stack_b(IO_param,                        &
     &    IO_param%nprocs_in, IO_param%istack_merged)
      call mpi_write_gzip_array_mul                                     &
     &   (IO_param%id_file, IO_param%nprocs_in, IO_param%nloop,         &
     &    IO_param%ioff_gl, IO_param%istack_merged, zbuffers)
!
      end subroutine gz_mpi_write_gzip_buffers
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_gzip_buffers(IO_param, zbuffers)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(buffer_4_gzip), intent(inout) :: zbuffers(IO_param%nloop)
!
!
      call gz_mpi_read_merged_stack_b(IO_param,                         &
     &    IO_param%nprocs_in, IO_param%istack_merged)
      call mpi_read_gzip_array_mul                                      &
     &   (IO_param%id_file, IO_param%nprocs_in, IO_param%nloop,         &
     &    IO_param%ioff_gl, IO_param%istack_merged, zbuffers)
!
      end subroutine gz_mpi_read_gzip_buffers
!
! -----------------------------------------------------------------------
!
      end module gz_mpi_IO_for_bufferes
