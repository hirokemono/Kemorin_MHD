!>@file  MPI_IO_array_bufferes_b.f90
!!       module MPI_IO_array_bufferes_b
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2016
!
!> @brief gzipped binary data IO to MPI-IO buffer
!!
!!@verbatim
!!      subroutine mpi_write_int8_vector_mul(IO_param)
!!      subroutine mpi_write_int_vector_mul(IO_param)
!!      subroutine mpi_write_int2d_vector_mul(IO_param)
!!      subroutine mpi_write_1d_vector_mul(IO_param)
!!      subroutine mpi_write_2d_vector_mul(IO_param)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!
!!      subroutine mpi_read_int8_vector_mul(IO_param)
!!      subroutine mpi_read_int_vector_mul(IO_param)
!!      subroutine mpi_read_int2d_vector_mul(IO_param)
!!      subroutine mpi_read_1d_vector_mul(IO_param)
!!      subroutine mpi_read_2d_vector_mul(IO_param)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!@endverbatim
!
      module MPI_IO_array_bufferes_b
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
!
      use t_calypso_mpi_IO_param
      use MPI_binery_IO_4_buffers
      use MPI_binary_head_IO
      use defleat_4_merged_arrays
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_write_int8_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call set_istack_by_i8_buffer(IO_param%nprocs_in,                  &
     &    IO_param%nloop, IO_param%i8_array, IO_param%istack_merged)
!
      call mpi_write_mul_int8head_b                                     &
     &   (IO_param, IO_param%nprocs_in, IO_param%istack_merged)
      call mpi_write_i8_vect_mul_b                                      &
     &   (IO_param%id_file, IO_param%nprocs_in, IO_param%nloop,         &
     &    IO_param%ioff_gl, IO_param%istack_merged, IO_param%i8_array)
!
      end subroutine mpi_write_int8_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_int_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call set_istack_by_int_buffer(IO_param%nprocs_in,                 &
     &    IO_param%nloop, IO_param%i_array, IO_param%istack_merged)
!
      call mpi_write_mul_int8head_b                                     &
     &   (IO_param, IO_param%nprocs_in, IO_param%istack_merged)
      call mpi_write_intvect_mul_b                                      &
     &   (IO_param%id_file, IO_param%nprocs_in, IO_param%nloop,         &
     &    IO_param%ioff_gl, IO_param%istack_merged, IO_param%i_array)
!
      end subroutine mpi_write_int_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_int2d_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call set_istack_by_int2d_buffer(IO_param%nprocs_in,               &
     &    IO_param%nloop, IO_param%iv_array, IO_param%istack_merged)
!
      call mpi_write_mul_int8head_b                                     &
     &   (IO_param, IO_param%nprocs_in, IO_param%istack_merged)
      call mpi_write_i2dvect_mul_b                                      &
     &   (IO_param%id_file, IO_param%nprocs_in, IO_param%nloop,         &
     &    IO_param%ioff_gl, IO_param%istack_merged, IO_param%iv_array)
!
      end subroutine mpi_write_int2d_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_1d_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call set_istack_by_real_buffer(IO_param%nprocs_in,                &
     &    IO_param%nloop, IO_param%r_array, IO_param%istack_merged)
!
      call mpi_write_mul_int8head_b                                     &
     &   (IO_param, IO_param%nprocs_in, IO_param%istack_merged)
      call mpi_read_realvect_mul_b                                      &
     &   (IO_param%id_file, IO_param%nprocs_in, IO_param%nloop,         &
     &    IO_param%ioff_gl, IO_param%istack_merged, IO_param%r_array)
!
      end subroutine mpi_write_1d_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_2d_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call set_istack_by_vector_buffer(IO_param%nprocs_in,              &
     &    IO_param%nloop, IO_param%v_array, IO_param%istack_merged)
!
      call mpi_write_mul_int8head_b                                     &
     &   (IO_param, IO_param%nprocs_in, IO_param%istack_merged)
      call mpi_write_r2dvect_mul_b                                      &
     &   (IO_param%id_file, IO_param%nprocs_in, IO_param%nloop,         &
     &    IO_param%ioff_gl, IO_param%istack_merged, IO_param%v_array)
!
      end subroutine mpi_write_2d_vector_mul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_read_int8_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call set_istack_by_i8_buffer(IO_param%nprocs_in,                  &
     &    IO_param%nloop, IO_param%i8_array, IO_param%istack_merged)
      IO_param%ioff_gl = IO_param%ioff_gl + kint_gl*IO_param%nprocs_in
!
      call mpi_read_i8_vect_mul_b                                       &
     &   (IO_param%id_file, IO_param%nprocs_in, IO_param%nloop,         &
     &    IO_param%ioff_gl, IO_param%istack_merged, IO_param%i8_array)
!
      end subroutine mpi_read_int8_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_int_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call set_istack_by_int_buffer(IO_param%nprocs_in,                 &
     &    IO_param%nloop, IO_param%i_array, IO_param%istack_merged)
      IO_param%ioff_gl = IO_param%ioff_gl + kint_gl*IO_param%nprocs_in
!
      call mpi_read_intvect_mul_b                                       &
     &   (IO_param%id_file, IO_param%nprocs_in, IO_param%nloop,         &
     &    IO_param%ioff_gl, IO_param%istack_merged, IO_param%i_array)
!
      end subroutine mpi_read_int_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_int2d_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call set_istack_by_int2d_buffer(IO_param%nprocs_in,               &
     &    IO_param%nloop, IO_param%iv_array, IO_param%istack_merged)
      IO_param%ioff_gl = IO_param%ioff_gl + kint_gl*IO_param%nprocs_in
!
      call mpi_read_i2dvect_mul_b                                       &
     &   (IO_param%id_file, IO_param%nprocs_in, IO_param%nloop,         &
     &    IO_param%ioff_gl, IO_param%istack_merged, IO_param%iv_array)
!
      end subroutine mpi_read_int2d_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_1d_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call set_istack_by_real_buffer(IO_param%nprocs_in,                &
     &    IO_param%nloop, IO_param%r_array, IO_param%istack_merged)
      IO_param%ioff_gl = IO_param%ioff_gl + kint_gl*IO_param%nprocs_in
!
      call mpi_write_realvect_mul_b                                     &
     &   (IO_param%id_file, IO_param%nprocs_in, IO_param%nloop,         &
     &    IO_param%ioff_gl, IO_param%istack_merged, IO_param%r_array)
!
      end subroutine mpi_read_1d_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_2d_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call set_istack_by_vector_buffer(IO_param%nprocs_in,              &
     &    IO_param%nloop, IO_param%v_array, IO_param%istack_merged)
      IO_param%ioff_gl = IO_param%ioff_gl + kint_gl*IO_param%nprocs_in
!
      call mpi_read_r2dvect_mul_b                                       &
     &   (IO_param%id_file, IO_param%nprocs_in, IO_param%nloop,         &
     &    IO_param%ioff_gl, IO_param%istack_merged, IO_param%v_array)
!
      end subroutine mpi_read_2d_vector_mul
!
! -----------------------------------------------------------------------
!
      end module MPI_IO_array_bufferes_b
