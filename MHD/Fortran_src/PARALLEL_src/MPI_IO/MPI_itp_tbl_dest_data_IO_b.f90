!>@file   MPI_itp_tbl_dest_data_IO_b.f90
!!       module MPI_itp_tbl_dest_data_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in Sep. 2012
!!
!> @brief gzipped data IO for interpolation
!!
!!@verbatim
!!      subroutine mpi_write_itp_domain_dest_b(IO_param, IO_itp_dest)
!!      subroutine mpi_write_itp_table_dest_b(IO_param, IO_itp_dest)
!!      subroutine mpi_write_itp_idx_dest_b                             &
!!     &         (IO_param, IO_itp_dest, IO_itp_c_dest)
!!      subroutine mpi_write_itp_coefs_dest_b                           &
!!     &         (IO_param, IO_itp_dest, IO_itp_c_dest)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(interpolate_table_dest), intent(in) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine mpi_read_itp_domain_dest_b(IO_param, IO_itp_dest)
!!      subroutine mpi_read_itp_table_dest_b(IO_param, IO_itp_dest)
!!      subroutine mpi_read_itp_idx_dest_b                              &
!!     &         (IO_param, IO_itp_dest, IO_itp_c_dest)
!!      subroutine mpi_read_itp_coefs_dest_b                            &
!!     &         (IO_param, IO_itp_dest, IO_itp_c_dest)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module MPI_itp_tbl_dest_data_IO_b
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
      use t_calypso_mpi_IO_param
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine mpi_write_itp_domain_dest_b(IO_param, IO_itp_dest)
!
      use MPI_binary_head_IO
      use MPI_binary_data_IO
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
!
      integer(kind = kint_gl) :: num64
!
!
      call mpi_write_process_id_b(IO_param)
      call mpi_write_one_integer_b                                      &
     &   (IO_param, IO_itp_dest%num_org_domain)
!
      num64 = cast_long(IO_itp_dest%num_org_domain)
      call istack64_4_parallel_data(num64, IO_param)
      call mpi_write_int_vector_b(IO_param, num64,                      &
     &    IO_itp_dest%id_org_domain)
!
      end subroutine mpi_write_itp_domain_dest_b
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_domain_dest_b(IO_param, IO_itp_dest)
!
      use MPI_binary_head_IO
      use MPI_binary_data_IO
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
!
      call mpi_read_process_id_b(IO_param)
      call mpi_read_one_integer_b                                       &
     &   (IO_param, IO_itp_dest%num_org_domain)
!
      call alloc_itp_num_dest(IO_itp_dest)
!
      call mpi_read_int_vector_b(IO_param,                              &
     &    cast_long(IO_itp_dest%num_org_domain),                        &
     &    IO_itp_dest%id_org_domain)
!
      end subroutine mpi_read_itp_domain_dest_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine mpi_write_itp_table_dest_b(IO_param, IO_itp_dest)
!
      use MPI_binary_data_IO
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
!
      integer(kind = kint_gl) :: num64
!
!
      call mpi_write_integer_stack_b(IO_param,                          &
     &    cast_long(IO_itp_dest%num_org_domain),                        &
     &    IO_itp_dest%istack_nod_tbl_dest)
!
      num64 = cast_long(IO_itp_dest%ntot_table_dest)
      call istack64_4_parallel_data(num64, IO_param)
      call mpi_write_int_vector_b(IO_param, num64,                      &
     &    IO_itp_dest%inod_dest_4_dest)
!
      end subroutine mpi_write_itp_table_dest_b
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_table_dest_b(IO_param, IO_itp_dest)
!
      use MPI_binary_data_IO
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
!
      call mpi_read_integer_stack_b(IO_param,                           &
     &    cast_long(IO_itp_dest%num_org_domain),                        &
     &    IO_itp_dest%istack_nod_tbl_dest, IO_itp_dest%ntot_table_dest)
!
      call alloc_itp_table_dest(IO_itp_dest)
      call mpi_read_int_vector_b(IO_param,                              &
     &    cast_long(IO_itp_dest%ntot_table_dest),                       &
     &    IO_itp_dest%inod_dest_4_dest)
!
      end subroutine mpi_read_itp_table_dest_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine mpi_write_itp_idx_dest_b                               &
     &         (IO_param, IO_itp_dest, IO_itp_c_dest)
!
      use MPI_binary_data_IO
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
!
      integer(kind = kint) :: num_tmp
      integer(kind = kint_gl) :: num64
!
      num_tmp = 4*IO_itp_dest%num_org_domain
      call mpi_write_integer_stack_b(IO_param, cast_long(num_tmp),      &
     &    IO_itp_c_dest%istack_nod_tbl_wtype_dest)
!
      num64 = cast_long(IO_itp_dest%ntot_table_dest)
      call istack64_4_parallel_data(num64, IO_param)
      call mpi_write_int_vector_b(IO_param,                             &
     &    cast_long(IO_itp_dest%ntot_table_dest),                       &
     &    IO_itp_c_dest%iele_org_4_dest)
!
      call istack64_4_parallel_data(num64, IO_param)
      call mpi_write_int_vector_b(IO_param,                             &
     &    cast_long(IO_itp_dest%ntot_table_dest),                       &
     &    IO_itp_c_dest%itype_inter_dest)
!
      call istack64_4_parallel_data(num64, IO_param)
      call mpi_write_int8_vector_b                                      &
     &   (IO_param, cast_long(IO_itp_dest%ntot_table_dest),             &
     &    IO_itp_c_dest%inod_gl_dest)
!
      end subroutine mpi_write_itp_idx_dest_b
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_idx_dest_b                                &
     &         (IO_param, IO_itp_dest, IO_itp_c_dest)
!
      use MPI_binary_data_IO
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      integer(kind = kint) :: num_tmp
!
!
      call alloc_itp_coef_stack                                         &
     &     (IO_itp_dest%num_org_domain, IO_itp_c_dest)
!
      num_tmp = 4*IO_itp_dest%num_org_domain
      call mpi_read_integer_stack_b(IO_param, cast_long(num_tmp),       &
     &    IO_itp_c_dest%istack_nod_tbl_wtype_dest,                      &
     &    IO_itp_dest%ntot_table_dest)
!
      call alloc_itp_coef_dest(IO_itp_dest, IO_itp_c_dest)
!
      call mpi_read_int_vector_b(IO_param,                              &
     &    cast_long(IO_itp_dest%ntot_table_dest),                       &
     &    IO_itp_c_dest%iele_org_4_dest)
!
      call mpi_read_int_vector_b(IO_param,                              &
     &    cast_long(IO_itp_dest%ntot_table_dest),                       &
     &    IO_itp_c_dest%itype_inter_dest)
!
      call mpi_read_int8_vector_b                                       &
     &   (IO_param, cast_long(IO_itp_dest%ntot_table_dest),             &
     &    IO_itp_c_dest%inod_gl_dest)
!
      end subroutine mpi_read_itp_idx_dest_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine mpi_write_itp_coefs_dest_b                             &
     &         (IO_param, IO_itp_dest, IO_itp_c_dest)
!
      use MPI_binary_data_IO
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
!
!
      call mpi_write_2d_vector_b                                        &
     &   (IO_param, cast_long(IO_itp_dest%ntot_table_dest), ithree,     &
     &    IO_itp_c_dest%coef_inter_dest)
!
      end subroutine mpi_write_itp_coefs_dest_b
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_coefs_dest_b                              &
     &         (IO_param, IO_itp_dest, IO_itp_c_dest)
!
      use MPI_binary_data_IO
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
!
      call mpi_read_2d_vector_b                                         &
     &   (IO_param, cast_long(IO_itp_dest%ntot_table_dest), ithree,     &
     &    IO_itp_c_dest%coef_inter_dest)
!
      end subroutine mpi_read_itp_coefs_dest_b
!
!-----------------------------------------------------------------------
!
      end module MPI_itp_tbl_dest_data_IO_b
