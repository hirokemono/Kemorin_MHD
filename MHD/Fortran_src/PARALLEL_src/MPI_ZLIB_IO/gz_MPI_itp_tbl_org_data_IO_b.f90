!>@file  gz_MPI_itp_tbl_org_data_IO_b.f90
!!       module gz_MPI_itp_tbl_org_data_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in Sep. 2012
!!
!> @brief gzipped data IO for interpolation
!!
!!@verbatim
!!      subroutine gz_mpi_write_itp_domain_org_b(IO_param, IO_itp_org)
!!      subroutine gz_mpi_write_itp_table_org_b(IO_param, IO_itp_org)
!!      subroutine gz_mpi_write_itp_index_org_b(IO_param, IO_itp_org)
!!      subroutine gz_mpi_write_itp_coefs_org_b(IO_param, IO_itp_org)
!!        type(interpolate_table_org), intent(in) :: IO_itp_org
!!
!!      subroutine gz_mpi_read_itp_domain_org_b(IO_param, IO_itp_org)
!!      subroutine gz_mpi_read_itp_table_org_b(IO_param, IO_itp_org)
!!      subroutine gz_mpi_read_itp_index_org_b(IO_param, IO_itp_org)
!!      subroutine gz_mpi_read_itp_coefs_org_b(IO_param, IO_itp_org)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(interpolate_table_org), intent(inout) :: IO_itp_org
!!@endverbatim
!
      module gz_MPI_itp_tbl_org_data_IO_b
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
      subroutine gz_mpi_write_itp_domain_org_b(IO_param, IO_itp_org)
!
      use gz_MPI_binary_datum_IO
      use gz_MPI_binary_data_IO
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_org), intent(in) :: IO_itp_org
!
!
      call gz_mpi_write_one_integer_b                                   &
     &   (IO_param, IO_itp_org%num_dest_domain)
      call gz_mpi_write_int_vector_b(IO_param,                          &
     &    cast_long(IO_itp_org%num_dest_domain),                        &
     &    IO_itp_org%id_dest_domain)
!
      end subroutine gz_mpi_write_itp_domain_org_b
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_read_itp_domain_org_b(IO_param, IO_itp_org)
!
      use gz_MPI_binary_datum_IO
      use gz_MPI_binary_data_IO
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_org), intent(inout) :: IO_itp_org
!
!
      call gz_mpi_read_one_integer_b                                    &
     &   (IO_param, IO_itp_org%num_dest_domain)
      call alloc_itp_num_org(np_smp, IO_itp_org)
!
      call gz_mpi_read_int_vector_b(IO_param,                           &
     &    cast_long(IO_itp_org%num_dest_domain),                        &
     &    IO_itp_org%id_dest_domain)
!
      end subroutine gz_mpi_read_itp_domain_org_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_write_itp_table_org_b(IO_param, IO_itp_org)
!
      use gz_MPI_binary_data_IO
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_org), intent(in) :: IO_itp_org
!
!
      call gz_mpi_write_integer_stack_b(IO_param,                       &
     &    cast_long(IO_itp_org%num_dest_domain),                        &
     &    IO_itp_org%istack_nod_tbl_org)
      call gz_mpi_write_int_vector_b(IO_param,                          &
     &    cast_long(IO_itp_org%ntot_table_org),                         &
     &    IO_itp_org%inod_itp_send)
!
      end subroutine gz_mpi_write_itp_table_org_b
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_read_itp_table_org_b(IO_param, IO_itp_org)
!
      use gz_MPI_binary_data_IO
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_org), intent(inout) :: IO_itp_org
!
!
      call gz_mpi_read_integer_stack_b(IO_param,                        &
     &    cast_long(IO_itp_org%num_dest_domain),                        &
     &    IO_itp_org%istack_nod_tbl_org, IO_itp_org%ntot_table_org)
!
      call alloc_itp_table_org(IO_itp_org)
      call gz_mpi_read_int_vector_b(IO_param,                           &
     &    cast_long(IO_itp_org%ntot_table_org),                         &
     &    IO_itp_org%inod_itp_send)
!
      end subroutine gz_mpi_read_itp_table_org_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_write_itp_index_org_b(IO_param, IO_itp_org)
!
      use gz_MPI_binary_data_IO
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_org), intent(in) :: IO_itp_org
!
!
      call gz_mpi_write_integer_stack_b(IO_param, cast_long(ifour),     &
     &    IO_itp_org%istack_itp_type_org)
!
      call gz_mpi_write_int_vector_b(IO_param,                          &
     &    cast_long(IO_itp_org%ntot_table_org),                         &
     &    IO_itp_org%iele_org_4_org)
      call gz_mpi_write_int_vector_b(IO_param,                          &
     &    cast_long(IO_itp_org%ntot_table_org),                         &
     &    IO_itp_org%itype_inter_org)
!
      call gz_mpi_write_int8_vector_b                                   &
     &   (IO_param, cast_long(IO_itp_org%ntot_table_org),               &
     &    IO_itp_org%inod_gl_dest_4_org)
!
      end subroutine gz_mpi_write_itp_index_org_b
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_read_itp_index_org_b(IO_param, IO_itp_org)
!
      use gz_MPI_binary_data_IO
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_org), intent(inout) :: IO_itp_org
!
      integer(kind = kint) :: num_tmp
!
!
      call gz_mpi_read_integer_stack_b(IO_param, cast_long(ifour),      &
     &    IO_itp_org%istack_itp_type_org, num_tmp)
!
      call gz_mpi_read_int_vector_b(IO_param,                           &
     &    cast_long(IO_itp_org%ntot_table_org),                         &
     &    IO_itp_org%iele_org_4_org)
!
      call gz_mpi_read_int_vector_b(IO_param,                           &
     &    cast_long(IO_itp_org%ntot_table_org),                         &
     &    IO_itp_org%itype_inter_org)
!
      call gz_mpi_read_int8_vector_b                                    &
     &   (IO_param, cast_long(IO_itp_org%ntot_table_org),               &
     &    IO_itp_org%inod_gl_dest_4_org)
!
      end subroutine gz_mpi_read_itp_index_org_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_write_itp_coefs_org_b(IO_param, IO_itp_org)
!
      use gz_MPI_binary_data_IO
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_org), intent(in) :: IO_itp_org
!
!
      call gz_mpi_write_2d_vector_b                                     &
     &   (IO_param, cast_long(IO_itp_org%ntot_table_org), ithree,       &
     &    IO_itp_org%coef_inter_org)
!
      end subroutine gz_mpi_write_itp_coefs_org_b
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_read_itp_coefs_org_b(IO_param, IO_itp_org)
!
      use gz_MPI_binary_data_IO
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_org), intent(inout) :: IO_itp_org
!
!
      call gz_mpi_read_2d_vector_b                                      &
     &   (IO_param, cast_long(IO_itp_org%ntot_table_org), ithree,       &
     &    IO_itp_org%coef_inter_org)
!
      end subroutine gz_mpi_read_itp_coefs_org_b
!
!-----------------------------------------------------------------------
!
      end module gz_MPI_itp_tbl_org_data_IO_b
