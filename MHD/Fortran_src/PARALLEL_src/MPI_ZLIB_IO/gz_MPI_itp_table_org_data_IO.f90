!>@file  gz_MPI_itp_table_org_data_IO.f90
!!       module gz_MPI_itp_table_org_data_IO
!!
!!@author H. Matsui
!!@date   Programmed in Sep. 2012
!!
!> @brief gzipped data IO for interpolation
!!
!!@verbatim
!!      subroutine gz_mpi_write_itp_domain_org(IO_param, IO_itp_org)
!!      subroutine gz_mpi_write_itp_table_org(IO_param, IO_itp_org)
!!      subroutine gz_mpi_write_itp_index_org(IO_param, IO_itp_org)
!!      subroutine gz_mpi_write_itp_coefs_org(IO_param, IO_itp_org)
!!        type(interpolate_table_org), intent(in) :: IO_itp_org
!!
!!      subroutine gz_mpi_read_itp_domain_org(IO_param, IO_itp_org)
!!      subroutine gz_mpi_read_itp_table_org(IO_param, IO_itp_org)
!!      subroutine gz_mpi_read_itp_index_org(IO_param, IO_itp_org)
!!      subroutine gz_mpi_read_itp_coefs_org(IO_param, IO_itp_org)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(interpolate_table_org), intent(inout) :: IO_itp_org
!!@endverbatim
!
      module gz_MPI_itp_table_org_data_IO
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
      use m_interpolation_data_labels
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_write_itp_domain_org(IO_param, IO_itp_org)
!
      use gz_MPI_domain_data_IO
      use gz_MPI_ascii_data_IO
      use data_IO_to_textline
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_org), intent(in) :: IO_itp_org
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_itp_export_pe()), hd_itp_export_pe())
      call gz_mpi_write_int_items(IO_param, iten,                       &
     &    IO_itp_org%num_dest_domain, IO_itp_org%id_dest_domain)
!
      end subroutine gz_mpi_write_itp_domain_org
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_read_itp_domain_org(IO_param, IO_itp_org)
!
      use gz_MPI_domain_data_IO
      use gz_MPI_ascii_data_IO
      use data_IO_to_textline
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_org), intent(inout) :: IO_itp_org
!
!
      call gz_mpi_skip_header(IO_param, len(hd_itp_export_pe()))
      call gz_mpi_read_num_of_data                                      &
     &   (IO_param, IO_itp_org%num_dest_domain)
      call alloc_itp_num_org(np_smp, IO_itp_org)
!
      call gz_mpi_read_int_items(IO_param, iten,                        &
     &    IO_itp_org%num_dest_domain, IO_itp_org%id_dest_domain)
!
      end subroutine gz_mpi_read_itp_domain_org
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_write_itp_table_org(IO_param, IO_itp_org)
!
      use gz_MPI_domain_data_IO
      use gz_MPI_ascii_data_IO
      use data_IO_to_textline
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_org), intent(in) :: IO_itp_org
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_itp_export_item()), hd_itp_export_item())
      call gz_mpi_write_int_stack(IO_param, IO_itp_org%num_dest_domain, &
     &                            IO_itp_org%istack_nod_tbl_org)
      call gz_mpi_write_int_items(IO_param, iten,                       &
     &    IO_itp_org%ntot_table_org, IO_itp_org%inod_itp_send)
!
      end subroutine gz_mpi_write_itp_table_org
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_read_itp_table_org(IO_param, IO_itp_org)
!
      use gz_MPI_domain_data_IO
      use gz_MPI_ascii_data_IO
      use data_IO_to_textline
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_org), intent(inout) :: IO_itp_org
!
      integer(kind = kint) :: num_tmp
!
!
      call gz_mpi_skip_header(IO_param, len(hd_itp_export_item()))
!
      call gz_mpi_read_num_of_data(IO_param, num_tmp)
      call gz_mpi_read_int_stack(IO_param, IO_itp_org%num_dest_domain,  &
     &    IO_itp_org%istack_nod_tbl_org, IO_itp_org%ntot_table_org)
!
      call gz_mpi_read_num_of_data(IO_param, num_tmp)
      call alloc_itp_table_org(IO_itp_org)
      call gz_mpi_read_int_items(IO_param, iten,                        &
     &    IO_itp_org%ntot_table_org, IO_itp_org%inod_itp_send)
!
      end subroutine gz_mpi_read_itp_table_org
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_write_itp_index_org(IO_param, IO_itp_org)
!
      use gz_MPI_position_IO
      use gz_MPI_domain_data_IO
      use gz_MPI_ascii_data_IO
      use data_IO_to_textline
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_org), intent(in) :: IO_itp_org
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_itp_export_coef()), hd_itp_export_coef())
      call gz_mpi_write_int_stack(IO_param, ifour,                      &
     &                            IO_itp_org%istack_itp_type_org)
!
      call gz_mpi_write_int_items(IO_param, iten,                       &
     &    IO_itp_org%ntot_table_org, IO_itp_org%iele_org_4_org)
      call gz_mpi_write_int_items(IO_param, iten,                       &
     &    IO_itp_org%ntot_table_org, IO_itp_org%itype_inter_org)
!
      call gz_mpi_write_num_of_data                                     &
     &   (IO_param, IO_itp_org%ntot_table_org)
!
      end subroutine gz_mpi_write_itp_index_org
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_read_itp_index_org(IO_param, IO_itp_org)
!
      use gz_MPI_position_IO
      use gz_MPI_domain_data_IO
      use gz_MPI_ascii_data_IO
      use data_IO_to_textline
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_org), intent(inout) :: IO_itp_org
!
      integer(kind = kint) :: num_tmp
!
!
      call gz_mpi_skip_header(IO_param, len(hd_itp_export_coef()))
!
      call gz_mpi_read_num_of_data(IO_param, num_tmp)
      call gz_mpi_read_int_stack(IO_param, ifour,                       &
     &    IO_itp_org%istack_itp_type_org, num_tmp)
!
      call gz_mpi_read_num_of_data(IO_param, num_tmp)
      call gz_mpi_read_int_items(IO_param, iten,                        &
     &    IO_itp_org%ntot_table_org, IO_itp_org%iele_org_4_org)
!
      call gz_mpi_read_num_of_data(IO_param, num_tmp)
      call gz_mpi_read_int_items(IO_param, iten,                        &
     &    IO_itp_org%ntot_table_org, IO_itp_org%itype_inter_org)
!
      call gz_mpi_read_num_of_data(IO_param, num_tmp)
!
      end subroutine gz_mpi_read_itp_index_org
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_write_itp_coefs_org(IO_param, IO_itp_org)
!
      use gz_MPI_position_IO
      use gz_MPI_domain_data_IO
      use gz_MPI_ascii_data_IO
      use data_IO_to_textline
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_org), intent(in) :: IO_itp_org
!
!
      call gz_mpi_write_node_position                                   &
     &   (IO_param, cast_long(IO_itp_org%ntot_table_org), ithree,       &
     &    IO_itp_org%inod_gl_dest_4_org, IO_itp_org%coef_inter_org)
!
      end subroutine gz_mpi_write_itp_coefs_org
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_read_itp_coefs_org(IO_param, IO_itp_org)
!
      use gz_MPI_position_IO
      use gz_MPI_domain_data_IO
      use gz_MPI_ascii_data_IO
      use data_IO_to_textline
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_org), intent(inout) :: IO_itp_org
!
!
      call gz_mpi_read_node_position                                    &
     &   (IO_param, cast_long(IO_itp_org%ntot_table_org), ithree,       &
     &    IO_itp_org%inod_gl_dest_4_org, IO_itp_org%coef_inter_org)
!
      end subroutine gz_mpi_read_itp_coefs_org
!
!-----------------------------------------------------------------------
!
      end module gz_MPI_itp_table_org_data_IO
