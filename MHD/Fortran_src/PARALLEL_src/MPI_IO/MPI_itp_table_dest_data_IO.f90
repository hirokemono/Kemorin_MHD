!>@file   MPI_itp_table_dest_data_IO.f90
!!@brief  module MPI_itp_table_dest_data_IO
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2006
!!
!>@brief Routines for ASCII group data IO
!!
!!@verbatim
!!      subroutine mpi_write_itp_domain_dest(IO_param, IO_itp_dest)
!!      subroutine mpi_write_itp_table_dest(IO_param, IO_itp_dest)
!!      subroutine mpi_write_itp_index_dest                             &
!!     &         (IO_param, IO_itp_dest, IO_itp_c_dest)
!!      subroutine mpi_write_itp_coefs_dest                             &
!!     &         (IO_param, IO_itp_dest, IO_itp_c_dest)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(interpolate_table_dest), intent(in) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
!!
!!      subroutine mpi_read_itp_domain_dest(IO_param, IO_itp_dest)
!!      subroutine mpi_read_itp_table_dest(IO_param, IO_itp_dest)
!!      subroutine mpi_read_itp_index_dest                              &
!!     &         (IO_param, IO_itp_dest, IO_itp_c_dest)
!!      subroutine mpi_read_itp_coefs_dest                              &
!!     &         (IO_param, IO_itp_dest, IO_itp_c_dest)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!!@endverbatim
!
      module MPI_itp_table_dest_data_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_interpolation_data_labels
!
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
      subroutine mpi_write_itp_domain_dest(IO_param, IO_itp_dest)
!
      use MPI_domain_data_IO
      use MPI_ascii_data_IO
      use data_IO_to_textline
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
!
      integer(kind = kint) :: nlength
!
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_itp_import_pe()), hd_itp_import_pe())
      nlength = int(IO_param%nprocs_in,KIND(nlength))
      call mpi_write_charahead(IO_param, len_int_txt,                   &
     &    integer_textline(nlength))
!
      call mpi_write_int_items(IO_param, iten,                          &
     &    IO_itp_dest%num_org_domain, IO_itp_dest%id_org_domain)
!
      end subroutine mpi_write_itp_domain_dest
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_domain_dest(IO_param, IO_itp_dest)
!
      use MPI_domain_data_IO
      use MPI_ascii_data_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
!
      call mpi_skip_read(IO_param, ilen_itp_import_pe)
      call mpi_check_num_of_domains(IO_param)
!
      call mpi_read_num_of_data(IO_param, IO_itp_dest%num_org_domain)
      call alloc_itp_num_dest(IO_itp_dest)
!
      call mpi_read_int_items(IO_param, iten,                           &
     &    IO_itp_dest%num_org_domain, IO_itp_dest%id_org_domain)
!
      end subroutine mpi_read_itp_domain_dest
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine mpi_write_itp_table_dest(IO_param, IO_itp_dest)
!
      use MPI_domain_data_IO
      use MPI_ascii_data_IO
      use data_IO_to_textline
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
!
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_itp_import_item()), hd_itp_import_item())
      call mpi_write_int_stack(IO_param, IO_itp_dest%num_org_domain,    &
     &                         IO_itp_dest%istack_nod_tbl_dest)
      call mpi_write_int_items(IO_param, iten,                          &
     &    IO_itp_dest%ntot_table_dest, IO_itp_dest%inod_dest_4_dest)
!
      end subroutine mpi_write_itp_table_dest
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_table_dest(IO_param, IO_itp_dest)
!
      use MPI_domain_data_IO
      use MPI_ascii_data_IO
      use data_IO_to_textline
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
      integer(kind = kint) :: num_tmp
!
!
      call mpi_skip_read(IO_param, ilen_itp_import_item)
!
      call mpi_read_num_of_data(IO_param, num_tmp)
      call mpi_read_int_stack(IO_param, IO_itp_dest%num_org_domain,     &
     &    IO_itp_dest%istack_nod_tbl_dest, IO_itp_dest%ntot_table_dest)
!
      call mpi_read_num_of_data(IO_param, num_tmp)
      call alloc_itp_table_dest(IO_itp_dest)
      call mpi_read_int_items(IO_param, iten,                           &
     &    IO_itp_dest%ntot_table_dest, IO_itp_dest%inod_dest_4_dest)
!
      end subroutine mpi_read_itp_table_dest
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine mpi_write_itp_index_dest                               &
     &         (IO_param, IO_itp_dest, IO_itp_c_dest)
!
      use MPI_position_IO
      use MPI_domain_data_IO
      use MPI_ascii_data_IO
      use data_IO_to_textline
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
!
      integer(kind = kint) :: num_tmp
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_itp_dest_coef()), hd_itp_dest_coef())
!
      num_tmp = 4*IO_itp_dest%num_org_domain
      call mpi_write_int_stack(IO_param, num_tmp,                       &
     &    IO_itp_c_dest%istack_nod_tbl_wtype_dest)
!
      call mpi_write_int_items(IO_param, iten,                          &
     &    IO_itp_dest%ntot_table_dest, IO_itp_c_dest%iele_org_4_dest)
      call mpi_write_int_items(IO_param, iten,                          &
     &    IO_itp_dest%ntot_table_dest, IO_itp_c_dest%itype_inter_dest)
!
      end subroutine mpi_write_itp_index_dest
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_index_dest                                &
     &         (IO_param, IO_itp_dest, IO_itp_c_dest)
!
      use MPI_position_IO
      use MPI_domain_data_IO
      use MPI_ascii_data_IO
      use data_IO_to_textline
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      integer(kind = kint) :: num_tmp
!
!
      call mpi_skip_read(IO_param, len(hd_itp_dest_coef()))
      call alloc_itp_coef_stack                                         &
     &     (IO_itp_dest%num_org_domain, IO_itp_c_dest)
!
      num_tmp = 4*IO_itp_dest%num_org_domain
      call mpi_read_num_of_data(IO_param, num_tmp)
      call mpi_read_int_stack(IO_param, num_tmp,                        &
     &    IO_itp_c_dest%istack_nod_tbl_wtype_dest,                      &
     &    IO_itp_dest%ntot_table_dest)
!
      call alloc_itp_coef_dest(IO_itp_dest, IO_itp_c_dest)

      call mpi_read_num_of_data(IO_param, num_tmp)
      call mpi_read_int_items(IO_param, iten,                           &
     &    IO_itp_dest%ntot_table_dest, IO_itp_c_dest%iele_org_4_dest)

      call mpi_read_num_of_data(IO_param, num_tmp)
      call mpi_read_int_items(IO_param, iten,                           &
     &    IO_itp_dest%ntot_table_dest, IO_itp_c_dest%itype_inter_dest)

      end subroutine mpi_read_itp_index_dest
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine mpi_write_itp_coefs_dest                               &
     &         (IO_param, IO_itp_dest, IO_itp_c_dest)
!
      use MPI_position_IO
      use MPI_domain_data_IO
      use MPI_ascii_data_IO
      use data_IO_to_textline
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
!
!
      call mpi_write_num_of_data(IO_param, IO_itp_dest%ntot_table_dest)
      call mpi_write_node_position                                      &
     &   (IO_param, cast_long(IO_itp_dest%ntot_table_dest), ithree,     &
     &    IO_itp_c_dest%inod_gl_dest, IO_itp_c_dest%coef_inter_dest)
!
      end subroutine mpi_write_itp_coefs_dest
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_coefs_dest                                &
     &         (IO_param, IO_itp_dest, IO_itp_c_dest)
!
      use MPI_position_IO
      use MPI_domain_data_IO
      use MPI_ascii_data_IO
      use data_IO_to_textline
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      integer(kind = kint) :: num_tmp
!
!
      call mpi_read_num_of_data(IO_param, num_tmp)
      call mpi_read_node_position                                       &
     &   (IO_param, cast_long(IO_itp_dest%ntot_table_dest), ithree,     &
     &    IO_itp_c_dest%inod_gl_dest, IO_itp_c_dest%coef_inter_dest)
!
      end subroutine mpi_read_itp_coefs_dest
!
!-----------------------------------------------------------------------
!
      end module MPI_itp_table_dest_data_IO
