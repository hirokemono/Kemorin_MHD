!>@file  element_refine_file_IO.f90
!!       module element_refine_file_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2010
!
!>@brief File IO for element refinment data
!!
!!@verbatim
!!      subroutine read_element_refine_file                             &
!!     &          (my_rank, ifile_type, IO_itp_org)
!!      subroutine write_element_refine_file                            &
!!     &         (my_rank, ifile_type, IO_itp_org)
!!        type(interpolate_table_org), intent(inout) :: IO_itp_org
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!@endverbatim
!
      module element_refine_file_IO
!
      use m_precision
!
      use m_element_refinement_IO
      use t_interpolate_tbl_dest
      use t_interpolate_tbl_org
      use itp_table_data_IO
      use itp_table_data_IO_b
      use set_parallel_file_name
!
      implicit  none
!
      private :: write_element_refine_data_b
      private :: read_element_refine_data_b
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_element_refine_file                               &
    &          (my_rank, ifile_type, IO_itp_org, IO_itp_dest)
!
      use binary_IO
!
      integer(kind = kint), intent(in) :: my_rank, ifile_type
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
      integer(kind = kint) :: nrank_ref
!
!
      call add_int_suffix(my_rank, refine_info_fhead,                   &
     &    refine_info_fname)
!
      if (ifile_type .eq. 1) then
        write(*,*) 'binary element refine information: ',               &
     &            trim(refine_info_fname)
        call open_read_binary_file(refine_info_fname, my_rank)
        call read_element_refine_data_b(IO_itp_org, IO_itp_dest)
        call close_binary_file
!
!
      else
        write(*,*) 'element refine information: ',                      &
     &            trim(refine_info_fname)
        open (id_refine_table,file = refine_info_fname)
!
        call read_interpolate_table_dest(id_refine_table, IO_itp_dest)
        call read_interpolate_domain_org                                &
     &     (id_refine_table, nrank_ref, IO_itp_org)
        call read_interpolate_table_org(id_refine_table, IO_itp_org)
        call read_interpolate_coefs_org(id_refine_table, IO_itp_org)
!
        call read_element_refine_data(id_refine_table)
        close(id_refine_table)
!
      end if
!
      end subroutine read_element_refine_file
!
! ----------------------------------------------------------------------
!
      subroutine write_element_refine_file                              &
     &         (my_rank, ifile_type, IO_itp_org, IO_itp_dest)
!
      integer(kind = kint), intent(in) :: my_rank, ifile_type
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
!
      call add_int_suffix(my_rank, refine_info_fhead,                   &
     &    refine_info_fname)
!
      if (ifile_type .eq. 1) then
        write(*,*) 'binary element refine information: ',               &
     &            trim(refine_info_fname)
        call open_write_binary_file(refine_info_fname)
        call write_element_refine_data_b                                &
     &     (my_rank, IO_itp_org, IO_itp_dest)
        call close_binary_file
!
      else
        write(*,*) 'element refine information: ',                      &
     &            trim(refine_info_fname)
        open (id_refine_table,file = refine_info_fname)
!
        call write_interpolate_table_dest                               &
     &     (id_refine_table, my_rank, IO_itp_dest)
        call write_interpolate_table_org                                &
     &     (id_refine_table, my_rank, IO_itp_org)
        call write_interpolate_coefs_org(id_refine_table, IO_itp_org)
!
        call write_element_refine_data(id_refine_table)
        close(id_refine_table)
!
      end if
!
      call deallocate_element_refine_IO
      call dealloc_itp_num_org(IO_itp_org)
      call dealloc_itp_table_org(IO_itp_org)
!
      call dealloc_itp_table_dest(IO_itp_dest)
      call dealloc_itp_num_dest(IO_itp_dest)
!
      end subroutine write_element_refine_file
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_element_refine_data_b                            &
     &         (my_rank, IO_itp_org, IO_itp_dest)
!
      use binary_IO
!
      integer(kind = kint), intent(in) :: my_rank
      type(interpolate_table_org), intent(in) :: IO_itp_org
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
!
!
      call write_interpolate_table_dest_b(my_rank, IO_itp_dest)
      call write_interpolate_table_org_b(my_rank, IO_itp_org)
      call write_interpolate_coefs_org_b(IO_itp_org)
!
      call write_one_integer_b(max_refine_level_IO)
      call write_one_integer_b(nele_ref_IO)
      call write_one_integer_b(nele_org_IO)
!
      call write_mul_integer_b(nele_ref_IO, iele_global_new_IO)
      call write_mul_integer_b(nele_ref_IO, ilevel_refine_IO)
      call write_mul_integer_b(nele_ref_IO, iflag_refine_ele_IO)
      call write_mul_integer_b(nele_ref_IO, iele_global_org_IO)
      call write_mul_integer_b(nele_ref_IO, icou_global_org_IO)
!
      end subroutine write_element_refine_data_b
!
! ----------------------------------------------------------------------
!
      subroutine read_element_refine_data_b(IO_itp_org, IO_itp_dest)
!
      use binary_IO
!
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
      integer(kind = kint) :: nrank_ref
!
!
      call read_interpolate_table_dest_b(IO_itp_dest)
      call read_interpolate_domain_org_b(nrank_ref, IO_itp_org)
      call read_interpolate_table_org_b(IO_itp_org)
      call read_interpolate_coefs_org_b(IO_itp_org)
!
      call read_one_integer_b(max_refine_level_IO)
      call read_one_integer_b(nele_ref_IO)
      call read_one_integer_b(nele_org_IO)
!
      call allocate_element_refine_IO
!
      call read_mul_integer_b(nele_ref_IO, iele_global_new_IO)
      call read_mul_integer_b(nele_ref_IO, ilevel_refine_IO)
      call read_mul_integer_b(nele_ref_IO, iflag_refine_ele_IO)
      call read_mul_integer_b(nele_ref_IO, iele_global_org_IO)
      call read_mul_integer_b(nele_ref_IO, icou_global_org_IO)
!
      end subroutine read_element_refine_data_b
!
! ----------------------------------------------------------------------
!
      end module  element_refine_file_IO
