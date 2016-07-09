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
!!@endverbatim
!
      module element_refine_file_IO
!
      use m_precision
!
      use m_element_refinement_IO
      use m_interpolate_table_dest_IO
      use t_interpolate_tbl_org
      use itp_table_data_IO
      use itp_table_data_IO_b
      use set_parallel_file_name
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_element_refine_file                               &
    &          (my_rank, ifile_type, IO_itp_org)
!
      integer(kind = kint), intent(in) :: my_rank, ifile_type
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      integer(kind = kint) :: nrank_ref
!
!
      call add_int_suffix(my_rank, refine_info_fhead,                   &
     &    refine_info_fname)
!
      if (ifile_type .eq. 1) then
        write(*,*) 'binary element refine information: ',               &
     &            trim(refine_info_fname)
        open (id_refine_table,file=refine_info_fname,                   &
     &      form='unformatted')
!
        call read_interpolate_table_dest_b(id_refine_table)
        call read_interpolate_domain_org_b                              &
     &     (id_refine_table, nrank_ref, IO_itp_org)
        call read_interpolate_table_org_b(id_refine_table, IO_itp_org)
        call read_interpolate_coefs_org_b(id_refine_table, IO_itp_org)
!
        call read_element_refine_data_b(id_refine_table)
        close(id_refine_table)
!
!
      else
        write(*,*) 'element refine information: ',                      &
     &            trim(refine_info_fname)
        open (id_refine_table,file = refine_info_fname)
!
        call read_interpolate_table_dest(id_refine_table)
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
     &         (my_rank, ifile_type, IO_itp_org)
!
      integer(kind = kint), intent(in) :: my_rank, ifile_type
      type(interpolate_table_org), intent(inout) :: IO_itp_org
!
!
      call add_int_suffix(my_rank, refine_info_fhead,                   &
     &    refine_info_fname)
!
      if (ifile_type .eq. 1) then
        write(*,*) 'binary element refine information: ',               &
     &            trim(refine_info_fname)
        open (id_refine_table,file=refine_info_fname,                   &
     &      form='unformatted')
!
        call write_interpolate_table_dest_b(id_refine_table, my_rank)
        call write_interpolate_table_org_b                              &
     &     (id_refine_table, my_rank, IO_itp_org)
        call write_interpolate_coefs_org_b(id_refine_table, IO_itp_org)
!
        call write_element_refine_data_b(id_refine_table)
        close(id_refine_table)
!
      else
        write(*,*) 'element refine information: ',                      &
     &            trim(refine_info_fname)
        open (id_refine_table,file = refine_info_fname)
!
        call write_interpolate_table_dest(id_refine_table, my_rank)
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
      call deallocate_itp_nod_dst_IO
      call deallocate_itp_num_dst_IO
!
      end subroutine write_element_refine_file
!
! ----------------------------------------------------------------------
!
      end module  element_refine_file_IO
