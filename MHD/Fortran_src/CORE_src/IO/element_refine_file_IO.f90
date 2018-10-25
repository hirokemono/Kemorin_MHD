!>@file  element_refine_file_IO.f90
!!       module element_refine_file_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2010
!
!>@brief File IO for element refinment data
!!
!!@verbatim
!!      subroutine read_element_refine_file(my_rank, ifile_type,        &
!!    &           IO_itp_org, IO_itp_dest, e_ref_IO)
!!      subroutine write_element_refine_file(my_rank, ifile_type,       &
!!     &          IO_itp_org, IO_itp_dest, e_ref_IO)
!!        type(interpolate_table_org), intent(inout) :: IO_itp_org
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(ele_refine_IO_type), intent(inout) :: e_ref_IO
!!@endverbatim
!
      module element_refine_file_IO
!
      use m_precision
!
      use t_element_refinement_IO
      use t_interpolate_tbl_dest
      use t_interpolate_tbl_org
      use itp_table_data_IO
      use itp_table_data_IO_b
      use set_parallel_file_name
!
      implicit  none
!
      integer(kind = kint), parameter, private :: id_refine_table = 19
      type(file_IO_flags), private :: bin_rfnflags
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_element_refine_file(my_rank, ifile_type,          &
    &           IO_itp_org, IO_itp_dest, e_ref_IO)
!
      use binary_IO
!
      integer(kind = kint), intent(in) :: my_rank, ifile_type
!
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(ele_refine_IO_type), intent(inout) :: e_ref_IO
!
      integer(kind = kint) :: nrank_ref
      character(len = kchara) :: refine_fname
!
!
      call add_int_suffix(my_rank, e_ref_IO%file_head,                  &
     &    refine_fname)
!
      if (ifile_type .eq. 1) then
        write(*,*) 'binary element refine information: ',               &
     &            trim(refine_fname)
        call open_read_binary_file                                      &
     &     (refine_fname, my_rank, bin_rfnflags%iflag_bin_swap)
!
        call read_interpolate_table_dest_b(bin_rfnflags, IO_itp_dest)
        if(bin_rfnflags%ierr_IO .gt. 0) goto 99
!
        call read_interpolate_domain_org_b                              &
     &     (bin_rfnflags, nrank_ref, IO_itp_org)
        if(bin_rfnflags%ierr_IO .gt. 0) goto 99
!
        call read_interpolate_table_org_b(bin_rfnflags, IO_itp_org)
        if(bin_rfnflags%ierr_IO .gt. 0) goto 99
!
        call read_interpolate_coefs_org_b(bin_rfnflags, IO_itp_org)
        if(bin_rfnflags%ierr_IO .gt. 0) goto 99
!
        call read_element_refine_data_b(bin_rfnflags, e_ref_IO)
!
  99    continue
        call close_binary_file
        if(bin_rfnflags%ierr_IO .gt. 0) stop "Reading error"
      else
        write(*,*) 'element refine information: ',                      &
     &            trim(refine_fname)
        open (id_refine_table,file = refine_fname)
!
        call read_interpolate_table_dest(id_refine_table, IO_itp_dest)
        call read_interpolate_domain_org                                &
     &     (id_refine_table, nrank_ref, IO_itp_org)
        call read_interpolate_table_org(id_refine_table, IO_itp_org)
        call read_interpolate_coefs_org(id_refine_table, IO_itp_org)
!
        call read_element_refine_data(id_refine_table, e_ref_IO)
        close(id_refine_table)
      end if
!
      end subroutine read_element_refine_file
!
! ----------------------------------------------------------------------
!
      subroutine write_element_refine_file(my_rank, ifile_type,         &
     &          IO_itp_org, IO_itp_dest, e_ref_IO)
!
      integer(kind = kint), intent(in) :: my_rank, ifile_type
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(ele_refine_IO_type), intent(inout) :: e_ref_IO
!
      character(len = kchara) :: refine_fname
!
!
      call add_int_suffix(my_rank, e_ref_IO%file_head,                  &
     &    refine_fname)
!
      if (ifile_type .eq. 1) then
        write(*,*) 'binary element refine information: ',               &
     &            trim(refine_fname)
        call open_write_binary_file(refine_fname)
!
        call write_interpolate_table_dest_b(my_rank, IO_itp_dest)
        call write_interpolate_table_org_b(my_rank, IO_itp_org)
        call write_interpolate_coefs_org_b(IO_itp_org)
!
        call write_element_refine_data_b(e_ref_IO)
        call close_binary_file
!
      else
        write(*,*) 'element refine information: ',                      &
     &            trim(refine_fname)
        open (id_refine_table,file = refine_fname)
!
        call write_interpolate_table_dest                               &
     &     (id_refine_table, my_rank, IO_itp_dest)
        call write_interpolate_table_org                                &
     &     (id_refine_table, my_rank, IO_itp_org)
        call write_interpolate_coefs_org(id_refine_table, IO_itp_org)
!
        call write_element_refine_data(id_refine_table, e_ref_IO)
        close(id_refine_table)
      end if
!
      call dealloc_element_refine_IO(e_ref_IO)
      call dealloc_itp_num_org(IO_itp_org)
      call dealloc_itp_table_org(IO_itp_org)
!
      call dealloc_itp_table_dest(IO_itp_dest)
      call dealloc_itp_num_dest(IO_itp_dest)
!
      end subroutine write_element_refine_file
!
! ----------------------------------------------------------------------
!
      end module  element_refine_file_IO
