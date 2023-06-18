!>@file  element_refine_file_IO.f90
!!       module element_refine_file_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2010
!
!>@brief File IO for element refinment data
!!
!!@verbatim
!!      subroutine read_element_refine_file(id_rank, ifile_type,        &
!!    &           IO_itp_org, IO_itp_dest, e_ref_IO, ierr)
!!      subroutine write_element_refine_file(id_rank, ifile_type,       &
!!     &          IO_itp_org, IO_itp_dest, e_ref_IO, ierr)
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
      use t_binary_IO_buffer
      use set_parallel_file_name
!
      implicit  none
!
      integer(kind = kint), parameter :: id_refine_table = 19
      integer(kind = kint), parameter :: id_read_rfin =  21
      integer(kind = kint), parameter :: id_write_rfin = 22
      type(binary_IO_buffer) :: bbuf_rfin
!
      private :: id_refine_table, id_read_rfin, id_write_rfin
      private :: bbuf_rfin
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_element_refine_file(id_rank, ifile_type,          &
    &           IO_itp_org, IO_itp_dest, e_ref_IO, ierr)
!
      use binary_IO
      use itp_table_org_data_IO
      use itp_table_dest_data_IO
      use itp_table_org_data_IO_b
      use itp_table_dest_data_IO_b
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: ifile_type
!
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(ele_refine_IO_type), intent(inout) :: e_ref_IO
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: nrank_ref
      character(len = kchara) :: refine_fname
!
!
      refine_fname = add_process_id(id_rank, e_ref_IO%file_head)
!
      if (ifile_type .eq. 1) then
        write(*,*) 'binary element refine information: ',               &
     &            trim(refine_fname)
        bbuf_rfin%id_binary = id_read_rfin
        call open_read_binary_file(refine_fname, id_rank, bbuf_rfin)
        if(bbuf_rfin%ierr_bin .ne. 0) goto 99
!
        call read_interpolate_table_dest_b(bbuf_rfin, IO_itp_dest)
        if(bbuf_rfin%ierr_bin .ne. 0) goto 99
!
        call read_interpolate_domain_org_b                              &
     &     (bbuf_rfin, nrank_ref, IO_itp_org)
        if(bbuf_rfin%ierr_bin .gt. 0) goto 99
!
        call read_interpolate_table_org_b(bbuf_rfin, IO_itp_org)
        if(bbuf_rfin%ierr_bin .ne. 0) goto 99
!
        call read_interpolate_idx_org_b(bbuf_rfin, IO_itp_org)
        if(bbuf_rfin%ierr_bin .ne. 0) goto 99
        call read_interpolate_coefs_org_b(bbuf_rfin, IO_itp_org)
        if(bbuf_rfin%ierr_bin .ne. 0) goto 99
!
        call read_element_refine_data_b(bbuf_rfin, e_ref_IO)
!
  99    continue
        call close_binary_file(bbuf_rfin)
        if(bbuf_rfin%ierr_bin .ne. 0) then
          ierr = 99
          return
        end if
      else
        write(*,*) 'element refine information: ',                      &
     &            trim(refine_fname)
        open (id_refine_table,file = refine_fname)
!
        call read_interpolate_table_dest(id_refine_table,               &
     &                                   IO_itp_dest, ierr)
        if(ierr .gt. 0) return
        call read_interpolate_domain_org                                &
     &     (id_refine_table, nrank_ref, IO_itp_org, ierr)
        if(ierr .gt. 0) return
        call read_interpolate_table_org(id_refine_table,                &
     &                                  IO_itp_org, ierr)
        if(ierr .gt. 0) return
        call read_interpolate_coefs_org(id_refine_table,                &
     &                                  IO_itp_org, ierr)
        if(ierr .gt. 0) return
!
        call read_element_refine_data(id_refine_table, e_ref_IO, ierr)
        if(ierr .gt. 0) return
        close(id_refine_table)
      end if
!
      end subroutine read_element_refine_file
!
! ----------------------------------------------------------------------
!
      subroutine write_element_refine_file(id_rank, ifile_type,         &
     &          IO_itp_org, IO_itp_dest, e_ref_IO, ierr)
!
      use itp_table_org_data_IO
      use itp_table_dest_data_IO
      use itp_table_org_data_IO_b
      use itp_table_dest_data_IO_b
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: ifile_type
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(ele_refine_IO_type), intent(inout) :: e_ref_IO
      integer(kind = kint), intent(inout) :: ierr
!
      character(len = kchara) :: refine_fname
!
!
      refine_fname = add_process_id(id_rank, e_ref_IO%file_head)
!
      if (ifile_type .eq. 1) then
        write(*,*) 'binary element refine information: ',               &
     &            trim(refine_fname)
        bbuf_rfin%id_binary = id_write_rfin
        call open_write_binary_file(refine_fname, bbuf_rfin)
        if(bbuf_rfin%ierr_bin .gt. 0) go to 99
!
        call write_interpolate_table_dest_b                             &
     &     (id_rank, IO_itp_dest, bbuf_rfin)
        if(bbuf_rfin%ierr_bin .ne. 0) go to 99
!
        call write_interpolate_table_org_b                              &
     &     (id_rank, IO_itp_org, bbuf_rfin)
        if(bbuf_rfin%ierr_bin .ne. 0) go to 99
        call write_interpolate_idx_org_b(IO_itp_org, bbuf_rfin)
        if(bbuf_rfin%ierr_bin .ne. 0) go to 99
        call write_interpolate_coefs_org_b(IO_itp_org, bbuf_rfin)
        if(bbuf_rfin%ierr_bin .ne. 0) go to 99
!
        call write_element_refine_data_b(e_ref_IO, bbuf_rfin)
        if(bbuf_rfin%ierr_bin .ne. 0) go to 99
!
  99    continue
        call close_binary_file(bbuf_rfin)
        ierr = bbuf_rfin%ierr_bin
      else
        write(*,*) 'element refine information: ',                      &
     &            trim(refine_fname)
        open (id_refine_table,file = refine_fname)
!
        call write_interpolate_table_dest                               &
     &     (id_refine_table, id_rank, IO_itp_dest)
        call write_interpolate_table_org                                &
     &     (id_refine_table, id_rank, IO_itp_org)
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
