!
!      module const_interpolate_4_org
!
!        programmed by H.Matsui on Sep. 2006 (ver 1.2)
!
!!      subroutine const_interpolate_table_4_orgin                      &
!!     &         (gen_itp_p, cst_itp_wk)
!!        type(ctl_params_4_gen_table), intent(in) :: gen_itp_p
!!        type(work_const_itp_table), intent(inout) :: cst_itp_wk
!!
!!      subroutine count_interpolate_4_orgin                            &
!!     &         (id_org_rank, nprocs_dest, itp_org)
!!      subroutine search_interpolate_4_orgin                           &
!!     &          (id_org_rank, nprocs_dest, itp_org, cst_itp_wk)
!!  v      type(interpolate_table_org), intent(inout)  :: itp_org
!!        type(work_const_itp_table), intent(inout) :: cst_itp_wk
!
      module const_interpolate_4_org
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
      use t_work_const_itp_table
      use t_file_IO_parameter
!
      implicit none
!
!> Structure of interpolation table for source grid
      type(interpolate_table_org), save, private :: itp_org_c
!
!> Structure of interpolation table for IO
      type(interpolate_table), save, private :: itp_tbl_IO_c
!
!> Structure of interpolation coefficients for target grid
      type(interpolate_coefs_dest), save, private :: IO_itp_c_dest
!
      type(field_IO_params), save, private ::  tmp_tbl_IO
!
      character(len=kchara), parameter, private :: work_header = 'work'
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_interpolate_table_4_orgin                        &
     &         (gen_itp_p, cst_itp_wk)
!
      use t_ctl_params_4_gen_table
      use m_2nd_pallalel_vector
      use m_interpolate_table_IO
!
      use itp_table_IO_select_4_zlib
      use copy_interpolate_types
!
      integer :: jp, my_rank_2nd
      integer(kind = kint) :: ierr
      integer(kind = kint) :: np
!
      type(ctl_params_4_gen_table), intent(in) :: gen_itp_p
      type(work_const_itp_table), intent(inout) :: cst_itp_wk
!
!    set domain ID to be searched
!
      np = int(nprocs,KIND(np))
      do jp = 1, nprocs_2nd
        my_rank_2nd = mod(my_rank+jp-1,nprocs_2nd)
!
        if(my_rank .eq. mod(my_rank_2nd,nprocs)) then
          call set_num_dest_domain(np, itp_org_c)
          call alloc_itp_num_org(np_smp, itp_org_c)
!
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'count_interpolate_4_orgin', my_rank_2nd, nprocs
          call count_interpolate_4_orgin                                &
     &       (my_rank_2nd, np, itp_org_c)
!
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'allocate_itp_table_org'
          call alloc_itp_table_org(itp_org_c)
!
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'search_interpolate_4_orgin'
          call search_interpolate_4_orgin                               &
     &       (my_rank_2nd, np, itp_org_c, cst_itp_wk)
!
!
!
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'copy_itp_tbl_types_org', my_rank_2nd, nprocs
          call copy_itp_tbl_types_org                                   &
     &       (my_rank, itp_org_c, itp_tbl_IO_c%tbl_org)
          call dealloc_itp_table_org(itp_org_c)
          call dealloc_itp_num_org(itp_org_c)
!
          if (my_rank_2nd .ge. nprocs) then
            itp_tbl_IO_c%tbl_dest%num_org_domain = 0
          else
            tmp_tbl_IO%file_prefix = work_header
!
            write(*,*) 'sel_read_itp_table_dest', my_rank_2nd
            call sel_read_itp_table_dest                                &
     &         (my_rank_2nd, tmp_tbl_IO, itp_tbl_IO_c%tbl_dest, ierr)
!
            if (ierr.ne.0) then
              call calypso_MPI_abort(ierr,'Check work file')
            end if
!
          end if
!
          write(*,*) 'sel_write_interpolate_table',                     &
     &              tmp_tbl_IO%file_prefix
          call sel_write_interpolate_table                              &
     &       (my_rank_2nd, gen_itp_p%itp_file_IO, itp_tbl_IO_c)
!
        end if
      end do
!
!
      if(my_rank .ge. nprocs_2nd) then
        tmp_tbl_IO%file_prefix = work_header
!
        call sel_read_itp_table_dest                                    &
     &     (my_rank, tmp_tbl_IO, itp_tbl_IO_c%tbl_dest, ierr)
!
        if (ierr.ne.0) call calypso_MPI_abort(ierr,'Check work file')
!
        itp_tbl_IO_c%tbl_org%num_dest_domain = 0
!
        call sel_write_interpolate_table                                &
     &     (my_rank, gen_itp_p%itp_file_IO, itp_tbl_IO_c)
!
      end if
!
      end subroutine const_interpolate_table_4_orgin
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_interpolate_4_orgin                              &
     &         (id_org_rank, nprocs_dest, itp_org)
!
      use itp_table_IO_select_4_zlib
      use set_itp_destIO_2_org
!
      integer, intent(in) :: id_org_rank
      integer(kind = kint), intent(in) :: nprocs_dest
      type(interpolate_table_org), intent(inout)  :: itp_org
!
      integer :: n_dest_rank
      integer(kind = kint) :: ip, ierr
!
!
      itp_org%num_dest_domain = 0
      itp_org%istack_nod_tbl_org(0:nprocs_dest) = 0
      do ip = 1, nprocs_dest
!
        n_dest_rank = int(mod(id_org_rank+ip,nprocs_dest))
        tmp_tbl_IO%file_prefix = work_header
!
        call sel_read_itp_table_dest                                    &
     &     (n_dest_rank, tmp_tbl_IO, itp_tbl_IO_c%tbl_dest, ierr)
!
        if (ierr.ne.0) call calypso_MPI_abort(ierr,'Check work file')
!
        call count_num_interpolation_4_orgin                            &
     &     (id_org_rank, n_dest_rank, itp_tbl_IO_c%tbl_dest, itp_org)
!
      end do
      itp_org%ntot_table_org                                            &
     &     = itp_org%istack_nod_tbl_org(itp_org%num_dest_domain)
!
      end subroutine count_interpolate_4_orgin
!
!-----------------------------------------------------------------------
!
      subroutine search_interpolate_4_orgin                             &
     &          (id_org_rank, nprocs_dest, itp_org, cst_itp_wk)
!
      use itp_table_IO_select_4_zlib
      use set_itp_destIO_2_org
      use ordering_itp_org_tbl
!
      integer, intent(in) :: id_org_rank
      integer(kind = kint), intent(in) :: nprocs_dest
      type(interpolate_table_org), intent(inout)  :: itp_org
      type(work_const_itp_table), intent(inout) :: cst_itp_wk
!
      integer :: n_dest_rank
      integer(kind = kint) :: ip, ierr
!
!
      call alloc_istack_org_ptype(nprocs_dest, cst_itp_wk)
!
      itp_org%num_dest_domain = 0
      do ip = 1, nprocs_dest
        n_dest_rank = int(mod(id_org_rank+ip,nprocs_dest))
        tmp_tbl_IO%file_prefix = work_header
        call sel_read_itp_coefs_dest(n_dest_rank, tmp_tbl_IO,           &
     &      itp_tbl_IO_c%tbl_dest, IO_itp_c_dest, ierr)
        if (ierr.ne.0) call calypso_MPI_abort(ierr,'Check work file')
!
        call set_interpolation_4_orgin                                  &
     &     (id_org_rank, itp_tbl_IO_c%tbl_dest, IO_itp_c_dest, itp_org, &
     &      cst_itp_wk%istack_org_para_type)
      end do
!
      call ordering_itp_orgin_tbl_t(cst_itp_wk, itp_org)
!
      end subroutine search_interpolate_4_orgin
!
!-----------------------------------------------------------------------
!
      end module const_interpolate_4_org
