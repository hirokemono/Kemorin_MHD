!
!      module const_interpolate_4_org
!
!        programmed by H.Matsui on Sep. 2006 (ver 1.2)
!
!      subroutine const_interpolate_table_4_orgin
!!
!!      subroutine count_interpolate_4_orgin                            &
!!     &         (n_org_rank, nprocs_dest, itp_org)
!!      subroutine search_interpolate_4_orgin                           &
!!     &          (n_org_rank, nprocs_dest, itp_org)
!
      module const_interpolate_4_org
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use t_interpolate_tbl_org
!
      implicit none
!
!> Structure of interpolation table for source grid
      type(interpolate_table_org), private :: itp_org_c
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_interpolate_table_4_orgin
!
      use m_2nd_pallalel_vector
      use m_ctl_params_4_gen_table
!
      use m_interpolate_table_dest_IO
      use m_interpolate_table_org_IO
!
      use itp_table_IO_select_4_zlib
      use copy_interpolate_type_IO
      use copy_interpolate_types
!
      integer(kind = kint) :: jp
      integer(kind = kint) :: my_rank_2nd, ierr
!
!    set domain ID to be searched
!
      do jp = 1, nprocs_2nd
        my_rank_2nd = mod(my_rank+jp-1,nprocs_2nd)
!
        if (my_rank .eq. mod(my_rank_2nd,nprocs) ) then
          call set_num_dest_domain(nprocs, itp_org_c)
          call alloc_itp_num_org(np_smp, itp_org_c)
!
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'count_interpolate_4_orgin', my_rank_2nd, nprocs
          call count_interpolate_4_orgin                                &
     &       (my_rank_2nd, nprocs, itp_org_c)
!
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'allocate_itp_table_org'
          call alloc_itp_table_org(itp_org_c)
!
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'search_interpolate_4_orgin'
          call search_interpolate_4_orgin                               &
     &       (my_rank_2nd, nprocs, itp_org_c)
!
!
!
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'copy_itp_tbl_types_org', my_rank_2nd, nprocs
          call copy_itp_tbl_types_org(my_rank, itp_org_c, IO_itp_org)
          call dealloc_itp_table_org(itp_org_c)
          call dealloc_itp_num_org(itp_org_c)
!
          if (my_rank_2nd .ge. nprocs) then
            IO_itp_dest%num_org_domain = 0
          else
            table_file_header = work_header
!
            call sel_read_itp_table_dest(my_rank_2nd, ierr)
!
            if (ierr.ne.0) then
              call calypso_MPI_abort(ierr,'Check work file')
            end if
!
          end if
!
          table_file_header = table_file_head
          call sel_write_interpolate_table(my_rank_2nd)
!
        end if
      end do
!
!
      if ( my_rank .ge. nprocs_2nd) then
        table_file_header = work_header
!
        call sel_read_itp_table_dest(my_rank, ierr)
!
        if (ierr.ne.0) call calypso_MPI_abort(ierr,'Check work file')
!
        IO_itp_org%num_dest_domain = 0
!
        table_file_header = table_file_head
        call sel_write_interpolate_table(my_rank)
!
      end if
!
      end subroutine const_interpolate_table_4_orgin
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_interpolate_4_orgin                              &
     &         (n_org_rank, nprocs_dest, itp_org)
!
      use itp_table_IO_select_4_zlib
      use set_itp_destIO_2_org
!
      integer(kind = kint), intent(in) :: n_org_rank, nprocs_dest
      type(interpolate_table_org), intent(inout)  :: itp_org
!
      integer(kind = kint) :: ip, n_dest_rank, ierr
!
!
      itp_org%num_dest_domain = 0
      itp_org%istack_nod_tbl_org(0:nprocs_dest) = 0
      do ip = 1, nprocs_dest
!
        n_dest_rank = mod(n_org_rank+ip,nprocs_dest)
        table_file_header = work_header
!
        call sel_read_itp_table_dest(n_dest_rank, ierr)
!
        if (ierr.ne.0) call calypso_MPI_abort(ierr,'Check work file')
!
        call count_num_interpolation_4_orgin                            &
     &     (n_org_rank, n_dest_rank, itp_org)
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
     &          (n_org_rank, nprocs_dest, itp_org)
!
      use itp_table_IO_select_4_zlib
      use set_itp_destIO_2_org
      use ordering_itp_org_tbl
      use m_work_const_itp_table
!
      integer(kind = kint), intent(in) :: n_org_rank, nprocs_dest
      type(interpolate_table_org), intent(inout)  :: itp_org
!
      integer(kind = kint) :: ip, n_dest_rank, ierr
!
      call allocate_istack_org_ptype(nprocs_dest)
!
      itp_org%num_dest_domain = 0
      do ip = 1, nprocs_dest
        n_dest_rank = mod(n_org_rank+ip,nprocs_dest)
        table_file_header = work_header
        call sel_read_itp_coefs_dest(n_dest_rank, ierr)
        if (ierr.ne.0) call calypso_MPI_abort(ierr,'Check work file')
!
        call set_interpolation_4_orgin(n_org_rank, itp_org)
      end do
!
      call ordering_itp_orgin_tbl_t(itp_org)
      call deallocate_istack_org_ptype
!
      end subroutine search_interpolate_4_orgin
!
!-----------------------------------------------------------------------
!
      end module const_interpolate_4_org
