!const_rev_ele_itp_table.f90
!      module const_rev_ele_itp_table
!
      module const_rev_ele_itp_table
!
      use m_precision
!
      use m_machine_parameter
      use t_interpolate_table
!
      implicit none
!
      type(interpolate_table) :: itp_ele_c2f, itp_ele_f2c
!
      private :: reverse_ele_itp_table_type
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_rev_ele_interpolate_table
!
      use calypso_mpi
      use m_2nd_pallalel_vector
      use m_ctl_params_4_gen_table
!
      use m_interpolate_table_dest_IO
      use m_interpolate_table_org_IO
      use m_interpolate_table_orgin
!
      use copy_interpolate_type_IO
      use copy_interpolate_org_IO
      use itp_table_IO_select_4_zlib
      use const_interpolate_4_org
!
      integer(kind = kint) :: jp
      integer(kind = kint) :: my_rank_2nd, ierr
!
!
!    set domain ID to be searched
!
      do jp = 1, nprocs_2nd
        my_rank_2nd = mod(my_rank+jp-1,nprocs_2nd)
!
        if (my_rank .eq. mod(my_rank_2nd,nprocs) ) then
          call set_num_dest_domain(nprocs, itp1_org)
          call alloc_type_itp_num_org(np_smp, itp1_org)
!
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'count_interpolate_4_orgin', my_rank_2nd, nprocs
          call count_interpolate_4_orgin(my_rank_2nd, nprocs)
!
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'allocate_itp_table_org'
          call allocate_itp_table_org
!
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'search_interpolate_4_orgin'
          call search_interpolate_4_orgin(my_rank_2nd, nprocs)
!
!
!
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'copy_itp_table_org_to_IO', my_rank_2nd, nprocs
          call copy_itp_table_org_to_IO
!
          if (my_rank_2nd .ge. nprocs) then
            num_org_domain_IO = 0
          else
            table_file_header = work_header
!
            call sel_read_itp_table_dest(my_rank_2nd, ierr)
            if (ierr .ne. 0) then
              call calypso_MPI_abort(ierr,'Check work file')
            end if
!
          end if
!
          call copy_interpolate_types_from_IO(my_rank_2nd, itp_ele_c2f)
          call set_stack_tbl_wtype_org_smp(itp_ele_c2f%tbl_org)
!
          call reverse_ele_itp_table_type(itp_ele_c2f, itp_ele_f2c)
          call copy_interpolate_types_to_IO(itp_ele_f2c)
!
          table_file_header = table_file_head
          call sel_write_interpolate_table(my_rank)
        end if
      end do
!
      if (my_rank .ge. nprocs_2nd) then
        table_file_header = work_header
        call sel_read_itp_table_dest(my_rank, ierr)
!
        if (ierr.ne.0) call calypso_MPI_abort(ierr,'Check work file')
!
        num_dest_domain_IO = 0
        ntot_table_org_IO =  0
        call copy_interpolate_types_from_IO(my_rank, itp_ele_c2f)
        call set_stack_tbl_wtype_org_smp(itp_ele_c2f%tbl_org)
!
        call reverse_ele_itp_table_type(itp_ele_c2f, itp_ele_f2c)
        call copy_interpolate_types_to_IO(itp_ele_f2c)
!
        table_file_header = table_file_head
        call sel_write_interpolate_table(my_rank)
      end if
!
      end subroutine const_rev_ele_interpolate_table
!
!-----------------------------------------------------------------------
!
      subroutine reverse_ele_itp_table_type(itp_tbl, itp_rev)
!
      use t_interpolate_table
      use m_work_const_itp_table
!
      type(interpolate_table), intent(inout) :: itp_tbl
      type(interpolate_table), intent(inout) :: itp_rev
      integer(kind = kint) :: num, i
!
!
      itp_rev%tbl_org%num_dest_domain = itp_tbl%tbl_dest%num_org_domain
      itp_rev%tbl_org%ntot_table_org = itp_tbl%tbl_dest%ntot_table_dest
      call allocate_istack_org_ptype(itp_rev%tbl_org%num_dest_domain)
!
      if(itp_rev%tbl_org%num_dest_domain .gt. 0) then
        call alloc_type_itp_num_org(np_smp, itp_rev%tbl_org)
        call alloc_type_itp_table_org(itp_rev%tbl_org)
!
        num = itp_rev%tbl_org%num_dest_domain
        itp_rev%tbl_org%id_dest_domain(1:num)                           &
     &    = itp_tbl%tbl_dest%id_org_domain(1:num)
        itp_rev%tbl_org%istack_nod_tbl_org(0:num)                       &
     &    = itp_tbl%tbl_dest%istack_nod_tbl_dest(0:num)
!
        do i = 1, num
          istack_org_para_type(4*i-3)                                   &
     &      = itp_rev%tbl_org%istack_nod_tbl_org(i)
          istack_org_para_type(4*i-2)                                   &
     &      = itp_rev%tbl_org%istack_nod_tbl_org(i)
          istack_org_para_type(4*i-1)                                   &
     &      = itp_rev%tbl_org%istack_nod_tbl_org(i)
          istack_org_para_type(4*i  )                                   &
     &      = itp_rev%tbl_org%istack_nod_tbl_org(i)
        end do
!
        do i = 1, itp_rev%tbl_org%ntot_table_org
          itp_rev%tbl_org%iele_org_4_org(i)                             &
     &      = itp_tbl%tbl_dest%inod_dest_4_dest(i)
          itp_rev%tbl_org%inod_itp_send(i) =      i
          itp_rev%tbl_org%itype_inter_org(i) =    0
          itp_rev%tbl_org%inod_gl_dest_4_org(i) = 0
          itp_rev%tbl_org%coef_inter_org(i,1:3) =     0.0d0
          itp_rev%tbl_org%coef_inter_org(i,1:3) =     0.0d0
          itp_rev%tbl_org%coef_inter_org(i,1:3) =     0.0d0
        end do
!
        call dealloc_type_itp_num_dest(itp_tbl%tbl_dest)
        call dealloc_type_itp_table_dest(itp_tbl%tbl_dest)
      end if
!
!
!
      itp_rev%tbl_dest%num_org_domain = itp_tbl%tbl_org%num_dest_domain
      itp_rev%tbl_dest%ntot_table_dest = itp_tbl%tbl_org%ntot_table_org
!
      if(itp_rev%tbl_dest%num_org_domain .gt. 0) then
        call alloc_type_itp_num_dest(itp_rev%tbl_dest)
        call alloc_type_itp_table_dest(itp_rev%tbl_dest)
!
        num = itp_rev%tbl_dest%num_org_domain
        itp_rev%tbl_dest%id_org_domain(1:num)                           &
     &    = itp_tbl%tbl_org%id_dest_domain(1:num)
        itp_rev%tbl_dest%istack_nod_tbl_dest(0:num)                     &
     &    = itp_tbl%tbl_org%istack_nod_tbl_org(0:num)
!
        num = itp_rev%tbl_dest%ntot_table_dest
        itp_rev%tbl_dest%inod_dest_4_dest(1:num)                        &
     &    = itp_tbl%tbl_org%iele_org_4_org(1:num)
!
        call deallocate_itp_num_org(itp_tbl%tbl_org)
        call dealloc_type_itp_table_org(itp_tbl%tbl_org)
      end if
!
      call deallocate_istack_org_ptype
!
      end subroutine reverse_ele_itp_table_type
!
!-----------------------------------------------------------------------
!
      end module const_rev_ele_itp_table
