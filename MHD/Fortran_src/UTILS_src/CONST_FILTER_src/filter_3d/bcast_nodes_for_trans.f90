!bcast_nodes_for_trans.f90
!      module bcast_nodes_for_trans
!
!      Written by H. Matsui on May, 2008
!
!      subroutine bcast_parallel_domain_tbl(mesh_head)
!
!      subroutine bcast_num_filter_part_table(nprocs_2nd)
!      subroutine bcast_xx_whole_nod(nnod_global)
!
      module bcast_nodes_for_trans
!
      use m_precision
!
      use m_constants
      use m_parallel_var_dof
      use m_internal_4_partitioner
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine bcast_parallel_domain_tbl(mesh_head)
!
      use m_2nd_pallalel_vector
      use m_domain_group_4_partition
      use const_domain_tbl_by_file
!
      character(len=kchara), intent(in) :: mesh_head
!
!
      START_TIME= MPI_WTIME()
      if(my_rank .eq. 0) then
        call count_nnod_whole_domain(mesh_head)
      end if
!
      call MPI_Bcast(nnod_s_domin, ione, MPI_INTEGER, izero,            &
     &    SOLVER_COMM, ierr)
!
      END_TIME= MPI_WTIME()
      COMMtime = END_TIME - START_TIME
      START_TIME = END_TIME
      if(my_rank .eq. 0) write(*,*) 'count total node time: ', COMMtime
!
      call allocate_domain_nese_group
      call allocate_local_nese_id_tbl
      call allocate_org_gl_nese_id
!
      if(my_rank .eq. 0) then
        call set_domain_grp_whole_domain(mesh_head)
      else
        call set_domain_grp_each_domain(mesh_head, my_rank)
      end if
!
      END_TIME= MPI_WTIME()
      COMMtime = END_TIME - START_TIME
      START_TIME = END_TIME
      if(my_rank .eq. 0) write(*,*) 'set domain table time: ', COMMtime
!
      end subroutine bcast_parallel_domain_tbl
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_num_filter_part_table(nprocs_2nd)
!
      use set_filters_4_new_domains
!
      integer(kind = kint), intent(in) :: nprocs_2nd
!
!
      call MPI_Bcast(num_intnod_sub(1),     nprocs_2nd, MPI_INTEGER,    &
     &    izero, SOLVER_COMM, ierr)
      call MPI_Bcast(istack_intnod_sub(1),  nprocs_2nd, MPI_INTEGER,    &
     &    izero, SOLVER_COMM, ierr)
      call MPI_Bcast(ntot_intnod_sub,  ione, MPI_INTEGER,               &
     &    izero, SOLVER_COMM, ierr)
      call MPI_Bcast(nmax_intnod_sub,  ione, MPI_INTEGER,               &
     &    izero, SOLVER_COMM, ierr)
      call MPI_Bcast(nmin_intnod_sub,  ione, MPI_INTEGER,               &
     &    izero, SOLVER_COMM, ierr)
!
      call MPI_Bcast(numnod_4_subdomain(1), nprocs_2nd, MPI_INTEGER,    &
     &    izero, SOLVER_COMM, ierr)
      call MPI_Bcast(istack_numnod_sub(1),  nprocs_2nd, MPI_INTEGER,    &
     &    izero, SOLVER_COMM, ierr)
      call MPI_Bcast(ntot_numnod_sub,  ione, MPI_INTEGER,               &
     &    izero, SOLVER_COMM, ierr)
      call MPI_Bcast(nmax_numnod_sub,  ione, MPI_INTEGER,               &
     &    izero, SOLVER_COMM, ierr)
      call MPI_Bcast(nmin_numnod_sub,  ione, MPI_INTEGER,               &
     &    izero, SOLVER_COMM, ierr)
!
      end subroutine bcast_num_filter_part_table
!
!   --------------------------------------------------------------------
!
      subroutine bcast_xx_whole_nod(nnod_global)
!
      use set_filters_4_new_domains
!
      integer(kind = kint), intent(in) :: nnod_global
      integer(kind = kint) :: num
!
!
      num = 3 * nnod_global
!
      call MPI_Bcast(xx_whole_nod(1,1), num, MPI_DOUBLE_PRECISION,      &
     &    izero, SOLVER_COMM, ierr)
      call MPI_Bcast(inod_intnod_sub(1), ntot_intnod_sub, MPI_INTEGER,  &
     &    izero, SOLVER_COMM, ierr)
      call MPI_Bcast(inod_4_subdomain(1), ntot_numnod_sub, MPI_INTEGER, &
     &    izero, SOLVER_COMM, ierr)
!
      end subroutine bcast_xx_whole_nod
!
!   --------------------------------------------------------------------
!
      end module bcast_nodes_for_trans
