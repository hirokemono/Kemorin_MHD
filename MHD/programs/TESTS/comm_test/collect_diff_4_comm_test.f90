!
!      module collect_diff_4_comm_test
!
!     Written by H. Matsui on Sep., 2007
!
!      subroutine allocate_cflag_collect_diff
!      subroutine deallocate_cflag_collect_diff
!
!      subroutine s_count_diff_4_comm_test
!      subroutine s_collect_diff_4_comm_test
!
!      subroutine count_diff_nod_comm_test
!      subroutine collect_diff_nod_comm_test
!
!      subroutine count_diff_ctest(num_diff_l, ntot_diff_pe,            &
!     &          num_diff_pe, istack_diff_pe)
!      subroutine collect_diff_ctest(np, num_diff_l, id_diff,           &
!     &          id_gl_diff, x_diff, ntot_diff_pe, num_diff_pe,         &
!     &          istack_diff_pe, id_diff_IO, id_gl_diff_IO, x_diff_IO)
!
      module collect_diff_4_comm_test
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_parallel_var_dof
!
      implicit  none
!
      integer, allocatable :: sta1(:,:)
!    work array for communication (wait)
      integer, allocatable :: sta2(:,:)
!    work array for communication (wait)
      integer, allocatable :: req1(:  )
!    work array for communication (wait)
      integer, allocatable :: req2(:  )
!    work array for communication (wait)
!
      private :: sta1, sta2, req1, req2
!
      private :: count_diff_ele_comm_test
      private :: count_diff_surf_comm_test
      private :: count_diff_edge_comm_test
!
      private :: collect_diff_ele_comm_test
      private :: collect_diff_surf_comm_test
      private :: collect_diff_edge_comm_test
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_cflag_collect_diff
!
      allocate(sta1(MPI_STATUS_SIZE,ione))
      allocate(req1(ione))
      if (my_rank.eq.0) then
        allocate(sta2(MPI_STATUS_SIZE,nprocs))
        allocate(req2(nprocs))
      end if
!
      end subroutine allocate_cflag_collect_diff
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_cflag_collect_diff
!
      deallocate(sta1, req1)
      if (my_rank.eq.0)  deallocate(sta2, req2)
!
      end subroutine deallocate_cflag_collect_diff
!
! ----------------------------------------------------------------------
!
      subroutine s_count_diff_4_comm_test
!
      use m_geometry_4_comm_test
!
      write(*,*) 'nnod_diff_local, nele_diff_local, ',                  &
     &                      'nsurf_diff_local, nedge_diff_local '
      write(*,*) my_rank, nnod_diff_local, nele_diff_local,             &
     &                      nsurf_diff_local, nedge_diff_local
      call count_diff_ele_comm_test
      call count_diff_surf_comm_test
      call count_diff_edge_comm_test
!
      end subroutine s_count_diff_4_comm_test
!
! ----------------------------------------------------------------------
!
      subroutine s_collect_diff_4_comm_test
!
!
      call collect_diff_ele_comm_test
      call collect_diff_surf_comm_test
      call collect_diff_edge_comm_test
!
      end subroutine s_collect_diff_4_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_diff_nod_comm_test
!
      use m_geometry_4_comm_test
!
      call count_diff_ctest(nnod_diff_local, ntot_nod_diff_pe,          &
     &    num_nod_diff_pe, istack_nod_diff_pe)
!
      end subroutine count_diff_nod_comm_test
!
! ----------------------------------------------------------------------
!
      subroutine count_diff_ele_comm_test
!
      use m_geometry_4_comm_test
!
      call count_diff_ctest(nele_diff_local, ntot_ele_diff_pe,          &
     &    num_ele_diff_pe, istack_ele_diff_pe)
!
      end subroutine count_diff_ele_comm_test
!
! ----------------------------------------------------------------------
!
      subroutine count_diff_surf_comm_test
!
      use m_geometry_4_comm_test
!
      call count_diff_ctest(nsurf_diff_local, ntot_surf_diff_pe,        &
     &    num_surf_diff_pe, istack_surf_diff_pe)
!
      end subroutine count_diff_surf_comm_test
!
! ----------------------------------------------------------------------
!
      subroutine count_diff_edge_comm_test
!
      use m_geometry_4_comm_test
!
      call count_diff_ctest(nedge_diff_local, ntot_edge_diff_pe,        &
     &    num_edge_diff_pe, istack_edge_diff_pe)
!
      end subroutine count_diff_edge_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine collect_diff_nod_comm_test
!
      use m_geometry_4_comm_test
!
      call collect_diff_ctest(nprocs, nnod_diff_local, inod_diff,       &
     &    inod_gl_diff, xx_diff, ntot_nod_diff_pe,                      &
     &    num_nod_diff_pe, istack_nod_diff_pe,                          &
     &    inod_diff_IO, inod_gl_diff_IO, xx_diff_IO)
!
      end subroutine collect_diff_nod_comm_test
!
! ----------------------------------------------------------------------
!
      subroutine collect_diff_ele_comm_test
!
      use m_geometry_4_comm_test
!
      call collect_diff_ctest(nprocs, nele_diff_local, iele_diff,       &
     &    iele_gl_diff, xele_diff, ntot_ele_diff_pe,                    &
     &    num_ele_diff_pe, istack_ele_diff_pe,                          &
     &    iele_diff_IO, iele_gl_diff_IO, xele_diff_IO)
!
      end subroutine collect_diff_ele_comm_test
!
! ----------------------------------------------------------------------
!
      subroutine collect_diff_surf_comm_test
!
      use m_geometry_4_comm_test
!
      call collect_diff_ctest(nprocs, nsurf_diff_local, isurf_diff,     &
     &    isurf_gl_diff, xsurf_diff, ntot_surf_diff_pe,                 &
     &    num_surf_diff_pe, istack_surf_diff_pe,                        &
     &    isurf_diff_IO, isurf_gl_diff_IO, xsurf_diff_IO)
!
      end subroutine collect_diff_surf_comm_test
!
! ----------------------------------------------------------------------
!
      subroutine collect_diff_edge_comm_test
!
      use m_geometry_4_comm_test
!
!
      call collect_diff_ctest(nprocs, nedge_diff_local, iedge_diff,     &
     &    iedge_gl_diff, xedge_diff, ntot_edge_diff_pe,                 &
     &    num_edge_diff_pe, istack_edge_diff_pe,                        &
     &    iedge_diff_IO, iedge_gl_diff_IO, xedge_diff_IO)
!
      end subroutine collect_diff_edge_comm_test
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_diff_ctest(num_diff_l, ntot_diff_pe,             &
     &          num_diff_pe, istack_diff_pe)
!
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: num_diff_l
!
      integer(kind = kint), intent(inout) :: ntot_diff_pe
      integer(kind = kint), intent(inout) :: num_diff_pe(nprocs)
      integer(kind = kint), intent(inout) :: istack_diff_pe(0:nprocs)
!
      integer(kind = kint) :: ip, id_rank
!
!
      call MPI_ISEND (num_diff_l, ione, MPI_INTEGER,                    &
     &    izero, 0, SOLVER_COMM, req1(ione), ierr)
      if (my_rank .eq. 0) then
        do ip = 1, nprocs
          id_rank = ip - 1
          call MPI_IRECV(num_diff_pe(ip), ione, MPI_INTEGER, &
     &        id_rank, 0, SOLVER_COMM, req2(ip), ierr)
        end do
        call MPI_WAITALL (nprocs, req2(1), sta2(1,1), ierr)
      end if
      call MPI_WAITALL (ione, req1(1), sta1(1,1), ierr)
!
      if (my_rank .eq. 0) then
        call s_cal_total_and_stacks(nprocs, num_diff_pe,     &
     &      izero, istack_diff_pe, ntot_diff_pe)
      end if
!
      end subroutine count_diff_ctest
!
! ----------------------------------------------------------------------
!
      subroutine collect_diff_ctest(np, num_diff_l, id_diff,            &
     &          id_gl_diff, x_diff, ntot_diff_pe, num_diff_pe,          &
     &          istack_diff_pe, id_diff_IO, id_gl_diff_IO, x_diff_IO)
!
!
      integer(kind = kint), intent(in) :: np
      integer(kind = kint), intent(in) :: num_diff_l
      integer(kind = kint), intent(in) :: id_diff(num_diff_l)
      integer(kind = kint), intent(in) :: id_gl_diff(2*num_diff_l)
      real(kind = kreal), intent(in) :: x_diff(6*num_diff_l)
!
      integer(kind = kint), intent(in) :: ntot_diff_pe
      integer(kind = kint), intent(in) :: num_diff_pe(np)
      integer(kind = kint), intent(in) :: istack_diff_pe(0:np)
!
      integer(kind = kint), intent(inout) :: id_diff_IO(ntot_diff_pe)
      integer(kind = kint), intent(inout)                               &
     &      :: id_gl_diff_IO(2*ntot_diff_pe)
      real(kind = kreal), intent(inout) :: x_diff_IO(6*ntot_diff_pe)
!
      integer(kind = kint) :: num, ist, ip, id_rank
!
!
      num = num_diff_l
      call MPI_ISEND (id_diff, num, MPI_INTEGER,                        &
     &    izero, 0, SOLVER_COMM, req1(ione), ierr)
      if (my_rank .eq. 0) then
        do ip = 1, np
          id_rank = ip - 1
          ist = istack_diff_pe(ip-1) + 1
          num = num_diff_pe(ip)
          call MPI_IRECV (id_diff_IO(ist), num, MPI_INTEGER,            &
     &        id_rank, 0, SOLVER_COMM, req2(ip), ierr)
        end do
        call MPI_WAITALL (np, req2(1), sta2(1,1), ierr)
      end if
      call MPI_WAITALL (ione, req1(1), sta1(1,1), ierr)
!
!
      num = 2*num_diff_l
      call MPI_ISEND (id_gl_diff, num, MPI_INTEGER,                     &
     &    izero, 0, SOLVER_COMM, req1(ione), ierr)
      if (my_rank .eq. 0) then
        do ip = 1, np
          id_rank = ip - 1
          ist = 2*istack_diff_pe(ip-1) + 1
          num = 2*num_diff_pe(ip)
          call MPI_IRECV(id_gl_diff_IO(ist), num, MPI_INTEGER,          &
     &        id_rank, 0, SOLVER_COMM, req2(ip), ierr)
        end do
        call MPI_WAITALL (np, req2(1), sta2(1,1), ierr)
      end if
      call MPI_WAITALL (ione, req1(1), sta1(1,1), ierr)
!
!
      num = 6*num_diff_l
      call MPI_ISEND (x_diff, num, MPI_DOUBLE_PRECISION,                &
     &    izero, 0, SOLVER_COMM, req1(ione), ierr)
      if (my_rank .eq. 0) then
        do ip = 1, np
          id_rank = ip - 1
          ist = 6*istack_diff_pe(ip-1) + 1
          num = 6*num_diff_pe(ip)
          call MPI_IRECV (x_diff_IO(ist), num,                          &
     &         MPI_DOUBLE_PRECISION, id_rank, 0, SOLVER_COMM, req2(ip), &
     &        ierr)
        end do
        call MPI_WAITALL (np, req2(1), sta2(1,1), ierr)
      end if
      call MPI_WAITALL (ione, req1(1), sta1(1,1), ierr)
!
      end subroutine collect_diff_ctest
!
! ----------------------------------------------------------------------
!
      end module collect_diff_4_comm_test
