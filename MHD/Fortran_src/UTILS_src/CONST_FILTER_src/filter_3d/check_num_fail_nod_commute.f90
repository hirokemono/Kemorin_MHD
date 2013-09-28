!check_num_fail_nod_commute.f90
!      module check_num_fail_nod_commute
!
!     Written by H. Matsui on Nov., 2006
!
      module check_num_fail_nod_commute
!
      use m_precision
!
      use calypso_mpi
      use m_parallel_var_dof
!
      implicit none
!
      character(len=kchara), parameter                                  &
     &                  :: num_failed_nod_name = 'num_failed_nodes.dat'
      integer(kind = kint), parameter :: id_failed_nod = 15
!
      integer(kind = kint), allocatable :: num_failed_whole_gl(:)
      integer(kind = kint), allocatable :: num_failed_fluid_gl(:)
!
      integer, save ::              i_req1
      integer, save, allocatable :: i_req2(:)
      integer, save ::              i_sta1(MPI_STATUS_SIZE)
      integer, save, allocatable :: i_sta2(:,:)
      integer(kind = kint), parameter ::izero = 0, ione = 1, itwo = 2
!
      integer(kind = kint) ::              isend_failed(2)
      integer(kind = kint), allocatable :: irecv_failed(:,:)
!
      private :: num_failed_whole_gl, num_failed_fluid_gl
      private :: isend_failed, irecv_failed
      private :: i_req1, i_req2, i_sta1, i_sta2
      private :: izero, itwo
      private :: allocate_num_failed_nodes, deallocate_num_failed_nodes
!
!      subroutine s_check_num_fail_nod_commute
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_num_failed_nodes
!
!
      allocate(num_failed_whole_gl(nprocs))
      allocate(num_failed_fluid_gl(nprocs))
!
      allocate(irecv_failed(2,nprocs-1))
      allocate( i_sta2(MPI_STATUS_SIZE,nprocs-1) )
      allocate( i_req2(nprocs-1) )
!
      num_failed_whole_gl = 0
      num_failed_fluid_gl = 0
      irecv_failed = 0
!
      end subroutine allocate_num_failed_nodes
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_num_failed_nodes
!
!
      deallocate(num_failed_whole_gl)
      deallocate(num_failed_fluid_gl)
!
      deallocate(irecv_failed)
      deallocate( i_req2, i_sta2)
!
      end subroutine deallocate_num_failed_nodes
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine s_check_num_fail_nod_commute
!
      use m_filter_coefs
!
      integer(kind = kint) :: ip, id_dest
!
!
      if (my_rank .eq. izero) call allocate_num_failed_nodes
!
!C-- SEND
      if (my_rank .ne. izero) then
        isend_failed(1) = num_failed_whole
        isend_failed(2) = num_failed_fluid
        call MPI_ISEND (isend_failed(1), itwo, MPI_INTEGER,             &
     &      izero, izero, SOLVER_COMM, i_req1, ierr)
      end if
!C
!C-- RECEIVE
      if (my_rank .eq. izero) then
        do ip = 2, nprocs
          id_dest = ip - 1
          call MPI_IRECV (irecv_failed(1,id_dest), itwo, MPI_INTEGER,   &
     &        id_dest, izero, SOLVER_COMM,  i_req2(id_dest), ierr)
        end do
!
        call MPI_WAITALL (nprocs-1, i_req2(1), i_sta2(1,1), ierr)
!
        do ip= 2, nprocs
          id_dest = ip - 1
          num_failed_whole_gl(ip) = irecv_failed(1,id_dest)
          num_failed_fluid_gl(ip) = irecv_failed(2,id_dest)
        end do
!
        num_failed_whole_gl(1) = num_failed_whole
        num_failed_fluid_gl(1) = num_failed_fluid
      end if
!
      if (my_rank .ne. izero) then
        call MPI_WAITALL (ione, i_req1, i_sta1(1), ierr)
      end if
!
!
      if (my_rank .eq. izero) then
!
        open (id_failed_nod,file=num_failed_nod_name)
        write(id_failed_nod,'(a)') 'Num. failed_nodes'
        write(id_failed_nod,'(a)') '(ID, whole, fluid)'
!
        do ip = 1, nprocs
          id_dest = ip - 1
          write(id_failed_nod,'(3i10)') id_dest,                        &
     &            num_failed_whole_gl(ip), num_failed_fluid_gl(ip)
        end do
        close(id_failed_nod)
!
        call deallocate_num_failed_nodes
      end if
!
      end subroutine s_check_num_fail_nod_commute
!
! -----------------------------------------------------------------------
!
      end module check_num_fail_nod_commute
