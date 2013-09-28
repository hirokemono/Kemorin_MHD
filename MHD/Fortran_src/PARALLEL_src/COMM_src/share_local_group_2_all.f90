!share_local_group_2_all.f90
!      module share_local_group_2_all
!
!     Written by H. Matsui on Aug., 2011
!
!
!      subroutine share_local_num_to_all(n_local, ntot, nmax, num_each, &
!     &          istack)
!
!      subroutine share_local_int_to_all(n_local, ntot, nmax, num_each, &
!     &          istack, int_send, int_recv, int_local, int_share)
!      subroutine share_local_scalar_to_all(n_local, ntot, nmax,        &
!     &          num_each, istack, real_send, real_recv,                &
!     &          real_local, real_share)
!      subroutine share_local_vector_to_all(n_local, ntot, nmax,        &
!     &          num_each, istack, real_send, real_recv,                &
!     &          vect_local, vect_share)
!      subroutine share_local_tensor_to_all(n_local, ntot, nmax,        &
!     &          num_each, istack, real_send, real_recv,                &
!     &          vect_local, vect_share)
!
      module share_local_group_2_all
!
      use m_precision
!
      use m_constants
      use m_parallel_var_dof
      use calypso_mpi
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine share_local_num_to_all(n_local, ntot, nmax, num_each,  &
     &          istack)
!
      integer(kind = kint), intent(in) :: n_local
!
      integer(kind = kint), intent(inout) :: ntot, nmax
      integer(kind = kint), intent(inout) :: num_each(nprocs)
      integer(kind = kint), intent(inout) :: istack(0:nprocs)
!
      integer(kind = kint) :: ip, i, inod, inum
!
!
      call MPI_AllGather(n_local, ione, MPI_INTEGER,                    &
     &    num_each, ione, MPI_INTEGER, SOLVER_COMM, ierr)
!
      istack(0) = 0
      do ip = 1, nprocs
          istack(ip) = istack(ip-1) + num_each(ip)
      end do
      ntot = istack(nprocs)
      nmax = maxval(num_each)
!
      end subroutine share_local_num_to_all
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine share_local_int_to_all(n_local, ntot, nmax, num_each,  &
     &          istack, int_send, int_recv, int_local, int_share)
!
      integer(kind = kint), intent(in) :: n_local
      integer(kind = kint), intent(in) :: ntot, nmax
      integer(kind = kint), intent(in) :: num_each(nprocs)
      integer(kind = kint), intent(in) :: istack(0:nprocs)
      integer(kind = kint), intent(in) :: int_local(n_local)
!
      integer(kind = kint), intent(inout) :: int_send(nmax)
      integer(kind = kint), intent(inout) :: int_recv(nmax*nprocs)
      integer(kind = kint), intent(inout) :: int_share(ntot)
!
      integer(kind = kint) :: ip, i, inod, inum
!
!
      int_send(1:n_local) = int_local(1:n_local)
!
      call MPI_AllGather(int_send, nmax, MPI_INTEGER,                   &
     &    int_recv, nmax, MPI_INTEGER, SOLVER_COMM, ierr)
!
      do ip = 1, nprocs
        do i = 1, num_each(ip)
          inod = i + istack(ip-1)
          inum = i + (ip-1)*nmax
          int_share(inod) = int_recv(inum)
        end do
      end do
!
      end subroutine share_local_int_to_all
!
!  ---------------------------------------------------------------------
!
      subroutine share_local_scalar_to_all(n_local, ntot, nmax,         &
     &          num_each, istack, real_send, real_recv,                 &
     &          real_local, real_share)
!
      integer(kind = kint), intent(in) :: n_local
      integer(kind = kint), intent(in) :: ntot, nmax
      integer(kind = kint), intent(in) :: num_each(nprocs)
      integer(kind = kint), intent(in) :: istack(0:nprocs)
      real(kind = kreal), intent(in) :: real_local(n_local)
!
      real(kind = kreal), intent(inout) :: real_send(nmax)
      real(kind = kreal), intent(inout) :: real_recv(nmax*nprocs)
      real(kind = kreal), intent(inout) :: real_share(ntot)
!
      integer(kind = kint) :: ip, i, inod, inum
!
!
      real_send(1:n_local) = real_local(1:n_local)
!
      call MPI_AllGather(real_send(1), nmax, MPI_DOUBLE_PRECISION,      &
     &    real_recv(1), nmax, MPI_DOUBLE_PRECISION,  SOLVER_COMM, ierr)
!
      do ip = 1, nprocs
        do i = 1, num_each(ip)
          inod = i + istack(ip-1)
          inum = i + (ip-1)*nmax
          real_share(inod) = real_recv(inum)
        end do
      end do
!
      end subroutine share_local_scalar_to_all
!
!  ---------------------------------------------------------------------
!
      subroutine share_local_vector_to_all(n_local, ntot, nmax,         &
     &          num_each, istack, real_send, real_recv,                 &
     &          vect_local, vect_share)
!
      integer(kind = kint), intent(in) :: n_local
      integer(kind = kint), intent(in) :: ntot, nmax
      integer(kind = kint), intent(in) :: num_each(nprocs)
      integer(kind = kint), intent(in) :: istack(0:nprocs)
      real(kind = kreal), intent(in) :: vect_local(n_local,3)
!
      real(kind = kreal), intent(inout) :: real_send(3*nmax)
      real(kind = kreal), intent(inout) :: real_recv(3*nmax*nprocs)
      real(kind = kreal), intent(inout) :: vect_share(ntot,3)
!
      integer(kind = kint) :: ip, i, inod, inum
!
!
      do inod = 1, n_local
        real_send(3*inod-2) = vect_local(inod,1)
        real_send(3*inod-1) = vect_local(inod,2)
        real_send(3*inod  ) = vect_local(inod,3)
      end do
!
      call MPI_AllGather                                                &
     &    (real_send(1), (ithree*nmax), MPI_DOUBLE_PRECISION,           &
     &     real_recv(1), (ithree*nmax), MPI_DOUBLE_PRECISION,           &
     &     SOLVER_COMM, ierr)
!
      do ip = 1, nprocs
        do i = 1, num_each(ip)
          inod = i + istack(ip-1)
          inum = i + (ip-1)*nmax
          vect_share(inod,1) = real_recv(3*inum-2)
          vect_share(inod,2) = real_recv(3*inum-1)
          vect_share(inod,3) = real_recv(3*inum  )
        end do
      end do
!
      end subroutine share_local_vector_to_all
!
!  ---------------------------------------------------------------------
!
      subroutine share_local_tensor_to_all(n_local, ntot, nmax,         &
     &          num_each, istack, real_send, real_recv,                 &
     &          vect_local, vect_share)
!
      integer(kind = kint), intent(in) :: n_local
      integer(kind = kint), intent(in) :: ntot, nmax
      integer(kind = kint), intent(in) :: num_each(nprocs)
      integer(kind = kint), intent(in) :: istack(0:nprocs)
      real(kind = kreal), intent(in) :: vect_local(n_local,6)
!
      real(kind = kreal), intent(inout) :: real_send(6*nmax)
      real(kind = kreal), intent(inout) :: real_recv(6*nmax*nprocs)
      real(kind = kreal), intent(inout) :: vect_share(ntot,6)
!
      integer(kind = kint) :: ip, i, inod, inum
!
!
      do inod = 1, n_local
        real_send(6*inod-5) = vect_local(inod,1)
        real_send(6*inod-4) = vect_local(inod,2)
        real_send(6*inod-3) = vect_local(inod,3)
        real_send(6*inod-2) = vect_local(inod,4)
        real_send(6*inod-1) = vect_local(inod,5)
        real_send(6*inod  ) = vect_local(inod,6)
      end do
!
      call MPI_AllGather                                                &
     &    (real_send(1), (isix*nmax), MPI_DOUBLE_PRECISION,             &
     &     real_recv(1), (isix*nmax), MPI_DOUBLE_PRECISION,             &
     &     SOLVER_COMM, ierr)
!
      do ip = 1, nprocs
        do i = 1, num_each(ip)
          inod = i + istack(ip-1)
          inum = i + (ip-1)*nmax
          vect_share(inod,1) = real_recv(6*inum-5)
          vect_share(inod,2) = real_recv(6*inum-4)
          vect_share(inod,3) = real_recv(6*inum-3)
          vect_share(inod,4) = real_recv(6*inum-2)
          vect_share(inod,5) = real_recv(6*inum-1)
          vect_share(inod,6) = real_recv(6*inum  )
        end do
      end do
!
      end subroutine share_local_tensor_to_all
!
!  ---------------------------------------------------------------------
!
      end module share_local_group_2_all
