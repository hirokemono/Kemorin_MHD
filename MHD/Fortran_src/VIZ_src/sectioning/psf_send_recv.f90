!psf_send_recv.f90
!      module psf_send_recv
!
!      Written by H. Matsui on March, 2007
!
!      subroutine psf_grids_send_recv(num_psf, nnod_psf, ntot_output,   &
!     &          istack_nod_para, nnod_recv, istack_nod_recv, xx_psf,   &
!     &          send, recv, xx_output)
!      subroutine psf_hash_send_recv(num_psf, nnod_psf, ntot_output,    &
!     &          istack_nod_para, nnod_recv, istack_nod_recv, ihash_psf,&
!     &          isend, irecv, ihash_psf_gl)
!
!      subroutine psf_connect_send_recv(num_psf, nele_psf, ntot_output, &
!     &          istack_nod_para, istack_ele_para,                      &
!     &          nele_recv, istack_ele_recv, ie_patch,                  &
!     &          isend, irecv, ie_output)
!      subroutine psf_results_send_recv(nnod_psf, ntot_output,          &
!     &          istack_nod_para, nnod_recv, istack_nod_recv, ncomp_dat,&
!     &          dat_psf, send, recv, dat_out)
!
      module psf_send_recv
!
      use m_precision
!
      use m_constants
      use m_phys_constants
      use m_parallel_var_dof
      use m_mpi_flags_4_section
!
      use copy_psf_data_to_SR
!
      implicit  none
!
      integer(kind = kint), parameter :: rank0 = 0
      private :: rank0
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine psf_grids_send_recv(num_psf, nnod_psf, ntot_output,    &
     &          istack_nod_para, nnod_recv, istack_nod_recv, xx_psf,    &
     &          send, recv, xx_output)
!
      integer(kind = kint), intent(in) :: nnod_psf, num_psf
      integer(kind = kint), intent(in)                                  &
     &      :: istack_nod_para(0:nprocs*num_psf)
      integer(kind = kint), intent(in) :: nnod_recv(nprocs*num_psf)
      integer(kind = kint), intent(in)                                  &
     &      :: istack_nod_recv(0:nprocs*num_psf)
      integer(kind = kint), intent(in) :: ntot_output
      real(kind = kreal), intent(in) :: xx_psf(nnod_psf,n_vector)
!
      real(kind = kreal), intent(inout) :: send(nnod_psf*n_vector)
      real(kind = kreal), intent(inout) :: recv(ntot_output*n_vector)
      real(kind = kreal), intent(inout)                                 &
     &                   :: xx_output(ntot_output,n_vector)
!
      integer(kind = kint) :: num_send, num_recv
      integer(kind = kint) :: ip, ip_sent, ist, i
!
!      do i = 1, nnod_psf
!        write(70+my_rank,*) i, xx_psf(i,1:3)
!      end do
!      close(70+my_rank)
!
      call set_real_data_2_send_psf(nnod_psf, n_vector, xx_psf, send)
!
      num_send = n_vector*nnod_psf
      call MPI_ISEND (send(1), num_send, MPI_DOUBLE_PRECISION,          &
     &      rank0, 0, SOLVER_COMM,  req1_psf(1), ierr)
!
      if (my_rank .eq. rank0) then
        do ip = 1, nprocs
          ip_sent = ip - 1
          ist = n_vector*istack_nod_recv( (ip-1)*num_psf ) + 1
          num_recv = n_vector * (istack_nod_recv(ip*num_psf)            &
     &                       - istack_nod_recv( (ip-1)*num_psf ) )
          call MPI_IRECV (recv(ist), num_recv, MPI_DOUBLE_PRECISION,    &
     &        ip_sent, 0, SOLVER_COMM, req2_psf(ip), ierr)
        end do
!
        call MPI_WAITALL (nprocs, req2_psf, sta2_psf, ierr)
!
        call set_recv_2_real_data_psf(nprocs, num_psf,                  &
     &      ntot_output, istack_nod_para, nnod_recv, istack_nod_recv,   &
     &      n_vector, recv, xx_output)
!
!        do i = 1, ntot_output
!          write(80+my_rank,*) i, xx_output(i,1:3)
!        end do
!        close(80+my_rank)
!
      end if
!
      call MPI_WAITALL (ione, req1_psf(ione), sta1_psf(ione,ione),      &
     &    ierr)
!
      end subroutine psf_grids_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine psf_hash_send_recv(num_psf, nnod_psf, ntot_output,     &
     &          istack_nod_para, nnod_recv, istack_nod_recv, ihash_psf, &
     &          isend, irecv, ihash_psf_gl)
!
      integer(kind = kint), intent(in) :: nnod_psf, num_psf
      integer(kind = kint), intent(in)                                  &
     &      :: istack_nod_para(0:nprocs*num_psf)
      integer(kind = kint), intent(in) :: nnod_recv(nprocs*num_psf)
      integer(kind = kint), intent(in)                                  &
     &      :: istack_nod_recv(0:nprocs*num_psf)
      integer(kind = kint), intent(in) :: ntot_output
      integer(kind = kint), intent(in) :: ihash_psf(nnod_psf)
!
      integer(kind = kint), intent(inout) :: isend(nnod_psf)
      integer(kind = kint), intent(inout) :: irecv(ntot_output)
      integer(kind = kint), intent(inout)                               &
     &                   :: ihash_psf_gl(ntot_output)
!
      integer(kind = kint) :: num_recv
      integer(kind = kint) :: ip, ip_sent, ist
!
!      do i = 1, nnod_psf
!        write(70+my_rank,*) i, ihash_psf(i)
!      end do
!      close(70+my_rank)
!
      call set_int_data_2_send_psf(nnod_psf, ione, ihash_psf, isend(1))
!
      call MPI_ISEND(isend(1), nnod_psf, MPI_INTEGER,                   &
     &    rank0, 0, SOLVER_COMM,  req1_psf(1), ierr)
!
      if (my_rank .eq. rank0) then
        do ip = 1, nprocs
          ip_sent = ip - 1
          ist = istack_nod_recv( (ip-1)*num_psf ) + 1
          num_recv = (istack_nod_recv(ip*num_psf)                       &
     &              - istack_nod_recv( (ip-1)*num_psf ) )
          call MPI_IRECV (irecv(ist), num_recv, MPI_INTEGER,            &
     &        ip_sent, 0, SOLVER_COMM, req2_psf(ip), ierr)
        end do
!
        call MPI_WAITALL (nprocs, req2_psf, sta2_psf, ierr)
!
        call set_recv_2_int_data_psf(nprocs, num_psf,                  &
     &      ntot_output, istack_nod_para, nnod_recv, istack_nod_recv,   &
     &      ione, irecv, ihash_psf_gl)
!
!        do i = 1, ntot_output
!          write(80+my_rank,*) i, ihash_psf_gl(i,1:3)
!        end do
!        close(80+my_rank)
!
      end if
!
      call MPI_WAITALL (ione, req1_psf(ione), sta1_psf(ione,ione),      &
     &    ierr)
!
      end subroutine psf_hash_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine psf_connect_send_recv(num_psf, nele_psf, ntot_output,  &
     &          istack_nod_para, istack_ele_para,                       &
     &          nele_recv, istack_ele_recv, ie_patch,                   &
     &          isend, irecv, ie_output)
!
      integer(kind = kint), intent(in) :: num_psf, nele_psf
      integer(kind = kint), intent(in) :: ntot_output
      integer(kind = kint), intent(in)                                  &
     &      :: istack_nod_para(0:nprocs*num_psf)
      integer(kind = kint), intent(in)                                  &
     &      :: istack_ele_para(0:nprocs*num_psf)
      integer(kind = kint), intent(in) :: nele_recv(nprocs*num_psf)
      integer(kind = kint), intent(in)                                  &
     &      :: istack_ele_recv(0:nprocs*num_psf)
      integer(kind = kint), intent(in) :: ie_patch(nele_psf,3)
!
      integer(kind = kint), intent(inout) :: isend(nele_psf*3)
      integer(kind = kint), intent(inout) :: irecv(ntot_output*3)
      integer(kind = kint), intent(inout) :: ie_output(ntot_output,3)
!
      integer(kind = kint) :: num_send, num_recv
      integer(kind = kint) :: ip, ip_sent, ist
!
!      do i = 1, nele_psf
!        write(50+my_rank,*) i, ie_patch(i,1:3)
!      end do
!      close(50+my_rank)
!
!
      call set_int_data_2_send_psf(nele_psf, ithree, ie_patch, isend)
!
      num_send = ithree*nele_psf
      call MPI_ISEND (isend(1), num_send, MPI_INTEGER,                  &
     &      rank0, 0, SOLVER_COMM,  req1_psf(1), ierr)
!
      if (my_rank .eq. rank0) then
        do ip = 1, nprocs
          ip_sent = ip - 1
          ist = ithree*istack_ele_recv( (ip-1)*num_psf ) + 1
          num_recv = ithree * (istack_ele_recv(ip*num_psf)              &
     &                     - istack_ele_recv( (ip-1)*num_psf ) )
          call MPI_IRECV (irecv(ist), num_recv, MPI_INTEGER,    &
     &        ip_sent, 0, SOLVER_COMM, req2_psf(ip), ierr)
        end do
!
        call MPI_WAITALL (nprocs, req2_psf, sta2_psf, ierr)
!
        call set_recv_2_int_data_psf(nprocs, num_psf,                   &
     &      ntot_output, istack_ele_para, nele_recv, istack_ele_recv,   &
     &      ithree, irecv, ie_output)
!
        call adjust_patch_connect_4_collect(nprocs, num_psf,            &
     &      ntot_output, istack_nod_para,                               &
     &      istack_ele_para, ithree, ie_output)
!
!        write(60+my_rank,*) 'istack_ele_para', istack_ele_para
!        write(60+my_rank,*) 'istack_nod_para', istack_nod_para
!        do i = 1, ntot_output
!          write(60+my_rank,*) i, ie_output(i,1:3)
!        end do
!        close(60+my_rank)
!
      end if
!
      call MPI_WAITALL (ione, req1_psf(ione), sta1_psf(ione,ione),      &
     &    ierr)
!
      end subroutine psf_connect_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine psf_results_send_recv(num_psf, nnod_psf, ntot_output,  &
     &          istack_nod_para, nnod_recv, istack_nod_recv, ncomp_dat, &
     &          dat_psf, send, recv, dat_out)
!
      integer(kind = kint), intent(in) :: num_psf, nnod_psf, ncomp_dat
      integer(kind = kint), intent(in) :: ntot_output
      integer(kind = kint), intent(in)                                  &
     &      :: istack_nod_para(0:nprocs*num_psf)
      integer(kind = kint), intent(in) :: nnod_recv(nprocs*num_psf)
      integer(kind = kint), intent(in)                                  &
     &      :: istack_nod_recv(0:nprocs*num_psf)
      real(kind = kreal), intent(in) :: dat_psf(nnod_psf, ncomp_dat)
!
      real(kind = kreal), intent(inout) :: send(nnod_psf*ncomp_dat)
      real(kind = kreal), intent(inout) :: recv(ntot_output*ncomp_dat)
      real(kind = kreal), intent(inout)                                 &
     &                   :: dat_out(ntot_output,ncomp_dat)
!
      integer(kind = kint) :: num_send, num_recv
      integer(kind = kint) :: ip, ip_sent, ist
!
!
!
      call set_real_data_2_send_psf(nnod_psf, ncomp_dat, dat_psf, send)
!
      num_send = ncomp_dat*nnod_psf
      call MPI_ISEND (send(1), num_send, MPI_DOUBLE_PRECISION,          &
     &      rank0, 0, SOLVER_COMM,  req1_psf(1), ierr)
!
      if (my_rank .eq. rank0) then
        do ip = 1, nprocs
          ip_sent = ip - 1
          ist = ncomp_dat*istack_nod_recv( (ip-1)*num_psf ) + 1
          num_recv = ncomp_dat * (istack_nod_recv(ip*num_psf)           &
     &                          - istack_nod_recv( (ip-1)*num_psf ) )
          call MPI_IRECV (recv(ist), num_recv, MPI_DOUBLE_PRECISION,    &
     &        ip_sent, 0, SOLVER_COMM, req2_psf(ip), ierr)
        end do
!
        call MPI_WAITALL (nprocs, req2_psf, sta2_psf, ierr)
!
        call set_recv_2_real_data_psf(nprocs, num_psf,                  &
     &      ntot_output, istack_nod_para, nnod_recv, istack_nod_recv,   &
     &      ncomp_dat, recv, dat_out)
!
      end if
!
      call MPI_WAITALL (ione, req1_psf(ione), sta1_psf(ione,ione),      &
     &    ierr)
!
      end subroutine psf_results_send_recv
!
! ----------------------------------------------------------------------
!
      end module psf_send_recv
