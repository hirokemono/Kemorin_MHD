!second_fields_send_recv.f90
!      module second_fields_send_recv
!
!      Written by H. Matsui on July, 2005
!      Modified by H. Matsui on July, 2008
!
!      subroutine phys_2nd_send_recv_all
!
!      subroutine scalar_2nd_send_recv(id_phys)
!      subroutine vector_2nd_send_recv(id_phys)
!      subroutine sym_tensor_2nd_send_recv(id_phys)
!         id_phys:  field ID of nodal fields
!
      module second_fields_send_recv
!
      use m_precision
!
      use m_parallel_var_dof
      use m_2nd_pallalel_vector
      use m_2nd_geometry_param
      use m_2nd_nod_comm_table
      use m_2nd_phys_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine phys_2nd_send_recv_all
!
      use m_machine_parameter
      use m_phys_constants
!
      integer (kind=kint) :: i, ist
!
!
      do i = 1, num_nod_phys_2nd
        ist = istack_nod_comps_2nd(i-1) + 1
!
        if (ncomps_nod_2nd(i) .eq. n_vector) then
!
          if (iflag_debug .gt.0)                                        &
     &     write(*,*) 'comm. 2nd vect: ', trim(phys_nod_name_2nd(i))
          call vector_2nd_send_recv(ist)
!
        else if (ncomps_nod_2nd(i) .eq. n_scalar) then
!
         if (iflag_debug .gt.0)                                         &
     &     write(*,*) 'comm. 2nd scalar: ', trim(phys_nod_name_2nd(i))
          call scalar_2nd_send_recv(ist)
!
        else if (ncomps_nod_2nd(i) .eq. n_sym_tensor) then
!
         if (iflag_debug .gt.0)                                         &
     &     write(*,*) 'comm. 2nd tensor: ', trim(phys_nod_name_2nd(i))
          call sym_tensor_2nd_send_recv(ist)
        end if
      end do
!
      end subroutine phys_2nd_send_recv_all
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine scalar_2nd_send_recv(id_phys)
!
      use solver_SR
!
      integer (kind = kint), intent(in) :: id_phys
      integer (kind = kint) :: inod
!
!
!$omp parallel do
      do inod=1, nnod_2nd
        xvec_2nd(inod) = d_nod_2nd(inod,id_phys)
      end do
!$omp end parallel do
!
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV(nnod_2nd, num_neib_2, id_neib_2,            &
     &                      istack_import_2, item_import_2,             &
     &                      istack_export_2, item_export_2,             &
     &                      xvec_2nd(1) )
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!
!$omp parallel do
      do inod=1, nnod_2nd
        d_nod_2nd(inod,id_phys) = xvec_2nd(inod)
      end do
!$omp end parallel do
!
      end subroutine scalar_2nd_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine vector_2nd_send_recv(id_phys)
!
      use solver_SR_3
!
      integer (kind = kint), intent(in) :: id_phys
      integer (kind = kint) :: inod
!
!
!$omp parallel do
      do inod=1, nnod_2nd
        xvec_2nd(3*inod-2) = d_nod_2nd(inod,id_phys  )
        xvec_2nd(3*inod-1) = d_nod_2nd(inod,id_phys+1)
        xvec_2nd(3*inod  ) = d_nod_2nd(inod,id_phys+2)
      end do
!$omp end parallel do
!
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_3(nnod_2nd, num_neib_2, id_neib_2,          &
     &                        istack_import_2, item_import_2,           &
     &                        istack_export_2, item_export_2,           &
     &                        xvec_2nd(1) )
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!
!$omp parallel do
      do inod=1, nnod_2nd
        d_nod_2nd(inod,id_phys  ) = xvec_2nd(3*inod-2)
        d_nod_2nd(inod,id_phys+1) = xvec_2nd(3*inod-1)
        d_nod_2nd(inod,id_phys+2) = xvec_2nd(3*inod  )
      end do
!$omp end parallel do
!
      end subroutine vector_2nd_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine sym_tensor_2nd_send_recv(id_phys)
!
      use solver_SR_6
!
      integer (kind = kint), intent(in) :: id_phys
      integer (kind = kint) :: inod
!
!
!$omp parallel do
      do inod=1, nnod_2nd
        xvec_2nd(6*inod-5) = d_nod_2nd(inod,id_phys  )
        xvec_2nd(6*inod-4) = d_nod_2nd(inod,id_phys+1)
        xvec_2nd(6*inod-3) = d_nod_2nd(inod,id_phys+2)
        xvec_2nd(6*inod-2) = d_nod_2nd(inod,id_phys+3)
        xvec_2nd(6*inod-1) = d_nod_2nd(inod,id_phys+4)
        xvec_2nd(6*inod  ) = d_nod_2nd(inod,id_phys+5)
      end do
!$omp end parallel do
!
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_6                                           &
     &      (nnod_2nd, num_neib_2, id_neib_2,                           &
     &       istack_import_2, item_import_2,                            &
     &       istack_export_2, item_export_2, xvec_2nd(1) )
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!
!$omp parallel do
      do inod=1, nnod_2nd
        d_nod_2nd(inod,id_phys  ) = xvec_2nd(6*inod-5)
        d_nod_2nd(inod,id_phys+1) = xvec_2nd(6*inod-4)
        d_nod_2nd(inod,id_phys+2) = xvec_2nd(6*inod-3)
        d_nod_2nd(inod,id_phys+3) = xvec_2nd(6*inod-2)
        d_nod_2nd(inod,id_phys+4) = xvec_2nd(6*inod-1)
        d_nod_2nd(inod,id_phys+5) = xvec_2nd(6*inod  )
      end do
!$omp end parallel do
!
      end subroutine sym_tensor_2nd_send_recv
!
! ----------------------------------------------------------------------
!
      end module second_fields_send_recv
