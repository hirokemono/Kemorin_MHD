!
!      module prepare_field_2_filter
!
!      Written by H. Matsui on July, 2005
!      Modified by H. Matsui on July, 2008
!
!!      subroutine prepare_scalar_2_filter                              &
!!     &         (numnod, internal_node, ntot_comp, id_phys, d_nod)
!!      subroutine prepare_vector_2_filter                              &
!!     &         (numnod, internal_node, ntot_comp, id_phys, d_nod)
!!      subroutine prepare_sym_tensor_2_filter                          &
!!     &         (numnod, internal_node, ntot_comp, id_phys, d_nod)
!         id_phys:  field ID of nodal fields
!
      module prepare_field_2_filter
!
      use m_precision
!
      use calypso_mpi
      use m_work_time
      use m_nod_filter_comm_table
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine prepare_scalar_2_filter                                &
     &         (numnod, internal_node, ntot_comp, id_phys, d_nod)
!
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: ntot_comp, id_phys
      real(kind = kreal), intent(in) :: d_nod(numnod,ntot_comp)
!
      integer (kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, internal_node
        x_vec_filtering(inod) = d_nod(inod,id_phys)
      end do
!$omp end parallel do
!
!$omp parallel do
      do inod = internal_node+1, nnod_filtering
        x_vec_filtering(inod) = 0.0d0
      end do
!$omp end parallel do
!
      START_SRtime= MPI_WTIME()
      call SOLVER_SEND_RECV_type                                        &
     &    (nnod_filtering, flt_comm, x_vec_filtering(1))
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      end subroutine prepare_scalar_2_filter
!
! ----------------------------------------------------------------------
!
      subroutine prepare_vector_2_filter                                &
     &         (numnod, internal_node, ntot_comp, id_phys, d_nod)
!
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: ntot_comp, id_phys
      real(kind = kreal), intent(in) :: d_nod(numnod,ntot_comp)
!
      integer (kind = kint) :: inod
!
!
!$omp parallel do
      do inod=1, internal_node
        x_vec_filtering(3*inod-2) = d_nod(inod,id_phys  )
        x_vec_filtering(3*inod-1) = d_nod(inod,id_phys+1)
        x_vec_filtering(3*inod  ) = d_nod(inod,id_phys+2)
      end do
!$omp end parallel do
!
!$omp parallel do
      do inod = internal_node+1, nnod_filtering
        x_vec_filtering(3*inod-2) = 0.0d0
        x_vec_filtering(3*inod-1) = 0.0d0
        x_vec_filtering(3*inod  ) = 0.0d0
      end do
!$omp end parallel do
!
!      write(*,*) 'flt_comm%istack_import(flt_comm%num_neib)',          &
!     &           flt_comm%istack_import(flt_comm%num_neib)
!      write(*,*) 'flt_comm%istack_export(flt_comm%num_neib)',          &
!     &           flt_comm%istack_export(flt_comm%num_neib)
!      write(*,*) 'flt_comm%item_import', size(flt_comm%item_import)
!      write(*,*) 'flt_comm%item_export', size(flt_comm%item_export)
!
      START_SRtime= MPI_WTIME()
      call SOLVER_SEND_RECV_3_type                                      &
     &    (nnod_filtering, flt_comm, x_vec_filtering(1))
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      end subroutine prepare_vector_2_filter
!
! ----------------------------------------------------------------------
!
      subroutine prepare_sym_tensor_2_filter                            &
     &         (numnod, internal_node, ntot_comp, id_phys, d_nod)
!
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: ntot_comp, id_phys
      real(kind = kreal), intent(in) :: d_nod(numnod,ntot_comp)
!
      integer (kind = kint) :: inod
!
!
!$omp parallel do
      do inod=1, internal_node
        x_vec_filtering(6*inod-5) = d_nod(inod,id_phys  )
        x_vec_filtering(6*inod-4) = d_nod(inod,id_phys+1)
        x_vec_filtering(6*inod-3) = d_nod(inod,id_phys+2)
        x_vec_filtering(6*inod-2) = d_nod(inod,id_phys+3)
        x_vec_filtering(6*inod-1) = d_nod(inod,id_phys+4)
        x_vec_filtering(6*inod  ) = d_nod(inod,id_phys+5)
      end do
!$omp end parallel do
!
!$omp parallel do
      do inod = internal_node+1, nnod_filtering
        x_vec_filtering(6*inod-5) = 0.0d0
        x_vec_filtering(6*inod-4) = 0.0d0
        x_vec_filtering(6*inod-3) = 0.0d0
        x_vec_filtering(6*inod-2) = 0.0d0
        x_vec_filtering(6*inod-1) = 0.0d0
        x_vec_filtering(6*inod  ) = 0.0d0
      end do
!$omp end parallel do
!
      START_SRtime= MPI_WTIME()
      call SOLVER_SEND_RECV_6_type                                      &
     &    (nnod_filtering, flt_comm, x_vec_filtering(1))
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      end subroutine prepare_sym_tensor_2_filter
!
! ----------------------------------------------------------------------
!
      end module prepare_field_2_filter
