!prepare_field_2_w_filter.f90
!      module prepare_field_2_w_filter
!
!      Written by H. Matsui on Nov., 2008
!
!      subroutine prepare_scalar_2_w_fil(ntot_comp, id_phys, d_nod)
!      subroutine prepare_vector_2_w_fil(ntot_comp, id_phys, d_nod)
!      subroutine prepare_sym_tensor_2_w_fil(ntot_comp, id_phys, d_nod)
!         id_phys:  field ID of nodal fields
!
      module prepare_field_2_w_filter
!
      use m_precision
!
      use calypso_mpi
      use m_work_time
      use m_geometry_data
      use m_nod_w_filter_comm_table
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine prepare_scalar_2_w_fil(ntot_comp, id_phys, d_nod)
!
      use solver_SR
!
      integer(kind = kint), intent(in) :: ntot_comp, id_phys
      real(kind = kreal), intent(in) :: d_nod(node1%numnod,ntot_comp)
!
      integer (kind = kint) :: inod
!
!
!$omp parallel do
      do inod=1, node1%internal_node
        x_vec_w_fil(inod) = d_nod(inod,id_phys)
      end do
!$omp end parallel do
!
!$omp parallel do
      do inod = node1%internal_node+1, nnod_w_filtering
        x_vec_w_fil(inod) = 0.0d0
      end do
!$omp end parallel do
!
      START_SRtime= MPI_WTIME()
      call SOLVER_SEND_RECV                                             &
     &    (nnod_w_filtering, num_neib_w_fil, id_neib_w_fil,             &
     &     istack_import_w_fil, item_import_w_fil,                      &
     &     istack_export_w_fil, item_export_w_fil, x_vec_w_fil(1) )
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      end subroutine prepare_scalar_2_w_fil
!
! ----------------------------------------------------------------------
!
      subroutine prepare_vector_2_w_fil(ntot_comp, id_phys, d_nod)
!
      use solver_SR_3
!
      integer(kind = kint), intent(in) :: ntot_comp, id_phys
      real(kind = kreal), intent(in) :: d_nod(node1%numnod,ntot_comp)
!
      integer (kind = kint) :: inod
!
!
!$omp parallel do
      do inod=1, node1%internal_node
        x_vec_w_fil(3*inod-2) = d_nod(inod,id_phys  )
        x_vec_w_fil(3*inod-1) = d_nod(inod,id_phys+1)
        x_vec_w_fil(3*inod  ) = d_nod(inod,id_phys+2)
      end do
!$omp end parallel do
!
!$omp parallel do
      do inod = node1%internal_node+1, nnod_w_filtering
        x_vec_w_fil(3*inod-2) = 0.0d0
        x_vec_w_fil(3*inod-1) = 0.0d0
        x_vec_w_fil(3*inod  ) = 0.0d0
      end do
!$omp end parallel do
!
      START_SRtime= MPI_WTIME()
      call SOLVER_SEND_RECV_3                                           &
     &    (nnod_w_filtering, num_neib_w_fil, id_neib_w_fil,             &
     &     istack_import_w_fil, item_import_w_fil,                      &
     &     istack_export_w_fil, item_export_w_fil, x_vec_w_fil(1))
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      end subroutine prepare_vector_2_w_fil
!
! ----------------------------------------------------------------------
!
      subroutine prepare_sym_tensor_2_w_fil(ntot_comp, id_phys, d_nod)
!
      use solver_SR_6
!
      integer(kind = kint), intent(in) :: ntot_comp, id_phys
      real(kind = kreal), intent(in) :: d_nod(node1%numnod,ntot_comp)
!
      integer (kind = kint) :: inod
!
!
!$omp parallel do
      do inod=1, node1%internal_node
        x_vec_w_fil(6*inod-5) = d_nod(inod,id_phys  )
        x_vec_w_fil(6*inod-4) = d_nod(inod,id_phys+1)
        x_vec_w_fil(6*inod-3) = d_nod(inod,id_phys+2)
        x_vec_w_fil(6*inod-2) = d_nod(inod,id_phys+3)
        x_vec_w_fil(6*inod-1) = d_nod(inod,id_phys+4)
        x_vec_w_fil(6*inod  ) = d_nod(inod,id_phys+5)
      end do
!$omp end parallel do
!
!$omp parallel do
      do inod = node1%internal_node+1, nnod_w_filtering
        x_vec_w_fil(6*inod-5) = 0.0d0
        x_vec_w_fil(6*inod-4) = 0.0d0
        x_vec_w_fil(6*inod-3) = 0.0d0
        x_vec_w_fil(6*inod-2) = 0.0d0
        x_vec_w_fil(6*inod-1) = 0.0d0
        x_vec_w_fil(6*inod  ) = 0.0d0
      end do
!$omp end parallel do
!
      START_SRtime= MPI_WTIME()
      call SOLVER_SEND_RECV_6                                           &
     &    (nnod_w_filtering, num_neib_w_fil, id_neib_w_fil,             &
     &     istack_import_w_fil, item_import_w_fil,                      &
     &     istack_export_w_fil, item_export_w_fil, x_vec_w_fil(1))
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      end subroutine prepare_sym_tensor_2_w_fil
!
! ----------------------------------------------------------------------
!
      end module prepare_field_2_w_filter
