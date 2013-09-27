!
!      module prepare_field_2_filter
!
!      Written by H. Matsui on July, 2005
!      Modified by H. Matsui on July, 2008
!
!      subroutine prepare_scalar_2_filter(id_phys)
!      subroutine prepare_vector_2_filter(id_phys)
!      subroutine prepare_sym_tensor_2_filter(id_phys)
!         id_phys:  field ID of nodal fields
!
      module prepare_field_2_filter
!
      use m_precision
!
      use m_parallel_var_dof
      use m_geometry_parameter
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
      subroutine prepare_scalar_2_filter(id_phys)
!
      use m_node_phys_data
      use solver_SR
!
      integer (kind = kint), intent(in) :: id_phys
      integer (kind = kint) :: inod
!
!
!$omp parallel do
      do inod=1, internal_node
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
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV                                             &
     &    (nnod_filtering, num_neib_filter, id_neib_filter,             &
     &     istack_import_filter, item_import_filter,                    &
     &     istack_export_filter, item_export_filter,                    &
     &                      x_vec_filtering(1), SOLVER_COMM, my_rank )
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!
!
      end subroutine prepare_scalar_2_filter
!
! ----------------------------------------------------------------------
!
      subroutine prepare_vector_2_filter(id_phys)
!
      use m_node_phys_data
      use solver_SR_3
!
      integer (kind = kint), intent(in) :: id_phys
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
!      write(*,*) 'istack_import_filter(num_neib_filter)', istack_import_filter(num_neib_filter)
!      write(*,*) 'istack_export_filter(num_neib_filter)', istack_export_filter(num_neib_filter)
!      write(*,*) 'item_import_filter', size(item_import_filter)
!      write(*,*) 'item_export_filter', size(item_export_filter)
!
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_3                                           &
     &    (nnod_filtering, num_neib_filter, id_neib_filter,             &
     &     istack_import_filter, item_import_filter,                    &
     &     istack_export_filter, item_export_filter,                    &
     &     x_vec_filtering(1), SOLVER_COMM, my_rank )
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!
      end subroutine prepare_vector_2_filter
!
! ----------------------------------------------------------------------
!
      subroutine prepare_sym_tensor_2_filter(id_phys)
!
      use m_node_phys_data
      use solver_SR_6
!
      integer (kind = kint), intent(in) :: id_phys
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
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_6                                           &
     &    (nnod_filtering, num_neib_filter, id_neib_filter,             &
     &     istack_import_filter, item_import_filter,                    &
     &     istack_export_filter, item_export_filter,                    &
     &     x_vec_filtering(1), SOLVER_COMM, my_rank )
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!
!
      end subroutine prepare_sym_tensor_2_filter
!
! ----------------------------------------------------------------------
!
      end module prepare_field_2_filter
