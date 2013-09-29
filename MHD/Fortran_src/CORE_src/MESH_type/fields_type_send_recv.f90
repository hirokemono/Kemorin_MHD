!fields_type_send_recv.f90
!      module fields_type_send_recv
!
!      Written by H. Matsui on July, 2005
!      Modified by H. Matsui on July, 2008
!
!      subroutine phys_type_send_recv_all(mesh, nod_phys)
!
!      subroutine scalar_type_send_recv(id_phys, mesh, nod_phys)
!      subroutine vector_type_send_recv(id_phys, mesh, nod_phys)
!      subroutine sym_tensor_type_send_recv(id_phys, mesh, nod_phys)
!         id_phys:  field ID of nodal fields
!
      module fields_type_send_recv
!
      use m_precision
!
      use calypso_mpi
      use m_work_time
!
      use t_mesh_data
      use t_phys_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine phys_type_send_recv_all(mesh, nod_phys)
!
      use m_machine_parameter
      use m_phys_constants
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(inout) :: nod_phys
!
      integer (kind=kint) :: i, ist
!
!
      do i = 1, nod_phys%num_phys
        ist = nod_phys%istack_component(i-1) + 1
!
        if (nod_phys%num_component(i) .eq. n_vector) then
!
          if (iflag_debug .gt.0)                                        &
     &     write(*,*) 'comm. vect: ', trim(nod_phys%phys_name(i))
          call vector_type_send_recv(ist, mesh, nod_phys)
!
        else if (nod_phys%num_component(i) .eq. n_scalar) then
!
         if (iflag_debug .gt.0)                                         &
     &     write(*,*) 'comm. scalar: ', trim(nod_phys%phys_name(i))
          call scalar_type_send_recv(ist, mesh, nod_phys)
!
        else if (nod_phys%num_component(i) .eq. n_sym_tensor) then
!
         if (iflag_debug .gt.0)                                         &
     &     write(*,*) 'comm. tensor: ', trim(nod_phys%phys_name(i))
          call sym_tensor_type_send_recv(ist, mesh, nod_phys)
        end if
      end do
!
      end subroutine phys_type_send_recv_all
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine scalar_type_send_recv(id_phys, mesh, nod_phys)
!
      use m_array_for_send_recv
      use solver_SR
!
      integer (kind = kint), intent(in) :: id_phys
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(inout) :: nod_phys
      integer (kind = kint) :: inod
!
!
!$omp parallel do
      do inod=1, mesh%node%numnod
        x_vec(inod) = nod_phys%d_fld(inod,id_phys)
      end do
!$omp end parallel do
!
      START_SRtime= MPI_WTIME()
      call SOLVER_SEND_RECV(mesh%node%numnod,                           &
     &    mesh%nod_comm%num_neib, mesh%nod_comm%id_neib,                &
     &    mesh%nod_comm%istack_import, mesh%nod_comm%item_import,       &
     &    mesh%nod_comm%istack_export, mesh%nod_comm%item_export,       &
     &    x_vec(1) )
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
!$omp parallel do
      do inod=1, mesh%node%numnod
        nod_phys%d_fld(inod,id_phys) = x_vec(inod)
      end do
!$omp end parallel do
!
      end subroutine scalar_type_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine vector_type_send_recv(id_phys, mesh, nod_phys)
!
      use m_array_for_send_recv
      use solver_SR_3
!
      integer (kind = kint), intent(in) :: id_phys
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(inout) :: nod_phys
      integer (kind = kint) :: inod
!
!
!$omp parallel do
      do inod=1, mesh%node%numnod
        x_vec(3*inod-2) = nod_phys%d_fld(inod,id_phys  )
        x_vec(3*inod-1) = nod_phys%d_fld(inod,id_phys+1)
        x_vec(3*inod  ) = nod_phys%d_fld(inod,id_phys+2)
      end do
!$omp end parallel do
!
      START_SRtime= MPI_WTIME()
      call SOLVER_SEND_RECV_3(mesh%node%numnod,                         &
     &    mesh%nod_comm%num_neib, mesh%nod_comm%id_neib,                &
     &    mesh%nod_comm%istack_import, mesh%nod_comm%item_import,       &
     &    mesh%nod_comm%istack_export, mesh%nod_comm%item_export,       &
     &    x_vec(1) )
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
!$omp parallel do
      do inod=1, mesh%node%numnod
        nod_phys%d_fld(inod,id_phys  ) = x_vec(3*inod-2)
        nod_phys%d_fld(inod,id_phys+1) = x_vec(3*inod-1)
        nod_phys%d_fld(inod,id_phys+2) = x_vec(3*inod  )
      end do
!$omp end parallel do
!
      end subroutine vector_type_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine sym_tensor_type_send_recv(id_phys, mesh, nod_phys)
!
      use m_array_for_send_recv
      use solver_SR_6
!
      integer (kind = kint), intent(in) :: id_phys
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(inout) :: nod_phys
      integer (kind = kint) :: inod
!
!
!$omp parallel do
      do inod=1, mesh%node%numnod
        x_vec(6*inod-5) = nod_phys%d_fld(inod,id_phys  )
        x_vec(6*inod-4) = nod_phys%d_fld(inod,id_phys+1)
        x_vec(6*inod-3) = nod_phys%d_fld(inod,id_phys+2)
        x_vec(6*inod-2) = nod_phys%d_fld(inod,id_phys+3)
        x_vec(6*inod-1) = nod_phys%d_fld(inod,id_phys+4)
        x_vec(6*inod  ) = nod_phys%d_fld(inod,id_phys+5)
      end do
!$omp end parallel do
!
      START_SRtime= MPI_WTIME()
      call SOLVER_SEND_RECV_6(mesh%node%numnod,                         &
     &    mesh%nod_comm%num_neib, mesh%nod_comm%id_neib,                &
     &    mesh%nod_comm%istack_import, mesh%nod_comm%item_import,       &
     &    mesh%nod_comm%istack_export, mesh%nod_comm%item_export,       &
     &    x_vec(1))
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
!$omp parallel do
      do inod=1, mesh%node%numnod
        nod_phys%d_fld(inod,id_phys  ) = x_vec(6*inod-5)
        nod_phys%d_fld(inod,id_phys+1) = x_vec(6*inod-4)
        nod_phys%d_fld(inod,id_phys+2) = x_vec(6*inod-3)
        nod_phys%d_fld(inod,id_phys+3) = x_vec(6*inod-2)
        nod_phys%d_fld(inod,id_phys+4) = x_vec(6*inod-1)
        nod_phys%d_fld(inod,id_phys+5) = x_vec(6*inod  )
      end do
!$omp end parallel do
!
      end subroutine sym_tensor_type_send_recv
!
! ----------------------------------------------------------------------
!
      end module fields_type_send_recv
