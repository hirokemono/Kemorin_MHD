!>@file   nod_phys_send_recv.f90
!!@brief  module nod_phys_send_recv
!!
!!@author H. Matsui
!!@date Programmed in July, 2005
!!      modified in July, 2008
!
!>@brief  Data communication for nodal field
!!
!!@verbatim
!!      subroutine init_nod_send_recv(mesh)
!!      subroutine nod_fields_send_recv(mesh, nod_fld, vect)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(phys_data),intent(inout) :: nod_fld
!!        type(vectors_4_solver), intent(inout) :: vect
!!
!!      subroutine init_send_recv(nod_comm)
!!      subroutine fields_send_recv(nod_comm, nod_fld, vect)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(phys_data),intent(inout) :: nod_fld
!!        type(vectors_4_solver), intent(inout) :: vect
!!
!!      subroutine scalar_send_recv(id_phys, nod_comm, nod_fld, vect)
!!      subroutine vector_send_recv(id_phys, nod_comm, nod_fld, vect)
!!      subroutine sym_tensor_send_recv(id_phys, nod_comm, nod_fld, vect)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(vectors_4_solver), intent(inout) :: vect
!!        type(phys_data),intent(inout) :: nod_fld
!!         id_phys:  field ID of nodal fields
!!
!!      subroutine nod_scalar_send_recv(numnod, nod_comm, scl_nod, vect)
!!      subroutine nod_vector_send_recv(numnod, nod_comm, vec_nod, vect)
!!      subroutine nod_tensor_send_recv(numnod, nod_comm, tsr_nod, vect)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(vectors_4_solver), intent(inout) :: vect
!!@endverbatim
!
      module nod_phys_send_recv
!
      use m_precision
!
      use calypso_mpi
      use t_mesh_data
      use t_phys_data
      use t_vector_for_solver
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_nod_send_recv(mesh)
!
      type(mesh_geometry), intent(in) :: mesh
!
!
      call init_send_recv(mesh%nod_comm)
!
      end subroutine init_nod_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine nod_fields_send_recv(mesh, nod_fld, vect)
!
      use m_machine_parameter
      use m_phys_constants
      use t_phys_data
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data),intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: vect
!
!
      call fields_send_recv(mesh%nod_comm, nod_fld, vect)
!
      end subroutine nod_fields_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine init_send_recv(nod_comm)
!
      use m_phys_constants
      use t_solver_SR
      use m_solver_SR
!
      type(communication_table), intent(in) :: nod_comm
!
!
      call resize_work_SR                                               &
     &   (n_sym_tensor, nod_comm%num_neib, nod_comm%num_neib,           &
     &    nod_comm%ntot_export, nod_comm%ntot_import, SR_sig1, SR_r1)
      call resize_iwork_SR_t(nod_comm%num_neib, nod_comm%num_neib,      &
     &    nod_comm%ntot_export, nod_comm%ntot_import, SR_sig1, SR_i1)
      call resize_i8work_SR(nod_comm%num_neib, nod_comm%num_neib,       &
     &    nod_comm%ntot_export, nod_comm%ntot_import, SR_sig1, SR_il1)
!
      end subroutine init_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine fields_send_recv(nod_comm, nod_fld, vect)
!
      use m_machine_parameter
      use m_phys_constants
      use t_phys_data
!
      type(communication_table), intent(in) :: nod_comm
      type(phys_data),intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: vect
!
      integer (kind=kint) :: i, ist
!
!
      do i = 1, nod_fld%num_phys
        ist = nod_fld%istack_component(i-1) + 1
!
        if (nod_fld%num_component(i) .eq. n_vector) then
          if (iflag_debug .ge. iflag_routine_msg) write(*,*)            &
     &      'comm. for vector of ', trim(nod_fld%phys_name(i))
          call vector_send_recv(ist, nod_comm, nod_fld, vect)
!
        else if (nod_fld%num_component(i) .eq. n_scalar) then
          if (iflag_debug .ge. iflag_routine_msg) write(*,*)            &
     &      'comm. for scaler of ', trim(nod_fld%phys_name(i))
          call scalar_send_recv(ist, nod_comm, nod_fld, vect)
!
        else if (nod_fld%num_component(i) .eq. n_sym_tensor) then
          if (iflag_debug .ge. iflag_routine_msg) write(*,*)            &
     &      'comm. for tensor of ', trim(nod_fld%phys_name(i))
          call sym_tensor_send_recv(ist, nod_comm, nod_fld, vect)
        end if
      end do
!
      end subroutine fields_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine scalar_send_recv(id_phys, nod_comm, nod_fld, vect)
!
      use t_phys_data
!
      integer(kind = kint), intent(in) :: id_phys
      type(communication_table), intent(in) :: nod_comm
      type(phys_data),intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: vect
!
!
      call nod_scalar_send_recv                                         &
     &   (nod_fld%n_point, nod_comm, nod_fld%d_fld(1,id_phys), vect)
!
      end subroutine scalar_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine vector_send_recv(id_phys, nod_comm, nod_fld, vect)
!
      use t_phys_data
!
      integer(kind = kint), intent(in) :: id_phys
      type(communication_table), intent(in) :: nod_comm
      type(phys_data),intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: vect
!
!
      call nod_vector_send_recv                                         &
     &   (nod_fld%n_point, nod_comm,  nod_fld%d_fld(1,id_phys), vect)
!
      end subroutine vector_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine sym_tensor_send_recv(id_phys, nod_comm, nod_fld, vect)
!
      use t_phys_data
!
      integer(kind = kint), intent(in) :: id_phys
      type(communication_table), intent(in) :: nod_comm
      type(phys_data),intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: vect
!
!
      call nod_tensor_send_recv(nod_fld%n_point, nod_comm,              &
     &    nod_fld%d_fld(1,id_phys), vect)
!
      end subroutine sym_tensor_send_recv
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine nod_scalar_send_recv(numnod, nod_comm, scl_nod, vect)
!
      use m_work_time
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
!
      type(vectors_4_solver), intent(inout) :: vect
      real(kind = kreal), intent(inout) :: scl_nod(numnod)
!
      integer(kind=kint)  :: inod
!
!
!$omp parallel do
       do inod=1, numnod
        vect%x_vec(inod) = scl_nod(inod)
       end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_type(numnod, nod_comm, vect%x_vec(1))
!
!$omp parallel do
      do inod=1, numnod
        scl_nod(inod) = vect%x_vec(inod)
      end do
!$omp end parallel do
!
      end subroutine nod_scalar_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine nod_vector_send_recv(numnod, nod_comm, vec_nod, vect)
!
      use m_work_time
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
!
      type(vectors_4_solver), intent(inout) :: vect
      real(kind = kreal), intent(inout) :: vec_nod(numnod,3)
!
      integer (kind = kint) :: inod
!
!$omp parallel do
      do inod=1, numnod
        vect%x_vec(3*inod-2) = vec_nod(inod,1)
        vect%x_vec(3*inod-1) = vec_nod(inod,2)
        vect%x_vec(3*inod  ) = vec_nod(inod,3)
      end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_3_type(numnod, nod_comm, vect%x_vec(1))
!
!$omp parallel do
      do inod=1, numnod
        vec_nod(inod,1) = vect%x_vec(3*inod-2)
        vec_nod(inod,2) = vect%x_vec(3*inod-1)
        vec_nod(inod,3) = vect%x_vec(3*inod  )
      end do
!$omp end parallel do
!
      end subroutine nod_vector_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine nod_tensor_send_recv(numnod, nod_comm, tsr_nod, vect)
!
      use m_work_time
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
!
      type(vectors_4_solver), intent(inout) :: vect
      real(kind = kreal), intent(inout) :: tsr_nod(numnod,6)
!
      integer (kind = kint) :: inod
!
!$omp parallel do
      do inod=1, numnod
        vect%x_vec(6*inod-5) = tsr_nod(inod,1)
        vect%x_vec(6*inod-4) = tsr_nod(inod,2)
        vect%x_vec(6*inod-3) = tsr_nod(inod,3)
        vect%x_vec(6*inod-2) = tsr_nod(inod,4)
        vect%x_vec(6*inod-1) = tsr_nod(inod,5)
        vect%x_vec(6*inod  ) = tsr_nod(inod,6)
      end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_6_type(numnod, nod_comm, vect%x_vec(1))
!
!$omp parallel do
      do inod=1, numnod
        tsr_nod(inod,1) = vect%x_vec(6*inod-5)
        tsr_nod(inod,2) = vect%x_vec(6*inod-4)
        tsr_nod(inod,3) = vect%x_vec(6*inod-3)
        tsr_nod(inod,4) = vect%x_vec(6*inod-2)
        tsr_nod(inod,5) = vect%x_vec(6*inod-1)
        tsr_nod(inod,6) = vect%x_vec(6*inod  )
      end do
!$omp end parallel do
!
      end subroutine nod_tensor_send_recv
!
! ----------------------------------------------------------------------
!
      end module nod_phys_send_recv
