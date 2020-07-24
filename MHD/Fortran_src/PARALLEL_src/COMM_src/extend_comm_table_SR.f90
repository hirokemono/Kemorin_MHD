!> @file  extend_comm_table_SR.f90
!!      module extend_comm_table_SR
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node and element to extend export table
!!
!!@verbatim
!!      subroutine added_geometry_send_recv(num_neib, id_neib,          &
!!     &          istack_send_added, ntot_send_added, xx_send_added,    &
!!     &          istack_recv_added, ntot_recv_added, xx_recv_added)
!!      subroutine added_global_id_send_recv(num_neib, id_neib,         &
!!     &         istack_send_added, ntot_send_added, inod_gl_send_added,&
!!     &         istack_recv_added, ntot_recv_added, inod_gl_recv_added)
!!      subroutine added_nod_id_send_recv(num_neib, id_neib,            &
!!     &         istack_send_added, ntot_send_added, inod_send_added,   &
!!     &         istack_recv_added, ntot_recv_added, inod_recv_added)
!!@endverbatim
!
      module extend_comm_table_SR
!
      use m_precision
      use m_constants
      use m_phys_constants
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine added_geometry_send_recv(num_neib, id_neib,            &
     &          istack_send_added, ntot_send_added, xx_send_added,      &
     &          istack_recv_added, ntot_recv_added, xx_recv_added)
!
      use m_solver_SR
      use calypso_SR_core
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
!
      integer(kind = kint), intent(in) :: ntot_send_added
      integer(kind = kint), intent(in) :: istack_send_added(0:num_neib)
      real(kind = kreal), intent(in)                                    &
     &                   :: xx_send_added(ntot_send_added,3)
!
      integer(kind = kint), intent(in) :: ntot_recv_added
      integer(kind = kint), intent(in) :: istack_recv_added(0:num_neib)
      real(kind = kreal), intent(inout)                                 &
     &                 :: xx_recv_added(ntot_recv_added,3)
!
      integer(kind = kint) :: inum
!
!
      call resize_work_4_SR(n_vector, num_neib, num_neib,               &
     &    istack_send_added(num_neib), istack_recv_added(num_neib))
!
!$omp parallel do private(inum)
      do inum = 1, ntot_send_added
        SR_r1%WS(3*inum-2) = xx_send_added(inum,1)
        SR_r1%WS(3*inum-1) = xx_send_added(inum,2)
        SR_r1%WS(3*inum  ) = xx_send_added(inum,3)
      end do
!$omp end parallel do
!
      call calypso_send_recv_core(n_vector,                             &
     &    num_neib, izero, id_neib, istack_send_added,                  &
     &    num_neib, izero, id_neib, istack_recv_added)
!
!$omp parallel do private(inum)
      do inum = 1, ntot_recv_added
        xx_recv_added(inum,1) = SR_r1%WR(3*inum-2)
        xx_recv_added(inum,2) = SR_r1%WR(3*inum-1)
        xx_recv_added(inum,3) = SR_r1%WR(3*inum  )
      end do
!$omp end parallel do
!
      call calypso_send_recv_fin(num_neib, izero)
!
      end subroutine added_geometry_send_recv
!
!  ---------------------------------------------------------------------
!
      subroutine added_global_id_send_recv(num_neib, id_neib,           &
     &         istack_send_added, ntot_send_added, inod_gl_send_added,  &
     &         istack_recv_added, ntot_recv_added, inod_gl_recv_added)
!
      use t_solver_SR
      use t_solver_SR_int8
      use solver_SR_int8
      use m_solver_SR
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
!
      integer(kind = kint), intent(in) :: ntot_send_added
      integer(kind = kint), intent(in) :: istack_send_added(0:num_neib)
      integer(kind = kint_gl), intent(in)                               &
     &                   :: inod_gl_send_added(ntot_send_added)
!
      integer(kind = kint), intent(in) :: ntot_recv_added
      integer(kind = kint), intent(in) :: istack_recv_added(0:num_neib)
      integer(kind = kint_gl), intent(inout)                            &
     &                 :: inod_gl_recv_added(ntot_recv_added)
!
!
      call resize_i8work_SR(num_neib, num_neib,                         &
     &    istack_send_added(num_neib), istack_recv_added(num_neib),     &
     &    SR_sig1, SR_il1)
!
!$omp parallel workshare
        SR_il1%i8WS(1:ntot_send_added)                                  &
     &    = inod_gl_send_added(1:ntot_send_added)
!$omp end parallel workshare
!
      call calypso_send_recv_i8core                                     &
     &   (num_neib, izero, id_neib, istack_send_added,                  &
     &    num_neib, izero, id_neib, istack_recv_added, SR_sig1, SR_il1)
!
!$omp parallel workshare
      inod_gl_recv_added(1:ntot_recv_added)                             &
     &        = SR_il1%i8WR(1:ntot_recv_added)
!$omp end parallel workshare
!
      call calypso_send_recv_fin_t(num_neib, izero, SR_sig1)
!
      end subroutine added_global_id_send_recv
!
!  ---------------------------------------------------------------------
!
      subroutine added_nod_id_send_recv(num_neib, id_neib,              &
     &         istack_send_added, ntot_send_added, inod_send_added,     &
     &         istack_recv_added, ntot_recv_added, inod_recv_added)
!
      use t_solver_SR
      use m_solver_SR
      use solver_SR_int
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
!
      integer(kind = kint), intent(in) :: ntot_send_added
      integer(kind = kint), intent(in)                                  &
     &                   :: istack_send_added(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &                   :: inod_send_added(ntot_send_added)
!
      integer(kind = kint), intent(in) :: ntot_recv_added
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_recv_added(0:num_neib)
      integer(kind = kint), intent(inout)                               &
     &                   :: inod_recv_added(ntot_recv_added)
!
!
      call resize_iwork_SR_t(num_neib, num_neib,                        &
     &    istack_send_added(num_neib), istack_recv_added(num_neib),     &
     &    SR_sig1, SR_i1)
!
!$omp parallel workshare
      SR_i1%iWS(1:ntot_send_added) = inod_send_added(1:ntot_send_added)
!$omp end parallel workshare
!
      call calypso_send_recv_intcore                                    &
     &  (num_neib, izero, id_neib, istack_send_added,                   &
     &   num_neib, izero, id_neib, istack_recv_added, SR_sig1, SR_i1)
!
!$omp parallel workshare
      inod_recv_added(1:ntot_recv_added) = SR_i1%iWR(1:ntot_recv_added)
!$omp end parallel workshare
!
      call calypso_send_recv_fin_t(num_neib, izero, SR_sig1)
!
      end subroutine added_nod_id_send_recv
!
!  ---------------------------------------------------------------------
!
      end module extend_comm_table_SR

