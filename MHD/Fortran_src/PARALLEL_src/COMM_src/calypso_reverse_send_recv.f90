!> @file  calypso_reverse_send_recv.f90
!!      module calypso_reverse_send_recv
!!
!! @author  H. Matsui
!! @date Programmed in Dec., 2020
!
!> @brief Data communication to new partitioned mesh
!!
!!@verbatim
!!      subroutine calypso_reverse_SR_1(cps_tbl, nnod_new, nnod_org,    &
!!     &                                X_new, X_org, SR_sig, SR_r)
!!        type(calypso_comm_table), intent(in) :: cps_tbl
!!        integer(kind = kint), intent(in) :: nnod_new
!!        integer(kind = kint), intent(in) :: nnod_org
!!        real (kind=kreal), intent(in)::    X_new(nnod_new)
!!        real (kind=kreal), intent(inout):: X_org(nnod_org)
!!      subroutine calypso_reverse_SR_2(cps_tbl, nnod_new, nnod_org,    &
!!     &                                X_new, X_org, SR_sig, SR_r)
!!        type(calypso_comm_table), intent(in) :: cps_tbl
!!        integer(kind = kint), intent(in) :: nnod_new
!!        integer(kind = kint), intent(in) :: nnod_org
!!        real (kind=kreal), intent(in)::    X_new(2*nnod_new)
!!        real (kind=kreal), intent(inout):: X_org(2*nnod_org)
!!      subroutine calypso_reverse_SR_3(cps_tbl, nnod_new, nnod_org,    &
!!     &                                X_new, X_org, SR_sig, SR_r)
!!        type(calypso_comm_table), intent(in) :: cps_tbl
!!        integer(kind = kint), intent(in) :: nnod_new
!!        integer(kind = kint), intent(in) :: nnod_org
!!        real (kind=kreal), intent(in)::    X_new(3*nnod_new)
!!        real (kind=kreal), intent(inout):: X_org(3*nnod_org)
!!@endverbatim
!
      module calypso_reverse_send_recv
!
      use m_precision
      use calypso_mpi
      use t_calypso_comm_table
      use t_solver_SR
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine calypso_reverse_SR_1(cps_tbl, nnod_new, nnod_org,      &
     &                                X_new, X_org, SR_sig, SR_r)
!
      use calypso_SR_core
      use set_to_send_buffer
      use set_from_recv_buffer
!
      type(calypso_comm_table), intent(in) :: cps_tbl
      integer(kind = kint), intent(in) :: nnod_new
      integer(kind = kint), intent(in) :: nnod_org
      real (kind=kreal), intent(in)::    X_new(nnod_new)
!
      real (kind=kreal), intent(inout):: X_org(nnod_org)
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call resize_work_SR(ione,                                         &
     &    cps_tbl%nrank_import, cps_tbl%nrank_export,                   &
     &    cps_tbl%istack_import(cps_tbl%nrank_import),                  &
     &    cps_tbl%istack_export(cps_tbl%nrank_export), SR_sig, SR_r)
!
!C-- SEND
      call set_to_send_buf_1(nnod_new,                                  &
     &    cps_tbl%istack_import(cps_tbl%nrank_import),                  &
     &    cps_tbl%item_import, X_new, SR_r%WS)
!C
!C-- COMM
      call calypso_send_recv_core                                       &
     &   (ione, cps_tbl%nrank_import, cps_tbl%irank_import,             &
     &          cps_tbl%istack_import, SR_r%WS(1),                      &
     &          cps_tbl%nrank_export, cps_tbl%irank_export,             &
     &          cps_tbl%istack_export, cps_tbl%iflag_self_copy,         &
     &          SR_r%WR(1), SR_sig)
!
!C-- RECV
!$omp parallel workshare
      X_org(1:nnod_org) = 0.0d0
!$omp end parallel workshare
!
      call set_from_recv_buf_1(nnod_org,                                &
     &    cps_tbl%istack_export(cps_tbl%nrank_export),                  &
     &    cps_tbl%item_export, SR_r%WR(1), X_org)
!
!C-- WAIT
      call calypso_send_recv_fin(cps_tbl%nrank_import,                  &
     &                           cps_tbl%iflag_self_copy, SR_sig)
!
      end subroutine calypso_reverse_SR_1
!
! ----------------------------------------------------------------------
!
      subroutine calypso_reverse_SR_2(cps_tbl, nnod_new, nnod_org,      &
     &                                X_new, X_org, SR_sig, SR_r)
!
      use calypso_SR_core
      use set_to_send_buffer
      use set_from_recv_buffer
!
      type(calypso_comm_table), intent(in) :: cps_tbl
      integer(kind = kint), intent(in) :: nnod_new
      integer(kind = kint), intent(in) :: nnod_org
      real (kind=kreal), intent(in)::    X_new(2*nnod_new)
!
      real (kind=kreal), intent(inout):: X_org(2*nnod_org)
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call resize_work_SR(itwo,                                         &
     &    cps_tbl%nrank_import, cps_tbl%nrank_export,                   &
     &    cps_tbl%istack_import(cps_tbl%nrank_import),                  &
     &    cps_tbl%istack_export(cps_tbl%nrank_export), SR_sig, SR_r)
!
!C-- SEND
      call set_to_send_buf_2(nnod_new,                                  &
     &    cps_tbl%istack_import(cps_tbl%nrank_import),                  &
     &    cps_tbl%item_import, X_new, SR_r%WS)
!C
!C-- COMM
      call calypso_send_recv_core                                       &
     &   (itwo, cps_tbl%nrank_import, cps_tbl%irank_import,             &
     &          cps_tbl%istack_import, SR_r%WS(1),                      &
     &          cps_tbl%nrank_export, cps_tbl%irank_export,             &
     &          cps_tbl%istack_export, cps_tbl%iflag_self_copy,         &
     &          SR_r%WR(1), SR_sig)
!
!C-- RECV
!$omp parallel workshare
      X_org(1:2*nnod_org) = 0.0d0
!$omp end parallel workshare
!
      call set_from_recv_buf_2(nnod_org,                                &
     &    cps_tbl%istack_export(cps_tbl%nrank_export),                  &
     &    cps_tbl%item_export, SR_r%WR(1), X_org)
!
!C-- WAIT
      call calypso_send_recv_fin(cps_tbl%nrank_import,                  &
     &                           cps_tbl%iflag_self_copy, SR_sig)
!
      end subroutine calypso_reverse_SR_2
!
! ----------------------------------------------------------------------
!
      subroutine calypso_reverse_SR_3(cps_tbl, nnod_new, nnod_org,      &
     &                                X_new, X_org, SR_sig, SR_r)
!
      use calypso_SR_core
      use set_to_send_buffer
      use set_from_recv_buffer
!
      type(calypso_comm_table), intent(in) :: cps_tbl
      integer(kind = kint), intent(in) :: nnod_new
      integer(kind = kint), intent(in) :: nnod_org
      real (kind=kreal), intent(in)::    X_new(3*nnod_new)
!
      real (kind=kreal), intent(inout):: X_org(3*nnod_org)
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call resize_work_SR(ithree,                                       &
     &    cps_tbl%nrank_import, cps_tbl%nrank_export,                   &
     &    cps_tbl%istack_import(cps_tbl%nrank_import),                  &
     &    cps_tbl%istack_export(cps_tbl%nrank_export), SR_sig, SR_r)
!
!C-- SEND
      call set_to_send_buf_3(nnod_new,                                  &
     &    cps_tbl%istack_import(cps_tbl%nrank_import),                  &
     &    cps_tbl%item_import, X_new, SR_r%WS)
!C
!C-- COMM
      call calypso_send_recv_core                                       &
     &   (ithree, cps_tbl%nrank_import, cps_tbl%irank_import,           &
     &            cps_tbl%istack_import, SR_r%WS(1),                    &
     &            cps_tbl%nrank_export, cps_tbl%irank_export,           &
     &            cps_tbl%istack_export, cps_tbl%iflag_self_copy,       &
     &            SR_r%WR(1), SR_sig)
!
!C-- RECV
!$omp parallel workshare
      X_org(1:3*nnod_org) = 0.0d0
!$omp end parallel workshare
!
      call set_from_recv_buf_3(nnod_org,                                &
     &    cps_tbl%istack_export(cps_tbl%nrank_export),                  &
     &    cps_tbl%item_export, SR_r%WR(1), X_org)
!
!C-- WAIT
      call calypso_send_recv_fin(cps_tbl%nrank_import,                  &
     &                           cps_tbl%iflag_self_copy, SR_sig)
!
      end subroutine calypso_reverse_SR_3
!
! ----------------------------------------------------------------------
!
      end module calypso_reverse_send_recv
