!>@file   send_recv_3d_filtering.f90
!!@brief  module send_recv_3d_filtering
!!
!!@author H. Matsui
!!@date Programmed in July, 2005
!!      Modified in July, 2008
!
!>@brief  Communication of apatial filetering for each field
!!
!!@verbatim
!!      subroutine scalar_send_recv_3d_filter(nod_comm, nnod, x_vec,    &
!!     &          ncomp_nod, i_fld, d_nod, SR_sig, SR_r)
!!      subroutine vector_send_recv_3d_filter(nod_comm, nnod, x_vec,    &
!!     &          ncomp_nod, i_fld, d_nod, SR_sig, SR_r)
!!      subroutine tensor_send_recv_3d_filter(nod_comm, nnod, x_vec,    &
!!     &          ncomp_nod, i_fld, d_nod, SR_sig, SR_r)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
      module send_recv_3d_filtering
!
      use m_precision
      use m_work_time
      use calypso_mpi
!
      use t_comm_table
      use t_solver_SR
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine scalar_send_recv_3d_filter(nod_comm, nnod, x_vec,      &
     &          ncomp_nod, i_fld, d_nod, SR_sig, SR_r)
!
      use solver_SR_type
!
      type(communication_table), intent(in) :: nod_comm
      integer(kind = kint), intent(in) :: nnod, ncomp_nod, i_fld
!
      real(kind = kreal), intent(inout) :: x_vec(nnod)
      real(kind = kreal), intent(inout) :: d_nod(nnod, ncomp_nod)
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind = kint) :: inod
!
!
      call SOLVER_SEND_RECV_type(nnod, nod_comm,                        &
     &                           SR_sig, SR_r, x_vec(1))
!
!$omp parallel do
      do inod=1, nnod
        d_nod(inod,i_fld) = x_vec(inod  )
      end do
!$omp end parallel do
!
      end subroutine scalar_send_recv_3d_filter
!
! ----------------------------------------------------------------------
!
      subroutine vector_send_recv_3d_filter(nod_comm, nnod, x_vec,      &
     &          ncomp_nod, i_fld, d_nod, SR_sig, SR_r)
!
      use solver_SR_type
!
      type(communication_table), intent(in) :: nod_comm
      integer(kind = kint), intent(in) :: nnod, ncomp_nod, i_fld
!
      real(kind = kreal), intent(inout) :: x_vec(3*nnod)
      real(kind = kreal), intent(inout) :: d_nod(nnod,ncomp_nod)
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind = kint) :: inod
!
!
      call SOLVER_SEND_RECV_3_type(nnod, nod_comm,                      &
     &                             SR_sig, SR_r, x_vec(1))
!
!$omp parallel do
      do inod=1, nnod
        d_nod(inod,i_fld  ) = x_vec(3*inod-2)
        d_nod(inod,i_fld+1) = x_vec(3*inod-1)
        d_nod(inod,i_fld+2) = x_vec(3*inod  )
      end do
!$omp end parallel do
!
      end subroutine vector_send_recv_3d_filter
!
! ----------------------------------------------------------------------
!
      subroutine tensor_send_recv_3d_filter(nod_comm, nnod, x_vec,      &
     &          ncomp_nod, i_fld, d_nod, SR_sig, SR_r)
!
      use solver_SR_type
!
      type(communication_table), intent(in) :: nod_comm
      integer(kind = kint), intent(in) :: nnod, ncomp_nod, i_fld
!
      real(kind = kreal), intent(inout) :: x_vec(6*nnod)
      real(kind = kreal), intent(inout) :: d_nod(nnod,ncomp_nod)
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind = kint) :: inod
!
!
      call SOLVER_SEND_RECV_6_type(nnod, nod_comm,                      &
     &                             SR_sig, SR_r, x_vec(1))
!
!$omp parallel do
      do inod =1, nnod
        d_nod(inod,i_fld  ) = x_vec(6*inod-5)
        d_nod(inod,i_fld+1) = x_vec(6*inod-4)
        d_nod(inod,i_fld+2) = x_vec(6*inod-3)
        d_nod(inod,i_fld+3) = x_vec(6*inod-2)
        d_nod(inod,i_fld+4) = x_vec(6*inod-1)
        d_nod(inod,i_fld+5) = x_vec(6*inod  )
      end do
!$omp end parallel do
!
      end subroutine tensor_send_recv_3d_filter
!
! ----------------------------------------------------------------------
!
      end module send_recv_3d_filtering
