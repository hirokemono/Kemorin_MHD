!
!      module init_RMA_SR_4_fl
!
!        programmed by H.Matsui on Feb. 2003
!        modified by H. Matsui on June. 2006
!
!      subroutine init_RMA_SendRecv_fl
!
      module init_RMA_SR_4_fl
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_RMA_SendRecv_fl
!
      use m_parallel_var_dof
      use m_comm_table_4_MHD
      use m_RMA_SR_fl
!
      integer(kind = kint), parameter :: isix = 6
!
      call init_work_4_SR_fl( neigh_pe_num_fl, neigh_pe_data_fl,        &
     &       istack_import_fl)
      call init_window_4_SR_fl(isix, neigh_pe_num_fl, istack_import_fl)
!
      end subroutine init_RMA_SendRecv_fl
!
!-----------------------------------------------------------------------
!
      end module init_RMA_SR_4_fl
