!
!      module filter_moments_send_recv
!
!     Written by H. Matsui on Apr., 2008
!     Modified by H. Matsui on Apr., 2008
!
!      subroutine jacobi_nod_send_recv
!      subroutine dxidx_nod_send_recv
!
!      subroutine elength_nod_send_recv
!      subroutine diff_elen_nod_send_recv
!
!      subroutine filter_mom_nod_send_recv(ifil)
!      subroutine diff_filter_mom_nod_send_recv(ifil)
!
      module filter_moments_send_recv
!
      use m_precision
!
      implicit none
!
      private :: vector_send_recv_filter
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine jacobi_nod_send_recv
!
      use m_filter_dxdxi
!
!
      call scalar_send_recv_filter(dxdxi_nod(1))
      call scalar_send_recv_filter(dxdei_nod(1))
      call scalar_send_recv_filter(dxdzi_nod(1))
!
      call scalar_send_recv_filter(dydxi_nod(1))
      call scalar_send_recv_filter(dydei_nod(1))
      call scalar_send_recv_filter(dydzi_nod(1))
!
      call scalar_send_recv_filter(dzdxi_nod(1))
      call scalar_send_recv_filter(dzdei_nod(1))
      call scalar_send_recv_filter(dzdzi_nod(1))
!
      end subroutine jacobi_nod_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine dxidx_nod_send_recv
!
      use m_dxi_dxes_3d_node
!
!
      call scalar_send_recv_filter(dxidx_nod(1))
      call scalar_send_recv_filter(deidx_nod(1))
      call scalar_send_recv_filter(dzidx_nod(1))
!
      call scalar_send_recv_filter(dxidy_nod(1))
      call scalar_send_recv_filter(deidy_nod(1))
      call scalar_send_recv_filter(dzidy_nod(1))
!
      call scalar_send_recv_filter(dxidz_nod(1))
      call scalar_send_recv_filter(deidz_nod(1))
      call scalar_send_recv_filter(dzidz_nod(1))
!
      end subroutine dxidx_nod_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine elength_nod_send_recv
!
      use m_filter_elength
!
!
      call scalar_send_recv_filter(elen_dx2_nod(1))
      call scalar_send_recv_filter(elen_dy2_nod(1))
      call scalar_send_recv_filter(elen_dz2_nod(1))
!
      call scalar_send_recv_filter(elen_dxdy_nod(1))
      call scalar_send_recv_filter(elen_dydz_nod(1))
      call scalar_send_recv_filter(elen_dzdx_nod(1))
!
      end subroutine elength_nod_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine diff_elen_nod_send_recv
!
      use m_filter_elength
!
!
      call vector_send_recv_filter(elen_dx2_nod_dx(1,1))
      call vector_send_recv_filter(elen_dy2_nod_dx(1,1))
      call vector_send_recv_filter(elen_dz2_nod_dx(1,1))
!
      call vector_send_recv_filter(elen_dxdy_nod_dx(1,1))
      call vector_send_recv_filter(elen_dydz_nod_dx(1,1))
      call vector_send_recv_filter(elen_dzdx_nod_dx(1,1))
!
      end subroutine diff_elen_nod_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine filter_mom_nod_send_recv(ifil)
!
      use m_filter_moments
!
!
      integer(kind = kint), intent(in) :: ifil
!
      call scalar_send_recv_filter(filter_x_nod(1,ifil))
      call scalar_send_recv_filter(filter_y_nod(1,ifil))
      call scalar_send_recv_filter(filter_z_nod(1,ifil))
!
      call scalar_send_recv_filter(filter_x2_nod(1,ifil))
      call scalar_send_recv_filter(filter_y2_nod(1,ifil))
      call scalar_send_recv_filter(filter_z2_nod(1,ifil))
!
      call scalar_send_recv_filter(filter_xy_nod(1,ifil))
      call scalar_send_recv_filter(filter_yz_nod(1,ifil))
      call scalar_send_recv_filter(filter_zx_nod(1,ifil))
!
      end subroutine filter_mom_nod_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine diff_filter_mom_nod_send_recv(ifil)
!
      use m_filter_moments
!
!
      integer(kind = kint), intent(in) :: ifil
!
      call vector_send_recv_filter(filter_x_nod_dx(1,1,ifil))
      call vector_send_recv_filter(filter_y_nod_dx(1,1,ifil))
      call vector_send_recv_filter(filter_z_nod_dx(1,1,ifil))
!
      call vector_send_recv_filter(filter_x2_nod_dx(1,1,ifil))
      call vector_send_recv_filter(filter_y2_nod_dx(1,1,ifil))
      call vector_send_recv_filter(filter_z2_nod_dx(1,1,ifil))
!
      call vector_send_recv_filter(filter_xy_nod_dx(1,1,ifil))
      call vector_send_recv_filter(filter_yz_nod_dx(1,1,ifil))
      call vector_send_recv_filter(filter_zx_nod_dx(1,1,ifil))
!
      end subroutine diff_filter_mom_nod_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine scalar_send_recv_filter(scalar)
!
      use calypso_mpi
      use m_work_time
      use m_geometry_parameter
      use m_parallel_var_dof
      use m_array_for_send_recv
      use m_nod_comm_table
!
      use solver_SR
!
      real(kind = kreal), intent(inout) :: scalar(numnod)
      integer (kind = kint) :: inod
!
!
!$omp parallel do
      do inod=1, numnod
        x_vec(inod) = scalar(inod)
      end do
!cdir end parallel do
!
      START_SRtime= MPI_WTIME()
      call SOLVER_SEND_RECV(numnod, num_neib, id_neib,                  &
     &                      istack_import, item_import,                 &
     &                      istack_export, item_export, x_vec(1) )
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
!$omp parallel do
      do inod=1, numnod
        scalar(inod) = x_vec(inod)
      end do
!cdir end parallel do
!
      end subroutine scalar_send_recv_filter
!
! ----------------------------------------------------------------------
!
      subroutine vector_send_recv_filter(vector)
!
      use calypso_mpi
      use m_work_time
      use m_geometry_parameter
      use m_parallel_var_dof
      use m_array_for_send_recv
      use m_nod_comm_table
!
      use solver_SR_3
!
      real(kind = kreal), intent(inout) :: vector(numnod,3)
      integer (kind = kint) :: inod
!
!
!$omp parallel do
      do inod=1, numnod
        x_vec(3*inod-2) = vector(inod,1)
        x_vec(3*inod-1) = vector(inod,2)
        x_vec(3*inod  ) = vector(inod,3)
      end do
!$omp end parallel do
!
      START_SRtime= MPI_WTIME()
      call SOLVER_SEND_RECV_3(numnod, num_neib, id_neib,                &
     &                        istack_import, item_import,               &
     &                        istack_export, item_export, x_vec(1) )
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
!$omp parallel do
      do inod=1, numnod
        vector(inod,1) = x_vec(3*inod-2)
        vector(inod,2) = x_vec(3*inod-1)
        vector(inod,3) = x_vec(3*inod  )
      end do
!$omp end parallel do
!
      end subroutine vector_send_recv_filter
!
! ----------------------------------------------------------------------
!
      end module filter_moments_send_recv
