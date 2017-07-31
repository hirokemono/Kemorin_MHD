!
!      module mesh_send_recv_test
!
!     Written by H. Matsui on Sep., 2007
!     Written by H. Matsui on Apr., 2008
!
!      subroutine s_mesh_send_recv_test
!      subroutine node_send_recv4_test(node, nod_comm)
!
!      subroutine node_send_recv_test(node, nod_comm)
!
      module mesh_send_recv_test
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_geometry_4_comm_test
!
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      implicit  none
!
      private :: ele_send_recv_test, surf_send_recv_test
      private :: edge_send_recv_test
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_mesh_send_recv_test(ele, surf, edge,                 &
     &          ele_comm, surf_comm, edge_comm)
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(communication_table), intent(in) :: ele_comm
      type(communication_table), intent(in) :: surf_comm
      type(communication_table), intent(in) :: edge_comm
!
      call ele_send_recv_test(ele, ele_comm)
      call surf_send_recv_test(surf, surf_comm)
      call edge_send_recv_test(edge, edge_comm)
!
      end subroutine s_mesh_send_recv_test
!
! ----------------------------------------------------------------------
!
      subroutine node_send_recv_test(node, nod_comm)
!
      use m_array_for_send_recv
      use solver_SR_type
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      integer(kind = kint) :: inod
!
!
      do inod = 1, node%internal_node
        i8x_vec(inod) = int(node%inod_global(inod))
        x_vec(3*inod-2) = node%xx(inod,1)
        x_vec(3*inod-1) = node%xx(inod,2)
        x_vec(3*inod  ) = node%xx(inod,3)
      end do
!
      call SOLVER_SEND_RECV_int8_type(node%numnod, nod_comm, i8x_vec)
      call SOLVER_SEND_RECV_3_type(node%numnod, nod_comm, x_vec)
!
      end subroutine node_send_recv_test
!
! ----------------------------------------------------------------------
!
      subroutine node_send_recv4_test(node, nod_comm)
!
      use m_work_time
      use m_array_for_send_recv
      use m_solver_SR
      use solver_SR_type
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      integer(kind = kint) :: inod
      integer(kind = kint), parameter :: NB = 12
      integer(kind = kint), allocatable :: irev_import(:)
      real(kind = kreal),  allocatable :: xx4(:)
!
      integer (kind = kint) :: neib, ist, inum, ied, num
      integer (kind = kint) :: k, ii, ix, nd
!
      allocate(irev_import(nod_comm%istack_import(nod_comm%num_neib)))
      allocate(xx4(NB*node%numnod))
!
      do inod = 1, node%internal_node
        i8x_vec(inod) = node%inod_global(inod)
        xx4(12*inod-11) = node%xx(inod,1)
        xx4(12*inod-10) = node%xx(inod,2)
        xx4(12*inod- 9) = node%xx(inod,3)
        xx4(12*inod- 8) = node%xx(inod,1) + 100.0
        xx4(12*inod- 7) = node%xx(inod,2) + 100.0
        xx4(12*inod- 6) = node%xx(inod,3) + 100.0
        xx4(12*inod- 5) = node%xx(inod,1) + 200.0
        xx4(12*inod- 4) = node%xx(inod,2) + 200.0
        xx4(12*inod- 3) = node%xx(inod,3) + 200.0
        xx4(12*inod- 2) = node%xx(inod,1) + 300.0
        xx4(12*inod- 1) = node%xx(inod,2) + 300.0
        xx4(12*inod   ) = node%xx(inod,3) + 300.0
      end do
!
      call SOLVER_SEND_RECV_int8_type(node%numnod, nod_comm, i8x_vec)
!
      call SOLVER_SEND_RECV_N_type(node%numnod, NB, nod_comm, xx4)
!
      do ii = 1, nod_comm%istack_import(nod_comm%num_neib)
        k = nod_comm%item_import(ii) - node%internal_node
        irev_import(k) = ii
      end do
!
      call start_elapsed_time(1)
      call start_elapsed_time(2)
!$omp parallel private(nd,neib,ist,ied)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_export(neib-1)
        ied = nod_comm%istack_export(neib  )
        do nd = 1, NB
!$omp do private(k,ii,ix)
          do k= ist+1, ied
                 ii   = NB * (nod_comm%item_export(k)-1) + nd
                 ix   = NB * (k-1) + nd
             WS(ix)= xx4(ii)
           end do
!$omp end do nowait
         end do
      end do
!$omp end parallel
      call end_elapsed_time(2)

!$omp parallel private(nd,neib,ist,ied)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_import(neib-1)
        ied = nod_comm%istack_import(neib  )
        do nd = 1, NB
!$omp do private(k,ii,ix)
          do k= ist+1, ied
            ii   = NB * (nod_comm%item_import(k)-1) + nd
            ix   = NB * (k-1) + nd
            xx4(ii)= WR(ix)
          end do
!$omp end do nowait
        enddo
      enddo
!$omp end parallel
      call end_elapsed_time(1)
      call start_elapsed_time(2)
!$omp parallel private(nd,neib,ist,ied)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_import(neib-1)
        ied = nod_comm%istack_import(neib  )
        do nd = 1, NB
!$omp do private(k,ii,ix)
          do k= ist+1, ied
            ii   = NB * (node%internal_node+k-1) + nd
            ix   = NB * (irev_import(k)-1) + nd
            xx4(ii)= WR(ix)
          end do
!$omp end do nowait
        enddo
      enddo
!$omp end parallel
      call end_elapsed_time(2)
!
      call start_elapsed_time(3)
      call start_elapsed_time(4)
!$omp parallel private(neib,ist,ied)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_export(neib-1)
        ied = nod_comm%istack_export(neib  )
!$omp do private(k,nd,ii,ix)
        do k= ist+1, ied
          do nd = 1, NB
                 ii   = NB * (nod_comm%item_export(k)-1) + nd
                 ix   = NB * (k-1) + nd
             WS(ix)= xx4(ii)
           end do
         end do
!$omp end do nowait
      end do
!$omp end parallel
      call end_elapsed_time(4)

!$omp parallel private(nd,neib,ist,ied)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_import(neib-1) 
        ied = nod_comm%istack_import(neib  )
!$omp do private(k,nd,ii,ix)
        do nd = 1, NB
          do k= ist+1, ied
            ii   = NB * (nod_comm%item_import(k)-1) + nd
            ix   = NB * (k-1) + nd
            xx4(ii)= WR(ix)
          end do
        enddo
!$omp end do nowait
      enddo
!$omp end parallel
      call end_elapsed_time(3)
      call start_elapsed_time(4)
!$omp parallel private(nd,neib,ist,ied)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_import(neib-1)
        ied = nod_comm%istack_import(neib  )
!$omp do private(k,ii,ix)
        do nd = 1, NB
          do k= ist+1, ied
            ii   = NB * (node%internal_node+k-1) + nd
            ix   = NB * (irev_import(k)-1) + nd
            xx4(ii)= WR(ix)
          end do
        enddo
!$omp end do nowait
      enddo
!$omp end parallel
      call end_elapsed_time(4)
!
!
!
      call start_elapsed_time(5)
      call start_elapsed_time(6)
!$omp parallel private(neib,nd,ist,num)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_export(neib-1)
        num = nod_comm%istack_export(neib  )                            &
     &       - nod_comm%istack_export(neib-1)
        do nd = 1, NB
!$omp do private(k,ii,ix)
          do k= 1, num
                 ii   = NB * (nod_comm%item_export(k+ist) - 1) + nd
                 ix   = k + (nd-1) * num + NB*ist
             WS(ix)= xx4(ii)
           end do
!$omp end do nowait
         end do
      end do
!$omp end parallel
      call end_elapsed_time(6)

!$omp parallel private(nd,neib,ist,num)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_import(neib-1)
        num = nod_comm%istack_import(neib  )                            &
     &       - nod_comm%istack_import(neib-1)
        do nd = 1, NB
!$omp do private(k,ii,ix)
          do k= 1, num
            ii   = NB * (nod_comm%item_import(k+ist)-1) + nd
            ix   = k + (nd-1) * num + NB*ist
            xx4(ii)= WR(ix)
          end do
!$omp end do nowait
        enddo
      enddo
!$omp end parallel
      call end_elapsed_time(5)
      call start_elapsed_time(6)
!$omp parallel private(nd,neib,ist,num)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_import(neib-1)
        num = nod_comm%istack_import(neib  )                            &
     &       - nod_comm%istack_import(neib-1)
        do nd = 1, NB
!$omp do private(k,ii,ix)
          do k= 1, num
            ii   = NB * (node%internal_node+k+ist-1) + nd
            ix   = NB * (irev_import(k+ist)-1) + nd
            xx4(ii)= WR(ix)
          end do
!$omp end do nowait
        enddo
      enddo
!$omp end parallel
      call end_elapsed_time(6)
!
      call start_elapsed_time(7)
      call start_elapsed_time(8)
!$omp parallel private(neib,ist,num)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_export(neib-1)
        num = nod_comm%istack_export(neib  )                            &
     &       - nod_comm%istack_export(neib-1)
!$omp do private(k,nd,ii,ix)
        do inum = 1, NB*num
          k = mod(inum-ione,num) + ione
          nd = (inum-k) / NB + ione
                 ii   = NB * (nod_comm%item_export(k+ist) - 1) + nd
                 ix   = inum + NB*ist
             WS(ix)= xx4(ii)
        end do
!$omp end do nowait
      end do
!$omp end parallel
      call end_elapsed_time(8)

!$omp parallel private(nd,neib,ist,num,inum)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_import(neib-1)
        num = nod_comm%istack_import(neib  )                            &
     &       - nod_comm%istack_import(neib-1)
!$omp do private(inum,k,ii,ix)
        do inum = 1, NB*num
          nd = mod(inum-ione,NB) + ione
          k = (inum-nd) / NB + ione
            ii   = NB * (nod_comm%item_import(k+ist)-1) + nd
            ix   = k + (nd-1) * num + NB*ist
            xx4(ii)= WR(ix)
        end do
!$omp end do nowait
      enddo
!$omp end parallel
      call end_elapsed_time(7)
!
      call start_elapsed_time(8)
!$omp parallel private(neib,ist,num,inum)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_import(neib-1)
        num = nod_comm%istack_import(neib  )                            &
     &       - nod_comm%istack_import(neib-1)
!$omp do private(nd,k,ii,ix)
        do inum = 1, NB*num
          nd = mod(inum-ione,NB) + ione
          k = (inum-nd) / NB + ione
            ii   = NB * (node%internal_node+k+ist-1) + nd
            ix   = (irev_import(k)-ist) + (nd-1) * num + NB*ist
            xx4(ii)= WR(ix)
        end do
!$omp end do nowait
      enddo
!$omp end parallel
      call end_elapsed_time(8)
      
      deallocate(xx4)
!
      end subroutine node_send_recv4_test
!
! ----------------------------------------------------------------------
!
      subroutine ele_send_recv_test(ele, ele_comm)
!
      use solver_SR_type
!
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: ele_comm
!
      integer(kind = kint) :: iele, inum
!
!
      do iele = 1, ele%numele
        x_ele_comm(3*iele-2) = ele%x_ele(iele,1)
        x_ele_comm(3*iele-1) = ele%x_ele(iele,2)
        x_ele_comm(3*iele  ) = ele%x_ele(iele,3)
      end do
      do inum = 1, ele_comm%ntot_import
        iele = ele_comm%item_import(inum)
        x_ele_comm(3*iele-2) = 0.0d0
        x_ele_comm(3*iele-1) = 0.0d0
        x_ele_comm(3*iele  ) = 0.0d0
      end do
!
      call SOLVER_SEND_RECV_3_type(ele%numele, ele_comm, x_ele_comm)
!
      end subroutine ele_send_recv_test
!
! ----------------------------------------------------------------------
!
      subroutine surf_send_recv_test(surf, surf_comm)
!
      use solver_SR_type
!
      type(surface_data), intent(in) :: surf
      type(communication_table), intent(in) :: surf_comm
!
      integer(kind = kint) :: isurf, inum
!
!
      do isurf = 1, surf%numsurf
        x_surf_comm(3*isurf-2) = surf%x_surf(isurf,1)
        x_surf_comm(3*isurf-1) = surf%x_surf(isurf,2)
        x_surf_comm(3*isurf  ) = surf%x_surf(isurf,3)
      end do
      do inum = 1, surf_comm%ntot_import
        isurf = surf_comm%item_import(inum)
        x_surf_comm(3*isurf-2) = 0.0d0
        x_surf_comm(3*isurf-1) = 0.0d0
        x_surf_comm(3*isurf  ) = 0.0d0
      end do
!
      call SOLVER_SEND_RECV_3_type                                      &
     &   (surf%numsurf, surf_comm, x_surf_comm)
!
      end subroutine surf_send_recv_test
!
! ----------------------------------------------------------------------
!
      subroutine edge_send_recv_test(edge, edge_comm)
!
      use solver_SR_type
!
      type(edge_data), intent(in) :: edge
      type(communication_table), intent(in) :: edge_comm
!
      integer(kind = kint) :: iedge, inum
!
!
      do iedge = 1, edge%numedge
        x_edge_comm(3*iedge-2) = edge%x_edge(iedge,1)
        x_edge_comm(3*iedge-1) = edge%x_edge(iedge,2)
        x_edge_comm(3*iedge  ) = edge%x_edge(iedge,3)
      end do
      do inum = 1, edge_comm%ntot_import
        iedge = edge_comm%item_import(inum)
        x_edge_comm(3*iedge-2) = 0.0d0
        x_edge_comm(3*iedge-1) = 0.0d0
        x_edge_comm(3*iedge  ) = 0.0d0
      end do
!
      call SOLVER_SEND_RECV_3_type                                      &
     &   (edge%numedge, edge_comm, x_edge_comm)
!
      end subroutine edge_send_recv_test
!
! ----------------------------------------------------------------------
!
      end module mesh_send_recv_test
