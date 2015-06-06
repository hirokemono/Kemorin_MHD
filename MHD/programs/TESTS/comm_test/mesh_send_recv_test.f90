!
!      module mesh_send_recv_test
!
!     Written by H. Matsui on Sep., 2007
!     Written by H. Matsui on Apr., 2008
!
!      subroutine s_mesh_send_recv_test
!
!      subroutine node_send_recv_test
!      subroutine ele_send_recv_test
!      subroutine surf_send_recv_test
!      subroutine edge_send_recv_test
!
      module mesh_send_recv_test
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_geometry_parameter
      use m_geometry_data
      use m_geometry_4_comm_test
      use solver_SR_3
      use solver_SR_int
      use solver_SR_N
      use solver_SR_type
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
      subroutine s_mesh_send_recv_test
!
      call ele_send_recv_test
      call surf_send_recv_test
      call edge_send_recv_test
!
      end subroutine s_mesh_send_recv_test
!
! ----------------------------------------------------------------------
!
      subroutine node_send_recv_test
!
      use m_nod_comm_table
      use m_array_for_send_recv
!
      integer(kind = kint) :: inod
!
!
      do inod = 1, internal_node
        ix_vec(inod) = int(inod_global(inod))
        x_vec(3*inod-2) = xx(inod,1)
        x_vec(3*inod-1) = xx(inod,2)
        x_vec(3*inod  ) = xx(inod,3)
      end do
!
      call solver_send_recv_i(numnod, num_neib, id_neib,                &
     &                        istack_import, item_import,               &
     &                        istack_export, item_export, ix_vec)
!
      call solver_send_recv_3(numnod, num_neib, id_neib,                &
     &                        istack_import, item_import,               &
     &                        istack_export, item_export, x_vec)
!
      end subroutine node_send_recv_test
!
! ----------------------------------------------------------------------
!
      subroutine node_send_recv4_test
!
      use m_work_time
      use m_nod_comm_table
      use m_array_for_send_recv
      use m_solver_SR
!
      integer(kind = kint) :: inod
      integer(kind = kint), parameter :: NB = 12
      integer(kind = kint), allocatable :: irev_import(:)
      real(kind = kreal),  allocatable :: xx4(:)
!
      integer (kind = kint) :: neib, ist, inum, ied, num
      integer (kind = kint) :: k, ii, ix, nd
!
      allocate(irev_import( istack_import(num_neib) ))
      allocate(xx4(NB*numnod))
!
      do inod = 1, internal_node
        ix_vec(inod) = int(inod_global(inod))
        xx4(12*inod-11) = xx(inod,1)
        xx4(12*inod-10) = xx(inod,2)
        xx4(12*inod- 9) = xx(inod,3)
        xx4(12*inod- 8) = xx(inod,1) + 100.0
        xx4(12*inod- 7) = xx(inod,2) + 100.0
        xx4(12*inod- 6) = xx(inod,3) + 100.0
        xx4(12*inod- 5) = xx(inod,1) + 200.0
        xx4(12*inod- 4) = xx(inod,2) + 200.0
        xx4(12*inod- 3) = xx(inod,3) + 200.0
        xx4(12*inod- 2) = xx(inod,1) + 300.0
        xx4(12*inod- 1) = xx(inod,2) + 300.0
        xx4(12*inod   ) = xx(inod,3) + 300.0
      end do
!
      call solver_send_recv_i(numnod, num_neib, id_neib,                &
     &                        istack_import, item_import,               &
     &                        istack_export, item_export, ix_vec)
!
      call solver_send_recv_N(numnod, NB, num_neib, id_neib,            &
     &                        istack_import, item_import,               &
     &                        istack_export, item_export, xx4)
!
      do ii = 1, istack_import(num_neib)
        k = item_import(ii) - internal_node
        irev_import(k) = ii
      end do
!
      num_elapsed = 8
      call allocate_elapsed_times
!
      write(elapse_labels( 1),'(a)') 'Original         component_out'
      write(elapse_labels( 2),'(a)') 'Original reverse comopnent_out'
      write(elapse_labels( 3),'(a)') 'Original          component_in'
      write(elapse_labels( 4),'(a)') 'Original reverse  component_in'
      write(elapse_labels( 5),'(a)') 'item_inner          component_out'
      write(elapse_labels( 6),'(a)') 'item_inner reverse component_out'
      write(elapse_labels( 7),'(a)') 'item_inner_mod         comp_out'
      write(elapse_labels( 8),'(a)') 'item_inner_mod reverse comp_out'
!
      call start_eleps_time(1)
      call start_eleps_time(2)
!$omp parallel private(nd,neib,ist,ied)
      do neib= 1, num_neib
        ist = istack_export(neib-1)
        ied = istack_export(neib  )
        do nd = 1, NB
!$omp do private(k,ii,ix)
          do k= ist+1, ied
                 ii   = NB * (item_export(k)-1) + nd
                 ix   = NB * (k-1) + nd
             WS(ix)= xx4(ii)
           end do
!$omp end do nowait
         end do
      end do
!$omp end parallel
      call end_eleps_time(2)

!$omp parallel private(nd,neib,ist,ied)
      do neib= 1, num_neib
        ist = istack_import(neib-1)
        ied = istack_import(neib  )
        do nd = 1, NB
!$omp do private(k,ii,ix)
          do k= ist+1, ied
            ii   = NB * (item_import(k)-1) + nd
            ix   = NB * (k-1) + nd
            xx4(ii)= WR(ix)
          end do
!$omp end do nowait
        enddo
      enddo
!$omp end parallel
      call end_eleps_time(1)
      call start_eleps_time(2)
!$omp parallel private(nd,neib,ist,ied)
      do neib= 1, num_neib
        ist = istack_import(neib-1)
        ied = istack_import(neib  )
        do nd = 1, NB
!$omp do private(k,ii,ix)
          do k= ist+1, ied
            ii   = NB * (internal_node+k-1) + nd
            ix   = NB * (irev_import(k)-1) + nd
            xx4(ii)= WR(ix)
          end do
!$omp end do nowait
        enddo
      enddo
!$omp end parallel
      call end_eleps_time(2)
!
      call start_eleps_time(3)
      call start_eleps_time(4)
!$omp parallel private(neib,ist,ied)
      do neib= 1, num_neib
        ist = istack_export(neib-1)
        ied = istack_export(neib  )
!$omp do private(k,nd,ii,ix)
        do k= ist+1, ied
          do nd = 1, NB
                 ii   = NB * (item_export(k)-1) + nd
                 ix   = NB * (k-1) + nd
             WS(ix)= xx4(ii)
           end do
         end do
!$omp end do nowait
      end do
!$omp end parallel
      call end_eleps_time(4)

!$omp parallel private(nd,neib,ist,ied)
      do neib= 1, num_neib
        ist = istack_import(neib-1) 
        ied = istack_import(neib  )
!$omp do private(k,nd,ii,ix)
        do nd = 1, NB
          do k= ist+1, ied
            ii   = NB * (item_import(k)-1) + nd
            ix   = NB * (k-1) + nd
            xx4(ii)= WR(ix)
          end do
        enddo
!$omp end do nowait
      enddo
!$omp end parallel
      call end_eleps_time(3)
      call start_eleps_time(4)
!$omp parallel private(nd,neib,ist,ied)
      do neib= 1, num_neib
        ist = istack_import(neib-1)
        ied = istack_import(neib  )
!$omp do private(k,ii,ix)
        do nd = 1, NB
          do k= ist+1, ied
            ii   = NB * (internal_node+k-1) + nd
            ix   = NB * (irev_import(k)-1) + nd
            xx4(ii)= WR(ix)
          end do
        enddo
!$omp end do nowait
      enddo
!$omp end parallel
      call end_eleps_time(4)
!
!
!
      call start_eleps_time(5)
      call start_eleps_time(6)
!$omp parallel private(neib,nd,ist,num)
      do neib= 1, num_neib
        ist = istack_export(neib-1)
        num = istack_export(neib  ) - istack_export(neib-1)
        do nd = 1, NB
!$omp do private(k,ii,ix)
          do k= 1, num
                 ii   = NB * (item_export(k+ist) - 1) + nd
                 ix   = k + (nd-1) * num + NB*ist
             WS(ix)= xx4(ii)
           end do
!$omp end do nowait
         end do
      end do
!$omp end parallel
      call end_eleps_time(6)

!$omp parallel private(nd,neib,ist,num)
      do neib= 1, num_neib
        ist = istack_import(neib-1)
        num = istack_import(neib  ) - istack_import(neib-1)
        do nd = 1, NB
!$omp do private(k,ii,ix)
          do k= 1, num
            ii   = NB * (item_import(k+ist)-1) + nd
            ix   = k + (nd-1) * num + NB*ist
            xx4(ii)= WR(ix)
          end do
!$omp end do nowait
        enddo
      enddo
!$omp end parallel
      call end_eleps_time(5)
      call start_eleps_time(6)
!$omp parallel private(nd,neib,ist,num)
      do neib= 1, num_neib
        ist = istack_import(neib-1)
        num = istack_import(neib  ) - istack_import(neib-1)
        do nd = 1, NB
!$omp do private(k,ii,ix)
          do k= 1, num
            ii   = NB * (internal_node+k+ist-1) + nd
            ix   = NB * (irev_import(k+ist)-1) + nd
            xx4(ii)= WR(ix)
          end do
!$omp end do nowait
        enddo
      enddo
!$omp end parallel
      call end_eleps_time(6)
!
!
      call start_eleps_time(7)
      call start_eleps_time(8)
!$omp parallel private(neib,ist,num)
      do neib= 1, num_neib
        ist = istack_export(neib-1)
        num = istack_export(neib  ) - istack_export(neib-1)
!$omp do private(k,nd,ii,ix)
        do inum = 1, NB*num
          k = mod(inum-ione,num) + ione
          nd = (inum-k) / NB + ione
                 ii   = NB * (item_export(k+ist) - 1) + nd
                 ix   = inum + NB*ist
             WS(ix)= xx4(ii)
        end do
!$omp end do nowait
      end do
!$omp end parallel
      call end_eleps_time(8)

!$omp parallel private(nd,neib,ist,num,inum)
      do neib= 1, num_neib
        ist = istack_import(neib-1)
        num = istack_import(neib  ) - istack_import(neib-1)
!$omp do private(inum,k,ii,ix)
        do inum = 1, NB*num
          nd = mod(inum-ione,NB) + ione
          k = (inum-nd) / NB + ione
            ii   = NB * (item_import(k+ist)-1) + nd
            ix   = k + (nd-1) * num + NB*ist
            xx4(ii)= WR(ix)
        end do
!$omp end do nowait
      enddo
!$omp end parallel
      call end_eleps_time(7)
!
      call start_eleps_time(8)
!$omp parallel private(nd,neib,ist,num,inum)
      do neib= 1, num_neib
        ist = istack_import(neib-1)
        num = istack_import(neib  ) - istack_import(neib-1)
!$omp do private(k,ii,ix)
        do inum = 1, NB*num
          nd = mod(inum-ione,NB) + ione
          k = (inum-nd) / NB + ione
            ii   = NB * (internal_node+k+ist-1) + nd
            ix   = irev_import(k) + (nd-1) * num + NB*ist
            xx4(ii)= WR(ix)
        end do
!$omp end do nowait
      enddo
!$omp end parallel
      call end_eleps_time(8)
      
      call output_elapsed_times
!
      deallocate(xx4)
!
      end subroutine node_send_recv4_test
!
! ----------------------------------------------------------------------
!
      subroutine ele_send_recv_test
!
      use m_ele_sf_eg_comm_tables
!
      integer(kind = kint) :: iele, inum
!
!
      do iele = 1, numele
        x_ele_comm(3*iele-2) = x_ele(iele,1)
        x_ele_comm(3*iele-1) = x_ele(iele,2)
        x_ele_comm(3*iele  ) = x_ele(iele,3)
      end do
      do inum = 1, ele_comm%ntot_import
        iele = ele_comm%item_import(inum)
        x_ele_comm(3*iele-2) = 0.0d0
        x_ele_comm(3*iele-1) = 0.0d0
        x_ele_comm(3*iele  ) = 0.0d0
      end do
!
      call SOLVER_SEND_RECV_3_type(numele, ele_comm, x_ele_comm)
!
      end subroutine ele_send_recv_test
!
! ----------------------------------------------------------------------
!
      subroutine surf_send_recv_test
!
      use m_surface_geometry_data
      use m_ele_sf_eg_comm_tables
!
      integer(kind = kint) :: isurf, inum
!
!
      do isurf = 1, numsurf
        x_surf_comm(3*isurf-2) = x_surf(isurf,1)
        x_surf_comm(3*isurf-1) = x_surf(isurf,2)
        x_surf_comm(3*isurf  ) = x_surf(isurf,3)
      end do
      do inum = 1, surf_comm%ntot_import
        isurf = surf_comm%item_import(inum)
        x_surf_comm(3*isurf-2) = 0.0d0
        x_surf_comm(3*isurf-1) = 0.0d0
        x_surf_comm(3*isurf  ) = 0.0d0
      end do
!
      call SOLVER_SEND_RECV_3_type(numsurf, surf_comm, x_surf_comm)
!
      end subroutine surf_send_recv_test
!
! ----------------------------------------------------------------------
!
      subroutine edge_send_recv_test
!
      use m_edge_geometry_data
      use m_ele_sf_eg_comm_tables
!
      integer(kind = kint) :: iedge, inum
!
!
      do iedge = 1, numedge
        x_edge_comm(3*iedge-2) = x_edge(iedge,1)
        x_edge_comm(3*iedge-1) = x_edge(iedge,2)
        x_edge_comm(3*iedge  ) = x_edge(iedge,3)
      end do
      do inum = 1, edge_comm%ntot_import
        iedge = edge_comm%item_import(inum)
        x_edge_comm(3*iedge-2) = 0.0d0
        x_edge_comm(3*iedge-1) = 0.0d0
        x_edge_comm(3*iedge  ) = 0.0d0
      end do
!
      call SOLVER_SEND_RECV_3_type(numedge, edge_comm, x_edge_comm)
!
      end subroutine edge_send_recv_test
!
! ----------------------------------------------------------------------
!
      end module mesh_send_recv_test
