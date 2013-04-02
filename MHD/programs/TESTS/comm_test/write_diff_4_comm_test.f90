!
!      module write_diff_4_comm_test
!
!      subroutine output_diff_node_comm_test
!      subroutine output_diff_mesh_comm_test
!      subroutine write_diff_comm_test(istack_diff_pe, ntot_diff_pe,    &
!     &          id_diff_IO, id_gl_diff_IO, x_diff_IO)
!
!     Written by H. Matsui on Sep., 2007
!
      module write_diff_4_comm_test
!
      use m_precision
!
      use m_parallel_var_dof
      use m_geometry_4_comm_test
!
      implicit  none
!
!
      integer(kind = kint), parameter :: id_comm_test = 31
      character(len=kchara), parameter                                  &
     &      :: comm_test_name = 'comm_test.dat'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine output_diff_node_comm_test
!
      if(ntot_nod_diff_pe .eq. 0) then
        write(*,*) 'No wrong communication for nodes'
        return
      end if
!
      open(id_comm_test, file = comm_test_name)
!
      write(id_comm_test,*) 'ntot_nod_diff_pe', ntot_nod_diff_pe
      write(id_comm_test,*) 'domain, local_nod_id, ',                   &
     &      'global_nod_org, global_nod_get, ',                         &
     &      'xx_org, yy_org, zz_org, xx_get, yy_get, zz_get'
!
      call write_diff_comm_test(istack_nod_diff_pe, ntot_nod_diff_pe,   &
     &    inod_diff_IO, inod_gl_diff_IO, xx_diff_IO)
!
      close(id_comm_test)
!
      end subroutine output_diff_node_comm_test
!
!  ---------------------------------------------------------------------
!
      subroutine output_diff_mesh_comm_test
!
      integer(kind = kint) :: ntot_error
!
      ntot_error = ntot_nod_diff_pe + ntot_ele_diff_pe                  &
     &            + ntot_surf_diff_pe + ntot_edge_diff_pe
      if(ntot_error .eq. 0) then
        write(*,*) 'No wrong communication for mesh'
        return
      end if
!
      open(id_comm_test, file = comm_test_name)
!
      write(id_comm_test,*) 'ntot_nod_diff_pe ', ntot_nod_diff_pe
      write(id_comm_test,*) 'ntot_ele_diff_pe ', ntot_ele_diff_pe
      write(id_comm_test,*) 'ntot_surf_diff_pe', ntot_surf_diff_pe
      write(id_comm_test,*) 'ntot_edge_diff_pe', ntot_edge_diff_pe
      write(*,*) 'ntot_nod_diff_pe ', ntot_nod_diff_pe
      write(*,*) 'ntot_ele_diff_pe ', ntot_ele_diff_pe
      write(*,*) 'ntot_surf_diff_pe', ntot_surf_diff_pe
      write(*,*) 'ntot_edge_diff_pe', ntot_edge_diff_pe
!
!
      write(id_comm_test,*) 'domain, local_nod_id, ',                   &
     &      'global_nod_org, global_nod_get, ',                         &
     &      'xx_org, yy_org, zz_org, xx_get, yy_get, zz_get'
      call write_diff_comm_test(istack_nod_diff_pe, ntot_nod_diff_pe,   &
     &    inod_diff_IO, inod_gl_diff_IO, xx_diff_IO)
!
      write(id_comm_test,*) 'domain, local_ele_id, ',                   &
     &      'global_ele_org, global_ele_get, ',                         &
     &      'xx_org, yy_org, zz_org, xx_get, yy_get, zz_get'
      call write_diff_comm_test(istack_ele_diff_pe, ntot_ele_diff_pe,   &
     &    iele_diff_IO, iele_gl_diff_IO, xele_diff_IO)
!
      write(id_comm_test,*) 'domain, local_surf_id, ',                  &
     &      'global_surf_org, global_surf_get, ',                       &
     &      'xx_org, yy_org, zz_org, xx_get, yy_get, zz_get'
      call write_diff_comm_test(istack_surf_diff_pe, ntot_surf_diff_pe, &
     &    isurf_diff_IO, isurf_gl_diff_IO, xsurf_diff_IO)
!
      write(id_comm_test,*) 'domain, local_edge_id, ',                  &
     &      'global_edge_org, global_edge_get, ',                       &
     &      'xx_org, yy_org, zz_org, xx_get, yy_get, zz_get'
      call write_diff_comm_test(istack_edge_diff_pe, ntot_edge_diff_pe, &
     &    iedge_diff_IO, iedge_gl_diff_IO, xedge_diff_IO)
!
      close(id_comm_test)
!
      end subroutine output_diff_mesh_comm_test
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_diff_comm_test(istack_diff_pe, ntot_diff_pe,     &
     &          id_diff_IO, id_gl_diff_IO, x_diff_IO)
!
      integer(kind = kint), intent(in) :: istack_diff_pe(0:nprocs)
      integer(kind = kint), intent(in) :: ntot_diff_pe
      integer(kind = kint), intent(in) :: id_diff_IO(ntot_diff_pe)
      integer(kind = kint), intent(in) :: id_gl_diff_IO(2*ntot_diff_pe)
      real(kind = kreal), intent(in) :: x_diff_IO(6*ntot_diff_pe)
!
      integer(kind = kint) :: ip, id_rank, ist, ied, inum
      integer(kind = kint) :: j1, j2, k1, k2
!
!
      do ip = 1, nprocs
        id_rank = ip - 1
        ist = istack_diff_pe(ip-1) + 1
        ied = istack_diff_pe(ip)
        do inum = ist, ied
          j1 = 2*inum-1
          j2 = 2*inum
          k1 = 6*inum-5
          k2 = 6*inum
          write(id_comm_test,1000) id_rank, id_diff_IO(inum),           &
     &        id_gl_diff_IO(j1:j2), x_diff_IO(k1:k2)
        end do
      end do
!
 1000 format(4i10, 1p6e20.12)
!
      end subroutine write_diff_comm_test
!
!  ---------------------------------------------------------------------
!
      end module write_diff_4_comm_test
