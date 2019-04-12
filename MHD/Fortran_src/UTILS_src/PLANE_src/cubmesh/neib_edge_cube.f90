!
!      module neib_edge_cube
!
!     written by H. Matsui
!
!!  ---------------------------------------------------------------------
!!
!!      binary data data format
!!        Number of node
!!        Number of depth, max. number of nodes for filtering
!!        Stack for filtering (1 to num. of node)
!!        Local node ID for filtering
!!        distance in xi-direction
!!        distance in eta-direction
!!        distance in zta-direction
!!
!!  ---------------------------------------------------------------------
!!
!!      subroutine neighboring_edge(id_rank, nb_rng)
!!        type(neib_range_cube), intent(in) :: nb_rng
!!
      module neib_edge_cube
!
      use m_precision
      use m_constants
!
      use m_size_4_plane
      use m_size_of_cube
      use m_local_node_id_cube
      use m_cube_files_data
      use m_filtering_edge_4_cubmesh
      use m_neib_edge_line_cube
!
      implicit none
!
!  ----------------------------------------------------------------------
!
      contains
!
!  ----------------------------------------------------------------------
!
       subroutine neighboring_edge(id_rank, nb_rng)
!
      use t_neib_range_cube
!
      integer, intent(in) :: id_rank
      type(neib_range_cube), intent(in) :: nb_rng
!
!
!       for edge on y=const and z=const
!       xi direction
!
      call count_neib_edge_x                                            &
     &   (ione, ione, nb_rng%iedge_st, nb_rng%iedge_end,                &
     &    nb_rng%j_st, nb_rng%j_end, nb_rng%k_st, nb_rng%k_end)
!
!       eta direction
!
      call count_neib_edge_y                                            &
     &   (ione, izero, nb_rng%iedge_st, nb_rng%iedge_end,               &
     &    nb_rng%j_st, nb_rng%j_end, nb_rng%k_st, nb_rng%k_end)
!
!       zeta direction
!
      call count_neib_edge_z(ione, nb_rng%iedge_st, nb_rng%iedge_end,   &
     &    nb_rng%j_st, nb_rng%j_end, nb_rng%k_st, nb_rng%k_end)
!
!       for edge on z=const and x=const
!
      call count_neib_edge_x(itwo, izero, nb_rng%i_st, nb_rng%i_end,    &
     &    nb_rng%jedge_st, nb_rng%jedge_end, nb_rng%k_st, nb_rng%k_end)
!
      call count_neib_edge_y(itwo, ione, nb_rng%i_st, nb_rng%i_end,     &
     &    nb_rng%jedge_st, nb_rng%jedge_end, nb_rng%k_st, nb_rng%k_end)
!
      call count_neib_edge_z(itwo, nb_rng%i_st, nb_rng%i_end,           &
     &    nb_rng%jedge_st, nb_rng%jedge_end, nb_rng%k_st, nb_rng%k_end)
!
!       for edge on x=const and y=const
!
      call count_neib_edge_x(ithree, izero, nb_rng%i_st, nb_rng%i_end,  &
     &    nb_rng%j_st, nb_rng%j_end, nb_rng%kedge_st, nb_rng%kedge_end)
!
      call count_neib_edge_y(ithree, izero, nb_rng%i_st, nb_rng%i_end,  &
     &    nb_rng%j_st, nb_rng%j_end, nb_rng%kedge_st, nb_rng%kedge_end)
!
      call count_neib_edge_z(ithree, nb_rng%i_st, nb_rng%i_end,         &
     &    nb_rng%j_st, nb_rng%j_end, nb_rng%kedge_st, nb_rng%kedge_end)
!
      call neib_edge_line(id_rank)
!
       end subroutine neighboring_edge
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine count_neib_edge_x(nd,nid,ist,ied,jst,jed, kst,ked)
!
       integer(kind = kint), intent(in) :: nd, nid
       integer(kind = kint), intent(in) :: ist,jst,kst
       integer(kind = kint), intent(in) :: ied,jed,ked
!
       integer(kind = kint), dimension(0:1) :: ndepth_x
       integer(kind = kint) :: i, j, k
       integer(kind = kint) :: ii,i1
!
!
       ndepth_x(0) = ndepth
!
       do k=kst,ked
        do j=jst,jed
         do i=ist,ied
          ndepth_x(1) = min(i+ndepth,ied) - i
          if ( ndepth_x( 1) .lt. ndepth) then
           ndepth_x(0) = 2*ndepth - ndepth_x( 1)
          end if
!
          nedge_neib_x(i,j,k,nd) = ndepth_x(0) + ndepth_x(nid) + 1
!
          do ii = ndepth_x(0), 1, -1
           i1 = ndepth_x(0) - ii + 1
           iedge_f_item_x(i1,i,j,k,nd) = i - ii
           iedge_f_dist_x(i1,i,j,k,nd) = -ii
          end do
          i1 = ndepth_x(0) + 1
          iedge_f_item_x(i1,i,j,k,nd) = i
          iedge_f_dist_x(i1,i,j,k,nd) = 0
          do ii = 1, ndepth_x(nid)
           i1 = ii + ndepth_x(0) + 1
           iedge_f_item_x(i1,i,j,k,nd) = i + ii
           iedge_f_dist_x(i1,i,j,k,nd) = ii
          end do
         enddo
        enddo
       enddo
!
        end subroutine count_neib_edge_x
!
!  ---------------------------------------------------------------------
!
       subroutine count_neib_edge_y(nd,nid,ist,ied,jst,jed,kst,ked)
!
       integer(kind = kint), intent(in) :: nd, nid
       integer(kind = kint), intent(in) :: ist,jst,kst
       integer(kind = kint), intent(in) :: ied,jed,ked
!
       integer(kind = kint), dimension(0:1) :: ndepth_y
       integer(kind = kint) :: i, j, k
       integer(kind = kint) :: jj,j1
!
!
       ndepth_y(0) = ndepth
!
       do k=kst,ked
        do j=jst,jed
         ndepth_y(1) = min(k+ndepth,jed) - j
         if ( ndepth_y( 1) .lt. ndepth) then
          ndepth_y(0) = 2*ndepth - ndepth_y( 1)
         end if
         do i=ist,ied
!
          nedge_neib_y(i,j,k,nd) = ndepth_y(0) + ndepth_y(nid) + 1
!
          do jj = ndepth_y(0), 1, -1
           j1 = ndepth_y(0) - jj + 1
           iedge_f_item_y(j1,i,j,k,nd) = j - jj
           iedge_f_dist_y(j1,i,j,k,nd) = -jj
          end do
          j1 = ndepth_y(0) + 1
          iedge_f_item_y(j1,i,j,k,nd) = j
          iedge_f_dist_y(j1,i,j,k,nd) = 0
          do jj = j+1,j+ndepth_y(nid)
           j1 = jj + ndepth_y(0) + 1
           iedge_f_item_y(j1,i,j,k,nd) = j + jj
           iedge_f_dist_y(j1,i,j,k,nd) = jj
          end do
         enddo
        enddo
       enddo
!
        end subroutine count_neib_edge_y
!
!  ---------------------------------------------------------------------
!
       subroutine count_neib_edge_z(nd,ist,ied,jst,jed,kst,ked)
!
       integer(kind = kint), intent(in) :: nd
       integer(kind = kint), intent(in) :: ist,jst,kst
       integer(kind = kint), intent(in) :: ied,jed,ked
!
       integer(kind = kint), dimension(-1:1) :: ndepth_z
       integer(kind = kint) :: i, j, k
       integer(kind = kint) :: kk, k1
!
!
       do k=kst,ked
        ndepth_z(-1) = k - max(k-ndepth,1)
        ndepth_z( 1) = min(k+ndepth,nz_all) - k
        if ( ndepth_z(-1) .lt. ndepth) then
          ndepth_z( 1) = 2*ndepth - ndepth_z(-1)
        end if
        if ( ndepth_z( 1) .lt. ndepth) then
          ndepth_z(-1) = 2*ndepth - ndepth_z( 1)
        end if
        do j=jst,jed
         do i=ist,ied
!
          nedge_neib_z(i,j,k,nd) = ndepth_z(-1) + ndepth_z(1) + 1
!
          do kk = ndepth_z(-1), 1, -1
           k1 = ndepth_z(-1) - kk + 1
           iedge_f_item_z(k1,i,j,k,nd) = k - kk
           iedge_f_dist_z(k1,i,j,k,nd) = -kk
          end do
          k1 = ndepth_z(-1) + 1
          iedge_f_item_z(k1,i,j,k,nd) = k
          iedge_f_dist_z(k1,i,j,k,nd) = 0
          do kk = k+1,k+ndepth_z(1)
           k1 = kk + ndepth_z(-1) + 1
           iedge_f_item_z(k1,i,j,k,nd) = k + kk
           iedge_f_dist_z(k1,i,j,k,nd) = kk
          end do
!
          enddo
         enddo
        enddo
!
        end subroutine count_neib_edge_z
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       end module neib_edge_cube
