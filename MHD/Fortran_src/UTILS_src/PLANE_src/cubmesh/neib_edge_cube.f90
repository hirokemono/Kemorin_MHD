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
!!      subroutine neighboring_edge                                     &
!!     &         (id_rank, c_size, c_each, nb_rng, loc_id, c_fil_edge)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(size_of_each_cube), intent(in) :: c_each
!!        type(neib_range_cube), intent(in) :: nb_rng
!!        type(local_node_id_cube), intent(in) :: loc_id
!!        type(filter_work_cubmesh), intent(inout) :: c_fil_edge(3)
!!
      module neib_edge_cube
!
      use m_precision
      use m_constants
!
      use t_filter_work_cubmesh
      use t_local_node_id_cube
!
      implicit none
!
!  ----------------------------------------------------------------------
!
      contains
!
!  ----------------------------------------------------------------------
!
      subroutine neighboring_edge                                       &
     &         (id_rank, c_size, c_each, nb_rng, loc_id, c_fil_edge)
!
      use t_size_of_cube
      use t_neib_range_cube
      use neib_edge_line_cube
!
      integer, intent(in) :: id_rank
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(in) :: c_each
      type(neib_range_cube), intent(in) :: nb_rng
      type(local_node_id_cube), intent(in) :: loc_id
      type(filter_work_cubmesh), intent(inout) :: c_fil_edge(3)
!
!
!       for edge on y=const and z=const
!       xi direction
!
      call count_neib_edge_x(ione, c_size%ndepth,                       &
     &    nb_rng%iedge_st, nb_rng%iedge_end, nb_rng%j_st, nb_rng%j_end, &
     &    nb_rng%k_st, nb_rng%k_end, c_fil_edge(1))
!
!       eta direction
!
      call count_neib_edge_y(izero, c_size%ndepth,                      &
     &    nb_rng%iedge_st, nb_rng%iedge_end, nb_rng%j_st, nb_rng%j_end, &
     &    nb_rng%k_st, nb_rng%k_end, c_fil_edge(1))
!
!       zeta direction
!
      call count_neib_edge_z(c_size%ndepth, c_size%nz_all,              &
     &    nb_rng%iedge_st, nb_rng%iedge_end, nb_rng%j_st, nb_rng%j_end, &
     &    nb_rng%k_st, nb_rng%k_end, c_fil_edge(1))
!
!       for edge on z=const and x=const
!
      call count_neib_edge_x(izero, c_size%ndepth,                      &
     &    nb_rng%i_st, nb_rng%i_end, nb_rng%jedge_st, nb_rng%jedge_end, &
     &    nb_rng%k_st, nb_rng%k_end, c_fil_edge(2))
!
      call count_neib_edge_y(ione, c_size%ndepth,                       &
     &    nb_rng%i_st, nb_rng%i_end, nb_rng%jedge_st, nb_rng%jedge_end, &
     &    nb_rng%k_st, nb_rng%k_end, c_fil_edge(2))
!
      call count_neib_edge_z(c_size%ndepth, c_size%nz_all,              &
     &    nb_rng%i_st, nb_rng%i_end, nb_rng%jedge_st, nb_rng%jedge_end, &
     &    nb_rng%k_st, nb_rng%k_end, c_fil_edge(2))
!
!       for edge on x=const and y=const
!
      call count_neib_edge_x(izero, c_size%ndepth,                      &
     &    nb_rng%i_st, nb_rng%i_end, nb_rng%j_st, nb_rng%j_end,         &
     &    nb_rng%kedge_st, nb_rng%kedge_end, c_fil_edge(3))
!
      call count_neib_edge_y(izero, c_size%ndepth,                      &
     &    nb_rng%i_st, nb_rng%i_end, nb_rng%j_st, nb_rng%j_end,         &
     &    nb_rng%kedge_st, nb_rng%kedge_end, c_fil_edge(3))
!
      call count_neib_edge_z(c_size%ndepth, c_size%nz_all,              &
     &    nb_rng%i_st, nb_rng%i_end, nb_rng%j_st, nb_rng%j_end,         &
     &    nb_rng%kedge_st, nb_rng%kedge_end, c_fil_edge(3))
!
      call neib_edge_line                                               &
     &   (id_rank, c_size, c_each, nb_rng, loc_id, c_fil_edge)
!
       end subroutine neighboring_edge
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_neib_edge_x                                      &
     &         (nid, ndepth, ist, ied, jst, jed, kst, ked, c_fil_edge)
!
       integer(kind = kint), intent(in) :: ndepth
       integer(kind = kint), intent(in) :: nid
       integer(kind = kint), intent(in) :: ist,jst,kst
       integer(kind = kint), intent(in) :: ied,jed,ked
!
      type(filter_work_cubmesh), intent(inout) :: c_fil_edge
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
          c_fil_edge%nnod_neib_x(i,j,k)                                 &
     &         = ndepth_x(0) + ndepth_x(nid) + 1
!
          do ii = ndepth_x(0), 1, -1
           i1 = ndepth_x(0) - ii + 1
           c_fil_edge%inod_f_item_x(i1,i,j,k) = i - ii
           c_fil_edge%inod_f_dist_x(i1,i,j,k) = -ii
          end do
          i1 = ndepth_x(0) + 1
          c_fil_edge%inod_f_item_x(i1,i,j,k) = i
          c_fil_edge%inod_f_dist_x(i1,i,j,k) = 0
          do ii = 1, ndepth_x(nid)
           i1 = ii + ndepth_x(0) + 1
           c_fil_edge%inod_f_item_x(i1,i,j,k) = i + ii
           c_fil_edge%inod_f_dist_x(i1,i,j,k) = ii
          end do
         enddo
        enddo
       enddo
!
        end subroutine count_neib_edge_x
!
!  ---------------------------------------------------------------------
!
      subroutine count_neib_edge_y                                      &
     &         (nid, ndepth, ist, ied, jst, jed, kst, ked, c_fil_edge)
!
       integer(kind = kint), intent(in) :: ndepth
       integer(kind = kint), intent(in) :: nid
       integer(kind = kint), intent(in) :: ist,jst,kst
       integer(kind = kint), intent(in) :: ied,jed,ked
!
      type(filter_work_cubmesh), intent(inout) :: c_fil_edge
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
          c_fil_edge%nnod_neib_y(i,j,k)                                 &
     &         = ndepth_y(0) + ndepth_y(nid) + 1
!
          do jj = ndepth_y(0), 1, -1
           j1 = ndepth_y(0) - jj + 1
           c_fil_edge%inod_f_item_y(j1,i,j,k) = j - jj
           c_fil_edge%inod_f_dist_y(j1,i,j,k) = -jj
          end do
          j1 = ndepth_y(0) + 1
          c_fil_edge%inod_f_item_y(j1,i,j,k) = j
          c_fil_edge%inod_f_dist_y(j1,i,j,k) = 0
          do jj = j+1,j+ndepth_y(nid)
           j1 = jj + ndepth_y(0) + 1
           c_fil_edge%inod_f_item_y(j1,i,j,k) = j + jj
           c_fil_edge%inod_f_dist_y(j1,i,j,k) = jj
          end do
         enddo
        enddo
       enddo
!
        end subroutine count_neib_edge_y
!
!  ---------------------------------------------------------------------
!
      subroutine count_neib_edge_z(ndepth, nz_all,                      &
     &          ist, ied, jst, jed, kst, ked, c_fil_edge)
!
      integer(kind = kint), intent(in) :: ndepth, nz_all
      integer(kind = kint), intent(in) :: ist,jst,kst
      integer(kind = kint), intent(in) :: ied,jed,ked
!
      type(filter_work_cubmesh), intent(inout) :: c_fil_edge
!
      integer(kind = kint), dimension(-1:1) :: ndepth_z
      integer(kind = kint) :: i, j, k
      integer(kind = kint) :: kk, k1
!
!
       do k=kst,ked
        ndepth_z(-1) = k - max(k-ndepth,1)
        ndepth_z( 1) = min(k+ndepth, nz_all) - k
        if ( ndepth_z(-1) .lt. ndepth) then
          ndepth_z( 1) = 2*ndepth - ndepth_z(-1)
        end if
        if ( ndepth_z( 1) .lt. ndepth) then
          ndepth_z(-1) = 2*ndepth - ndepth_z( 1)
        end if
        do j=jst,jed
         do i=ist,ied
!
          c_fil_edge%nnod_neib_z(i,j,k)                                 &
     &            = ndepth_z(-1) + ndepth_z(1) + 1
!
          do kk = ndepth_z(-1), 1, -1
           k1 = ndepth_z(-1) - kk + 1
           c_fil_edge%inod_f_item_z(k1,i,j,k) = k - kk
           c_fil_edge%inod_f_dist_z(k1,i,j,k) = -kk
          end do
          k1 = ndepth_z(-1) + 1
          c_fil_edge%inod_f_item_z(k1,i,j,k) = k
          c_fil_edge%inod_f_dist_z(k1,i,j,k) = 0
          do kk = k+1,k+ndepth_z(1)
           k1 = kk + ndepth_z(-1) + 1
           c_fil_edge%inod_f_item_z(k1,i,j,k) = k + kk
           c_fil_edge%inod_f_dist_z(k1,i,j,k) = kk
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
