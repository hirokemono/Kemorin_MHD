!
!      module m_neib_edge_line_cube
!
!     written by H. Matsui
!
!  ----------------------------------------------------------------------
!
!      binary data data format
!        Number of node
!        Number of depth, max. number of nodes for filtering
!        Stack for filtering (1 to num. of node)
!        Local node ID for filtering
!        distance in xi-direction
!        distance in eta-direction
!        distance in zta-direction
!
!  ----------------------------------------------------------------------
!
      module m_neib_edge_line_cube
!
      use m_precision
!
      use m_size_of_cube
      use m_local_node_id_cube
      use m_neighb_range_cube
      use m_cube_files_data
      use m_filtering_edge_4_cubmesh
      use set_parallel_file_name
!
      implicit none
!
      integer(kind = kint), dimension(:), allocatable :: iedge_neib_x
      integer(kind = kint), dimension(:), allocatable :: iedge_neib_y
      integer(kind = kint), dimension(:), allocatable :: iedge_neib_z
!
      integer(kind = kint), dimension(:), allocatable :: iedge_f_d_x
      integer(kind = kint), dimension(:), allocatable :: iedge_f_d_y
      integer(kind = kint), dimension(:), allocatable :: iedge_f_d_z
!
!
!  ----------------------------------------------------------------------!
      contains
!
!  ----------------------------------------------------------------------
!
       subroutine allocate_neighbour_edge_line
!
       allocate( iedge_neib_x(ndep_1*edgetot) )
       allocate( iedge_neib_y(ndep_1*edgetot) )
       allocate( iedge_neib_z(ndep_1*edgetot) )
!
       allocate( iedge_f_d_x(ndep_1*edgetot) )
       allocate( iedge_f_d_y(ndep_1*edgetot) )
       allocate( iedge_f_d_z(ndep_1*edgetot) )
!
       iedge_neib_x = 0
       iedge_neib_y = 0
       iedge_neib_z = 0
       iedge_f_d_x = 0
       iedge_f_d_y = 0
       iedge_f_d_z = 0
!
       end subroutine allocate_neighbour_edge_line
!
!  ----------------------------------------------------------------------
!
       subroutine deallocate_neighbour_edge_line
!
       deallocate( iedge_neib_x )
       deallocate( iedge_neib_y )
       deallocate( iedge_neib_z )
!
       end subroutine deallocate_neighbour_edge_line
!
!  ----------------------------------------------------------------------
!
       subroutine neib_edge_line(id_rank)
!
       use m_constants
!
       integer, intent(in) :: id_rank
!
!
       call allocate_neighbour_edge_line
!
       call set_neib_edge_line(ione, nb_rng1%iedge_st, nb_rng1%iedge_end, &
     &     nb_rng1%j_st, nb_rng1%j_end, nb_rng1%k_st, nb_rng1%k_end)
       call set_neib_edge_line(itwo, nb_rng1%i_st, nb_rng1%i_end,       &
     &     nb_rng1%jedge_st, nb_rng1%jedge_end, nb_rng1%k_st, nb_rng1%k_end)
       call set_neib_edge_line(ithree, nb_rng1%i_st, nb_rng1%i_end,     &
     &     nb_rng1%j_st, nb_rng1%j_end, nb_rng1%kedge_st, nb_rng1%kedge_end)
!
       call write_neib_edge_line(id_rank)
!
       call deallocate_neighbour_edge_line
!
       end subroutine neib_edge_line
!
!  ----------------------------------------------------------------------
!
       subroutine set_neib_edge_line(nd,ist,ied,jst,jed,kst,ked)
!
       integer(kind = kint) :: nd
       integer(kind = kint) :: ist,jst,kst
       integer(kind = kint) :: ied,jed,ked
!
       integer(kind = kint) :: i, j, k, iedge, iedge0
       integer(kind = kint) :: i1, ii, jj, kk
!
       if (nd.eq.1) then
         iedge = 0
       else if (nd.eq.2) then
         iedge = (nx-1) * ny  * nz
       else if (nd.eq.3) then
         iedge = (nx-1) * ny  * nz + nx * (ny-1)  * nz
       end if
!
       do k=kst,ked
        do j=jst,jed
         do i=ist,ied
!
          do i1 = 1, ndep_1
           iedge0 = ndep_1*(iedge-1) + i1
           ii = iedge_f_item_x(i1,i,j,k,nd)
           jj = iedge_f_item_y(i1,i,j,k,nd)
           kk = iedge_f_item_z(i1,i,j,k,nd)
           iedge_f_d_x(iedge0) = iedge_f_dist_x(i1,i,j,k,nd)
           iedge_neib_x(iedge0) = edge_id_lc(ii,j,k,nd)
           iedge_f_d_y(iedge0) = iedge_f_dist_y(i1,i,j,k,nd)
           iedge_neib_y(iedge0) = edge_id_lc(i,jj,k,nd)
           iedge_f_d_z(iedge0) = iedge_f_dist_z(i1,i,j,k,nd)
           iedge_neib_z(iedge0) = edge_id_lc(i,j,kk,nd)
          end do
!
         enddo
        enddo
       enddo
!
       end subroutine set_neib_edge_line
!
!  ----------------------------------------------------------------------
!
       subroutine write_neib_edge_line(id_rank)
!
       integer, intent(in) :: id_rank
!
       integer (kind = kint) :: ist, ied, iedge, i
!
!
       fname = add_process_id(id_rank, filter_edge_header)
       open (nb_out, file=nb_name)
!
       write(nb_out,'(a12)') '! num_edge:  '
       write(nb_out,'(i16)') edgetot
       write(nb_out,'(i16)') edgetot, edgetot, edgetot
       write(nb_out,'(a12)') '! num_depth: '
       write(nb_out,'(2i16)') ndep_1
!
       write(nb_out,'(a16)') '!  xi direction'
       do iedge = 1, edgetot
         ist = ndep_1*(iedge- 1) + 1
         ied = ndep_1*iedge
!
         write(nb_out,'(10i16)') (iedge_f_d_x(i),  i = ist, ied)
         write(nb_out,'(10i16)') (iedge_neib_x(i), i = ist, ied)
       end do
!
       write(nb_out,'(a16)') '!  eta direction'
       do iedge = 1, edgetot
        ist = ndep_1*(iedge- 1) + 1
        ied = ndep_1*iedge
!
          write(nb_out,'(10i16)') (iedge_f_d_y(i),  i = ist, ied)
          write(nb_out,'(10i16)') (iedge_neib_y(i), i = ist, ied)
!
       end do
!
       write(nb_out,'(a16)') '!   zi direction'
       do iedge = 1, edgetot
        ist = ndep_1*(iedge- 1) + 1
        ied = ndep_1*iedge
!
          write(nb_out,'(10i16)') (iedge_f_d_z(i),  i = ist, ied)
          write(nb_out,'(10i16)') (iedge_neib_z(i), i = ist, ied)
!
       end do
!
        close(nb_out)
!
       end subroutine write_neib_edge_line
!
!  ----------------------------------------------------------------------
!
       end module m_neib_edge_line_cube
