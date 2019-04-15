!
!      module neib_edge_line_cube
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
!!      subroutine neib_edge_line(id_rank, c_size, c_each, nb_rng)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(size_of_each_cube), intent(in) :: c_each
!!        type(neib_range_cube), intent(in) :: nb_rng
!
      module neib_edge_line_cube
!
      use m_precision
!
      use t_size_of_cube
      use m_local_node_id_cube
      use m_cube_files_data
      use m_filtering_edge_4_cubmesh
      use set_parallel_file_name
!
      implicit none
!
      integer(kind = kint) :: nneib_1d
      integer(kind = kint), allocatable :: iedge_neib_x(:)
      integer(kind = kint), allocatable :: iedge_neib_y(:)
      integer(kind = kint), allocatable :: iedge_neib_z(:)
!
      integer(kind = kint), allocatable :: iedge_f_d_x(:)
      integer(kind = kint), allocatable :: iedge_f_d_y(:)
      integer(kind = kint), allocatable :: iedge_f_d_z(:)
!
      private :: nneib_1d
      private :: iedge_neib_x, iedge_neib_y, iedge_neib_z
      private :: iedge_f_d_x, iedge_f_d_y, iedge_f_d_z
!
      private :: allocate_neighbour_edge_line
      private :: deallocate_neighbour_edge_line
      private :: set_neib_edge_line, write_neib_edge_line
!
!  ----------------------------------------------------------------------!
      contains
!
!  ----------------------------------------------------------------------
!
      subroutine neib_edge_line(id_rank, c_size, c_each, nb_rng)
!
      use t_neib_range_cube
      use m_constants
!
      integer, intent(in) :: id_rank
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(in) :: c_each
      type(neib_range_cube), intent(in) :: nb_rng
!
!
      call allocate_neighbour_edge_line(c_size, c_each)
!
      call set_neib_edge_line                                           &
     &   (c_size, c_each, ione, nb_rng%iedge_st, nb_rng%iedge_end,      &
     &    nb_rng%j_st, nb_rng%j_end, nb_rng%k_st, nb_rng%k_end)
      call set_neib_edge_line                                           &
     &   (c_size, c_each, itwo, nb_rng%i_st, nb_rng%i_end,              &
     &    nb_rng%jedge_st, nb_rng%jedge_end, nb_rng%k_st, nb_rng%k_end)
      call set_neib_edge_line                                           &
     &   (c_size, c_each, ithree, nb_rng%i_st, nb_rng%i_end,            &
     &    nb_rng%j_st, nb_rng%j_end, nb_rng%kedge_st, nb_rng%kedge_end)
!
      call write_neib_edge_line(c_size, c_each, id_rank)
!
      call deallocate_neighbour_edge_line
!
      end subroutine neib_edge_line
!
!  ----------------------------------------------------------------------
!
       subroutine allocate_neighbour_edge_line(c_size, c_each)
!
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(in) :: c_each
!
       nneib_1d = c_size%ndep_1 * c_each%edgetot
       allocate( iedge_neib_x(nneib_1d) )
       allocate( iedge_neib_y(nneib_1d) )
       allocate( iedge_neib_z(nneib_1d) )
!
       allocate( iedge_f_d_x(nneib_1d) )
       allocate( iedge_f_d_y(nneib_1d) )
       allocate( iedge_f_d_z(nneib_1d) )
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
       deallocate( iedge_f_d_x )
       deallocate( iedge_f_d_y )
       deallocate( iedge_f_d_z )
!
       end subroutine deallocate_neighbour_edge_line
!
!  ----------------------------------------------------------------------
!
       subroutine set_neib_edge_line(c_size, c_each,                    &
      &          nd, ist, ied, jst, jed, kst, ked)
!
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(in) :: c_each
!
       integer(kind = kint), intent(in) :: nd
       integer(kind = kint), intent(in) :: ist,jst,kst
       integer(kind = kint), intent(in) :: ied,jed,ked
!
       integer(kind = kint) :: i, j, k, iedge, iedge0
       integer(kind = kint) :: i1, ii, jj, kk
!
       iedge = 0
       if (nd.eq.1) then
         iedge = 0
       else if (nd.eq.2) then
         iedge = (c_each%nx-1) * c_each%ny  * c_each%nz
       else if (nd.eq.3) then
         iedge = (c_each%nx-1) * c_each%ny  * c_each%nz                 &
     &          + c_each%nx * (c_each%ny-1)  * c_each%nz
       end if
!
       do k=kst,ked
        do j=jst,jed
         do i=ist,ied
!
          do i1 = 1, c_size%ndep_1
           iedge0 = c_size%ndep_1*(iedge-1) + i1
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
      subroutine write_neib_edge_line(c_size, c_each, id_rank)
!
      integer, intent(in) :: id_rank
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(in) :: c_each
!
      integer (kind = kint) :: ist, ied, iedge, i
!
!
       fname = add_process_id(id_rank, filter_edge_header)
       open (nb_out, file=nb_name)
!
       write(nb_out,'(a12)') '! num_edge:  '
       write(nb_out,'(i16)') c_each%edgetot
       write(nb_out,'(i16)')                                            &
     &        c_each%edgetot, c_each%edgetot, c_each%edgetot
       write(nb_out,'(a12)') '! num_depth: '
       write(nb_out,'(2i16)') c_size%ndep_1
!
       write(nb_out,'(a16)') '!  xi direction'
       do iedge = 1, c_each%edgetot
         ist = c_size%ndep_1*(iedge- 1) + 1
         ied = c_size%ndep_1*iedge
!
         write(nb_out,'(10i16)') (iedge_f_d_x(i),  i = ist, ied)
         write(nb_out,'(10i16)') (iedge_neib_x(i), i = ist, ied)
       end do
!
       write(nb_out,'(a16)') '!  eta direction'
       do iedge = 1, c_each%edgetot
        ist = c_size%ndep_1*(iedge- 1) + 1
        ied = c_size%ndep_1*iedge
!
          write(nb_out,'(10i16)') (iedge_f_d_y(i),  i = ist, ied)
          write(nb_out,'(10i16)') (iedge_neib_y(i), i = ist, ied)
!
       end do
!
       write(nb_out,'(a16)') '!   zi direction'
       do iedge = 1, c_each%edgetot
        ist = c_size%ndep_1*(iedge- 1) + 1
        ied = c_size%ndep_1*iedge
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
       end module neib_edge_line_cube
