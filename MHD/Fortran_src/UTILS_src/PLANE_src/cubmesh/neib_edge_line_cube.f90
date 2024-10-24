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
!!      subroutine neib_edge_line                                       &
!!     &         (id_rank, c_size, c_each, nb_rng, loc_id, c_fil_edge)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(size_of_each_cube), intent(in) :: c_each
!!        type(neib_range_cube), intent(in) :: nb_rng
!!        type(local_node_id_cube), intent(in) :: loc_id
!!        type(filter_work_cubmesh), intent(in) :: c_fil_edge(3)
!
      module neib_edge_line_cube
!
      use m_precision
!
      use t_size_of_cube
      use t_local_node_id_cube
      use t_filter_work_cubmesh
      use set_parallel_file_name
!
      implicit none
!
      integer(kind=kint ), parameter  ::  id_file = 19
      character(len=kchara), parameter                                  &
     &                      :: filter_edge_header = 'filter_edge_l'
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
      private :: nneib_1d, id_file, filter_edge_header
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
      subroutine neib_edge_line                                         &
     &         (id_rank, c_size, c_each, nb_rng, loc_id, c_fil_edge)
!
      use t_neib_range_cube
      use m_constants
!
      integer, intent(in) :: id_rank
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(in) :: c_each
      type(neib_range_cube), intent(in) :: nb_rng
      type(local_node_id_cube), intent(in) :: loc_id
      type(filter_work_cubmesh), intent(in) :: c_fil_edge(3)
!
!
      call allocate_neighbour_edge_line(c_size, c_each)
!
      call set_neib_edge_line                                           &
     &   (c_size, c_each, loc_id, c_fil_edge, ione,                     &
     &    nb_rng%iedge_st, nb_rng%iedge_end, nb_rng%j_st, nb_rng%j_end, &
     &    nb_rng%k_st, nb_rng%k_end)
      call set_neib_edge_line                                           &
     &   (c_size, c_each, loc_id, c_fil_edge, itwo,                     &
     &    nb_rng%i_st, nb_rng%i_end, nb_rng%jedge_st, nb_rng%jedge_end, &
     &    nb_rng%k_st, nb_rng%k_end)
      call set_neib_edge_line                                           &
     &   (c_size, c_each, loc_id, c_fil_edge, ithree,                   &
     &    nb_rng%i_st, nb_rng%i_end, nb_rng%j_st, nb_rng%j_end,         &
     &    nb_rng%kedge_st, nb_rng%kedge_end)
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
       subroutine set_neib_edge_line                                    &
      &         (c_size, c_each, loc_id, c_fil_edge,                    &
      &          nd, ist, ied, jst, jed, kst, ked)
!
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(in) :: c_each
      type(local_node_id_cube), intent(in) :: loc_id
      type(filter_work_cubmesh), intent(in) :: c_fil_edge(3)
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
     &         + c_each%nx * (c_each%ny-1)  * c_each%nz
      end if
!
       do k=kst,ked
        do j=jst,jed
         do i=ist,ied
!
          do i1 = 1, c_size%ndep_1
           iedge0 = c_size%ndep_1*(iedge-1) + i1
           ii = c_fil_edge(nd)%inod_f_item_x(i1,i,j,k)
           jj = c_fil_edge(nd)%inod_f_item_y(i1,i,j,k)
           kk = c_fil_edge(nd)%inod_f_item_z(i1,i,j,k)
           iedge_f_d_x(iedge0) = c_fil_edge(nd)%inod_f_dist_x(i1,i,j,k)
           iedge_neib_x(iedge0) = loc_id%edge_id_lc(ii,j,k,nd)
           iedge_f_d_y(iedge0) = c_fil_edge(nd)%inod_f_dist_y(i1,i,j,k)
           iedge_neib_y(iedge0) = loc_id%edge_id_lc(i,jj,k,nd)
           iedge_f_d_z(iedge0) = c_fil_edge(nd)%inod_f_dist_z(i1,i,j,k)
           iedge_neib_z(iedge0) = loc_id%edge_id_lc(i,j,kk,nd)
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
      character(len=kchara) ::  fname
!
!
       fname = add_process_id(id_rank, filter_edge_header)
       open (id_file, file = fname)
!
       write(id_file,'(a12)') '! num_edge:  '
       write(id_file,'(i16)') c_each%edgetot
       write(id_file,'(i16)')                                           &
     &        c_each%edgetot, c_each%edgetot, c_each%edgetot
       write(id_file,'(a12)') '! num_depth: '
       write(id_file,'(2i16)') c_size%ndep_1
!
       write(id_file,'(a16)') '!  xi direction'
       do iedge = 1, c_each%edgetot
         ist = c_size%ndep_1*(iedge- 1) + 1
         ied = c_size%ndep_1*iedge
!
         write(id_file,'(10i16)') (iedge_f_d_x(i),  i = ist, ied)
         write(id_file,'(10i16)') (iedge_neib_x(i), i = ist, ied)
       end do
!
       write(id_file,'(a16)') '!  eta direction'
       do iedge = 1, c_each%edgetot
        ist = c_size%ndep_1*(iedge- 1) + 1
        ied = c_size%ndep_1*iedge
!
          write(id_file,'(10i16)') (iedge_f_d_y(i),  i = ist, ied)
          write(id_file,'(10i16)') (iedge_neib_y(i), i = ist, ied)
!
       end do
!
       write(id_file,'(a16)') '!   zi direction'
       do iedge = 1, c_each%edgetot
        ist = c_size%ndep_1*(iedge- 1) + 1
        ied = c_size%ndep_1*iedge
!
          write(id_file,'(10i16)') (iedge_f_d_z(i),  i = ist, ied)
          write(id_file,'(10i16)') (iedge_neib_z(i), i = ist, ied)
!
       end do
       close(id_file)
!
       end subroutine write_neib_edge_line
!
!  ----------------------------------------------------------------------
!
       end module neib_edge_line_cube
