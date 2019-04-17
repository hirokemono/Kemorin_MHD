!
!     module t_local_node_id_cube
!
      module t_local_node_id_cube
!
      use m_precision
      use t_size_of_cube
!
      implicit none
!
!
      type local_node_id_cube
!>        n_x:      array size for x direction
        integer(kind = kint)  ::  n_x
!>        n_y:      array size for y direction
        integer(kind = kint)  ::  n_y 
!>        n_z:      array size for z direction
        integer(kind = kint)  ::  n_z 
!
        integer(kind = kint), allocatable  :: node_id_lc(:,:,:)
        integer(kind = kint), allocatable :: edge_id_lc(:,:,:,:)
!
        integer(kind = kint), allocatable  :: inod_table(:,:)
      end type local_node_id_cube
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_node_informations(c_size, loc_id)
!
      use t_size_of_cube
!
      type(size_of_cube), intent(in) :: c_size
      type(local_node_id_cube), intent(inout) :: loc_id
!
! ***** allocate nodal id table
!
      loc_id%n_x  = c_size%numnod_x
      loc_id%n_y  = c_size%numnod_y
      loc_id%n_z  = c_size%numnod_z
!
      allocate(loc_id%node_id_lc(loc_id%n_x,loc_id%n_y,loc_id%n_z) )
      allocate(loc_id%inod_table(c_size%nnod_cubmesh,4) )
!
      call reset_node_info(loc_id)
!
      end subroutine alloc_node_informations
!
! ----------------------------------------------------------------------
!
       subroutine alloc_edge_informations(loc_id)
!
      type(local_node_id_cube), intent(inout) :: loc_id
!
! ***** allocate nodal id table
!
      allocate(loc_id%edge_id_lc(loc_id%n_x,loc_id%n_y,loc_id%n_z,3))
!
      call reset_edge_info(loc_id)
!
      end subroutine alloc_edge_informations
!
! ---------------------------------------------------------------------
!
      subroutine reset_node_info(loc_id)
!
      type(local_node_id_cube), intent(inout) :: loc_id
!
      loc_id%node_id_lc = 0
      loc_id%inod_table = 0
!
      end subroutine reset_node_info
!
! ----------------------------------------------------------------------
!
      subroutine reset_edge_info(loc_id)
!
      type(local_node_id_cube), intent(inout) :: loc_id
!
      loc_id%edge_id_lc = 0
!
      end subroutine reset_edge_info
!
! ---------------------------------------------------------------------
!
      subroutine set_inod_table(c_each, loc_id)
!
      type(size_of_each_cube), intent(in) :: c_each
      type(local_node_id_cube), intent(inout) :: loc_id
!
      integer (kind = kint) :: i, j, k, inod
!
!
      loc_id%inod_table = 0
!
      do k = 1, loc_id%n_z
        do j = 1, loc_id%n_y
          do i = 1, loc_id%n_x
            inod = loc_id%node_id_lc(i,j,k)
            if (inod .gt. c_each%nodtot) then
              write(*,*) 'node id is larger than number of node!',      &
     &              i, j, k, inod, c_each%nodtot
              stop
            else if(inod.gt.0) then
              loc_id%inod_table(inod,1) = i
              loc_id%inod_table(inod,2) = j
              loc_id%inod_table(inod,3) = k
            end if
          end do
        end do
      end do
!
      end subroutine set_inod_table
!
! ---------------------------------------------------------------------
!
      subroutine set_iedge_table(c_each, loc_id)
!
      type(size_of_each_cube), intent(in) :: c_each
      type(local_node_id_cube), intent(inout) :: loc_id
!
      integer (kind = kint) :: i, j, k, nd, iedge
!
!
      do nd = 1, 3
        do k = 1, loc_id%n_z
          do j = 1, loc_id%n_y
            do i = 1, loc_id%n_x
              iedge = loc_id%edge_id_lc(i,j,k,nd)
              if(iedge .gt. c_each%nodtot) then
                write(*,*) 'edge id is larger than number of node!'
                stop
              else if(iedge.gt.0) then
                loc_id%inod_table(iedge,1) = i
                loc_id%inod_table(iedge,2) = j
                loc_id%inod_table(iedge,3) = k
                loc_id%inod_table(iedge,4) = nd
              end if
            end do
          end do
        end do
      end do
!
      end subroutine set_iedge_table
!
! ---------------------------------------------------------------------
!
      subroutine check_inod_table(c_each, loc_id)
!
      type(size_of_each_cube), intent(in) :: c_each
      type(local_node_id_cube), intent(inout) :: loc_id
!
      integer (kind = kint) :: inod
!
!
      do inod = 1, c_each%nodtot
        if(loc_id%inod_table(inod,1).lt.1                               &
     &     .or. loc_id%inod_table(inod,1) .gt. loc_id%n_x) then
         write(*,*) 'x direction of node table is wrong!',              &
     &             loc_id%inod_table(inod,1)
         stop
        end if
        if(loc_id%inod_table(inod,2).lt.1                               &
     &     .or. loc_id%inod_table(inod,2) .gt. loc_id%n_y) then
         write(*,*) 'y direction of node table is wrong!',              &
     &             loc_id%inod_table(inod,2)
         stop
        end if
        if(loc_id%inod_table(inod,3).lt.1                               &
     &     .or. loc_id%inod_table(inod,3) .gt. loc_id%n_z) then
         write(*,*) 'z direction of node table is wrong!',              &
     &             loc_id%inod_table(inod,3)
         stop
        end if
      end do
!
      end subroutine check_inod_table
!
! ---------------------------------------------------------------------
!
!
      end module t_local_node_id_cube
