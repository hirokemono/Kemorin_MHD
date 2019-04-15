!
!     module m_local_node_id_cube
!
      module m_local_node_id_cube
!
      use m_precision
!
      implicit none
!

!>        numnod_x:      array size for x direction
        integer(kind=kint )  ::  numnod_x
!>        numnod_y:      array size for y direction
        integer(kind=kint )  ::  numnod_y 
!>        numnod_z:      array size for z direction
        integer(kind=kint )  ::  numnod_z 
!
        integer(kind=kint ), allocatable  :: node_id_lc(:,:,:)
        integer(kind=kint ), allocatable :: edge_id_lc(:,:,:,:)
!
        integer(kind=kint ), allocatable  :: inod_table(:,:)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
       subroutine allocate_node_informations(c_size)
!
       use t_size_of_cube
!
      type(size_of_cube), intent(inout) :: c_size
!
! ***** allocate nodal id table
!
      numnod_x  = c_size%numnod_x
      numnod_y  = c_size%numnod_y
      numnod_z  = c_size%numnod_z
!
      allocate ( node_id_lc(numnod_x,numnod_y,numnod_z) )
      allocate ( inod_table(c_size%nnod_cubmesh,4) )
!
      call reset_node_info
!
      end subroutine allocate_node_informations
!
! ----------------------------------------------------------------------
!
       subroutine allocate_edge_informations
!
       use m_size_of_cube
!
       implicit none
!
! ***** allocate nodal id table
!
      allocate ( edge_id_lc(numnod_x,numnod_y,numnod_z,3) )
!
      call reset_edge_info
!
      end subroutine allocate_edge_informations
!
! ---------------------------------------------------------------------
!
      subroutine reset_node_info
!
      node_id_lc = 0
      inod_table = 0
!
      end subroutine reset_node_info
!
! ----------------------------------------------------------------------
!
      subroutine reset_edge_info
!
      edge_id_lc = 0
!
      end subroutine reset_edge_info
!
! ---------------------------------------------------------------------
!
      subroutine set_inod_table
!
       use m_size_of_cube
!
      integer (kind = kint) :: i, j, k, inod
!
!
      inod_table = 0
!
      do k = 1, numnod_z
       do j = 1, numnod_y
        do i = 1, numnod_x
         inod = node_id_lc(i,j,k)
         if (inod.gt.nodtot ) then
          write(*,*) 'node id is larger than number of node!',          &
     &              i, j, k, inod, nodtot
          stop
         else if(inod.gt.0) then
          inod_table(inod,1) = i
          inod_table(inod,2) = j
          inod_table(inod,3) = k
         end if
        end do
       end do
      end do
!
      end subroutine set_inod_table
!
! ---------------------------------------------------------------------
!
      subroutine set_iedge_table
!
       use m_size_of_cube
!
      integer (kind = kint) :: i, j, k, nd, iedge
!
!
      do nd = 1, 3
       do k = 1, numnod_z
        do j = 1, numnod_y
         do i = 1, numnod_x
          iedge = edge_id_lc(i,j,k,nd)
          if (iedge.gt.nodtot ) then
           write(*,*) 'edge id is larger than number of node!'
           stop
          else if(iedge.gt.0) then
           inod_table(iedge,1) = i
           inod_table(iedge,2) = j
           inod_table(iedge,3) = k
           inod_table(iedge,4) = nd
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
      subroutine check_inod_table
!
       use m_size_of_cube
!
      integer (kind = kint) :: inod
!
!
      do inod = 1, nodtot
        if ( inod_table(inod,1).lt.1                                    &
     &     .or. inod_table(inod,1).gt.numnod_x ) then
         write(*,*) 'x direction of node table is wrong!',              &
     &             inod_table(inod,1)
         stop
        end if
        if ( inod_table(inod,2).lt.1                                    &
     &     .or. inod_table(inod,2).gt.numnod_y ) then
         write(*,*) 'y direction of node table is wrong!',              &
     &             inod_table(inod,2)
         stop
        end if
        if ( inod_table(inod,3).lt.1                                    &
     &     .or. inod_table(inod,3).gt.numnod_z ) then
         write(*,*) 'z direction of node table is wrong!',              &
     &             inod_table(inod,3)
         stop
        end if
      end do
!
      end subroutine check_inod_table
!
! ---------------------------------------------------------------------
!
!
      end module m_local_node_id_cube
