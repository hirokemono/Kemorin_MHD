!set_comm_nod_4_cube.f90
!     module set_comm_nod_4_cube
!
      module set_comm_nod_4_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
      use m_precision
!
      use m_size_of_cube
      use m_comm_data_cube_kemo
      use m_sleeve_cube
!
      implicit none
!
!      subroutine count_node_id(inod)
!      subroutine set_im_node(inod)
!      subroutine set_ex_node(inod)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine count_node_id(inod)
!
      integer (kind = kint), intent(inout)  :: inod
!
!
      inod = inod + (ie-is+1)*(je-js+1)*(ke-ks+1)
!
      end subroutine count_node_id
!
! ----------------------------------------------------------------------
!
      subroutine set_im_node(inod)
!
      use m_local_node_id_cube
!
      integer (kind = kint), intent(inout) :: inod
!
      integer (kind = kint) :: i, j, k
!
!
      do k=ks,ke
        do j=js,je
          do i=is,ie

            inod = inod + 1
            item_import(inod) =  node_id_lc(i,j,k)

          enddo
        enddo
      enddo
!
      end subroutine set_im_node
!
! ----------------------------------------------------------------------
!
      subroutine set_ex_node(inod)
!
      use m_local_node_id_cube
!
      integer (kind = kint), intent(inout) :: inod
!
      integer (kind = kint) :: i, j, k
!
!
      do k=ks,ke
        do j=js,je
          do i=is,ie

            inod = inod + 1
            item_export(inod) =  node_id_lc(i,j,k)

          enddo
        enddo
      enddo
!
      end subroutine set_ex_node
!!
! ----------------------------------------------------------------------
!
      end module set_comm_nod_4_cube
