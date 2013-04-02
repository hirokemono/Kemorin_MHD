!m_cutshell_nod_ele_flag.f90
!      module m_cutshell_nod_ele_flag
!
      module m_cutshell_nod_ele_flag
!
!     Written by H. Matsui
!
      use m_precision
!
      implicit none
!
      integer (kind=kint), allocatable  :: mark_new_node(:)
      integer (kind=kint), allocatable  :: mark_new_ele(:)
!
!      subroutine allocate_trans_table
!      subroutine deallocate_trans_table
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_trans_table
!
      use m_geometry_parameter
!
      allocate( mark_new_node(numnod) )
      allocate( mark_new_ele(numele)  )
!
      mark_new_node = 0
      mark_new_ele =  0
!
      end subroutine allocate_trans_table
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_trans_table
!
      deallocate( mark_new_node )
      deallocate( mark_new_ele  )
!
      end subroutine deallocate_trans_table
!
!  ---------------------------------------------------------------------
!
      subroutine check_trans_table
!
      use m_geometry_parameter
      integer(kind = kint) :: i
!
      write(50,*) 'mark_new_node'
      do i = 1, numnod
        write(50,*) i, mark_new_node(i)
      end do
      write(50,*) 'mark_new_ele'
      do i = 1, numele
        write(50,*) i, mark_new_ele(i)
      end do
!
      end subroutine check_trans_table
!
!  ---------------------------------------------------------------------
!
      end module m_cutshell_nod_ele_flag
