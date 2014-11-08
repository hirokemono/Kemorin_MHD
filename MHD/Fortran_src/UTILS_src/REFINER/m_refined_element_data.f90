!m_refined_element_data.f90
!      module m_refined_element_data
!
!      Written by H. Matsui
!
!      subroutine allocate_refine_flags
!      subroutine allocate_refined_num_element
!      subroutine allocate_refined_ele_connect
!      subroutine allocate_old_refine_level
!
!      subroutine deallocate_refine_flags
!      subroutine deallocate_refined_ele_connect
!      subroutine deallocate_refined_num_element
!
!      subroutine check_refine_flags
!      subroutine check_refine_stack
!      subroutine check_local_refine_flags
!
      module m_refined_element_data
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint) :: max_refine_level
      integer(kind = kint), allocatable :: ilevel_refine(:)
      integer(kind = kint), allocatable :: ilevel_refine_old(:)
!
      integer(kind = kint), allocatable :: iflag_refine_ele(:)
      integer(kind = kint), allocatable :: iflag_refine_surf(:)
      integer(kind = kint), allocatable :: iflag_refine_edge(:)
!
!
      integer(kind = kint), allocatable :: iflag_refine_sf_lcl(:,:)
      integer(kind = kint), allocatable :: iflag_refine_ed_lcl(:,:)
!
!
      integer(kind = kint) :: ntot_ele_refined
      integer(kind = kint) :: nnod_4_ele_refined
      integer(kind = kint), allocatable :: num_ele_refined(:)
      integer(kind = kint), allocatable :: istack_ele_refined(:)
      integer(kind = kint), allocatable :: ie_refined(:,:)
!
      integer(kind = kint) :: iflag_tmp_tri_refine = 0
!
!
      integer(kind = kint) :: inod_refine_local(64)
!
      integer(kind = kint) :: inod_refine_nod_local(8)
      integer(kind = kint) :: inod_refine_ele_local(8)
      integer(kind = kint) :: inod_refine_surf_local(6,4)
      integer(kind = kint) :: inod_refine_edge_local(12,2)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_refine_flags
!
      use m_geometry_constants
      use m_geometry_parameter
!
      allocate( ilevel_refine(numele) )
      allocate( iflag_refine_ele(numele) )
!
      allocate( iflag_refine_surf(numsurf) )
      allocate( iflag_refine_edge(numedge) )
!
      allocate( iflag_refine_sf_lcl(nsurf_4_ele,numele) )
      allocate( iflag_refine_ed_lcl(nedge_4_ele,numele) )
!
      ilevel_refine =     0
      iflag_refine_ele =  0
!
      iflag_refine_surf = 0
      iflag_refine_edge = 0
!
      iflag_refine_sf_lcl = 0
      iflag_refine_ed_lcl = 0
!
      end subroutine allocate_refine_flags
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_refined_num_element
!
      use m_geometry_parameter
!
      allocate(num_ele_refined(numele))
      allocate(istack_ele_refined(0:numele))
      num_ele_refined = 0
      istack_ele_refined = -1
!
      end subroutine allocate_refined_num_element
!
! -----------------------------------------------------------------------
!
      subroutine allocate_refined_ele_connect
!
      allocate(ie_refined(ntot_ele_refined,nnod_4_ele_refined))
      ie_refined = 0
!
      end subroutine allocate_refined_ele_connect
!
! -----------------------------------------------------------------------
!
      subroutine allocate_old_refine_level
!
      use m_geometry_parameter
!
      max_refine_level = 0
      allocate( ilevel_refine_old(numele) )
      ilevel_refine_old = 0
!
      end subroutine allocate_old_refine_level
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_refine_flags
!
      deallocate( ilevel_refine )
      deallocate( iflag_refine_ele )
!
      deallocate( iflag_refine_surf )
      deallocate( iflag_refine_edge )
!
      deallocate( iflag_refine_sf_lcl )
      deallocate( iflag_refine_ed_lcl )
!
      end subroutine deallocate_refine_flags
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_refined_num_element
!
      use m_geometry_parameter
!
      deallocate(num_ele_refined)
      deallocate(istack_ele_refined)
!
      end subroutine deallocate_refined_num_element
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_refined_ele_connect
!
      deallocate(ie_refined)
!
      end subroutine deallocate_refined_ele_connect
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_old_refine_level
!
      deallocate( ilevel_refine_old )
!
      end subroutine deallocate_old_refine_level
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_refine_flags
!
      use m_geometry_parameter
!
      integer(kind = kint) :: i
!
      write(50,*) 'i, iflag_refine_ele(i)'
      do i = 1, numele
        write(50,*) i, iflag_refine_ele(i)
      end do
!
      write(50,*) 'i, iflag_refine_surf(i)'
      do i = 1, numsurf
        write(50,*) i, iflag_refine_surf(i)
      end do
!
      write(50,*) 'i, iflag_refine_edge(i)'
      do i= 1, numedge
        write(50,*) i, iflag_refine_edge(i)
      end do
!
      end subroutine check_refine_flags
!
!  ---------------------------------------------------------------------
!
      subroutine check_refine_stack
!
      use m_geometry_parameter
!
      integer(kind = kint) :: i
!
      write(50,*) 'i, istack_ele_refined(i)'
      do i = 1, numele
        write(50,*) i, istack_ele_refined(i), num_ele_refined(i)
      end do
!
      end subroutine check_refine_stack
!
!  ---------------------------------------------------------------------
!
      subroutine check_local_refine_flags
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
!
      integer(kind = kint) :: i, j
!
      write(50,*) 'i, iflag_refine_sf_lcl(i)'
      do i = 1, numele
        write(50,'(i15,6i5)') i, iflag_refine_sf_lcl(1:nsurf_4_ele,i)
      end do
!
      write(50,*) 'i, iflag_refine_ed_lcl(i)'
      do i = 1, numele
        write(50,'(i15,12i5)') i, iflag_refine_ed_lcl(1:nedge_4_ele,i)
      end do
!
      end subroutine check_local_refine_flags
!
!  ---------------------------------------------------------------------
!
      end module m_refined_element_data
