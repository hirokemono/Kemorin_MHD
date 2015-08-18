!>@file   m_geometry_data.f90
!!@brief  module m_geometry_data
!!
!!@author H. Okuda and H. Matsui
!!@date Programmed in 2001
!!@date Modified in 2008
!
!> @brief geometry data for FEM mesh
!!   including node and element position, all connectivities
!!
!!@verbatim
!!      subroutine allocate_numnod_stack(nprocs)
!!      subroutine allocate_numele_stack(nprocs)
!!      subroutine allocate_numsurf_stack(nprocs)
!!      subroutine allocate_numedge_stack(nprocs)
!!
!!      subroutine deallocate_numnod_stack
!!      subroutine deallocate_numele_stack
!!      subroutine deallocate_numsurf_stack
!!      subroutine deallocate_numedge_stack
!!
!!      subroutine allocate_element_geometry
!!@endverbatim
!!
!>
!>@n@image html 1ele_node.png Node connectivity \p ie(iele,j)
!>                            for each element
!>@n@image html 1ele_edge.png Edge connectivity \p isurf_4_ele(iele,j)
!>                            for each element
!>@n@image html 1ele_surf.png Surface connectivity \p iedge_4_ele(iele,j)
!>                            for each element
!>@n@image html 1quad_ele_img.png Node connectivity \p ie(iele,j)
!>                            for quadrature element
!
!
      module   m_geometry_data
!
      use m_precision
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      implicit  none
!
!>  structure for node data (position)
      type(node_data), save :: node1
!    node1%a_s
!
!>  structure for element data (position and connectivity)
      type(element_data), save :: ele1
!    ele1%s_ele
!
!>      structure of surface data (geometry and connectivity)
      type(surface_data), save :: surf1
!  surf1%isf_isolate
!
!>     Structure for edge data
      type(edge_data), save :: edge1
!  edge1%isurf_4_edge
!
!>   Stack list of number of node
      integer(kind=kint_gl), allocatable, target  :: istack_numnod(:)
!>   Stack list of number of internal node
      integer(kind=kint_gl), allocatable, target  :: istack_internod(:)
!
!>   Stack list of number of element
      integer(kind=kint_gl), allocatable, target  :: istack_numele(:)
!>   Stack list of number of internal element
      integer(kind=kint_gl), allocatable, target  :: istack_interele(:)
!
!>   Stack list of number of surface
      integer(kind=kint_gl), allocatable, target  :: istack_numsurf(:)
!>   Stack list of number of internal surface
      integer(kind=kint_gl), allocatable, target  :: istack_intersurf(:)
!>   Stack list of number of edge
      integer(kind=kint_gl), allocatable, target  :: istack_numedge(:)
!>   Stack list of number of internal edge
      integer(kind=kint_gl), allocatable, target  :: istack_interedge(:)
!
!
!>   position of centre of element
!      real(kind=kreal)  , pointer :: x_ele(:,:)
!>   distance from the centre of element
!      real(kind=kreal)  , pointer :: r_ele(:)
!>   1/r_ele
      real(kind=kreal)  , pointer :: ar_ele(:)
!>   longitude of element
!      real(kind=kreal)  , pointer :: phi_ele(:)
!>  colatitude of element
!      real(kind=kreal)  , pointer :: theta_ele(:)
!>  cylindorical radius of element
      real(kind=kreal)  , pointer :: s_ele(:)
!>  1 / s_ele
      real(kind=kreal)  , pointer :: as_ele(:)
!
!>  Volume of each element
      real (kind=kreal), pointer :: volume_ele(:)
!>  1 / volume of each element
      real (kind=kreal), pointer :: a_vol_ele(:)
!
!
!>  Total volume of domain
      real(kind=kreal) :: volume
!>  1 / volume of domain
      real(kind=kreal) :: a_vol
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_numnod_stack(nprocs)
!
      integer(kind = kint), intent(in) :: nprocs
!
!
      allocate(istack_numnod(0:nprocs))
      allocate(istack_internod(0:nprocs))
      istack_numnod =   0
      istack_internod = 0
!
      end subroutine allocate_numnod_stack
!
! ------------------------------------------------------
!
      subroutine allocate_numele_stack(nprocs)
!
      integer(kind = kint), intent(in) :: nprocs
!
!
      allocate(istack_numele(0:nprocs))
      allocate(istack_interele(0:nprocs))
      istack_numele =   0
      istack_interele = 0
!
      end subroutine allocate_numele_stack
!
! ------------------------------------------------------
!
      subroutine allocate_numsurf_stack(nprocs)
!
      integer(kind = kint), intent(in) :: nprocs
!
!
      allocate(istack_numsurf(0:nprocs))
      allocate(istack_intersurf(0:nprocs))
      istack_numsurf =   0
      istack_intersurf = 0
!
      end subroutine allocate_numsurf_stack
!
! ------------------------------------------------------
!
      subroutine allocate_numedge_stack(nprocs)
!
      integer(kind = kint), intent(in) :: nprocs
!
!
      allocate(istack_numedge(0:nprocs))
      allocate(istack_interedge(0:nprocs))
      istack_numedge =   0
      istack_interedge = 0
!
      end subroutine allocate_numedge_stack
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine deallocate_numnod_stack
!
!
      deallocate(istack_numnod, istack_internod)
!
      end subroutine deallocate_numnod_stack
!
! ------------------------------------------------------
!
      subroutine deallocate_numele_stack
!
!
      deallocate(istack_numele, istack_interele)
!
      end subroutine deallocate_numele_stack
!
! ------------------------------------------------------
!
      subroutine deallocate_numsurf_stack
!
!
      deallocate(istack_numsurf, istack_intersurf)
!
      end subroutine deallocate_numsurf_stack
!
! ------------------------------------------------------
!
      subroutine deallocate_numedge_stack
!
!
      deallocate(istack_numedge, istack_interedge)
!
      end subroutine deallocate_numedge_stack
!
! ------------------------------------------------------
! ------------------------------------------------------
!
       subroutine allocate_element_geometry
!
!
      call allocate_ele_geometry_type(ele1)
!
!        allocate(x_ele(ele1%numele,3))
!        allocate(r_ele(ele1%numele))
        allocate(ar_ele(ele1%numele))
!        allocate(phi_ele(ele1%numele))
!        allocate(theta_ele(ele1%numele))
        allocate(s_ele(ele1%numele))
        allocate(as_ele(ele1%numele))
!
        allocate( volume_ele (ele1%numele))
        allocate( a_vol_ele (ele1%numele))
!
       ar_ele = 0.0d0
       s_ele = 0.0d0
       as_ele = 0.0d0
!
       volume_ele = 0.0d0
       a_vol_ele = 0.0d0
!
       end subroutine allocate_element_geometry
!
! ------------------------------------------------------
!
       subroutine deallocate_element_geometry
!
!
      call deallocate_ele_geometry_type(ele1)
!
        deallocate(ar_ele)
        deallocate(s_ele)
        deallocate(as_ele)
!
        deallocate( volume_ele )
        deallocate( a_vol_ele )
!
       end subroutine deallocate_element_geometry
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      end module m_geometry_data
