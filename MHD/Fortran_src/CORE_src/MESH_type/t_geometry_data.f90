!>@file   t_geometry_data.f90
!!@brief  module t_geometry_data
!!
!!@author  H. Matsui
!!@date Programmed in 2008
!
!>@brief structure of geometry data for FEM mesh
!!  including node and element position, connectivities
!!
!!@verbatim
!!      subroutine alloc_numnod_stack(num_pe, node)
!!      subroutine alloc_numele_stack(num_pe, ele)
!!      subroutine alloc_node_geometry_w_sph(node)
!!      subroutine alloc_node_geometry_base(node)
!!      subroutine alloc_sph_node_geometry(node)
!!        type(node_data), intent(inout) :: node
!!
!!      subroutine alloc_ele_connect(ele)
!!      subroutine alloc_element_types(ele)
!!      subroutine alloc_ele_connectivity(ele)
!!      subroutine alloc_overlapped_ele(ele)
!!      subroutine alloc_ele_geometry(ele)
!!      subroutine alloc_node_param_smp(node)
!!      subroutine alloc_ele_param_smp(ele)
!!        type(element_data), intent(inout) :: ele
!!
!!      subroutine dealloc_numnod_stack(node)
!!      subroutine dealloc_numele_stack(ele)
!!      subroutine dealloc_node_geometry_w_sph(node)
!!      subroutine dealloc_node_geometry_base(node)
!!      subroutine dealloc_sph_node_geometry(node)
!!        type(node_data), intent(inout) :: node
!!
!!      subroutine dealloc_ele_connect(ele)
!!      subroutine dealloc_overlapped_ele(ele)
!!      subroutine dealloc_ele_geometry(ele)
!!      subroutine dealloc_node_param_smp(node)
!!      subroutine dealloc_ele_param_smp(ele)
!!        type(element_data), intent(inout) :: ele
!!
!!      subroutine check_nod_size_smp_type(node, id_rank)
!!      subroutine check_ele_size_smp_type(ele, id_rank)
!!@endverbatim
!
      module t_geometry_data
!
      use m_precision
!
      implicit  none
!
!
!>  structure for node data (position)
      type node_data
!>        number of node on local PE (include external node)
        integer( kind=kint )  ::  numnod
!>        number of node on local PE
        integer( kind=kint )  ::  internal_node
!
!>        Stack list of number of node
        integer(kind=kint_gl), allocatable  :: istack_numnod(:)
!>        Stack list of number of internal node
        integer(kind=kint_gl), allocatable  :: istack_internod(:)
!
!>       end number of node for SMP on local PE
        integer( kind=kint ), allocatable :: istack_nod_smp(:)
!>       end number of internal node for SMP on local PE
        integer( kind=kint ), allocatable :: istack_internal_smp(:)
!>       maximum smp number of node on local PE
        integer( kind=kint )  ::  max_nod_smp
!>       maximum internal smp number of node on local PE
        integer( kind=kint )  ::  max_internal_nod_smp
!
!>       nodal coordinates (where i:x_1, x_2, x_3 , j:id)
        real(kind=kreal)  , allocatable  :: xx(:,:)
!
!>       global node    id (where i:node id)
        integer(kind=kint_gl), allocatable  ::  inod_global(:)
!
!>       distance from the centre
        real(kind=kreal)  , allocatable  :: rr(:)
!>       1/radius
        real(kind=kreal)  , allocatable  :: a_r(:)
!>       longitude of node
        real(kind=kreal)  , allocatable  :: phi(:)
!>       colatitude of node
        real(kind=kreal)  , allocatable  :: theta(:)
!>       cylindorical radius of node
        real(kind=kreal)  , allocatable  :: ss(:)
!>       1 / a_s_cylinder
        real(kind=kreal)  , allocatable  :: a_s(:)
!
!
!>       Minimum position at subdomain
        real(kind = kreal) :: xyz_min_lc(3)
!>       Maximum position at subdomain
        real(kind = kreal) :: xyz_max_lc(3)
!>       Minimum position at whole domain
        real(kind = kreal) :: xyz_min_gl(3)
!>       Minimum position at whole domain
        real(kind = kreal) :: xyz_max_gl(3)
      end type node_data
!
!
!>  structure for element data (position and connectivity)
      type element_data
!>       number of element on local PE
        integer(kind=kint)  ::  numele
!>       number of internal element on local PE
        integer(kind=kint)  ::  internal_ele
!>       number of nodes in each element
        integer(kind=kint) :: nnod_4_ele
!
!>        Stack list of number of element
        integer(kind=kint_gl), allocatable  :: istack_numele(:)
!>        Stack list of number of internal element
        integer(kind=kint_gl), allocatable  :: istack_interele(:)
!
!>       end number of element for SMP on local PE
        integer( kind=kint ), allocatable :: istack_ele_smp(:)
!>       maximum smp number of element on local PE
        integer( kind=kint )  ::  max_ele_smp
!>       maximum internal smp number of element on local PE
        integer( kind=kint )  ::  max_internal_ele_smp
!
!>       element connectivity  (where i:element id j:node id)
        integer(kind=kint), allocatable  :: ie(:,:)
!
!>       element type id   (where i:element id)
        integer(kind=kint), allocatable  ::  elmtyp(:)
!>       element type id   (where i:element id)
        integer(kind=kint), allocatable  ::  nodelm(:)
!>       global element id (where i:element id)
        integer(kind=kint_gl), allocatable  ::  iele_global(:)
!>        element type defined by the first element
        integer(kind=kint) ::  first_ele_type
!
!>       flag for interior element
        integer(kind = kint), allocatable :: interior_ele(:)
!
!>       position of centre of element
        real(kind=kreal)  , allocatable :: x_ele(:,:)
!>       distance from the centre of element
        real(kind=kreal)  , allocatable :: r_ele(:)
!>       1/r_ele
        real(kind=kreal)  , allocatable :: ar_ele(:)
!>       longitude of element
        real(kind=kreal)  , allocatable :: phi_ele(:)
!>       colatitude of element
        real(kind=kreal)  , allocatable :: theta_ele(:)
!>       cylindorical radius of element
        real(kind=kreal)  , allocatable :: s_ele(:)
!>       1 / s_ele
        real(kind=kreal)  , allocatable :: as_ele(:)
!
!>       volume of each element
        real (kind=kreal), allocatable :: volume_ele(:)
!>       1 / (volume of each element)
        real (kind=kreal), allocatable :: a_vol_ele(:)
!
!
!>      Volume of domain
        real(kind=kreal) :: volume
!>      1 / (Volume)
        real(kind=kreal) :: a_vol
      end type element_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_numnod_stack(num_pe, node)
!
      integer, intent(in) :: num_pe
      type(node_data), intent(inout) :: node
!
!
      allocate(node%istack_numnod(0:num_pe))
      allocate(node%istack_internod(0:num_pe))
      node%istack_numnod =   0
      node%istack_internod = 0
!
      end subroutine alloc_numnod_stack
!
! ------------------------------------------------------
!
      subroutine alloc_numele_stack(num_pe, ele)
!
      integer, intent(in) :: num_pe
      type(element_data), intent(inout) :: ele
!
!
      allocate(ele%istack_numele(0:num_pe))
      allocate(ele%istack_interele(0:num_pe))
      ele%istack_numele =   0
      ele%istack_interele = 0
!
      end subroutine alloc_numele_stack
!
! ------------------------------------------------------
!
      subroutine alloc_node_geometry_w_sph(node)
!
      type(node_data), intent(inout) :: node
!
      call alloc_node_geometry_base(node)
      call alloc_sph_node_geometry(node)
!
      end subroutine alloc_node_geometry_w_sph
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_node_geometry_base(node)
!
      type(node_data), intent(inout) :: node
!
      allocate(node%inod_global(node%numnod))
      allocate(node%xx(node%numnod,3))
!
      if (node%numnod .gt. 0) then
        node%inod_global = 0
        node%xx = 0.0d00
      end if
!
      end subroutine alloc_node_geometry_base
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_sph_node_geometry(node)
!
      type(node_data), intent(inout) :: node
!
      allocate(node%rr(node%numnod))
      allocate(node%a_r(node%numnod))
      allocate(node%ss(node%numnod))
      allocate(node%a_s(node%numnod))
      allocate(node%phi(node%numnod))
      allocate(node%theta(node%numnod))
!
      if (node%numnod .gt. 0) then
        node%rr = 0.0d00
        node%a_r = 0.0d00
        node%ss = 0.0d00
        node%a_s = 0.0d00
        node%phi = 0.0d00
        node%theta = 0.0d00
      end if
!
      end subroutine alloc_sph_node_geometry
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_ele_connect(ele)
!
      type(element_data), intent(inout) :: ele
!
      call alloc_element_types(ele)
      call alloc_ele_connectivity(ele)
!
      end subroutine alloc_ele_connect
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_element_types(ele)
!
      type(element_data), intent(inout) :: ele
!
      allocate(ele%iele_global(ele%numele))
      allocate(ele%elmtyp(ele%numele))
      allocate(ele%nodelm(ele%numele))
!
      if (ele%numele .gt. 0) then
        ele%iele_global = 0
        ele%elmtyp =      0
        ele%nodelm =      0
      end if
!
      end subroutine alloc_element_types
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_ele_connectivity(ele)
!
      type(element_data), intent(inout) :: ele
!
      allocate(ele%ie(ele%numele,ele%nnod_4_ele))
!
      if (ele%numele .gt. 0) ele%ie = 0
!
      end subroutine alloc_ele_connectivity
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_overlapped_ele(ele)
!
      type(element_data), intent(inout) :: ele
!
!
      allocate(ele%interior_ele(ele%numele) )
      if(ele%numele .gt. 0) ele%interior_ele = 1
!
      end subroutine alloc_overlapped_ele
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_ele_geometry(ele)
!
      type(element_data), intent(inout) :: ele
!
!
      allocate(ele%x_ele(ele%numele,3))
      allocate(ele%r_ele(ele%numele))
      allocate(ele%ar_ele(ele%numele))
      allocate(ele%phi_ele(ele%numele))
      allocate(ele%theta_ele(ele%numele))
      allocate(ele%s_ele(ele%numele))
      allocate(ele%as_ele(ele%numele))
!
      allocate(ele%volume_ele(ele%numele))
      allocate(ele%a_vol_ele(ele%numele))
!
      if(ele%numele .gt. 0) then
        ele%x_ele = 0.0d0
!
        ele%r_ele = 0.0d0
        ele%ar_ele = 0.0d0
        ele%phi_ele = 0.0d0
        ele%theta_ele = 0.0d0
        ele%s_ele = 0.0d0
        ele%as_ele = 0.0d0
!
        ele%volume_ele = 0.0d0
        ele%a_vol_ele = 0.0d0
      end if
!
      end subroutine alloc_ele_geometry
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_node_param_smp(node)
!
      use m_machine_parameter
!
      type(node_data), intent(inout) :: node
!
      allocate( node%istack_nod_smp(0:np_smp))
      allocate( node%istack_internal_smp(0:np_smp))
!
      node%istack_nod_smp =      0
      node%istack_internal_smp = 0
!
      end subroutine alloc_node_param_smp
!
!-----------------------------------------------------------------------
!
      subroutine alloc_ele_param_smp(ele)
!
      use m_machine_parameter
!
      type(element_data), intent(inout) :: ele
!
      allocate( ele%istack_ele_smp(0:np_smp))
      ele%istack_ele_smp = 0
!
      end subroutine alloc_ele_param_smp
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_numnod_stack(node)
!
      type(node_data), intent(inout) :: node
!
!
      if(allocated(node%istack_numnod) .eqv. .FALSE.) return
      deallocate(node%istack_numnod, node%istack_internod)
!
      end subroutine dealloc_numnod_stack
!
! ------------------------------------------------------
!
      subroutine dealloc_numele_stack(ele)
!
      type(element_data), intent(inout) :: ele
!
!
      if(allocated(ele%istack_numele) .eqv. .FALSE.) return
      deallocate(ele%istack_numele, ele%istack_interele)
!
      end subroutine dealloc_numele_stack
!
! ------------------------------------------------------
!
      subroutine dealloc_node_geometry_w_sph(node)
!
      type(node_data), intent(inout) :: node
!
      call dealloc_sph_node_geometry(node)
      call dealloc_node_geometry_base(node)
!
      end subroutine dealloc_node_geometry_w_sph
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_node_geometry_base(node)
!
      type(node_data), intent(inout) :: node
!
      if(allocated(node%inod_global) .eqv. .FALSE.) return
      deallocate(node%inod_global, node%xx)
!
      end subroutine dealloc_node_geometry_base
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_sph_node_geometry(node)
!
      type(node_data), intent(inout) :: node
!
      if(allocated(node%rr) .eqv. .FALSE.) return
      deallocate(node%rr, node%a_r, node%ss)
      deallocate(node%a_s, node%phi, node%theta)
!
      end subroutine dealloc_sph_node_geometry
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_ele_connect(ele)
!
      type(element_data), intent(inout) :: ele
!
      if(allocated(ele%iele_global) .eqv. .FALSE.) return
      deallocate(ele%iele_global)
      deallocate(ele%elmtyp, ele%nodelm)
      deallocate(ele%ie)
!
      end subroutine dealloc_ele_connect
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_overlapped_ele(ele)
!
      type(element_data), intent(inout) :: ele
!
!
      if(allocated(ele%interior_ele) .eqv. .FALSE.) return
      deallocate(ele%interior_ele)
!
      end subroutine dealloc_overlapped_ele
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_ele_geometry(ele)
!
      type(element_data), intent(inout) :: ele
!
!
      if(allocated(ele%x_ele) .eqv. .FALSE.) return
      deallocate(ele%x_ele)
      deallocate(ele%r_ele, ele%ar_ele)
      deallocate(ele%phi_ele, ele%theta_ele)
      deallocate(ele%s_ele, ele%as_ele)
!
      deallocate(ele%volume_ele)
      deallocate(ele%a_vol_ele)
!
      end subroutine dealloc_ele_geometry
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_node_param_smp(node)
!
      type(node_data), intent(inout) :: node
!
      if(allocated(node%istack_nod_smp) .eqv. .FALSE.) return
      deallocate(node%istack_nod_smp)
      deallocate(node%istack_internal_smp)
!
      end subroutine dealloc_node_param_smp
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_ele_param_smp(ele)
!
      type(element_data), intent(inout) :: ele
!
      if(allocated(ele%istack_ele_smp) .eqv. .FALSE.) return
      deallocate(ele%istack_ele_smp)
!
      end subroutine dealloc_ele_param_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_nod_size_smp_type(node, id_rank)
!
      use m_machine_parameter
!
      integer, intent(in) :: id_rank
      type(node_data), intent(in) :: node
!
       write(*,*) 'np_smp: ', np_smp
       write(*,*) 'PE: ', id_rank,                                      &
     &           'istack_nod_smp ', node%istack_nod_smp
       write(*,*) 'PE: ', id_rank,                                      &
     &           'istack_internal_smp ', node%istack_internal_smp
!
      end subroutine check_nod_size_smp_type
!
!-----------------------------------------------------------------------
!
      subroutine check_ele_size_smp_type(ele, id_rank)
!
      use m_machine_parameter
!
      integer, intent(in) :: id_rank
      type(element_data), intent(in) :: ele
!
       write(*,*) 'np_smp: ', np_smp
       write(*,*) 'PE: ', id_rank,                                      &
     &           'istack_ele_smp ', ele%istack_ele_smp
!
      end subroutine check_ele_size_smp_type
!
!-----------------------------------------------------------------------
!
      end module t_geometry_data
