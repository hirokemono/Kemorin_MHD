!m_geometry_data_MHD.f90
!     module m_geometry_data_MHD
!
!     written by H. Matsui on May, 2009
!
!      subroutine allocate_inner_core_ele_list
!      subroutine allocate_element_connect_org(numele, nnod_4_ele)
!
!      subroutine allocate_geometry_fluid_smp
!      subroutine allocate_geometry_conduct_smp
!      subroutine allocate_geometry_ins_smp
!      subroutine allocate_geometry_ins_smp
!
!      subroutine deallocate_fluid_node_list
!      subroutine deallocate_conduct_node_list
!      subroutine deallocate_inner_core_ele_list
!      subroutine deallocate_element_connect_org
!
      module m_geometry_data_MHD
!
      use m_precision
      use t_geometry_data_MHD
!
      implicit  none
!
!   for fluid layer
!
!       Mesh information for fluid segments
      type(field_geometry_data), save :: fluid1
!fluid1%istack_ele_fld_smp
!
      integer( kind=kint ), allocatable :: inod_fl_smp_stack(:)
!     smp stack of node on local PE
      integer( kind=kint ), allocatable :: inter_fl_smp_stack(:)
!     smp stack of internal node on local PE
!
      integer( kind=kint )  ::  maxnod_fl_smp
!     smp stack of node on local PE
      integer( kind=kint )  ::  max_in_nod_fl_smp
!     smp stack of node on local PE
      integer( kind=kint )  ::  maxele_fl_smp
!     smp stack of node on local PE
!
!   for conductive layer
!
!       Mesh information for conductor segments
      type(field_geometry_data), save :: conduct1
!conduct1%istack_ele_fld_smp
!
!      integer( kind=kint ), allocatable :: iele_cd_smp_stack(:)
!     smp stack of element on local PE
      integer( kind=kint ), allocatable :: inod_cd_smp_stack(:)
!     smp stack of node on local PE
      integer( kind=kint ), allocatable :: inter_cd_smp_stack(:)
!     smp stack of internal node on local PE
!
      integer( kind=kint )  ::  maxnod_cd_smp
!     smp stack of node on local PE
      integer( kind=kint )  ::  max_in_nod_cd_smp
!     smp stack of node on local PE
      integer( kind=kint )  ::  maxele_cd_smp
!     smp stack of node on local PE
!
!   for insulate layer
!
!       Mesh information for insulator segments
      type(field_geometry_data), save :: insulate1
!insulate1%istack_ele_fld_smp
!
      integer( kind=kint ), allocatable :: inod_ins_smp_stack(:)
!     number of node on local PE
      integer( kind=kint ), allocatable :: inter_ins_smp_stack(:)
!     smp stack of internal node on local PE
!
      integer( kind=kint )  ::  maxnod_ins_smp
!     smp stack of node on local PE
      integer( kind=kint )  ::  max_in_nod_ins_smp
!     smp stack of node on local PE
      integer( kind=kint )  ::  maxele_ins_smp
!     smp stack of node on local PE
!
!
!   for insulated core
!
!       Mesh information for inner core segments
      type(field_geometry_data), save :: inner_core
!inner_core%istack_ele_fld_smp
!
      integer( kind=kint ), allocatable :: inod_in_core_smp_stack(:)
!     smp stack of node on local PE
      integer( kind=kint ), allocatable :: inter_in_core_smp_stack(:)
!     smp stack of internal node on local PE
!
      integer( kind=kint )  ::  maxnod_in_core_smp
!     smp stack of node on local PE
      integer( kind=kint )  ::  max_in_nod_in_core_smp
!     smp stack of node on local PE
      integer( kind=kint )  ::  maxele_in_core_smp
!     smp stack of node on local PE
!
      integer( kind=kint )  ::  numele_in_core
!     number of element on local PE
!
      integer(kind=kint), allocatable :: iele_in_core(:)
!
!   original connectivity table
!
      integer(kind=kint), allocatable, target  :: ie_org(:,:)
!   element connectivity  (where i:nodal order j:element id)
      integer(kind=kint_gl), allocatable, target :: iele_global_org(:)
!   global element id (where i:element id)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
       subroutine allocate_inner_core_ele_list
!
!
       allocate(iele_in_core(numele_in_core))
       if(numele_in_core.gt.0) iele_in_core =   0
!
       end subroutine allocate_inner_core_ele_list
!
! ----------------------------------------------------------------------
!
      subroutine allocate_element_connect_org(numele, nnod_4_ele)
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
!
!
      allocate(iele_global_org(numele))
      allocate(ie_org(numele,nnod_4_ele))
      iele_global_org = 0
      ie_org =          0
!
      end subroutine allocate_element_connect_org
!
!------------------------------------------------------------------
!
       subroutine allocate_geometry_fluid_smp
!
       use m_machine_parameter
!
       allocate( fluid1%istack_ele_fld_smp(0:np_smp))
       allocate( inod_fl_smp_stack(0:np_smp))
       allocate( inter_fl_smp_stack(0:np_smp))
!
       fluid1%istack_ele_fld_smp = 0
       inod_fl_smp_stack = 0
       inter_fl_smp_stack = 0
!
       end subroutine allocate_geometry_fluid_smp
!
!-----------------------------------------------------------------------
!
       subroutine allocate_geometry_conduct_smp
!
       use m_machine_parameter
!
!   for conductive layer
!
       allocate( conduct1%istack_ele_fld_smp(0:np_smp))
       allocate( inod_cd_smp_stack(0:np_smp))
       allocate( inter_cd_smp_stack(0:np_smp))
!
       conduct1%istack_ele_fld_smp = 0
       inod_cd_smp_stack = 0
       inter_cd_smp_stack = 0
!
       end subroutine allocate_geometry_conduct_smp
!
! ----------------------------------------------------------------------
!
       subroutine allocate_geometry_ins_smp
!
       use m_machine_parameter
!
!   for insulate layer
!
       allocate( insulate1%istack_ele_fld_smp(0:np_smp))
       allocate( inod_ins_smp_stack(0:np_smp))
       allocate( inter_ins_smp_stack(0:np_smp))
!
       insulate1%istack_ele_fld_smp = 0
       inod_ins_smp_stack = 0
       inter_ins_smp_stack = 0
!
       end subroutine allocate_geometry_ins_smp
!
! ----------------------------------------------------------------------
!
       subroutine allocate_geometry_incore_smp
!
       use m_machine_parameter
!
!   for insulated core
!
       allocate( inner_core%istack_ele_fld_smp(0:np_smp))
       allocate( inod_in_core_smp_stack(0:np_smp))
       allocate( inter_in_core_smp_stack(0:np_smp))
!
       inner_core%istack_ele_fld_smp = 0
       inod_in_core_smp_stack = 0
       inter_in_core_smp_stack = 0
!
       end subroutine allocate_geometry_incore_smp
!
! ----------------------------------------------------------------------
!------------------------------------------------------------------
!
       subroutine deallocate_fluid_node_list
!
       call deallocate_field_nod_list(fluid1)
!
       end subroutine deallocate_fluid_node_list
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_conduct_node_list
!
!
       call deallocate_field_nod_list(conduct1)
       call deallocate_field_nod_list(insulate1)
       call deallocate_field_nod_list(inner_core)
!
       end subroutine deallocate_conduct_node_list
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_inner_core_ele_list
!
!
       deallocate(iele_in_core)
!
       end subroutine deallocate_inner_core_ele_list
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_element_connect_org
!
       deallocate(ie_org, iele_global_org)
!
       end subroutine deallocate_element_connect_org
!
!------------------------------------------------------------------
!
       subroutine deallocate_geometry_fluid_smp
!
       deallocate( fluid1%istack_ele_fld_smp )
       deallocate( inod_fl_smp_stack )
       deallocate( inter_fl_smp_stack )
!
       end subroutine deallocate_geometry_fluid_smp
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_geometry_conduct_smp
!
       deallocate( conduct1%istack_ele_fld_smp  )
       deallocate( inod_cd_smp_stack  )
       deallocate( inter_cd_smp_stack )
!
       end subroutine deallocate_geometry_conduct_smp
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_geometry_ins_smp
!
       deallocate( insulate1%istack_ele_fld_smp  )
       deallocate( inod_ins_smp_stack  )
       deallocate( inter_ins_smp_stack )
!
       end subroutine deallocate_geometry_ins_smp
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_geometry_incore_smp
!
       deallocate( inner_core%istack_ele_fld_smp  )
       deallocate( inod_in_core_smp_stack  )
       deallocate( inter_in_core_smp_stack )
!
       end subroutine deallocate_geometry_incore_smp
!
! ----------------------------------------------------------------------
!
      subroutine check_geometry_fluid_smp(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
       write(*,*) 'PE: ', my_rank,                                      &
     &           'inod_fl_smp_stack ', inod_fl_smp_stack
       write(*,*) 'PE: ', my_rank,                                      &
     &           'internal_node_fluid ', fluid1%internal_node_fld
       write(*,*) 'PE: ', my_rank,                                      &
     &           'inter_fl_smp_stack ', inter_fl_smp_stack
       write(*,*) 'PE: ', my_rank,                                      &
     &           'iele_fl_smp_stack ', fluid1%istack_ele_fld_smp
!
      end subroutine check_geometry_fluid_smp
!
! ----------------------------------------------------------------------
!
      subroutine check_geometry_conduct_smp(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
       write(*,*) 'PE: ', my_rank,                                      &
     &            'numnod_conduct ', conduct1%numnod_fld
       write(*,*) 'PE: ', my_rank,                                      &
     &           'inod_cd_smp_stack ', inod_cd_smp_stack
       write(*,*) 'PE: ', my_rank,                                      &
     &           'internal_node_conduct ', conduct1%internal_node_fld
       write(*,*) 'PE: ', my_rank,                                      &
     &           'inter_cd_smp_stack ', inter_cd_smp_stack
       write(*,*) 'PE: ', my_rank,                                      &
     &           'iele_cd_smp_stack ', conduct1%istack_ele_fld_smp
!
      end subroutine check_geometry_conduct_smp
!
! ----------------------------------------------------------------------
!
      end module m_geometry_data_MHD
