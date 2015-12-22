!m_geometry_data_MHD.f90
!     module m_geometry_data_MHD
!
!     written by H. Matsui on May, 2009
!
!      subroutine allocate_inner_core_ele_list
!      subroutine allocate_element_connect_org(numele, nnod_4_ele)
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
!>       Mesh information for fluid segments
      type(field_geometry_data), save :: fluid1
!
!>       Mesh information for conductor segments
      type(field_geometry_data), save :: conduct1
!
!>       Mesh information for insulator segments
      type(field_geometry_data), save :: insulate1
!
!
!>       Mesh information for inner core segments
      type(field_geometry_data), save :: inner_core
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
       call allocate_field_ele_list(inner_core)
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
       call deallocate_field_ele_list(inner_core)
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
       call deallocate_geometry_field_smp(fluid1)
!
       end subroutine deallocate_geometry_fluid_smp
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_geometry_conduct_smp
!
       call deallocate_geometry_field_smp(conduct1)
       call deallocate_geometry_field_smp(insulate1)
       call deallocate_geometry_field_smp(inner_core)
!
       end subroutine deallocate_geometry_conduct_smp
!
! ----------------------------------------------------------------------
!
      subroutine check_geometry_fluid_smp(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
       write(*,*) 'PE: ', my_rank,                                      &
     &           'inod_fl_smp_stack ', fluid1%istack_nod_fld_smp
       write(*,*) 'PE: ', my_rank,                                      &
     &           'internal_node_fluid ', fluid1%internal_node_fld
       write(*,*) 'PE: ', my_rank,                                      &
     &           'inter_fl_smp_stack ', fluid1%istack_inter_fld_smp
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
     &           'inod_cd_smp_stack ', conduct1%istack_nod_fld_smp
       write(*,*) 'PE: ', my_rank,                                      &
     &           'internal_node_conduct ', conduct1%internal_node_fld
       write(*,*) 'PE: ', my_rank,                                      &
     &           'inter_cd_smp_stack ', conduct1%istack_inter_fld_smp
       write(*,*) 'PE: ', my_rank,                                      &
     &           'iele_cd_smp_stack ', conduct1%istack_ele_fld_smp
!
      end subroutine check_geometry_conduct_smp
!
! ----------------------------------------------------------------------
!
      end module m_geometry_data_MHD
