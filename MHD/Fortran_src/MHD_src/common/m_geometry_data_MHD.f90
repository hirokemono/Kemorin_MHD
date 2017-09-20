!m_geometry_data_MHD.f90
!     module m_geometry_data_MHD
!
!     written by H. Matsui on May, 2009
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
!>       Strucutre for MHD mesh data
      type(mesh_data_MHD), save :: MHD_mesh1
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
       subroutine deallocate_fluid_node_list
!
       call deallocate_field_nod_list(MHD_mesh1%fluid)
!
       end subroutine deallocate_fluid_node_list
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_conduct_node_list
!
!
       call deallocate_field_nod_list(MHD_mesh1%conduct)
       call deallocate_field_nod_list(MHD_mesh1%insulate)
       call deallocate_field_nod_list(MHD_mesh1%inner_core)
!
       end subroutine deallocate_conduct_node_list
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_inner_core_ele_list
!
!
       call deallocate_field_ele_list(MHD_mesh1%inner_core)
!
       end subroutine deallocate_inner_core_ele_list
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_element_connect_org
!
      call dealloc_ele_connect_org_type(MHD_mesh1)
!
      end subroutine deallocate_element_connect_org
!
!------------------------------------------------------------------
!
      subroutine deallocate_geometry_fluid_smp
!
      call deallocate_geometry_field_smp(MHD_mesh1%fluid)
!
      end subroutine deallocate_geometry_fluid_smp
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_geometry_conduct_smp
!
       call deallocate_geometry_field_smp(MHD_mesh1%conduct)
       call deallocate_geometry_field_smp(MHD_mesh1%insulate)
       call deallocate_geometry_field_smp(MHD_mesh1%inner_core)
!
       end subroutine deallocate_geometry_conduct_smp
!
! ----------------------------------------------------------------------
!
      end module m_geometry_data_MHD
