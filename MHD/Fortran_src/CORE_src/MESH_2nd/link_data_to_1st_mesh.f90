!
!     module link_data_to_1st_mesh
!
!      Written by H. Matsui on Sep., 2006
!
!      subroutine link_nodal_field_names
!      subroutine link_nodal_field_data
!
      module link_data_to_1st_mesh
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine link_nodal_field_names
!
      use m_2nd_phys_data
      use m_node_phys_data
!
!
      num_nod_phys_2nd =  num_nod_phys
      ntot_nod_phys_2nd = num_tot_nod_phys
!
      num_nod_phys_2nd_vis =  num_nod_phys_vis
      ntot_nod_phys_2nd_vis = num_tot_nod_phys_vis
!
      ncomps_nod_2nd =>       num_nod_component
      istack_nod_comps_2nd => istack_nod_component
      phys_nod_name_2nd =>    phys_nod_name
      iorder_nod_phys_2nd =>  iorder_nod_phys
!
      end subroutine link_nodal_field_names
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine link_nodal_field_data
!
      use m_geometry_parameter
      use m_2nd_phys_data
      use m_node_phys_data
!
!
!
      call link_nodal_field_names
!
      d_nod_2nd => d_nod
!
      end subroutine link_nodal_field_data
!
! -------------------------------------------------------------------
!
      end module link_data_to_1st_mesh
