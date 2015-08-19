!
!     module link_data_type_to_1st_mesh
!
!      Written by H. Matsui on Sep., 2006
!
!      subroutine link_nodal_fld_type_names(nod_fld)
!      subroutine link_nodal_fld_type(nod_fld)
!
      module link_data_type_to_1st_mesh
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
      subroutine link_nodal_fld_type_names(nod_fld)
!
      use m_node_phys_data
      use t_phys_data
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      nod_fld%num_phys =  num_nod_phys
      nod_fld%ntot_phys = num_tot_nod_phys
!
      nod_fld%num_phys_viz =  num_nod_phys_vis
      nod_fld%ntot_phys_viz = num_tot_nod_phys_vis
!
      nod_fld%num_component =>    num_nod_component
      nod_fld%istack_component => istack_nod_component
      nod_fld%iorder_eletype =>   iorder_nod_phys
      nod_fld%iflag_monitor =>    iflag_nod_fld_monitor
      nod_fld%phys_name =>        phys_nod_name
!
      end subroutine link_nodal_fld_type_names
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine link_nodal_fld_type(nod_fld)
!
      use m_node_phys_data
      use t_phys_data
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      call link_nodal_fld_type_names(nod_fld)
!
      nod_fld%d_fld => d_nod
!
      end subroutine link_nodal_fld_type
!
! -------------------------------------------------------------------
!
      end module link_data_type_to_1st_mesh
