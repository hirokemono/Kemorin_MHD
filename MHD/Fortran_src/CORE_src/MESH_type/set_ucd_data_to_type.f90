!set_ucd_data_to_type.f90
!      module set_ucd_data_to_type
!
!        programmed by H.Matsui on July, 2006
!
!      subroutine link_num_field_type_2_output
!      subroutine link_node_data_type_2_output
!      subroutine link_ele_data_type_2_output
!      subroutine link_field_data_type_2_output
!
!      subroutine alloc_phys_name_type_by_output(phys_nod)
!      subroutine alloc_phys_data_type_by_output(node, phys_nod)
!
!      subroutine set_ucd_data_type_from_IO
!      subroutine add_by_ucd_data_type
!      subroutine subtract_by_ucd_data_type
!
      module set_ucd_data_to_type
!
      use m_precision
      use m_constants
!
      use m_ucd_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine link_num_field_type_2_output(node, ele)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
!
      fem_ucd%nnod = node%numnod
!
      fem_ucd%nele =       ele%numele
      fem_ucd%nnod_4_ele = ele%nnod_4_ele
!
      end subroutine link_num_field_type_2_output
!
!-----------------------------------------------------------------------
!
      subroutine link_node_data_type_2_output(node)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: node
!
!
      fem_ucd%xx =>          node%xx(1:node%numnod,1:3)
      fem_ucd%inod_global => node%inod_global(1:node%numnod)
!
      end subroutine link_node_data_type_2_output
!
!-----------------------------------------------------------------------
!
      subroutine link_ele_data_type_2_output(ele)
!
      use t_geometry_data
!
      type(element_data), intent(in) :: ele
!
!
      fem_ucd%ie =>          ele%ie(1:ele%numele,1:ele%nnod_4_ele)
      fem_ucd%iele_global => ele%iele_global(1:ele%numele)
!
      end subroutine link_ele_data_type_2_output
!
!-----------------------------------------------------------------------
!
      subroutine link_field_data_type_2_output(node, phys_nod)
!
      use t_geometry_data
      use t_phys_data
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: phys_nod
!
!
      fem_ucd%nnod =      node%numnod
      fem_ucd%num_field = phys_nod%num_phys_viz
      fem_ucd%ntot_comp = phys_nod%ntot_phys_viz
!
      fem_ucd%num_comp =>   phys_nod%num_component(1:fem_ucd%num_field)
      fem_ucd%phys_name =>  phys_nod%phys_name(1:fem_ucd%num_field)
!
      fem_ucd%d_ucd                                                     &
     &     => phys_nod%d_fld(1:fem_ucd%nnod,1:fem_ucd%ntot_comp)
!
      end subroutine link_field_data_type_2_output
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_phys_name_type_by_output(phys_nod)
!
      use t_phys_data
      use cal_minmax_and_stacks
!
      type(phys_data), intent(inout) :: phys_nod
!
!
      phys_nod%num_phys =     fem_ucd%num_field
      phys_nod%num_phys_viz = fem_ucd%num_field
!
      call alloc_phys_name_type(phys_nod)
!
      phys_nod%num_component(1:phys_nod%num_phys)                       &
     &           = fem_ucd%num_comp(1:phys_nod%num_phys)
      phys_nod%phys_name(1:phys_nod%num_phys)                           &
     &           = fem_ucd%phys_name(1:phys_nod%num_phys)
!
      call s_cal_total_and_stacks(phys_nod%num_phys,                    &
     &    phys_nod%num_component, izero, phys_nod%istack_component,     &
     &    phys_nod%ntot_phys)
      phys_nod%ntot_phys_viz = phys_nod%ntot_phys
!
      end subroutine alloc_phys_name_type_by_output
!
!-----------------------------------------------------------------------
!
      subroutine alloc_phys_data_type_by_output(node, phys_nod)
!
      use t_geometry_data
      use t_phys_data
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: phys_nod
!
!
      call alloc_phys_name_type_by_output(phys_nod)
      call alloc_phys_data_type(node%numnod, phys_nod)
!
      end subroutine alloc_phys_data_type_by_output
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_ucd_data_type_from_IO(node, phys_nod)
!
      use t_geometry_data
      use t_phys_data
      use set_and_cal_udt_data
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: phys_nod
!
!
      call set_field_by_udt_data(node%numnod, phys_nod%num_phys,        &
     &    phys_nod%ntot_phys, phys_nod%num_component,                   &
     &    phys_nod%phys_name, phys_nod%d_fld)
!
      end subroutine set_ucd_data_type_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine add_by_ucd_data_type(node, phys_nod)
!
      use t_geometry_data
      use t_phys_data
      use set_and_cal_udt_data
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: phys_nod
!
!
      call add_field_by_udt_data(node%numnod, phys_nod%num_phys,        &
     &    phys_nod%ntot_phys, phys_nod%num_component,                   &
     &    phys_nod%phys_name, phys_nod%d_fld)
!
      end subroutine add_by_ucd_data_type
!
! -----------------------------------------------------------------------
!
      subroutine subtract_by_ucd_data_type(node, phys_nod)
!
      use t_geometry_data
      use t_phys_data
      use set_and_cal_udt_data
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: phys_nod
!
!
      call subtract_field_by_udt_data(node%numnod, phys_nod%num_phys,   &
     &    phys_nod%ntot_phys, phys_nod%num_component,                   &
     &    phys_nod%phys_name, phys_nod%d_fld)
!
      end subroutine subtract_by_ucd_data_type
!
! -----------------------------------------------------------------------
!
      end module set_ucd_data_to_type
