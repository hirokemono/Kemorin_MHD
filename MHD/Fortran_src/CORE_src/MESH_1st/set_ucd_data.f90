!set_ucd_data.f90
!      module set_ucd_data
!
!        programmed by H.Matsui on July, 2006
!
!      subroutine link_num_field_2_output
!      subroutine link_global_mesh_4_ucd
!      subroutine link_local_mesh_4_ucd
!      subroutine link_field_data_2_output
!      subroutine allocate_phys_data_by_output(my_rank, istep_ucd)
!
!      subroutine set_ucd_data_from_IO(my_rank, istep_ucd)
!      subroutine add_by_ucd_data(my_rank, istep_ucd)
!      subroutine subtract_by_ucd_data(my_rank, istep_ucd)
!
      module set_ucd_data
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
      subroutine link_num_field_2_output
!
      use m_geometry_parameter
      use m_node_phys_data
!
!
      fem_ucd%nnod =      numnod
      fem_ucd%ntot_comp = num_nod_phys_vis
!
      end subroutine link_num_field_2_output
!
!-----------------------------------------------------------------------
!
      subroutine link_global_mesh_4_ucd
!
      use m_geometry_parameter
      use m_geometry_data
      use set_and_cal_udt_data
!
!
      fem_ucd%xx =>          xx
      fem_ucd%inod_global => globalnodid
!
      call count_udt_elements(internal_node, numele, nnod_4_ele, ie,    &
     &    fem_ucd)
      call allocate_ucd_ele(fem_ucd)
!
      call set_udt_global_connect(internal_node, numele, nnod_4_ele,    &
     &    globalelmid, ie, fem_ucd)
!
      end subroutine link_global_mesh_4_ucd
!
!-----------------------------------------------------------------------
!
      subroutine link_local_mesh_4_ucd
!
      use m_geometry_parameter
      use m_geometry_data
      use set_and_cal_udt_data
!
!
      call allocate_ucd_node(fem_ucd)
      call set_udt_local_nodes(numnod, xx, fem_ucd)
!
      call count_udt_elements(internal_node, numele, nnod_4_ele, ie,    &
     &    fem_ucd)
      call allocate_ucd_ele(fem_ucd)
!
      call set_udt_local_connect(internal_node, numele, nnod_4_ele, ie, &
     &    fem_ucd)
!
      end subroutine link_local_mesh_4_ucd
!
!-----------------------------------------------------------------------
!
      subroutine link_field_data_2_output
!
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
!
      fem_ucd%nnod =      numnod
      fem_ucd%num_field = num_nod_phys_vis
      fem_ucd%ntot_comp = num_tot_nod_phys_vis
!
      fem_ucd%num_comp =>  num_nod_component(1:fem_ucd%num_field)
      fem_ucd%phys_name => phys_nod_name(1:fem_ucd%num_field)
!
      fem_ucd%d_ucd =>     d_nod(1:fem_ucd%nnod,1:fem_ucd%ntot_comp)
!
      end subroutine link_field_data_2_output
!
!-----------------------------------------------------------------------
!
      subroutine allocate_phys_data_by_output(my_rank, istep_ucd)
!
      use m_node_phys_data
      use cal_minmax_and_stacks
      use ucd_IO_select
!
      integer(kind = kint),  intent(in) :: my_rank, istep_ucd
!
!
      call sel_read_udt_param(my_rank, istep_ucd, fem_ucd)
!
      num_nod_phys =     fem_ucd%num_field
      num_nod_phys_vis = fem_ucd%num_field
!
      call allocate_phys_name
!
      num_nod_component(1:num_nod_phys)                                 &
     &     = fem_ucd%num_comp(1:num_nod_phys)
      phys_nod_name(1:num_nod_phys) = fem_ucd%phys_name(1:num_nod_phys)
!
      call s_cal_total_and_stacks(num_nod_phys, num_nod_component,      &
     &    izero, istack_nod_component, num_tot_nod_phys)
      num_tot_nod_phys_vis = num_tot_nod_phys
!
      call allocate_data_arrays
!
      end subroutine allocate_phys_data_by_output
!
!-----------------------------------------------------------------------
!
      subroutine set_ucd_data_from_IO(my_rank, istep_ucd)
!
      use m_geometry_parameter
      use m_node_phys_data
      use set_and_cal_udt_data
      use ucd_IO_select
!
      integer(kind = kint),  intent(in) :: my_rank, istep_ucd
!
!
      call sel_read_udt_file(my_rank, istep_ucd, fem_ucd)
      call set_field_by_udt_data(numnod, num_nod_phys,                  &
     &    num_tot_nod_phys, num_nod_component, phys_nod_name, d_nod,    &
     &    fem_ucd)
!
      end subroutine set_ucd_data_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine add_by_ucd_data(my_rank, istep_ucd)
!
      use m_geometry_parameter
      use m_node_phys_data
      use set_and_cal_udt_data
      use ucd_IO_select
!
      integer(kind = kint),  intent(in) :: my_rank, istep_ucd
!
!
      call sel_read_udt_file(my_rank, istep_ucd, fem_ucd)
      call add_field_by_udt_data(numnod, num_nod_phys,                  &
     &    num_tot_nod_phys, num_nod_component, phys_nod_name, d_nod,    &
     &    fem_ucd)
!
      end subroutine add_by_ucd_data
!
! -----------------------------------------------------------------------
!
      subroutine subtract_by_ucd_data(my_rank, istep_ucd)
!
      use m_geometry_parameter
      use m_node_phys_data
      use set_and_cal_udt_data
      use ucd_IO_select
!
      integer(kind = kint),  intent(in) :: my_rank, istep_ucd
!
!
      call sel_read_udt_file(my_rank, istep_ucd, fem_ucd)
      call subtract_field_by_udt_data(numnod, num_nod_phys,             &
     &    num_tot_nod_phys, num_nod_component, phys_nod_name, d_nod,    &
     &    fem_ucd)
!
      end subroutine subtract_by_ucd_data
!
! -----------------------------------------------------------------------
!
      end module set_ucd_data
