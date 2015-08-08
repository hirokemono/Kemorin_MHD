!element_posi_2_nodal_array.f90
!      module element_posi_2_nodal_array
!
      module element_posi_2_nodal_array
!
!      modified by H. Matsui on Jan., 2009
!
      use m_precision
      use m_constants
!
      implicit none
!
!      subroutine s_element_posi_2_nodal_array
!      subroutine s_2nd_ele_posi_2_nodal_array
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_element_posi_2_nodal_array
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use cal_minmax_and_stacks
!
!
      integer(kind = kint) :: inod
!
      call deallocate_node_geometry
!
!
      node1%numnod =  numele
      numnod =       numele
      internal_node = numele
!
      call allocate_node_geometry
!
!$omp parallel do
      do inod = 1, numnod
!
        inod_global(inod) = iele_global(inod)
        xx(inod,1) =       x_ele(inod,1)
        xx(inod,2) =       x_ele(inod,2)
        xx(inod,3) =       x_ele(inod,3)
!
        radius(inod) =       r_ele(inod)
        a_radius(inod) =     ar_ele(inod)
        s_cylinder(inod) =   s_ele(inod)
        a_s_cylinder(inod) = as_ele(inod)
        longitude(inod) =    phi_ele(inod)
        colatitude(inod) =   theta_ele(inod)
!
      end do
!$omp end parallel do
!
!
       call count_number_4_smp( np_smp, ione, numnod,                   &
     &       inod_smp_stack, maxnod_4_smp )
!
       call count_number_4_smp( np_smp, ione, internal_node,            &
     &       inter_smp_stack, max_in_nod_4_smp )
!
!
      end subroutine s_element_posi_2_nodal_array
!
! ----------------------------------------------------------------------
!
      subroutine s_2nd_ele_posi_2_nodal_array(newmesh)
!
      use m_machine_parameter
      use cal_minmax_and_stacks
      use set_size_4_smp_types
      use cal_mesh_position_type
!
      use t_mesh_data
!
      type(mesh_geometry), intent(inout) :: newmesh
!
      integer(kind = kint) :: inod
!
!
      call count_size_4_smp_mesh_type(newmesh%node, newmesh%ele)
      call set_spherical_position_type(newmesh%node)
!
      call allocate_ele_geometry_type(newmesh%ele)
      call set_center_of_ele_type(newmesh%node, newmesh%ele)
!
!
      call deallocate_node_geometry_type(newmesh%node)
!
!
      newmesh%node%numnod =         newmesh%ele%numele
      newmesh%node%internal_node = newmesh%ele%numele
!
      call allocate_node_geometry_type(newmesh%node)
!
!$omp parallel do
      do inod = 1, newmesh%node%numnod
!
        newmesh%node%inod_global(inod) = newmesh%ele%iele_global(inod)
        newmesh%node%xx(inod,1) = newmesh%ele%x_ele(inod,1)
        newmesh%node%xx(inod,2) = newmesh%ele%x_ele(inod,2)
        newmesh%node%xx(inod,3) = newmesh%ele%x_ele(inod,3)
!
        newmesh%node%rr(inod) =   newmesh%ele%r_ele(inod)
        newmesh%node%a_r(inod) = newmesh%ele%ar_ele(inod)
        newmesh%node%ss(inod) =    newmesh%ele%s_ele(inod)
        newmesh%node%a_s(inod) =  newmesh%ele%as_ele(inod)
        newmesh%node%phi(inod) =  newmesh%ele%phi_ele(inod)
        newmesh%node%theta(inod) = newmesh%ele%theta_ele(inod)
!
      end do
!$omp end parallel do
!
!
       call count_number_4_smp( np_smp, ione, newmesh%node%numnod,      &
     &       newmesh%node%istack_nod_smp, newmesh%node%max_nod_smp)
!
       call count_number_4_smp                                          &
     &     ( np_smp, ione, newmesh%node%internal_node,                  &
     &       newmesh%node%istack_nod_smp,                               &
     &       newmesh%node%max_internal_nod_smp)
!
!
      end subroutine s_2nd_ele_posi_2_nodal_array
!
! ----------------------------------------------------------------------
!
      end module element_posi_2_nodal_array
