!element_posi_2_nodal_array.f90
!      module element_posi_2_nodal_array
!
!      modified by H. Matsui on Jan., 2009
!
!      subroutine s_element_posi_2_nodal_array(ele, node)
!      subroutine s_2nd_ele_posi_2_nodal_array(newmesh)
!
      module element_posi_2_nodal_array
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_element_posi_2_nodal_array(ele, node)
!
      use t_geometry_data
      use cal_minmax_and_stacks
!
      type(element_data), intent(in) :: ele
      type(node_data), intent(inout) :: node
!
!
      integer(kind = kint) :: inod
!
!
      call deallocate_node_geometry_type(node)
!
      node%numnod =        ele%numele
      node%internal_node = ele%numele
!
      call allocate_node_geometry_type(node)
!
!$omp parallel do
      do inod = 1, node%numnod
        node%inod_global(inod) = ele%iele_global(inod)
        node%xx(inod,1) =        ele%x_ele(inod,1)
        node%xx(inod,2) =        ele%x_ele(inod,2)
        node%xx(inod,3) =        ele%x_ele(inod,3)
!
        node%rr(inod) =    ele%r_ele(inod)
        node%a_r(inod) =   ele%ar_ele(inod)
        node%ss(inod) =    ele%s_ele(inod)
        node%a_s(inod) =   ele%as_ele(inod)
        node%phi(inod) =   ele%phi_ele(inod)
        node%theta(inod) = ele%theta_ele(inod)
      end do
!$omp end parallel do
!
       call count_number_4_smp( np_smp, ione, node%numnod,              &
     &     node%istack_nod_smp, node%max_nod_smp)
!
       call count_number_4_smp( np_smp, ione, node%internal_node,       &
     &     node%istack_internal_smp, node%max_internal_nod_smp)
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
      use const_mesh_information
!
      use t_mesh_data
!
      type(mesh_geometry), intent(inout) :: newmesh
!
      integer(kind = kint) :: inod
!
!
      call set_nod_and_ele_infos(newmesh%node, newmesh%ele)
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
