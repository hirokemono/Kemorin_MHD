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
      numnod =       numele
      internal_node = numele
!
      call allocate_node_geometry
!
!$omp parallel do
      do inod = 1, numnod
!
        globalnodid(inod) = globalelmid(inod)
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
      subroutine s_2nd_ele_posi_2_nodal_array
!
      use m_machine_parameter
      use m_2nd_geometry_data
      use cal_minmax_and_stacks
      use set_size_4_smp_types
      use cal_mesh_position_type
!
!
      integer(kind = kint) :: inod
!
!
      call count_size_4_smp_mesh_type(node_2nd, ele_2nd)
      call set_spherical_position_type(node_2nd)
!
      call allocate_ele_geometry_type(ele_2nd)
      call set_center_of_ele_type(node_2nd, ele_2nd)
!
!
      call deallocate_node_geometry_type(node_2nd)
!
!
      node_2nd%numnod =         ele_2nd%numele
      node_2nd%internal_node = ele_2nd%numele
!
      call allocate_node_geometry_type(node_2nd)
!
!$omp parallel do
      do inod = 1, node_2nd%numnod
!
        node_2nd%inod_global(inod) = ele_2nd%iele_global(inod)
        node_2nd%xx(inod,1) = ele_2nd%x_ele(inod,1)
        node_2nd%xx(inod,2) = ele_2nd%x_ele(inod,2)
        node_2nd%xx(inod,3) = ele_2nd%x_ele(inod,3)
!
        node_2nd%rr(inod) =   ele_2nd%r_ele(inod)
        node_2nd%a_r(inod) = ele_2nd%ar_ele(inod)
        node_2nd%ss(inod) =    ele_2nd%s_ele(inod)
        node_2nd%a_s(inod) =  ele_2nd%as_ele(inod)
        node_2nd%phi(inod) =  ele_2nd%phi_ele(inod)
        node_2nd%theta(inod) = ele_2nd%theta_ele(inod)
!
      end do
!$omp end parallel do
!
!
       call count_number_4_smp( np_smp, ione, node_2nd%numnod,          &
     &       node_2nd%istack_nod_smp, node_2nd%max_nod_smp)
!
       call count_number_4_smp( np_smp, ione, node_2nd%internal_node,   &
     &       node_2nd%istack_nod_smp, node_2nd%max_internal_nod_smp)
!
!
      end subroutine s_2nd_ele_posi_2_nodal_array
!
! ----------------------------------------------------------------------
!
      end module element_posi_2_nodal_array
