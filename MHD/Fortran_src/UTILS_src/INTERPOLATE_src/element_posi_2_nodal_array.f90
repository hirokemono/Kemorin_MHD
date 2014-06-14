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
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use cal_minmax_and_stacks
      use set_smp_size_4_2nd
      use cal_2nd_mesh_infos
!
!
      integer(kind = kint) :: inod
!
!
      call s_count_smp_size_4_2nd
      call set_2nd_spherical_position
!
      call allocate_ele_geometry_type(ele_2nd)
      call set_2nd_center_of_element
!
!
      call deallocate_2nd_node_position
!
!
      nnod_2nd =         ele_2nd%numele
      internal_nod_2nd = ele_2nd%numele
!
      call allocate_2nd_node_position
!
!$omp parallel do
      do inod = 1, nnod_2nd
!
        globalnodid_2nd(inod) = ele_2nd%iele_global(inod)
        xx_2nd(inod,1) =       ele_2nd%x_ele(inod,1)
        xx_2nd(inod,2) =       ele_2nd%x_ele(inod,2)
        xx_2nd(inod,3) =       ele_2nd%x_ele(inod,3)
!
        radius_2nd(inod) =   ele_2nd%r_ele(inod)
        a_radius_2nd(inod) = ele_2nd%ar_ele(inod)
        s_cyl_2nd(inod) =    ele_2nd%s_ele(inod)
        a_s_cyl_2nd(inod) =  ele_2nd%as_ele(inod)
        phi_2nd(inod) =      ele_2nd%phi_ele(inod)
        theta_2nd(inod) =    ele_2nd%theta_ele(inod)
!
      end do
!$omp end parallel do
!
!
       call count_number_4_smp( np_smp, ione, nnod_2nd,                 &
     &       inod_smp_stack_2nd, maxnod_4_smp_2nd )
!
       call count_number_4_smp( np_smp, ione, internal_nod_2nd,         &
     &       inter_smp_stack_2nd, max_in_nod_4_smp_2nd )
!
!
      end subroutine s_2nd_ele_posi_2_nodal_array
!
! ----------------------------------------------------------------------
!
      end module element_posi_2_nodal_array
