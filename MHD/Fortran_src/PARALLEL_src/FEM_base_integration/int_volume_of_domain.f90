!
!     module int_volume_of_domain
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on Aug., 2006
!        Modified by H. Matsui on June, 2007
!
!      subroutine const_jacobian_and_volume(node, sf_grp,               &
!     &         infinity_list, ele, jac_3d_l, jac_3d_q)
!      subroutine const_jacobian_and_vol_layer(node, sf_grp,            &
!     &          infinity_list, ele, jac_3d_l, jac_3d_q, layer_tbl)
!        type(node_data), intent(in) :: node
!        type(surface_group_data), intent(in) :: sf_grp
!        type(scalar_surf_BC_list), intent(in) :: infinity_list
!        type(element_data), intent(inout) :: ele
!        type(jacobians_3d), intent(inout) :: jac_3d_l, jac_3d_q
!        type(layering_tbl), intent(inout) :: layer_tbl
!      subroutine s_int_volume_of_domain(ele, jac_3d)
!
      module int_volume_of_domain
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_group_data
      use t_surface_boundary
      use t_jacobians
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_jacobian_and_volume(node, sf_grp,                &
     &         infinity_list, ele, jac_3d_l, jac_3d_q)
!
      use sum_volume_of_domain
      use const_jacobians_3d
!
      type(node_data), intent(in) :: node
      type(surface_group_data), intent(in) :: sf_grp
      type(scalar_surf_BC_list), intent(in) :: infinity_list
!
      type(element_data), intent(inout) :: ele
      type(jacobians_3d), intent(inout) :: jac_3d_l, jac_3d_q
!
!
      call cal_jacobian_element                                         &
     &   (node, ele, sf_grp, infinity_list, jac_3d_l, jac_3d_q)
!
      call allocate_volume_4_smp
      call s_int_volume_of_domain(ele, jac_3d_q)
      call deallocate_volume_4_smp
!
      call dealloc_dxi_dx_type(jac_3d_q)
      call dealloc_dxi_dx_type(jac_3d_l)
!
      end subroutine const_jacobian_and_volume
!
!-----------------------------------------------------------------------
!
      subroutine const_jacobian_and_vol_layer(node, sf_grp,             &
     &          infinity_list, ele, jac_3d_l, jac_3d_q, layer_tbl)
!
      use t_layering_ele_list
      use const_jacobians_3d
      use sum_volume_of_domain
      use cal_layered_volumes
!
      type(node_data), intent(in) :: node
      type(surface_group_data), intent(in) :: sf_grp
      type(scalar_surf_BC_list), intent(in) :: infinity_list
!
      type(element_data), intent(inout) :: ele
      type(jacobians_3d), intent(inout) :: jac_3d_l, jac_3d_q
      type(layering_tbl), intent(inout) :: layer_tbl
!
      call cal_jacobian_element                                         &
     &   (node, ele, sf_grp, infinity_list, jac_3d_l, jac_3d_q)
!
      call allocate_volume_4_smp
      call s_int_volume_of_domain(ele, jac_3d_q)
      call s_cal_layered_volumes(ele, layer_tbl)
      call deallocate_volume_4_smp
!
      call dealloc_dxi_dx_type(jac_3d_q)
      call dealloc_dxi_dx_type(jac_3d_l)
!
      end subroutine const_jacobian_and_vol_layer
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine s_int_volume_of_domain(ele, jac_3d)
!
      use calypso_mpi
      use m_fem_gauss_int_coefs
      use fem_element_volume
      use sum_volume_of_domain
!
      type(jacobians_3d), intent(in) :: jac_3d
      type(element_data), intent(inout) :: ele
!
!
!      write(*,*) 'fem_element_volume_pg', max_int_point
       call fem_element_volume_pg(ele, jac_3d, max_int_point)
!
!     ---  lead total volume
!
!      write(*,*) 'sum_4_volume'
      call sum_4_volume(ele%numele, ele%interior_ele,                   &
     &    ele%istack_ele_smp, ele%volume_ele, vol_local)
!
!      write(*,*) 'MPI_allREDUCE'
       call MPI_allREDUCE (vol_local, ele%volume, ione,                 &
     &  CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
       if (ele%volume .eq. 0.0d0) then
         ele%a_vol = 1.0d30
       else
         ele%a_vol = 1.0d0 / ele%volume
       end if
!
       end subroutine s_int_volume_of_domain
!
!-----------------------------------------------------------------------
!
      end module int_volume_of_domain
