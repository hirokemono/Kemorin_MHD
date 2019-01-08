!
!      module grouping_for_partition
!
!     Written by H. Matsui on Aug., 2007
!
!!      subroutine grouping_for_partitioner                             &
!!     &         (node, ele, edge, nod_grp, ele_grp,                    &
!!     &          ele_grp_data, node_volume, domain_grp)
!!        type(domain_groups_4_partitioner), intent(inout)  :: domain_grp
!!      subroutine regrouping_for_partition                             &
!!      &        (node, ele, part_tbl, part_volume, n_volume, domain_grp)
!!        type(domain_groups_4_partitioner), intent(inout)  :: domain_grp
!
      module grouping_for_partition
!
      use m_precision
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine grouping_for_partitioner                               &
     &         (node, ele, edge, nod_grp, ele_grp,                      &
     &          ele_grp_data, node_volume, domain_grp)
!
      use m_constants
      use m_error_IDs
      use m_ctl_param_partitioner
      use m_subdomain_table_IO
!
      use t_domain_group_4_partition
      use t_geometry_data
      use t_edge_data
      use t_group_data
      use t_group_connects
!
      use recursive_bisection
      use node_equaly_sectioning
      use devide_by_spherical_coord
      use set_domain_and_org_id
      use copy_domain_list_4_IO
      use set_partition_by_fine_mesh
      use const_metis_input
      use error_exit_4_part
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(group_data), intent(in) :: nod_grp
      type(group_data), intent(in) :: ele_grp
      type(element_group_table), intent(in) :: ele_grp_data
      real(kind = kreal), intent(in) :: node_volume(node%numnod)
!
      type(domain_groups_4_partitioner), intent(inout)  :: domain_grp
!
      integer(kind = kint) :: ierr
!
!C +-----+
!C | RCB |
!C +-----+
!C===
      if (NTYP_div .eq. iPART_RCB_XYZ) then
        call rc_bisection(node%numnod, node%internal_node, node%xx,     &
     &      domain_grp%nod_d_grp)
!C===
!C===
!C
!C +------------------------------+
!C | RCB for spherical coordinate |
!C +------------------------------+
!C===

      else if (NTYP_div .eq. iPART_RCB_SPH) then
        call rcb_spherical(node%numnod, node%internal_node,             &
     &      node%rr, node%theta, node%phi, domain_grp%nod_d_grp)
!
!C
!C +------------------------------+
!C | Coordinate Sectioning        |
!C +------------------------------+
!C===
!
      else if (NTYP_div .eq. iPART_EQ_XYZ) then
        write(*,*) 'equaly_bisection'
        call equaly_bisection(node%numnod, node%internal_node, node%xx, &
     &      domain_grp%nod_d_grp)
      else if (NTYP_div .eq. iPART_EQV_XYZ) then
        call equaly_volume_bisection                                    &
     &     (node%numnod, node%internal_node, node%xx,                   &
     &     node_volume, ele%volume, domain_grp%nod_d_grp)
!
      else if (NTYP_div .eq. iPART_EQ_SPH) then
        call eb_spherical(node%numnod, node%internal_node,              &
     &      node%rr, node%theta, node%phi, domain_grp%nod_d_grp)
!
      else if (NTYP_div .eq. iPART_LAYER_SPH) then
        call eb_spherical_w_egrp(node%numnod, node%internal_node,       &
     &    ele_grp%num_grp, ele_grp%grp_name,                            &
     &    ele_grp_data%node%ntot_e_grp, ele_grp_data%node%istack_e_grp, &
     &    ele_grp_data%node%item_e_grp,                                 &
     &    node%rr, node%theta, node%phi, domain_grp%nod_d_grp)
!
!
!C
!C +------------------------------+
!C | RCB for spherical coordinate |
!C +------------------------------+
!C===
      else if (NTYP_div .eq. iPART_CUBED_SPHERE) then
        call divide_by_sphere_coord(num_domain,                         &
     &      node%numnod, ele%nnod_4_ele, node%xx,                       &
     &      node%rr, node%theta, node%phi,                              &
     &      nod_grp%num_grp, nod_grp%num_item, nod_grp%istack_grp,      &
     &      nod_grp%item_grp, nod_grp%grp_name, domain_grp%nod_d_grp)
!C
!C +------------------------------+
!C | Partisioning by MeTiS output |
!C +------------------------------+
!C===
      else if (NTYP_div .eq. iPART_MeTiS_RSB) then
        write(*,*) 'read_group_by_metis'
        call read_group_by_metis                                        &
     &     (ierr, node%numnod, node%internal_node)
        if (ierr .eq. ierr_P_MPI) call ERROR_EXIT(ierr_P_MPI, izero)
        write(*,*) 'copy_domain_list_from_IO'
        call copy_domain_list_from_IO                                   &
     &     (node%numnod, node%internal_node, domain_grp%nod_d_grp)
!
      else if (NTYP_div .eq. iPART_GEN_MeTiS) then
        call s_const_metis_input                                        &
     &     (node%numnod, node%internal_node, edge)
        stop
!
!C
!C +------------------------------+
!C | Partisioning for Multigrid   |
!C +------------------------------+
!C===
!C===
      else if (NTYP_div .eq. iPART_FINE_MESH_TBL) then
        call s_set_partition_by_fine_mesh(domain_grp)
!
      else if (NTYP_div .eq. iPART_DECMP_MESH_TBL) then
        call read_group_4_partition
        call copy_domain_list_from_IO                                   &
     &     (node%numnod, node%internal_node, domain_grp%nod_d_grp)
      end if
!
!C
!C +------------------------------+
!C | Output domain grouping table |
!C +------------------------------+
!C===
      if     (NTYP_div.eq.iPART_RCB_XYZ                                 &
     &   .or. NTYP_div.eq.iPART_RCB_SPH                                 &
     &   .or. NTYP_div.eq.iPART_MeTiS_RSB                               &
     &   .or. NTYP_div.eq.iPART_CUBED_SPHERE                            &
     &   .or. NTYP_div.eq.iPART_EQ_XYZ                                  &
     &   .or. NTYP_div.eq.iPART_EQV_XYZ                                 &
     &   .or. NTYP_div.eq.iPART_EQ_SPH) then
        call copy_domain_list_to_IO                                     &
     &     (node%numnod, node%internal_node, domain_grp%nod_d_grp)
        call output_group_4_partition
      end if
!C
!C +------------------------------+
!C | set group ID for elements    |
!C +------------------------------+
!C===
!C===
      call set_ele_domain_groups                                        &
     &   (ele, domain_grp%nod_d_grp, domain_grp%ele_d_grp)
!
      end subroutine grouping_for_partitioner
!
!------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine regrouping_for_partition                               &
      &        (node, ele, part_tbl, part_volume, n_volume, domain_grp)
!
      use m_constants
      use m_error_IDs
      use m_ctl_param_partitioner
      use m_subdomain_table_IO
!
      use t_domain_group_4_partition
      use t_geometry_data
      use t_edge_data
      use t_group_data
      use t_group_connects
!
      use node_equaly_sectioning
      use set_domain_and_org_id
      use copy_domain_list_4_IO
      use set_partition_by_fine_mesh
      use error_exit_4_part
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      real(kind = kreal), intent(in) :: n_volume(node%numnod)
      real(kind = kreal), intent(inout) :: part_tbl(num_domain)
      real(kind = kreal), intent(inout) :: part_volume(num_domain)
!
      type(domain_groups_4_partitioner), intent(inout)  :: domain_grp
!
!
      write(*,*) 'regrouping dataset by: ', NTYP_div
!
      if (NTYP_div .eq. iPART_EQ_XYZ) then
        call proportionally_bisection                                   &
     &     (node%numnod, node%internal_node, node%xx,                   &
     &      part_tbl, domain_grp%nod_d_grp)
      end if
!
      if (NTYP_div .eq. iPART_EQV_XYZ) then
        call proportion_volume_bisection                                &
     &     (node%numnod, node%internal_node, node%xx,                   &
     &      part_volume, n_volume, domain_grp%nod_d_grp)
      end if

!
!C
!C +------------------------------+
!C | Output domain grouping table |
!C +------------------------------+
!C===
      if     (NTYP_div.eq.iPART_RCB_XYZ                                 &
     &   .or. NTYP_div.eq.iPART_RCB_SPH                                 &
     &   .or. NTYP_div.eq.iPART_MeTiS_RSB                               &
     &   .or. NTYP_div.eq.iPART_CUBED_SPHERE                            &
     &   .or. NTYP_div.eq.iPART_EQ_XYZ                                  &
     &   .or. NTYP_div.eq.iPART_EQV_XYZ                                 &
     &   .or. NTYP_div.eq.iPART_EQ_SPH) then
        call copy_domain_list_to_IO                                     &
     &     (node%numnod, node%internal_node, domain_grp%nod_d_grp)
        call output_group_4_partition
      end if
!C
!C +------------------------------+
!C | set group ID for elements    |
!C +------------------------------+
!C===
!C===
      call set_ele_domain_groups                                        &
     &   (ele, domain_grp%nod_d_grp, domain_grp%ele_d_grp)
!
      end subroutine regrouping_for_partition
!
! ----------------------------------------------------------------------
!
      end module grouping_for_partition
