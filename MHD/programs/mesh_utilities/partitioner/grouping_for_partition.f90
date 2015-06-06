!
!      module grouping_for_partition
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine grouping_for_partitioner
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
      subroutine grouping_for_partitioner
!
      use m_constants
      use m_error_IDs
      use m_ctl_param_partitioner
      use m_geometry_parameter
      use m_geometry_data
      use m_node_group
      use m_element_group
      use m_element_group_connect
      use m_subdomain_table_IO
      use m_domain_group_4_partition
!
      use recursive_bisection
      use node_equaly_sectioning
      use devide_by_spherical_coord
      use set_domain_and_org_id
      use const_metis_input
      use copy_domain_list_4_IO
      use set_partition_by_fine_mesh
      use error_exit_4_part
!
      integer(kind = kint) :: ierr
!
!C +-----+
!C | RCB |
!C +-----+
!C===
      if (NTYP_div .eq. iPART_RCB_XYZ) then
        call rc_bisection(numnod, internal_node, xx)
!C===
!C===
!C
!C +------------------------------+
!C | RCB for spherical coordinate |
!C +------------------------------+
!C===

      else if (NTYP_div .eq. iPART_RCB_SPH) then
        call rcb_spherical(numnod, internal_node,                       &
     &      radius, colatitude, longitude)
!
!C
!C +------------------------------+
!C | Coordinate Sectioning        |
!C +------------------------------+
!C===
!
      else if (NTYP_div .eq. iPART_EQ_XYZ) then
        call equaly_bisection(numnod, internal_node, xx)
!
      else if (NTYP_div .eq. iPART_EQ_SPH) then
        call eb_spherical(numnod, internal_node,                        &
     &      radius, colatitude, longitude)
!
      else if (NTYP_div .eq. iPART_LAYER_SPH) then
        call eb_spherical_w_egrp(numnod, internal_node, num_mat,        &
     &      mat_name, ntot_node_ele_grp, inod_stack_ele_grp,            &
     &      inod_ele_grp, radius, colatitude, longitude)
!
!
!C
!C +------------------------------+
!C | RCB for spherical coordinate |
!C +------------------------------+
!C===
      else if (NTYP_div .eq. iPART_CUBED_SPHERE) then
        call divide_by_sphere_coord(num_domain, numnod, nnod_4_ele, xx, &
     &      radius, colatitude, longitude, num_bc, num_nod_bc,          &
     &      bc_istack, bc_item, bc_name)
!C
!C +------------------------------+
!C | Partisioning by MeTiS output |
!C +------------------------------+
!C===
      else if (NTYP_div .eq. iPART_MeTiS_RSB) then
        write(*,*) 'read_group_by_metis'
        call read_group_by_metis(ierr, numnod, internal_node)
        if (ierr .eq. ierr_P_MPI) call ERROR_EXIT(ierr_P_MPI, izero)
        write(*,*) 'copy_domain_list_from_IO'
        call copy_domain_list_from_IO
!
      else if (NTYP_div .eq. iPART_GEN_MeTiS) then
        call s_const_metis_input
        write(*,*) 'generate MeTiS input'
        stop
!
!C
!C +------------------------------+
!C | Partisioning for Multigrid   |
!C +------------------------------+
!C===
!C===
      else if (NTYP_div .eq. iPART_FINE_MESH_TBL) then
        call s_set_partition_by_fine_mesh
!
      else if (NTYP_div .eq. iPART_DECMP_MESH_TBL) then
        call read_group_4_partition
        call copy_domain_list_from_IO
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
     &   .or. NTYP_div.eq.iPART_EQ_SPH) then
        call copy_domain_list_to_IO
        call output_group_4_partition
      end if
!C
!C +------------------------------+
!C | set group ID for elements    |
!C +------------------------------+
!C===
!C===
      call set_ele_domain_groups
!
      end subroutine grouping_for_partitioner
!
!------------------------------------------------------------------
!
      end module grouping_for_partition
