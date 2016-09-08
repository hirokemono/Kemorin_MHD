!
!      module set_FEM_mesh_4_sph
!
!     Written by H. Matsui on March, 2013
!
!!      subroutine s_const_FEM_mesh_for_sph                             &
!!     &         (iflag_output_mesh, ip_rank, nidx_rtp, r_global,       &
!!     &          sph_params, radial_rj_grp, mesh, group)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::  group
!
      module set_FEM_mesh_4_sph
!
      use m_precision
!
      implicit none
!
      private :: const_FEM_geometry_for_sph
      private :: const_FEM_groups_for_sph
      private :: const_nod_comm_table_for_sph
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_const_FEM_mesh_for_sph                               &
     &         (iflag_output_mesh, ip_rank, nidx_rtp, r_global,         &
     &          sph_params, radial_rj_grp, mesh, group)
!
      use t_spheric_parameter
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_group_data
      use m_spheric_global_ranks
      use m_read_boundary_data
      use m_sph_mesh_1d_connect
!
      use coordinate_converter
      use ordering_sph_mesh_to_rtp
      use set_comm_table_4_IO
      use set_node_data_4_IO
      use set_element_data_4_IO
!
      integer(kind = kint), intent(in) :: iflag_output_mesh
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: ip_rank
      real(kind= kreal), intent(in) :: r_global(nidx_global_fem(1))
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(group_data), intent(in) :: radial_rj_grp
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::  group
!
      integer(kind = kint) :: ip_r, ip_t
!
!
      ip_r = iglobal_rank_rtp(1,ip_rank) + 1
      ip_t = iglobal_rank_rtp(2,ip_rank) + 1
!
!  Construct element connectivity
      call const_FEM_geometry_for_sph(ip_r, ip_t, r_global,             &
     &    sph_params, mesh%node, mesh%ele)
!
!  Construct groups
      call const_FEM_groups_for_sph(ip_r, ip_t,                         &
     &    sph_params, radial_rj_grp, group)
!
! Set communication table
      call const_nod_comm_table_for_sph(ip_rank, ip_r, ip_t,            &
     &    mesh%nod_comm)
!
! Ordering to connect rtp data
      call s_ordering_sph_mesh_for_rtp(nidx_rtp, ip_r, ip_t,            &
     &    mesh%node, mesh%ele, group%nod_grp, mesh%nod_comm)
!
! Convert spherical coordinate to certesian
      call position_2_xyz(mesh%node%numnod,                             &
     &    mesh%node%rr, mesh%node%theta, mesh%node%phi,                 &
     &    mesh%node%xx(1:mesh%node%numnod,1),                           &
     &    mesh%node%xx(1:mesh%node%numnod,2),                           &
     &    mesh%node%xx(1:mesh%node%numnod,3))
!
! Output mesh data
      if(iflag_output_mesh .eq. 0) return
      call copy_comm_tbl_type_to_IO(ip_rank, mesh%nod_comm)
      call copy_node_geometry_to_IO(mesh%node)
      call copy_ele_connect_to_IO(mesh%ele)
      call set_grp_data_to_IO                                           &
     &   (group%nod_grp, group%ele_grp, group%surf_grp)
!
      end subroutine s_const_FEM_mesh_for_sph
!
! -----------------------------------------------------------------------
!
      subroutine const_FEM_geometry_for_sph(ip_r, ip_t, r_global,       &
     &          sph_params, node, ele)
!
      use calypso_mpi
      use t_geometry_data
      use t_spheric_parameter
      use m_spheric_global_ranks
      use m_sph_mesh_1d_connect
!
      use set_sph_local_node
      use set_sph_local_element
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      real(kind= kreal), intent(in) :: r_global(nidx_global_fem(1))
      type(sph_shell_parameters), intent(in) :: sph_params
!
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
!
!  Construct node geometry
      call count_numnod_local_sph_mesh                                  &
     &   (sph_params%iflag_shell_mode, ip_r, ip_t, node)
!
      call allocate_node_geometry_type(node)
      call set_local_nodes_sph_mesh                                     &
     &   (sph_params%iflag_shell_mode, ip_r, ip_t, r_global, node)
!
!  Construct element connectivity
      call count_local_elements_sph_mesh(ip_r, ip_t, sph_params, ele)
!
      call allocate_ele_connect_type(ele)
      call set_local_elements_sph_mesh(ip_r, ip_t, sph_params, ele)
!
      end subroutine const_FEM_geometry_for_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_FEM_groups_for_sph                               &
     &         (ip_r, ip_t, sph_params, radial_rj_grp, group)
!
      use t_mesh_data
      use t_group_data
!
      use set_sph_node_group
      use set_sph_ele_group
      use set_sph_surf_group
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(sph_shell_parameters), intent(in) :: sph_params
      type(group_data), intent(in) :: radial_rj_grp
!
      type(mesh_groups), intent(inout) ::  group
!
!
!  Construct node group
      call count_sph_local_node_group                                   &
     &   (sph_params, radial_rj_grp, group%nod_grp)
!
      call allocate_grp_type_num(group%nod_grp)
      call count_sph_local_node_grp_item                                &
     &   (ip_r, ip_t, sph_params, radial_rj_grp, group%nod_grp)
!
      call s_cal_total_and_stacks(group%nod_grp%num_grp,                &
     &    group%nod_grp%nitem_grp, izero, group%nod_grp%istack_grp,     &
     &    group%nod_grp%num_item)
!
      call allocate_grp_type_item(group%nod_grp)
      call set_sph_local_node_grp_item                                  &
     &   (ip_r, ip_t, sph_params, radial_rj_grp, group%nod_grp)
!
!  Construct element group
      call allocate_sph_ele_grp_flag
      call count_sph_local_ele_group(group%ele_grp, radial_rj_grp)
!
      call allocate_grp_type_num(group%ele_grp)
      call count_sph_local_ele_grp_item                                 &
     &   (ip_r, ip_t, sph_params, radial_rj_grp, group%ele_grp)
!
      call s_cal_total_and_stacks(group%ele_grp%num_grp,                &
     &    group%ele_grp%nitem_grp, izero, group%ele_grp%istack_grp,     &
     &    group%ele_grp%num_item)
!
      call allocate_grp_type_item(group%ele_grp)
      call set_sph_local_ele_grp_item                                   &
     &   (ip_r, ip_t, sph_params, radial_rj_grp, group%ele_grp)
!
      call deallocate_sph_ele_grp_flag
!
!  Construct surf group
      call count_sph_local_surf_group(radial_rj_grp, group%surf_grp)
!
      call allocate_sf_grp_type_num(group%surf_grp)
      call count_sph_local_surf_grp_item                                &
     &   (ip_r, ip_t, sph_params, radial_rj_grp, group%surf_grp)
!
      call s_cal_total_and_stacks(group%surf_grp%num_grp,               &
     &    group%surf_grp%nitem_grp, izero, group%surf_grp%istack_grp,   &
     &    group%surf_grp%num_item)
!
      call allocate_sf_grp_type_item(group%surf_grp)
      call set_sph_local_surf_grp_item                                  &
     &   (ip_r, ip_t, sph_params, radial_rj_grp, group%surf_grp)
!
      end subroutine const_FEM_groups_for_sph
!
! -----------------------------------------------------------------------
!
      subroutine const_nod_comm_table_for_sph(ip_rank, ip_r, ip_t,      &
     &          nod_comm)
!
      use t_comm_table
      use m_sph_mesh_1d_connect
      use const_comm_tbl_4_sph_mesh
!
      integer(kind = kint), intent(in) :: ip_rank, ip_r, ip_t
      type(communication_table), intent(inout) :: nod_comm
!
! Count subdomain to communicate
      call count_neib_4_sph_mesh(ip_rank, ip_r, ip_t, nod_comm)
      call count_neib_4_sph_center_mesh(ip_rank, ip_r, ip_t, nod_comm)
!
      call allocate_type_comm_tbl_num(nod_comm)
!
! Set subdomain ID to communicate
      call set_neib_4_sph_mesh(ip_rank, ip_r, ip_t, nod_comm)
      call set_neib_4_sph_center_mesh(ip_rank, ip_r, ip_t, nod_comm)
!
! Count number of nodes to communicate
      call count_import_4_sph_mesh(ip_r, ip_t, nod_comm)
      call count_export_4_sph_mesh(ip_r, ip_t, nod_comm)
!
!
      call allocate_type_import_item(nod_comm)
      call allocate_type_export_item(nod_comm)
      call allocate_1d_comm_tbl_4_sph(nod_comm%ntot_import,             &
     &    nod_comm%ntot_export)
!
! set node ID to communicate
      call set_import_rtp_sph_mesh(ip_r, ip_t, nod_comm)
      call set_export_rtp_sph_mesh(ip_r, ip_t, nod_comm)
!
      call deallocate_1d_comm_tbl_4_sph
!
      end subroutine const_nod_comm_table_for_sph
!
! -----------------------------------------------------------------------
!
      end module set_FEM_mesh_4_sph
