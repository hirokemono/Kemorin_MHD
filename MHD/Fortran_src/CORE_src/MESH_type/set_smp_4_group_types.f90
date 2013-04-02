!
!      module set_smp_4_group_types
!
!     Written by H. Matsui on Dec., 2008
!
!      subroutine count_num_groups_type_smp(group)
!        type(mesh_groups), intent(inout) :: group
!
!      subroutine count_surf_nod_grp_type_smp(sf_grp, sf_nod)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(surface_node_grp_data), intent(inout) :: sf_nod
!
      module set_smp_4_group_types
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
      private :: count_surf_grp_type_smp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_num_groups_type_smp(group)
!
      use t_mesh_data
      use cal_minmax_and_stacks
!
      type(mesh_groups), intent(inout) :: group
!
!
      call count_grp_type_smp(group%nod_grp)
      call count_grp_type_smp(group%ele_grp)
      call count_surf_grp_type_smp(group%surf_grp)
!
      end subroutine count_num_groups_type_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_grp_type_smp(grp)
!
      use t_group_data
      use cal_minmax_and_stacks
!
      type(group_data), intent(inout) :: grp
!
!
      grp%num_grp_smp = np_smp * grp%num_grp
!
      call allocate_grp_type_smp(grp)
!
      call set_group_size_4_smp(np_smp, grp%num_grp, grp%istack_grp,    &
     &    grp%istack_grp_smp, grp%max_grp_smp)
!
      end subroutine count_grp_type_smp
!
!-----------------------------------------------------------------------
!
      subroutine count_surf_grp_type_smp(sf_grp)
!
      use t_group_data
      use cal_minmax_and_stacks
!
      type(surface_group_data), intent(inout) :: sf_grp
!
!
      sf_grp%num_grp_smp = np_smp * sf_grp%num_grp
!
      call allocate_sf_grp_type_smp(sf_grp)
!
      call set_group_size_4_smp(np_smp,                                 &
     &    sf_grp%num_grp, sf_grp%istack_grp,                            &
     &    sf_grp%istack_grp_smp, sf_grp%max_grp_smp)
!
      end subroutine count_surf_grp_type_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_surf_nod_grp_type_smp(sf_grp, sf_nod)
!
      use t_group_data
      use t_surface_group_connect
      use cal_minmax_and_stacks
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(inout) :: sf_nod
!
!
     call alloc_num_surf_grp_nod_smp(sf_grp, sf_nod)
!
      call set_group_size_4_smp(np_smp,                                 &
     &    sf_grp%num_grp, sf_nod%inod_stack_sf_grp,                     &
     &    sf_nod%istack_surf_nod_smp, sf_nod%max_sf_nod_4_smp)
!
      end subroutine count_surf_nod_grp_type_smp
!
!-----------------------------------------------------------------------
!
      end module set_smp_4_group_types
