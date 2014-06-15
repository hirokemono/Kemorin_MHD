!set_smp_4_2nd_groups.f90
!     module set_smp_4_2nd_groups
!
!      Written by H. Matsui on Sep., 2005
!
!      subroutine count_num_2nd_groups_4_smp
!
      module set_smp_4_2nd_groups
!
      use m_precision
!
      use m_machine_parameter
      use m_2nd_group_data
      use cal_minmax_and_stacks
!
      implicit none
!
      private :: count_num_2nd_nod_grp_smp, count_num_2nd_ele_grp_smp
      private :: count_num_2nd_surf_grp_smp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_num_2nd_groups_4_smp
!
!
      call count_num_2nd_nod_grp_smp
      call count_num_2nd_ele_grp_smp
      call count_num_2nd_surf_grp_smp
!
      end subroutine count_num_2nd_groups_4_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_num_2nd_nod_grp_smp
!
!
      num_bc_2nd_smp = np_smp*num_bc_2nd
!
      call allocate_2nd_node_grp_num_smp
!
      call set_group_size_4_smp(np_smp, num_bc_2nd, bc_istack_2nd,      &
     &    ibc_smp_2nd_stack, max_bc_2nd_4_smp)
!
      end subroutine count_num_2nd_nod_grp_smp
!
!-----------------------------------------------------------------------
!
      subroutine count_num_2nd_ele_grp_smp
!
!
      num_mat_2nd_smp = np_smp*num_mat_2nd
!
      call allocate_2nd_ele_grp_num_smp
!
      call set_group_size_4_smp(np_smp, num_mat_2nd, mat_istack_2nd,    &
     &    imat_smp_2nd_stack, max_mat_2nd_4_smp)
!
      end subroutine count_num_2nd_ele_grp_smp
!
!-----------------------------------------------------------------------
!
      subroutine count_num_2nd_surf_grp_smp
!
!
      num_surf_2nd_smp = np_smp*num_surf_2nd
!
      call allocate_2nd_surf_grp_num_smp
!
      call set_group_size_4_smp(np_smp, num_surf_2nd, surf_istack_2nd,  &
     &    isurf_grp_2nd_smp_stack, max_sf_grp_2nd_4_smp)
!
      end subroutine count_num_2nd_surf_grp_smp
!
!-----------------------------------------------------------------------
!
      end module set_smp_4_2nd_groups
