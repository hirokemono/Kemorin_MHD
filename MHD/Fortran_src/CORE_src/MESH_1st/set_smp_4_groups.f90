!set_smp_4_groups.f90
!      module set_smp_4_groups
!
!     Written by H. Matsui on Sep., 2005
!
!      subroutine count_num_groups_4_smp
!
      module set_smp_4_groups
!
      use m_precision
!
      implicit none
!
      private :: count_bc_4_sheard_para, count_mat_4_sheard_para
      private :: count_surf_4_sheard_para
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_num_groups_4_smp
!
!
      call count_bc_4_sheard_para
      call count_mat_4_sheard_para
      call count_surf_4_sheard_para
!
      end subroutine count_num_groups_4_smp
!
!-----------------------------------------------------------------------
!
      subroutine count_bc_4_sheard_para
!
      use m_machine_parameter
      use m_node_group
      use cal_minmax_and_stacks
!
!
      num_bc_smp = np_smp*num_bc
!
      call allocate_boundary_param_smp
!
      call set_group_size_4_smp(np_smp, num_bc, bc_istack,              &
     &    ibc_smp_stack, max_bc_4_smp)
!
      end subroutine count_bc_4_sheard_para
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_mat_4_sheard_para
!
      use m_machine_parameter
      use m_element_group
      use cal_minmax_and_stacks
!
!
      ele_grp1%num_grp_smp = np_smp * ele_grp1%num_grp
!
      call allocate_grp_type_smp(ele_grp1)
!
      call set_group_size_4_smp                                         &
     &   (np_smp, ele_grp1%num_grp, ele_grp1%istack_grp,                &
     &    ele_grp1%istack_grp_smp, ele_grp1%max_grp_smp)
!
      end subroutine count_mat_4_sheard_para
!
!-----------------------------------------------------------------------
!
      subroutine count_surf_4_sheard_para
!
      use m_surface_group
      use set_smp_4_group_types
!
!
      call count_surf_grp_type_smp(sf_grp1)
!
      end subroutine count_surf_4_sheard_para
!
!-----------------------------------------------------------------------
!
      end module set_smp_4_groups
