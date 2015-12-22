!
!      module count_smp_size_4_MHD
!
!     Written by H. Matsui on Sep., 2005
!     Modified by H. Matsui on Aug., 2006
!     Modified by H. Matsui on Dec., 2008
!
!      subroutine count_smp_size_4_fluid
!      subroutine count_smp_size_4_conduct
!      subroutine count_smp_size_4_insulator
!      subroutine count_smp_size_4_inner_core
!
      module count_smp_size_4_MHD
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_geometry_data_MHD
      use cal_minmax_and_stacks
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_smp_size_4_fluid
!
!
       call allocate_geometry_fluid_smp
!
       call count_number_4_smp(np_smp, ione, fluid1%numnod_fld,         &
     &     inod_fl_smp_stack, maxnod_fl_smp )
!
       call count_number_4_smp(np_smp, ione, fluid1%internal_node_fld,  &
     &     inter_fl_smp_stack, max_in_nod_fl_smp )
!
       call count_number_4_smp                                          &
     &    (np_smp, fluid1%iele_start_fld, fluid1%iele_end_fld,          &
     &     fluid1%istack_ele_fld_smp, maxele_fl_smp )
!
      end subroutine count_smp_size_4_fluid
!
!-----------------------------------------------------------------------
!
      subroutine count_smp_size_4_conduct
!
!
      call allocate_geometry_conduct_smp
!
       call count_number_4_smp(np_smp, ione, conduct1%numnod_fld,       &
     &     inod_cd_smp_stack, maxnod_cd_smp )
!
       call count_number_4_smp                                          &
     &    (np_smp, ione, conduct1%internal_node_fld,                    &
     &     inter_cd_smp_stack, max_in_nod_cd_smp )
!
       call count_number_4_smp                                          &
     &    (np_smp, conduct1%iele_start_fld, conduct1%iele_end_fld,      &
     &     conduct1%istack_ele_fld_smp, maxele_cd_smp )
!
      end subroutine count_smp_size_4_conduct
!
!-----------------------------------------------------------------------
!
      subroutine count_smp_size_4_insulator
!
!
      call allocate_geometry_ins_smp
!
       call count_number_4_smp(np_smp, ione, insulate1%numnod_fld,      &
     &     inod_ins_smp_stack, maxnod_ins_smp )
!
       call count_number_4_smp                                          &
     &    (np_smp, ione, insulate1%internal_node_fld,                   &
     &     inter_ins_smp_stack, max_in_nod_ins_smp )
!
       call count_number_4_smp                                          &
     &    (np_smp, insulate1%iele_start_fld, insulate1%iele_end_fld,    &
     &     insulate1%istack_ele_fld_smp, maxele_ins_smp)
!
      end subroutine count_smp_size_4_insulator
!
!-----------------------------------------------------------------------
!
      subroutine count_smp_size_4_inner_core
!
!
      call allocate_geometry_incore_smp
!
       call count_number_4_smp(np_smp, ione, inner_core%numnod_fld,     &
     &     inod_in_core_smp_stack, maxnod_in_core_smp )
!
       call count_number_4_smp                                          &
     &    (np_smp, ione, inner_core%internal_node_fld,                  &
     &     inter_in_core_smp_stack, max_in_nod_in_core_smp )
!
       call count_number_4_smp(np_smp, ione, numele_in_core,            &
     &     inner_core%istack_ele_fld_smp, maxele_in_core_smp )
!
      end subroutine count_smp_size_4_inner_core
!
!-----------------------------------------------------------------------
!
      end module count_smp_size_4_MHD
