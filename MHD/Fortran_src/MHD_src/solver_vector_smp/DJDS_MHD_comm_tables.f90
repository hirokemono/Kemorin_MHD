!
!      module DJDS_MHD_comm_tables
!
!      Written by H. Matsui
!      Modified by H. Matsui on June, 2005
!      Modified by H. Matsui on Jan., 2006
!      Modified by H. Matsui on Sep., 2007
!      Modified by H. Matsui on Apr., 2008
!
!      subroutine set_new_comm_table_cd_l
!      subroutine set_new_comm_table_ins_l
!
      module DJDS_MHD_comm_tables
!
      use m_precision
!
      use m_geometry_parameter
      use DJDS_new_comm_table
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_new_comm_table_entire
!
      use m_nod_comm_table
      use m_solver_djds
!
       call allocate_new_comm_table(ntot_export)
!
       call set_new_comm_table(numnod, OLDtoNEW, num_neib,              &
                istack_export, item_export, NOD_EXPORT_NEW)
!
      end subroutine set_new_comm_table_entire
!
!-----------------------------------------------------------------------
!
      subroutine set_new_comm_table_fl
!
      use m_comm_table_4_MHD
      use m_solver_djds_fluid
!
       call allocate_new_comm_table_fl
!
       call set_new_comm_table(numnod, OLDtoNEW, neigh_pe_num_fl,       &
                istack_export_fl, item_export_fl, NOD_EXPORT_NEW_fl)
!
      end subroutine set_new_comm_table_fl
!
!-----------------------------------------------------------------------
!
!      subroutine set_new_comm_table_cd
!
!      use m_nod_comm_table
!      use m_solver_djds_conduct
!
!       call allocate_new_comm_table_cd
!
!       call set_new_comm_table(numnod, OLDtoNEW, num_neib,              &
!                istack_export, item_export, NOD_EXPORT_NEW_cd)
!
!      end subroutine set_new_comm_table_cd
!
!-----------------------------------------------------------------------
!
!      subroutine set_new_comm_table_ins
!
!      use m_nod_comm_table
!      use m_solver_djds_insulate
!
!       call allocate_new_comm_table_ins
!
!       call set_new_comm_table(numnod, OLDtoNEW, num_neib,              &
!                istack_export, item_export, NOD_EXPORT_NEW_ins)
!
!      end subroutine set_new_comm_table_ins
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_new_comm_table_l
!
      use m_nod_comm_table
      use m_solver_djds_linear
!
       call allocate_new_comm_table_l
!
       call set_new_comm_table(numnod, OLDtoNEW1, num_neib,             &
                istack_export, item_export, NOD_EXPORT_NEW1)
!
      end subroutine set_new_comm_table_l
!
!-----------------------------------------------------------------------
!
      subroutine set_new_comm_table_fl_l
!
      use m_comm_table_4_MHD
      use m_solver_djds_linear_fl
!
       call allocate_new_comm_table_fl_l
!
       call set_new_comm_table(numnod, OLDtoNEW1, neigh_pe_num_fl,      &
                istack_export_fl, item_export_fl, NOD_EXPORT_NEW_fl1)
!
      end subroutine set_new_comm_table_fl_l
!
!-----------------------------------------------------------------------
!
!      subroutine set_new_comm_table_cd_l
!
!      use m_nod_comm_table
!      use m_solver_djds_linear_cd
!
!       call allocate_new_comm_table_cd_l
!
!       call set_new_comm_table(numnod, OLDtoNEW1, num_neib,             &
!                istack_export, item_export, NOD_EXPORT_NEW_cd1)
!
!      end subroutine set_new_comm_table_cd_l
!
!-----------------------------------------------------------------------
!
!      subroutine set_new_comm_table_ins_l
!
!      use m_nod_comm_table
!      use m_solver_djds_linear_ins
!
!       call allocate_new_comm_table_ins_l
!
!       call set_new_comm_table(numnod, OLDtoNEW1, num_neib,             &
!                istack_export, item_export, NOD_EXPORT_NEW_ins1)
!
!      end subroutine set_new_comm_table_ins_l
!
!-----------------------------------------------------------------------
!
      end module DJDS_MHD_comm_tables
