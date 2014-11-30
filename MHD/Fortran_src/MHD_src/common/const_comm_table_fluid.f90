!>@file   const_comm_table_fluid.f90
!!@brief  module const_comm_table_fluid
!!
!!@author H. Matsui
!!@date     Programmed by H.Matsui in July, 2002
!!@n     Modified by H. Matsui in Sep., 2007
!!@n     Modified by H. Matsui in Apr., 2008
!!@n     Modified by H. Matsui in Dec., 2008
!!@n     Modified by H. Matsui in Dec., 2008
!
!>     Construct communication table for fluid region
!!
!!@verbatim
!!      subroutine s_const_comm_table_fluid
!!      subroutine deallocate_comm_table_fluid
!!@endverbatim
!
      module const_comm_table_fluid
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
      subroutine s_const_comm_table_fluid
!
      use calypso_mpi
      use t_comm_table
      use m_machine_parameter
      use m_control_parameter
      use m_nod_comm_table
      use m_geometry_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_solver_djds_MHD
      use set_comm_table_fluid
      use solver_SR_int
!
!
      call allocate_flags_reduced_comm(nprocs, numnod)
!
      call mark_4_fluid_nod_by_ele(numele, nnod_4_ele, ie,              &
     &    iele_fl_smp_stack(0), iele_fl_smp_stack(np_smp) )
!
      call solver_send_recv_i(numnod, num_neib, id_neib,                &
     &    istack_import, item_import, istack_export, item_export,       &
     &    iflag_nod)
!
!
      call mark_reduced_neib_domain(num_neib,                           &
     &    ntot_import, ntot_export, id_neib,                            &
     &    istack_import, istack_export, item_import, item_export)
!
!
      call count_reduced_neib_domain(num_neib, id_neib,                 &
     &    DJDS_comm_fl%num_neib)
!
      call allocate_type_comm_tbl_num(DJDS_comm_fl)
!
      call set_reduced_neib_domain(num_neib, id_neib,                   &
     &    DJDS_comm_fl%num_neib, DJDS_comm_fl%id_neib)
!
      call count_reduced_comm_stack                                     &
     &   (num_neib, ntot_import, id_neib, istack_import, item_import,   &
     &    DJDS_comm_fl%num_neib, DJDS_comm_fl%ntot_import,              &
     &    DJDS_comm_fl%num_import, DJDS_comm_fl%istack_import)
      call count_reduced_comm_stack                                     &
     &   (num_neib, ntot_export, id_neib, istack_export, item_export,   &
     &    DJDS_comm_fl%num_neib, DJDS_comm_fl%ntot_export,              &
     &    DJDS_comm_fl%num_export, DJDS_comm_fl%istack_export)
!
      call allocate_type_comm_tbl_item(DJDS_comm_fl)
!
      call set_reduced_comm_item                                        &
     &   (num_neib, ntot_import, id_neib, istack_import, item_import,   &
     &    DJDS_comm_fl%num_neib, DJDS_comm_fl%ntot_import,              &
     &    DJDS_comm_fl%istack_import, DJDS_comm_fl%item_import)
      call set_reduced_comm_item                                        &
     &   (num_neib, ntot_export, id_neib, istack_export, item_export,   &
     &    DJDS_comm_fl%num_neib, DJDS_comm_fl%ntot_export,              &
     &    DJDS_comm_fl%istack_export, DJDS_comm_fl%item_export)
!
      if (iflag_debug.ge.2) then
        write(*,*)'DJDS_comm_fl%num_neib',                              &
     &       my_rank, num_neib, DJDS_comm_fl%num_neib 
        write(*,*)'DJDS_comm_fl%ntot_import',                           &
     &       my_rank, ntot_import, DJDS_comm_fl%ntot_import
        write(*,*)'DJDS_comm_fl%ntot_export',                           &
     &       my_rank, ntot_export, DJDS_comm_fl%ntot_export
        write(*,*)'id_neib',  my_rank, id_neib
        write(*,*)'DJDS_comm_fl%id_neib', my_rank, DJDS_comm_fl%id_neib
        write(*,*)'istack_import',  my_rank, istack_import
        write(*,*)'DJDS_comm_fl%istack_import',                         &
     &       my_rank, DJDS_comm_fl%istack_import
        write(*,*)'istack_export',  my_rank, istack_export
        write(*,*)'DJDS_comm_fl%istack_export',                         &
     &       my_rank, DJDS_comm_fl%istack_export
      end if
!
!       write(50+my_rank,*) 'i, inod_global(i), iflag_nod(i)'
!       do i = 1, numnod
!         write(50+my_rank,*) i, inod_global(i), iflag_nod(i)
!       end do
!       write(50+my_rank,*) 'i, item_import(i)'
!       do i = 1, ntot_import
!         write(50+my_rank,*) i, item_import(i), inod_global(item_import(i))
!       end do
!       write(50+my_rank,*) 'i, item_export(i)'
!       do i = 1, ntot_export
!         write(50+my_rank,*) i, item_export(i), inod_global(item_export(i))
!       end do
!
      call deallocate_flags_reduced_comm
!
      end subroutine s_const_comm_table_fluid
!
!------------------------------------------------------------------
!
      subroutine deallocate_comm_table_fluid
!
      use t_comm_table
      use m_solver_djds_MHD
!
!
      call deallocate_type_comm_tbl(DJDS_comm_fl)
!
      end subroutine deallocate_comm_table_fluid
!
!  ---------------------------------------------------------------------
!
      end module const_comm_table_fluid
