!
!      module const_comm_table_fluid
!
!     Programmed by H.Matsui on July, 2002
!     Modified by H. Matsui on Sep., 2007
!     Modified by H. Matsui on Apr., 2008
!     Modified by H. Matsui on Dec., 2008
!
!      subroutine s_const_comm_table_fluid
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
      use m_machine_parameter
      use m_control_parameter
      use m_parallel_var_dof
      use m_nod_comm_table
      use m_comm_table_4_MHD
      use m_geometry_parameter
      use m_geometry_data
      use m_geometry_data_MHD
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
     &    iflag_nod, SOLVER_COMM, my_rank)
!
!
      call mark_reduced_neib_domain(num_neib,                           &
     &    ntot_import, ntot_export, id_neib,                            &
     &    istack_import, istack_export, item_import, item_export)
!
!
      call count_reduced_neib_domain(num_neib, id_neib,                 &
     &    neigh_pe_num_fl)
!
      call allocate_comm_stack_fluid
!
      call set_reduced_neib_domain(num_neib, id_neib,                   &
     &    neigh_pe_num_fl, neigh_pe_data_fl)
!
      call count_reduced_comm_stack(num_neib, ntot_import, id_neib,     &
     &    istack_import, item_import, neigh_pe_num_fl, ntot_import_fl, &
     &    num_import_fl, istack_import_fl)
      call count_reduced_comm_stack(num_neib, ntot_export, id_neib,     &
     &    istack_export, item_export, neigh_pe_num_fl, ntot_export_fl,  &
     &    num_export_fl, istack_export_fl)
!
      call allocate_comm_table_fluid
!
      call set_reduced_comm_item(num_neib, ntot_import, id_neib,        &
     &    istack_import, item_import, neigh_pe_num_fl,                  &
     &    ntot_import_fl, istack_import_fl, item_import_fl)
      call set_reduced_comm_item(num_neib, ntot_export, id_neib,        &
     &    istack_export, item_export, neigh_pe_num_fl,                  &
     &    ntot_export_fl, istack_export_fl, item_export_fl)
!
      if (iflag_debug.gt.0) then
        write(*,*)'neigh_pe_num_fl', my_rank, num_neib, neigh_pe_num_fl 
        write(*,*)'ntot_import_fl',  my_rank, ntot_import, ntot_import_fl
        write(*,*)'ntot_export_fl',  my_rank, ntot_export, ntot_export_fl
        write(*,*)'id_neib',  my_rank, id_neib
        write(*,*)'neigh_pe_data_fl',  my_rank, neigh_pe_data_fl
        write(*,*)'istack_import',  my_rank, istack_import
        write(*,*)'istack_import_fl',  my_rank, istack_import_fl
        write(*,*)'istack_export',  my_rank, istack_export
        write(*,*)'istack_export_fl',  my_rank, istack_export_fl
      end if
!
!       write(50+my_rank,*) 'i, globalnodid(i), iflag_nod(i)'
!       do i = 1, numnod
!         write(50+my_rank,*) i, globalnodid(i), iflag_nod(i)
!       end do
!       write(50+my_rank,*) 'i, item_import(i)'
!       do i = 1, ntot_import
!         write(50+my_rank,*) i, item_import(i), globalnodid(item_import(i))
!       end do
!       write(50+my_rank,*) 'i, item_export(i)'
!       do i = 1, ntot_export
!         write(50+my_rank,*) i, item_export(i), globalnodid(item_export(i))
!       end do
!
      call deallocate_flags_reduced_comm
!
      end subroutine s_const_comm_table_fluid
!
!------------------------------------------------------------------
!
      end module const_comm_table_fluid
