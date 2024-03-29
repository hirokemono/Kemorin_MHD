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
!!      subroutine s_const_comm_table_fluid                             &
!!     &         (num_pe, mesh, fluid_mesh, fluid_comm)
!!        type(mesh_geometry),    intent(in) :: mesh
!!        type(field_geometry_data), intent(in) :: fluid_mesh
!!        type(communication_table), intent(inout) :: fluid_comm
!!@endverbatim
!
      module const_comm_table_fluid
!
      use m_precision
      use m_machine_parameter
!
      implicit  none
!
      private :: set_const_comm_table_fluid
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine s_const_comm_table_fluid                               &
     &         (num_pe, mesh, fluid_mesh, fluid_comm)
!
      use t_mesh_data
      use t_geometry_data_MHD
!
      integer, intent(in) :: num_pe
      type(mesh_geometry), intent(in) :: mesh
      type(field_geometry_data), intent(in) :: fluid_mesh
!
      type(communication_table), intent(inout) :: fluid_comm
!
!
      if (mesh%node%numnod .eq. 0) then
        call empty_comm_table(fluid_comm)
      else
        call set_const_comm_table_fluid                                 &
     &     (num_pe, fluid_mesh%istack_ele_fld_smp,                      &
     &      mesh%node, mesh%ele, mesh%nod_comm, fluid_comm)
      end if
!
      end subroutine s_const_comm_table_fluid
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_const_comm_table_fluid(num_pe, iele_fl_smp_stack,  &
     &          node, ele, nod_comm, fluid_comm)
!
      use calypso_mpi
      use t_comm_table
      use t_geometry_data
!
      use set_comm_table_fluid
      use solver_SR_type
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: iele_fl_smp_stack(0:np_smp)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
!
      type(communication_table), intent(inout) :: fluid_comm
!
!
      call allocate_flags_reduced_comm(num_pe, node%numnod)
!
      call mark_4_fluid_nod_by_ele(ele%numele, ele%nnod_4_ele, ele%ie,  &
     &    iele_fl_smp_stack(0), iele_fl_smp_stack(np_smp) )
!
      call SOLVER_SEND_RECV_int_type(node%numnod, nod_comm, iflag_nod)
!
!
      call mark_reduced_neib_domain(nod_comm%num_neib,                  &
     &    nod_comm%ntot_import, nod_comm%ntot_export, nod_comm%id_neib, &
     &    nod_comm%istack_import, nod_comm%istack_export,               &
     &    nod_comm%item_import, nod_comm%item_export)
!
!
      call count_reduced_neib_domain                                    &
     &   (nod_comm%num_neib, nod_comm%id_neib, fluid_comm%num_neib)
!
      call alloc_comm_table_num(fluid_comm)
!
      call set_reduced_neib_domain(nod_comm%num_neib, nod_comm%id_neib, &
     &    fluid_comm%num_neib, fluid_comm%id_neib)
!
      call count_reduced_comm_stack                                     &
     &   (nod_comm%num_neib, nod_comm%ntot_import, nod_comm%id_neib,    &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    fluid_comm%num_neib, fluid_comm%ntot_import,                  &
     &    fluid_comm%num_import, fluid_comm%istack_import)
      call count_reduced_comm_stack                                     &
     &   (nod_comm%num_neib, nod_comm%ntot_export, nod_comm%id_neib,    &
     &    nod_comm%istack_export, nod_comm%item_export,                 &
     &    fluid_comm%num_neib, fluid_comm%ntot_export,                  &
     &    fluid_comm%num_export, fluid_comm%istack_export)
!
      call alloc_comm_table_item(fluid_comm)
!
      call set_reduced_comm_item                                        &
     &   (nod_comm%num_neib, nod_comm%ntot_import, nod_comm%id_neib,    &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    fluid_comm%num_neib, fluid_comm%ntot_import,                  &
     &    fluid_comm%istack_import, fluid_comm%item_import)
      call set_reduced_comm_item                                        &
     &   (nod_comm%num_neib, nod_comm%ntot_export, nod_comm%id_neib,    &
     &    nod_comm%istack_export, nod_comm%item_export,                 &
     &    fluid_comm%num_neib, fluid_comm%ntot_export,                  &
     &    fluid_comm%istack_export, fluid_comm%item_export)
!
      if (iflag_debug.ge.2) then
        call compare_comm_table_stacks(my_rank, nod_comm, fluid_comm)
      end if
!
!       write(50+my_rank,*) 'i, inod_global(i), iflag_nod(i)'
!       do i = 1, node%numnod
!         write(50+my_rank,*) i, node%inod_global(i), iflag_nod(i)
!       end do
!       write(50+my_rank,*) 'i, item_import(i)'
!       do i = 1, nod_comm%ntot_import
!         write(50+my_rank,*) i, nod_comm%item_import(i),               &
!     &                  node%inod_global(nod_comm%item_import(i))
!       end do
!       write(50+my_rank,*) 'i, nod_comm%item_export(i)'
!       do i = 1, nod_comm%ntot_export
!         write(50+my_rank,*) i, nod_comm%item_export(i),               &
!     &                  node%inod_global(nod_comm%item_export(i))
!       end do
!
      call deallocate_flags_reduced_comm
!
      end subroutine set_const_comm_table_fluid
!
!------------------------------------------------------------------
!
      end module const_comm_table_fluid
