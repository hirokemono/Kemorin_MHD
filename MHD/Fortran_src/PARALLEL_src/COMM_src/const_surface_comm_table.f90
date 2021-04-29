!>@file   const_surface_comm_table.f90
!!@brief  module const_surface_comm_table
!!
!!@author H. Matsui
!!@date Programmed in June, 2015
!
!> @brief Belonged element list for each node
!!
!!@verbatim
!!      subroutine const_surf_comm_table                                &
!!     &         (node, surf, nod_comm, surf_comm, surf_gl)
!!      subroutine dealloc_surf_comm_table(surf_comm, surf_gl)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(inout) :: surf_comm
!!        type(surface_data), intent(inout) :: surf
!!        type(global_surface_data), intent(inout) :: surf_gl
!!      subroutine dup_global_surface_id                                &
!!     &         (org_surf_comm, org_surf_gl, new_surf,                 &
!!     &         new_surf_comm, new_surf_gl)
!!        type(communication_table), intent(in) :: org_surf_comm
!!        type(global_surface_data), intent(in) :: org_surf_gl
!!        type(surface_data), intent(in) :: new_surf
!!        type(communication_table), intent(inout) :: new_surf_comm
!!        type(global_surface_data), intent(inout) :: new_surf_gl
!!      subroutine copy_global_surface_id                               &
!!     &         (org_surf_gl, numsurf, isurf_global, interior_surf)
!!        type(global_surface_data), intent(inout) :: org_surf_gl
!!        integer(kind=kint), intent(in) ::  numsurf
!!        integer(kind=kint_gl), intent(inout) :: isurf_global(numsurf)
!!        integer(kind=kint), intent(inout) ::    interior_surf(numsurf)
!!
!!      subroutine surf_send_recv_test                                  &
!!     &         (node, surf, surf_gl, surf_comm, surf_check)
!!        type(node_data), intent(in) :: node
!!        type(surface_data), intent(in) :: surf
!!        type(global_surface_data), intent(in) :: surf_gl
!!        type(communication_table), intent(in) :: surf_comm
!!        type(work_for_comm_check), intent(inout) :: surf_check
!!@endverbatim
!
      module const_surface_comm_table
!
      use m_precision
      use calypso_mpi
      use t_next_node_ele_4_node
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_comm_table
      use t_failed_export_list
!
      use m_machine_parameter
      use m_geometry_constants
!
      implicit none
!
      character(len=kchara), parameter :: txt_surf = 'surface'
!
      private :: txt_surf
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_surf_comm_table                                  &
     &         (node, surf, nod_comm, surf_comm, surf_gl)
!
      use t_para_double_numbering
      use t_element_double_number
      use t_const_comm_table
      use set_ele_id_4_node_type
      use const_global_element_ids
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(surface_data), intent(in) :: surf
      type(global_surface_data), intent(inout) :: surf_gl
      type(communication_table), intent(inout) :: surf_comm
!
      type(node_ele_double_number) :: inod_dbl
      type(element_double_number) :: isurf_dbl
      type(element_around_node) :: neib_surf
      type(failed_table) :: fail_tbl_s
!
      integer(kind = kint) :: internal_num = 0
      integer(kind = kint_gl), allocatable :: istack_inersurf(:)
!
!
      call alloc_interior_surf(surf, surf_gl)
!
      call alloc_double_numbering(node%numnod, inod_dbl)
      call set_node_double_numbering(node, nod_comm, inod_dbl)
!
      call alloc_ele_double_number(surf%numsurf, isurf_dbl)
      call find_belonged_pe_4_surf(my_rank, inod_dbl,                   &
     &    surf%numsurf, surf%nnod_4_surf, surf%ie_surf,                 &
     &    internal_num, surf_gl%interior_surf, isurf_dbl)
!
      call set_surf_id_4_node(node, surf, neib_surf)
!
      call alloc_failed_export(0, fail_tbl_s)
      call const_comm_table_by_connenct                                 &
     &   (txt_surf, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,       &
     &    surf%x_surf, node, nod_comm, inod_dbl, isurf_dbl,             &
     &    neib_surf, surf_comm, fail_tbl_s)
      call dealloc_ele_double_number(isurf_dbl)
      call dealloc_double_numbering(inod_dbl)
      call dealloc_iele_belonged(neib_surf)
      call dealloc_failed_export(fail_tbl_s)
!
      allocate(istack_inersurf(0:nprocs))
      istack_inersurf(0:nprocs) = 0
!
      call count_number_of_node_stack(internal_num, istack_inersurf)
      call set_global_ele_id                                            &
     &   (txt_surf, surf%numsurf, istack_inersurf,                      &
     &    surf_gl%interior_surf, surf_comm, surf_gl%isurf_global)
      deallocate(istack_inersurf)
!
      end subroutine const_surf_comm_table
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_surf_comm_table(surf_comm, surf_gl)
!
      type(communication_table), intent(inout) :: surf_comm
      type(global_surface_data), intent(inout) :: surf_gl
!
      call dealloc_comm_table(surf_comm)
      call dealloc_interior_surf(surf_gl)
!
      end subroutine dealloc_surf_comm_table
!
!-----------------------------------------------------------------------
!
      subroutine dup_global_surface_id                                  &
     &         (org_surf_comm, org_surf_gl, new_surf,                   &
     &         new_surf_comm, new_surf_gl)
!
      type(communication_table), intent(in) :: org_surf_comm
      type(global_surface_data), intent(in) :: org_surf_gl
      type(surface_data), intent(in) :: new_surf
!
      type(communication_table), intent(inout) :: new_surf_comm
      type(global_surface_data), intent(inout) :: new_surf_gl
!
!
      call copy_comm_tbl_type(org_surf_comm, new_surf_comm)
      call alloc_interior_surf(new_surf, new_surf_gl)
      call copy_global_surface_id(org_surf_gl, new_surf%numsurf,        &
     &    new_surf_gl%isurf_global, new_surf_gl%interior_surf)
!
      end subroutine dup_global_surface_id
!
!-----------------------------------------------------------------------
!
      subroutine copy_global_surface_id                                 &
     &         (org_surf_gl, numsurf, isurf_global, interior_surf)
!
      type(global_surface_data), intent(in) :: org_surf_gl
      integer(kind=kint), intent(in) ::  numsurf
      integer(kind=kint_gl), intent(inout) :: isurf_global(numsurf)
      integer(kind=kint), intent(inout) ::    interior_surf(numsurf)
!
!
      if(numsurf .le. 0) return
!$omp parallel workshare
      isurf_global(1:numsurf) =  org_surf_gl%isurf_global(1:numsurf)
      interior_surf(1:numsurf) = org_surf_gl%interior_surf(1:numsurf)
!$omp end parallel workshare
!
      end subroutine copy_global_surface_id
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine surf_send_recv_test                                    &
     &         (surf, surf_gl, surf_comm, surf_check)
!
      use t_work_for_comm_check
      use m_solver_SR
      use diff_geometory_comm_test
      use nod_phys_send_recv
      use solver_SR_type
      use mesh_send_recv_check
      use const_element_comm_tables
!
      type(surface_data), intent(in) :: surf
      type(global_surface_data), intent(in) :: surf_gl
      type(communication_table), intent(in) :: surf_comm
      type(work_for_comm_check), intent(inout) :: surf_check
!
!
      call alloc_geom_4_comm_test(surf%numsurf, surf_check)
      call set_element_4_comm_test(surf%numsurf, surf_gl%interior_surf, &
     &                             surf%x_surf, surf_check%xx_test)
      call SOLVER_SEND_RECV_3_type(surf%numsurf, surf_comm,             &
     &                             SR_sig1, SR_r1, surf_check%xx_test)
!
      call ele_send_recv_check                                          &
     &   (surf%numsurf, surf_gl%isurf_global, surf%x_surf, surf_check)
!
      if(i_debug .gt. 0)  write(*,*) my_rank,                           &
     &     'Failed communication for surface', surf_check%num_diff
      call collect_failed_comm(surf_check)
      if(my_rank .eq. 0) write(*,*) my_rank,                            &
     &   'Total Failed communication for surface',                      &
     &    surf_check%istack_diff_pe(nprocs)
!
      end subroutine surf_send_recv_test
!
! ----------------------------------------------------------------------
!
      end module const_surface_comm_table
