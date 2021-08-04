!>@file  t_lic_repart_reference.f90
!!       module t_lic_repart_reference
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine alloc_lic_repart_ref(node, rep_ref)
!!      subroutine dealloc_lic_repart_ref(rep_ref)
!!        type(lic_repart_reference), intent(inout) :: rep_ref
!!
!!      subroutine bring_back_rendering_time                            &
!!     &         (mesh, weight_prev, elapse_ray_trace, mesh_to_viz_tbl, &
!!     &          viz_mesh, field_lic, nod_fld_lic, ref_repart_mesh,    &
!!     &          m_SR)
!!        real(kind = kreal), intent(in) :: weight_prev
!!        real(kind = kreal), intent(in) :: elapse_ray_trace(2)
!!        type(calypso_comm_table), intent(in) :: mesh_to_viz_tbl
!!        type(mesh_geometry), intent(in) :: viz_mesh
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(lic_field_data), intent(inout) :: field_lic
!!        type(lic_field_data), intent(inout) :: nod_fld_lic
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: ref_repart_mesh(mesh%node%numnod,2)
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module t_lic_repart_reference
!
      use m_precision
      use m_constants
!
      implicit  none
!
!>  Structure for reference for LIC repartition
      type lic_repart_reference
!>    Work area for elapsed transfer time
        real(kind = kreal), allocatable :: elapse_rtrace_nod(:,:)
      end type lic_repart_reference
!
      private :: copy_average_elapsed_to_nod
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_lic_repart_ref(node, rep_ref)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: node
      type(lic_repart_reference), intent(inout) :: rep_ref
!
!
        allocate(rep_ref%elapse_rtrace_nod(node%numnod,2))
        if(node%numnod .gt. 0) then
!$omp parallel workshare
          rep_ref%elapse_rtrace_nod(1:node%numnod,1:2) = 1.0d0
!$omp end parallel workshare
        end if
!
      end subroutine alloc_lic_repart_ref
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_lic_repart_ref(rep_ref)
!
      type(lic_repart_reference), intent(inout) :: rep_ref
!
!
      if(allocated(rep_ref%elapse_rtrace_nod)) then
        deallocate(rep_ref%elapse_rtrace_nod)
      end if
!
      end subroutine dealloc_lic_repart_ref
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bring_back_rendering_time                              &
     &         (mesh, weight_prev, elapse_ray_trace, mesh_to_viz_tbl,   &
     &          rep_ref, SR_sig)
!
      use t_solver_SR
      use t_mesh_data
      use t_calypso_comm_table
      use calypso_reverse_send_recv
!
      real(kind = kreal), intent(in) :: weight_prev
      real(kind = kreal), intent(in) :: elapse_ray_trace(2)
      type(calypso_comm_table), intent(in) :: mesh_to_viz_tbl
      type(mesh_geometry), intent(in) :: mesh
!
      type(lic_repart_reference), intent(inout) :: rep_ref
      type(send_recv_status), intent(inout) :: SR_sig
!
      real(kind = kreal), allocatable :: elapse_rtraces_pe(:,:)
!
!
      allocate(elapse_rtraces_pe(2,mesh_to_viz_tbl%nrank_export))
      call calypso_gather_reverse_SR(itwo, mesh_to_viz_tbl,             &
     &    elapse_ray_trace, elapse_rtraces_pe(1,1), SR_sig)
      call copy_average_elapsed_to_nod(mesh%node, mesh_to_viz_tbl,      &
     &    weight_prev, elapse_rtraces_pe, rep_ref%elapse_rtrace_nod)
      deallocate(elapse_rtraces_pe)
!
      end subroutine bring_back_rendering_time
!
!  ---------------------------------------------------------------------
!
      subroutine copy_average_elapsed_to_nod(node, mesh_to_viz_tbl,     &
     &          weight_prev, elapse_rtraces_pe, ref_repart_mesh)
!
      use t_geometry_data
      use t_calypso_comm_table
!
      type(node_data), intent(in) :: node
      type(calypso_comm_table), intent(in) :: mesh_to_viz_tbl
      real(kind = kreal), intent(in) :: weight_prev
      real(kind = kreal), intent(in)                                    &
     &             :: elapse_rtraces_pe(2,mesh_to_viz_tbl%nrank_export)
      real(kind = kreal), intent(inout)                                 &
     &             :: ref_repart_mesh(node%numnod,2)
!
      integer(kind = kint) :: ip, ist, ied, inum, inod
!
!
      do ip = 1, mesh_to_viz_tbl%nrank_export
        ist = mesh_to_viz_tbl%istack_export(ip-1) + 1
        ied = mesh_to_viz_tbl%istack_export(ip)
!$omp parallel do private(inum,inod)
        do inum = ist, ied
          inod = mesh_to_viz_tbl%item_export(inum)
          ref_repart_mesh(inod,1)                                       &
     &      = weight_prev * elapse_rtraces_pe(1,ip)                     &
     &       + (1.0d0 - weight_prev) * ref_repart_mesh(inod,1)
          ref_repart_mesh(inod,2)                                       &
     &      = weight_prev * elapse_rtraces_pe(2,ip)                     &
     &       + (1.0d0 - weight_prev) * ref_repart_mesh(inod,2)
        end do
      end do
!
      end subroutine copy_average_elapsed_to_nod
!
!-----------------------------------------------------------------------
!
      end module t_lic_repart_reference
