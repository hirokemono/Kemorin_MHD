!>@file  t_lic_repart_reference.f90
!!       module t_lic_repart_reference
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine init_lic_repart_ref(mesh, rep_ref)
!!      subroutine dealloc_lic_repart_ref(rep_ref)
!!        type(lic_repart_reference), intent(inout) :: rep_ref
!!        type(mesh_geometry), intent(in) :: mesh
!!
!!      subroutine bring_back_rendering_time                            &
!!     &         (mesh, each_part_p, viz_mesh, mesh_to_viz_tbl,         &
!!     &          count_int_nod, count_line_int_now, rep_ref, m_SR)
!!        type(volume_partioning_param), intent(in) :: each_part_p
!!        type(calypso_comm_table), intent(in) :: mesh_to_viz_tbl
!!        type(mesh_geometry), intent(in) :: viz_mesh, mesh
!!        real(kind = kreal), intent(inout)                             &
!!     &                     :: count_int_nod(viz_mesh%node%numnod)
!!        real(kind = kreal), intent(inout)                             &
!!     &                     :: count_line_int_now(mesh%node%numnod)
!!        type(lic_repart_reference), intent(inout) :: rep_ref
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine set_average_line_int_time(mesh, each_part_p,         &
!!     &          elapse_ray_trace, mesh_to_viz_tbl, rep_ref, m_SR)
!!        type(volume_partioning_param), intent(in) :: each_part_p
!!        real(kind = kreal), intent(in) :: elapse_ray_trace(2)
!!        type(calypso_comm_table), intent(in) :: mesh_to_viz_tbl
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(lic_repart_reference), intent(inout) :: rep_ref
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
        real(kind = kreal), allocatable :: elapse_rtrace_nod(:)
      end type lic_repart_reference
!
      private :: alloc_lic_repart_ref, copy_average_elapsed_to_nod
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_lic_repart_ref(mesh, rep_ref)
!
      use t_mesh_data
      use int_volume_of_single_domain
!
      type(mesh_geometry), intent(in) :: mesh
      type(lic_repart_reference), intent(inout) :: rep_ref
!
!
      call alloc_lic_repart_ref(mesh%node, rep_ref)
      call cal_node_volue(mesh%node, mesh%ele,                          &
     &                    rep_ref%elapse_rtrace_nod)
!
      end subroutine init_lic_repart_ref
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
      allocate(rep_ref%elapse_rtrace_nod(node%numnod))
      if(node%numnod .gt. 0) then
!$omp parallel workshare
        rep_ref%elapse_rtrace_nod(1:node%numnod) = 1.0d0
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
     &         (mesh, each_part_p, viz_mesh, mesh_to_viz_tbl,           &
     &          count_int_nod, count_line_int_now, rep_ref, m_SR)
!
      use t_mesh_SR
      use t_mesh_data
      use t_calypso_comm_table
      use t_control_param_vol_grping
      use calypso_reverse_send_recv
!
      type(volume_partioning_param), intent(in) :: each_part_p
      type(calypso_comm_table), intent(in) :: mesh_to_viz_tbl
      type(mesh_geometry), intent(in) :: viz_mesh, mesh
      real(kind = kreal), intent(inout)                                 &
     &                     :: count_int_nod(viz_mesh%node%numnod)
!
      real(kind = kreal), intent(inout)                                 &
     &                     :: count_line_int_now(mesh%node%numnod)
      type(lic_repart_reference), intent(inout) :: rep_ref
      type(mesh_SR), intent(inout) :: m_SR
!
!
      call calypso_reverse_SR_1(mesh_to_viz_tbl,                        &
     &    viz_mesh%node%numnod, mesh%node%numnod,                       &
     &    count_int_nod, count_line_int_now, m_SR%SR_sig, m_SR%SR_r)
!
      call copy_line_integration_count                                  &
     &   (mesh%node, each_part_p%weight_prev,                           &
     &    count_line_int_now, rep_ref%elapse_rtrace_nod)
!
      end subroutine bring_back_rendering_time
!
!  ---------------------------------------------------------------------
!
      subroutine set_average_line_int_time(mesh, each_part_p,           &
     &          elapse_ray_trace, mesh_to_viz_tbl, rep_ref, m_SR)
!
      use t_mesh_SR
      use t_mesh_data
      use t_calypso_comm_table
      use t_control_param_vol_grping
      use calypso_reverse_send_recv
!
      type(volume_partioning_param), intent(in) :: each_part_p
      real(kind = kreal), intent(in) :: elapse_ray_trace(2)
      type(calypso_comm_table), intent(in) :: mesh_to_viz_tbl
      type(mesh_geometry), intent(in) :: mesh
!
      type(lic_repart_reference), intent(inout) :: rep_ref
      type(mesh_SR), intent(inout) :: m_SR
!
      real(kind = kreal), allocatable :: elapse_rtraces_pe(:,:)
!
!
      allocate(elapse_rtraces_pe(2,mesh_to_viz_tbl%nrank_export))
      call calypso_gather_reverse_SR(itwo, mesh_to_viz_tbl,             &
     &    elapse_ray_trace, elapse_rtraces_pe(1,1), m_SR%SR_sig)
      call copy_average_elapsed_to_nod(mesh%node, mesh_to_viz_tbl,      &
     &    each_part_p%weight_prev, elapse_rtraces_pe,                   &
     &    rep_ref%elapse_rtrace_nod)
      deallocate(elapse_rtraces_pe)
!
      end subroutine set_average_line_int_time
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_line_integration_count(node, weight_prev,         &
     &          count_line_int, ref_repart_mesh)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: node
      real(kind = kreal), intent(in) :: weight_prev
      real(kind = kreal), intent(in) :: count_line_int(node%numnod)
      real(kind = kreal), intent(inout) :: ref_repart_mesh(node%numnod)
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, node%numnod
        ref_repart_mesh(inod) = weight_prev * count_line_int(inod)      &
     &               + (1.0d0 - weight_prev) * ref_repart_mesh(inod)
      end do
!$omp end parallel do
!
      end subroutine copy_line_integration_count
!
!-----------------------------------------------------------------------
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
      real(kind = kreal), intent(inout) :: ref_repart_mesh(node%numnod)
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
          ref_repart_mesh(inod)                                         &
     &      = weight_prev * elapse_rtraces_pe(2,ip)                     &
     &       + (1.0d0 - weight_prev) * ref_repart_mesh(inod)
        end do
      end do
!
      end subroutine copy_average_elapsed_to_nod
!
!-----------------------------------------------------------------------
!
      end module t_lic_repart_reference
