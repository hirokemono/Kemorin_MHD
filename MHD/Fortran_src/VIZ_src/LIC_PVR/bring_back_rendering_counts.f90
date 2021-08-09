!>@file  bring_back_rendering_counts.f90
!!       module bring_back_rendering_counts
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine bring_back_rendering_time                            &
!!     &         (mesh_to_viz_tbl, rep_ref_viz,                         &
!!     &          rep_ref_snap, rep_ref, m_SR)
!!        type(calypso_comm_table), intent(in) :: mesh_to_viz_tbl
!!        type(lic_repart_reference), intent(in) :: rep_ref_viz
!!        type(lic_repart_reference), intent(inout) :: rep_ref_snap
!!        type(lic_repart_reference), intent(inout) :: rep_ref
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine set_average_line_int_time                            &
!!     &         (mesh, rep_ref_viz, mesh_to_viz_tbl, rep_ref, m_SR)
!!        type(lic_repart_reference), intent(in) :: rep_ref_viz
!!        type(calypso_comm_table), intent(in) :: mesh_to_viz_tbl
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(lic_repart_reference), intent(inout) :: rep_ref
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module bring_back_rendering_counts
!
      use m_precision
      use m_constants
!
      use t_lic_repart_reference
!
      implicit  none
!
      private :: copy_average_elapsed_to_nod
      private :: copy_line_integration_count
      private :: evo_line_integration_count
      private :: add_ave_line_integration_count
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine bring_back_rendering_time                              &
     &         (mesh_to_viz_tbl, rep_ref_viz,                           &
     &          rep_ref_snap, rep_ref, m_SR)
!
      use t_mesh_SR
      use t_calypso_comm_table
      use calypso_reverse_send_recv
!
      type(calypso_comm_table), intent(in) :: mesh_to_viz_tbl
      type(lic_repart_reference), intent(in) :: rep_ref_viz
!
      type(lic_repart_reference), intent(inout) :: rep_ref_snap
      type(lic_repart_reference), intent(inout) :: rep_ref
      type(mesh_SR), intent(inout) :: m_SR
!
!
      call calypso_reverse_SR_1(mesh_to_viz_tbl,                        &
     &    rep_ref_viz%nnod_lic, rep_ref_snap%nnod_lic,                  &
     &    rep_ref_viz%count_line_int, rep_ref_snap%count_line_int,      &
     &    m_SR%SR_sig, m_SR%SR_r)
!
!      if
        call copy_line_integration_count(rep_ref_snap%nnod_lic,         &
     &      rep_ref%weight_prev, rep_ref_snap%count_line_int,           &
     &      rep_ref%count_line_int, rep_ref%num_counts)
!      else
!        call evo_line_integration_count(rep_ref_snap%nnod_lic,         &
!     &      rep_ref%weight_prev, rep_ref_snap%count_line_int,          &
!     &      rep_ref%count_line_int, rep_ref%num_counts)
!      else
!        call add_ave_line_integration_count                            &
!     &     (rep_ref_snap%nnod_lic, rep_ref_snap%count_line_int,        &
!     &      rep_ref%count_line_int, rep_ref%num_counts)
!       end if
!
      end subroutine bring_back_rendering_time
!
!  ---------------------------------------------------------------------
!
      subroutine set_average_line_int_time                              &
     &         (mesh, rep_ref_viz, mesh_to_viz_tbl, rep_ref, m_SR)
!
      use t_mesh_SR
      use t_mesh_data
      use t_calypso_comm_table
      use calypso_reverse_send_recv
!
      type(lic_repart_reference), intent(in) :: rep_ref_viz
      type(calypso_comm_table), intent(in) :: mesh_to_viz_tbl
      type(mesh_geometry), intent(in) :: mesh
!
      type(lic_repart_reference), intent(inout) :: rep_ref
      type(mesh_SR), intent(inout) :: m_SR
!
      real(kind = kreal), allocatable :: elapse_rtraces_pe(:,:)
!
!
      rep_ref%num_counts = rep_ref%num_counts + 1
      allocate(elapse_rtraces_pe(2,mesh_to_viz_tbl%nrank_export))
      call calypso_gather_reverse_SR(itwo, mesh_to_viz_tbl,             &
     &    rep_ref_viz%elapse_ray_trace, elapse_rtraces_pe(1,1),         &
     &    m_SR%SR_sig)
      call copy_average_elapsed_to_nod(mesh%node, mesh_to_viz_tbl,      &
     &    rep_ref%weight_prev, elapse_rtraces_pe,                       &
     &    rep_ref%count_line_int)
      deallocate(elapse_rtraces_pe)
!
      end subroutine set_average_line_int_time
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_line_integration_count(numnod, weight_prev,       &
     &          count_line_int, ref_repart_mesh, num_counts)
!
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: weight_prev
      real(kind = kreal), intent(in) :: count_line_int(numnod)
!
      real(kind = kreal), intent(inout) :: ref_repart_mesh(numnod)
      integer(kind = kint), intent(inout) :: num_counts
!
!
!$omp parallel workshare
      ref_repart_mesh(1:numnod) = weight_prev*count_line_int(1:numnod)  &
     &             + (1.0d0 - weight_prev) * ref_repart_mesh(1:numnod)
!$omp end parallel workshare
      num_counts = num_counts + 1
!
      end subroutine copy_line_integration_count
!
!-----------------------------------------------------------------------
!
      subroutine evo_line_integration_count(numnod, weight_prev,        &
     &          count_line_int, ref_repart_mesh, num_counts)
!
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: weight_prev
      real(kind = kreal), intent(in) :: count_line_int(numnod)
!
      real(kind = kreal), intent(inout) :: ref_repart_mesh(numnod)
      integer(kind = kint), intent(inout) :: num_counts
!
      real(kind = kreal) :: evo_tmp
      integer(kind = kint) :: inod
!
!
!$omp parallel do private(inod,evo_tmp)
      do inod = 1, numnod
        evo_tmp = (1.0d0 + weight_prev) * count_line_int(inod)          &
     &           - weight_prev * ref_repart_mesh(inod)
        ref_repart_mesh(inod) = max(evo_tmp, ref_repart_mesh(inod))
      end do
!$omp end parallel do
      num_counts = num_counts + 1
!
      end subroutine evo_line_integration_count
!
!-----------------------------------------------------------------------
!
      subroutine add_ave_line_integration_count(numnod,                 &
     &          count_line_int, ref_repart_mesh, num_counts)
!
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: count_line_int(numnod)
!
      real(kind = kreal), intent(inout) :: ref_repart_mesh(numnod)
      integer(kind = kint), intent(inout) :: num_counts
!
      real(kind = kreal) :: acounts
!
!
      acounts = 1.0d0 / (num_counts + 1)
!$omp parallel workshare
      ref_repart_mesh(1:numnod) = count_line_int(1:numnod)              &
     &                         + num_counts * ref_repart_mesh(1:numnod)
      ref_repart_mesh(1:numnod) = ref_repart_mesh(1:numnod) * acounts
!$omp end parallel workshare
      num_counts = num_counts + 1
!
      end subroutine add_ave_line_integration_count
!
!-----------------------------------------------------------------------
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
!$omp end parallel do
      end do
!
      end subroutine copy_average_elapsed_to_nod
!
! -----------------------------------------------------------------------
!
      end module bring_back_rendering_counts
