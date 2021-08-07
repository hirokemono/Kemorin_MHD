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
!!      subroutine alloc_lic_repart_ref(node, rep_ref)
!!      subroutine dealloc_lic_repart_ref(rep_ref)
!!        type(lic_repart_reference), intent(inout) :: rep_ref
!!        type(mesh_geometry), intent(in) :: mesh
!!
!!      subroutine bring_back_rendering_time                            &
!!     &         (each_part_p, mesh_to_viz_tbl, rep_ref_viz,            &
!!     &          rep_ref_snap, rep_ref, m_SR)
!!        type(volume_partioning_param), intent(in) :: each_part_p
!!        type(calypso_comm_table), intent(in) :: mesh_to_viz_tbl
!!        type(lic_repart_reference), intent(in) :: rep_ref_viz
!!        type(lic_repart_reference), intent(inout) :: rep_ref_snap
!!        type(lic_repart_reference), intent(inout) :: rep_ref
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine set_average_line_int_time(mesh, each_part_p,         &
!!     &          rep_ref_viz, mesh_to_viz_tbl, rep_ref, m_SR)
!!        type(volume_partioning_param), intent(in) :: each_part_p
!!        type(lic_repart_reference), intent(in) :: rep_ref_viz
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
!>    Noumber of node
        integer(kind = kint) :: nnod_lic
!>    Work area for elapsed transfer time
        real(kind = kreal), allocatable :: count_line_int(:)
!
!>    Average elapsed time for line integration
        real(kind = kreal) :: elapse_ray_trace(2)
      end type lic_repart_reference
!
      private :: copy_average_elapsed_to_nod
      private :: copy_line_integration_count
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
      call cal_node_volue(mesh%node, mesh%ele, rep_ref%count_line_int)
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
      rep_ref%nnod_lic = node%numnod
      allocate(rep_ref%count_line_int(rep_ref%nnod_lic))
      if(rep_ref%nnod_lic .gt. 0) then
!$omp parallel workshare
        rep_ref%count_line_int(1:rep_ref%nnod_lic) = 0.0d0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_lic_repart_ref
!
! -----------------------------------------------------------------------
!
      subroutine reset_lic_count_line_int(rep_ref)
!
      type(lic_repart_reference), intent(inout) :: rep_ref
!
!
!$omp parallel workshare
      rep_ref%count_line_int(1:rep_ref%nnod_lic) = 0.0d0
!$omp end parallel workshare
!
      end subroutine reset_lic_count_line_int
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_lic_repart_ref(rep_ref)
!
      type(lic_repart_reference), intent(inout) :: rep_ref
!
!
      if(allocated(rep_ref%count_line_int)) then
        deallocate(rep_ref%count_line_int)
      end if
!
      end subroutine dealloc_lic_repart_ref
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bring_back_rendering_time                              &
     &         (each_part_p, mesh_to_viz_tbl, rep_ref_viz,              &
     &          rep_ref_snap, rep_ref, m_SR)
!
      use t_mesh_SR
      use t_calypso_comm_table
      use t_control_param_vol_grping
      use calypso_reverse_send_recv
!
      type(volume_partioning_param), intent(in) :: each_part_p
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
      call copy_line_integration_count                                  &
     &   (rep_ref_snap%nnod_lic, each_part_p%weight_prev,               &
     &    rep_ref_snap%count_line_int, rep_ref%count_line_int)
!
      end subroutine bring_back_rendering_time
!
!  ---------------------------------------------------------------------
!
      subroutine set_average_line_int_time(mesh, each_part_p,           &
     &          rep_ref_viz, mesh_to_viz_tbl, rep_ref, m_SR)
!
      use t_mesh_SR
      use t_mesh_data
      use t_calypso_comm_table
      use t_control_param_vol_grping
      use calypso_reverse_send_recv
!
      type(volume_partioning_param), intent(in) :: each_part_p
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
      allocate(elapse_rtraces_pe(2,mesh_to_viz_tbl%nrank_export))
      call calypso_gather_reverse_SR(itwo, mesh_to_viz_tbl,             &
     &    rep_ref_viz%elapse_ray_trace, elapse_rtraces_pe(1,1),         &
     &    m_SR%SR_sig)
      call copy_average_elapsed_to_nod(mesh%node, mesh_to_viz_tbl,      &
     &    each_part_p%weight_prev, elapse_rtraces_pe,                   &
     &    rep_ref%count_line_int)
      deallocate(elapse_rtraces_pe)
!
      end subroutine set_average_line_int_time
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_line_integration_count(numnod, weight_prev,       &
     &          count_line_int, ref_repart_mesh)
!
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: weight_prev
      real(kind = kreal), intent(in) :: count_line_int(numnod)
      real(kind = kreal), intent(inout) :: ref_repart_mesh(numnod)
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, numnod
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
