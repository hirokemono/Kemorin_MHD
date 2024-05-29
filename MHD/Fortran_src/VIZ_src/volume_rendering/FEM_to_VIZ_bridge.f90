!>@file   FEM_to_VIZ_bridge.f90
!!@brief  module FEM_to_VIZ_bridge
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Data structuresa for visualizers
!!
!!@verbatim
!!      subroutine init_FEM_to_VIZ_bridge                               &
!!     &         (viz_step, geofem, VIZ_DAT, m_SR)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(mesh_data), intent(inout) :: geofem
!!        type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine init_FEM_MHD_to_VIZ_bridge                           &
!!     &         (viz_step, next_tbl, jacobians, geofem, VIZ_DAT, m_SR)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(next_nod_ele_table), intent(in), target :: next_tbl
!!        type(jacobians_type), intent(in), target :: jacobians
!!        type(mesh_data), intent(inout) :: geofem
!!        type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module FEM_to_VIZ_bridge
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_comm_table
      use t_next_node_ele_4_node
      use t_shape_functions
      use t_jacobians
      use t_VIZ_step_parameter
      use t_VIZ_mesh_field
      use t_mesh_SR
      use t_work_time
      use m_elapsed_labels_4_VIZ
!
      implicit none
!
      private :: normals_and_jacobians_4_VIZ
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_FEM_to_VIZ_bridge                                 &
     &         (viz_step, geofem, VIZ_DAT, m_SR)
!
      use m_work_time
      use parallel_FEM_mesh_init
!
      type(VIZ_step_params), intent(in) :: viz_step
!
      type(mesh_data), intent(inout) :: geofem
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: iflag
!
      call FEM_mesh_initialization(geofem%mesh, geofem%group,           &
     &                             m_SR%SR_sig, m_SR%SR_i)
!
      if(iflag_debug.gt.0) write(*,*) 'normals_and_jacobians_VIZ_pre'
      call link_jacobians_4_viz                                         &
     &   (VIZ_DAT%next_tbl_v, VIZ_DAT%jacobians_v, VIZ_DAT)
      if(iflag_debug.gt.0) write(*,*) 'normals_and_jacobians_4_VIZ'
      call normals_and_jacobians_4_VIZ(viz_step, geofem,                &
     &    VIZ_DAT%ele_comm, VIZ_DAT%surf_comm, VIZ_DAT%edge_comm,       &
     &    VIZ_DAT%inod_dbl, VIZ_DAT%iele_dbl, VIZ_DAT%isurf_dbl,        &
     &    VIZ_DAT%next_tbl, VIZ_DAT%jacobians, m_SR)
!
      iflag = viz_step%FLINE_t%increment + viz_step%LIC_t%increment
      if(iflag .gt. 0) then
        allocate(VIZ_DAT%para_surf%isf_4_ele_dbl(geofem%mesh%ele%numele,nsurf_4_ele,2))
        allocate(VIZ_DAT%para_surf%iele_4_surf_dbl(geofem%mesh%surf%numsurf,2,3))
        call set_isf_4_ele_double_index                                 &
     &     (geofem%mesh%ele, geofem%mesh%surf,                          &
     &      VIZ_DAT%isurf_dbl, VIZ_DAT%ele_comm,                        &
     &      VIZ_DAT%para_surf%isf_4_ele_dbl, m_SR)
        call set_iele_4_surf_double_index                               &
     &     (geofem%mesh%surf, VIZ_DAT%iele_dbl, VIZ_DAT%surf_comm,      &
     &      VIZ_DAT%para_surf%iele_4_surf_dbl, m_SR)
      end if
!
      end subroutine init_FEM_to_VIZ_bridge
!
! ----------------------------------------------------------------------
!
      subroutine init_FEM_MHD_to_VIZ_bridge                             &
     &         (viz_step, next_tbl, jacobians, geofem, VIZ_DAT, m_SR)
!
      use m_work_time
      use const_element_comm_tables
      use parallel_FEM_mesh_init
!
      use int_volume_of_domain
      use set_element_id_4_node
      use parallel_FEM_mesh_init
      use const_element_comm_tables
      use const_surface_comm_table
      use set_normal_vectors
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(next_nod_ele_table), intent(in), target :: next_tbl
      type(jacobians_type), intent(in), target :: jacobians
!
      type(mesh_data), intent(inout) :: geofem
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: iflag
!
!
      call link_jacobians_4_viz(next_tbl, jacobians, VIZ_DAT)
!
!  -----  Construct Element communication table
      iflag = viz_step%FLINE_t%increment + viz_step%LIC_t%increment
      if(iflag .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+14)
        if(iflag_debug.gt.0) write(*,*)' const_ele_comm_table'
        call const_ele_comm_table                                       &
     &     (geofem%mesh%node, geofem%mesh%nod_comm, geofem%mesh%ele,    &
     &      VIZ_DAT%ele_comm, m_SR)
        if(iflag_debug.gt.0) write(*,*)' const_surf_comm_table'
        call const_surf_comm_table                                      &
     &     (geofem%mesh%node, geofem%mesh%nod_comm, VIZ_DAT%surf_comm,  &
     &      geofem%mesh%surf, m_SR)
!
        call alloc_double_numbering(geofem%mesh%node%numnod,            &
     &                              VIZ_DAT%inod_dbl)
        call alloc_double_numbering(geofem%mesh%ele%numele,             &
     &                              VIZ_DAT%iele_dbl)
        call set_node_ele_double_address                                &
     &     (geofem%mesh%node, geofem%mesh%ele, geofem%mesh%nod_comm,    &
     &      VIZ_DAT%ele_comm, VIZ_DAT%inod_dbl, VIZ_DAT%iele_dbl,       &
     &      m_SR%SR_sig, m_SR%SR_i)
!
        call alloc_double_numbering(geofem%mesh%surf%numsurf,           &
     &                              VIZ_DAT%isurf_dbl)
        call set_ele_double_numbering                                   &
     &     (geofem%mesh%surf%numsurf, geofem%mesh%surf%ie_surf(1,1),    &
     &      VIZ_DAT%surf_comm, VIZ_DAT%inod_dbl, VIZ_DAT%isurf_dbl,     &
     &      m_SR%SR_sig, m_SR%SR_i)
!
        allocate(VIZ_DAT%para_surf%isf_4_ele_dbl(geofem%mesh%ele%numele,nsurf_4_ele,2))
        allocate(VIZ_DAT%para_surf%iele_4_surf_dbl(geofem%mesh%surf%numsurf,2,3))
        call set_isf_4_ele_double_index                                 &
     &     (geofem%mesh%ele, geofem%mesh%surf,                          &
     &      VIZ_DAT%isurf_dbl, VIZ_DAT%ele_comm,                        &
     &      VIZ_DAT%para_surf%isf_4_ele_dbl, m_SR)
        call set_iele_4_surf_double_index                               &
     &     (geofem%mesh%surf, VIZ_DAT%iele_dbl, VIZ_DAT%surf_comm,      &
     &      VIZ_DAT%para_surf%iele_4_surf_dbl, m_SR)

        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+14)
      end if
!
!  -----  Construct Edge communication table
      iflag = viz_step%PSF_t%increment + viz_step%ISO_t%increment       &
     &       + viz_step%MAP_t%increment
      if(iflag .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+15)
        if(iflag_debug .gt. 0) write(*,*) 'const_edge_comm_table'
        call const_edge_comm_table                                      &
     &     (geofem%mesh%node, geofem%mesh%nod_comm, VIZ_DAT%edge_comm,  &
     &      geofem%mesh%edge, m_SR)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+15)
      end if
      call calypso_mpi_barrier
!
      end subroutine init_FEM_MHD_to_VIZ_bridge
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine normals_and_jacobians_4_VIZ(viz_step, geofem,          &
     &          ele_comm, surf_comm, edge_comm,                         &
     &          inod_dbl, iele_dbl, isurf_dbl,                          &
     &          next_tbl, jacobians, m_SR)
!
      use t_fem_gauss_int_coefs
      use int_volume_of_domain
      use set_element_id_4_node
      use set_normal_vectors
      use const_element_comm_tables
      use const_surface_comm_table
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(inout) :: geofem
      type(communication_table), intent(inout) :: ele_comm
      type(communication_table), intent(inout) :: surf_comm
      type(communication_table), intent(inout) :: edge_comm
      type(node_ele_double_number), intent(inout) :: inod_dbl
      type(node_ele_double_number), intent(inout) :: iele_dbl
      type(node_ele_double_number), intent(inout) :: isurf_dbl
      type(next_nod_ele_table), intent(inout) :: next_tbl
      type(jacobians_type), intent(inout) :: jacobians
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: iflag
      type(shape_finctions_at_points) :: spfs
!
!
!  -----  Const Neighboring information
      iflag = viz_step%FLINE_t%increment + viz_step%LIC_t%increment
      if(iflag .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+14)
        if(iflag_debug.gt.0) write(*,*) 'set_belonged_ele_and_next_nod'
        call set_belonged_ele_and_next_nod                              &
     &     (geofem%mesh, next_tbl%neib_ele, next_tbl%neib_nod)
!
!  -----  Construct Element communication table
        if(iflag_debug.gt.0) write(*,*)' const_ele_comm_table'
        call const_ele_comm_table                                       &
     &     (geofem%mesh%node, geofem%mesh%nod_comm, geofem%mesh%ele,    &
     &      ele_comm, m_SR)
        if(iflag_debug.gt.0) write(*,*)' const_surf_comm_table'
        call const_surf_comm_table                                      &
     &     (geofem%mesh%node, geofem%mesh%nod_comm, surf_comm,          &
     &      geofem%mesh%surf, m_SR)
!
        call alloc_double_numbering(geofem%mesh%node%numnod,            &
     &                              inod_dbl)
        call alloc_double_numbering(geofem%mesh%ele%numele,             &
     &                              iele_dbl)
        call set_node_ele_double_address                                &
     &     (geofem%mesh%node, geofem%mesh%ele, geofem%mesh%nod_comm,    &
     &      ele_comm, inod_dbl, iele_dbl, m_SR%SR_sig, m_SR%SR_i)
!
        call alloc_double_numbering(geofem%mesh%surf%numsurf,           &
     &                              isurf_dbl)
        call set_ele_double_numbering                                   &
     &     (geofem%mesh%surf%numsurf, geofem%mesh%surf%ie_surf(1,1),    &
     &      surf_comm, inod_dbl, isurf_dbl, m_SR%SR_sig, m_SR%SR_i)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+14)
      end if
!
!  -----  Construct Edge communication table
      iflag = viz_step%PSF_t%increment + viz_step%ISO_t%increment       &
     &       + viz_step%MAP_t%increment
      if(iflag .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+15)
        if(iflag_debug .gt. 0) write(*,*) 'const_edge_comm_table'
        call const_edge_comm_table                                      &
     &     (geofem%mesh%node, geofem%mesh%nod_comm, edge_comm,          &
     &      geofem%mesh%edge, m_SR)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+15)
      end if
!
      iflag = viz_step%PVR_t%increment + viz_step%LIC_t%increment
      if(iflag .gt. 0) then
        if(iflag_debug.gt.0) write(*,*) 'jacobian_and_element_volume'
!        call sel_max_int_point_by_etype                                &
!     &     (geofem%mesh%ele%nnod_4_ele, jacobians%g_FEM)
        call set_max_integration_points(ione, jacobians%g_FEM)
        call jacobian_and_element_volume(my_rank, nprocs,               &
     &      geofem%mesh, geofem%group, spfs, jacobians)
        if (iflag_debug.eq.1) write(*,*) 'surf_jacobian_sf_grp_normal'
        call surf_jacobian_sf_grp_normal(my_rank, nprocs,               &
     &      geofem%mesh, geofem%group, spfs, jacobians)
      end if
!
      end subroutine normals_and_jacobians_4_VIZ
!
! ----------------------------------------------------------------------
!
      subroutine set_iele_4_surf_double_index                           &
     &         (surf, iele_dbl, surf_comm, iele_4_surf_dbl, m_SR)
!
      use m_geometry_constants
      use  solver_SR_type
!
      type(surface_data), intent(in) :: surf
      type(node_ele_double_number), intent(in) :: iele_dbl
      type(communication_table), intent(in) :: surf_comm
!
      integer(kind = kint), intent(inout)                               &
     &    :: iele_4_surf_dbl(surf%numsurf,2,3)
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: isurf, iele
!
!
!$omp parallel do private(isurf,iele)
      do isurf = 1, surf%numsurf
        iele =             surf%iele_4_surf(isurf,1,1)
        iele_4_surf_dbl(isurf,1,1) = iele_dbl%irank(iele)
        iele_4_surf_dbl(isurf,1,2) = iele_dbl%index(iele)
        iele_4_surf_dbl(isurf,1,3) = surf%iele_4_surf(isurf,1,2)
        iele =             surf%iele_4_surf(isurf,2,1)
        iele_4_surf_dbl(isurf,2,1) = iele_dbl%irank(iele)
        iele_4_surf_dbl(isurf,2,2) = iele_dbl%index(iele)
        iele_4_surf_dbl(isurf,2,3) = surf%iele_4_surf(isurf,2,2)
      end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_int_type(surf%numsurf, surf_comm,           &
     &    m_SR%SR_sig, m_SR%SR_i, iele_4_surf_dbl(1,1,1))
      call SOLVER_SEND_RECV_int_type(surf%numsurf, surf_comm,           &
     &    m_SR%SR_sig, m_SR%SR_i, iele_4_surf_dbl(1,1,2))
      call SOLVER_SEND_RECV_int_type(surf%numsurf, surf_comm,           &
     &    m_SR%SR_sig, m_SR%SR_i, iele_4_surf_dbl(1,1,3))
      call SOLVER_SEND_RECV_int_type(surf%numsurf, surf_comm,           &
     &    m_SR%SR_sig, m_SR%SR_i, iele_4_surf_dbl(1,2,1))
      call SOLVER_SEND_RECV_int_type(surf%numsurf, surf_comm,           &
     &    m_SR%SR_sig, m_SR%SR_i, iele_4_surf_dbl(1,2,2))
      call SOLVER_SEND_RECV_int_type(surf%numsurf, surf_comm,           &
     &    m_SR%SR_sig, m_SR%SR_i, iele_4_surf_dbl(1,2,3))
!
      end subroutine set_iele_4_surf_double_index
!
!  ---------------------------------------------------------------------
!
      subroutine set_isf_4_ele_double_index                             &
     &         (ele, surf, isurf_dbl, ele_comm, isf_4_ele_dbl, m_SR)
!
      use  solver_SR_type
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(node_ele_double_number), intent(in) :: isurf_dbl
      type(communication_table), intent(in) :: ele_comm
!
      integer(kind = kint), intent(inout)                               &
     &    :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: isurf, iele, k1
!
!
      do k1 = 1, nsurf_4_ele
!$omp parallel do private(isurf,iele)
        do iele = 1, ele%numele
          isurf = abs(surf%isf_4_ele(iele,k1))
          
          isf_4_ele_dbl(iele,k1,1) = isurf_dbl%irank(isurf)
          isf_4_ele_dbl(iele,k1,2) = isurf_dbl%index(isurf)             &
    &                             * (surf%isf_4_ele(iele,k1) / isurf)

        end do
!$omp end parallel do
      end do
!
      do k1 = 1, nsurf_4_ele
        call SOLVER_SEND_RECV_int_type(ele%numele, ele_comm,            &
     &      m_SR%SR_sig, m_SR%SR_i, isf_4_ele_dbl(1,k1,1))
        call SOLVER_SEND_RECV_int_type(ele%numele, ele_comm,            &
     &      m_SR%SR_sig, m_SR%SR_i, isf_4_ele_dbl(1,k1,2))
      end do
!
      end subroutine set_isf_4_ele_double_index
!
!  ---------------------------------------------------------------------
!
      end module FEM_to_VIZ_bridge
