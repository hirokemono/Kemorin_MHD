!>@file   FEM_MHD_ucd_data.f90
!!@brief  module FEM_MHD_ucd_data
!!
!!@author H. Matsui
!!@date  Programmed in July 2000 (ver 1.1)
!!@n      Modified in Aug., 2007
!!@n      Modified in Dec., 2015
!
!>@brief Strucures for field data output
!!
!!@verbatim
!!      subroutine output_grd_file_w_org_connect(ucd_step, mesh,        &
!!     &          MHD_mesh, nod_fld, ucd_param, ucd)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(phys_data), intent(in) :: nod_fld
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(ucd_data), intent(inout) :: ucd
!!      subroutine link_global_org_mesh_4_ucd(mesh, MHD_mesh, ucd)
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!
      module FEM_MHD_ucd_data
!
      use m_precision
      use m_constants
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_phys_data
      use t_geometry_data_MHD
      use t_file_IO_parameter
      use t_IO_step_parameter
      use t_ucd_data
      use m_solver_SR
!
      implicit none
!
      private :: link_local_org_mesh_4_ucd, count_interier_ele_in_org
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine output_grd_file_w_org_connect(ucd_step, mesh,          &
     &          MHD_mesh, nod_fld, ucd_param, ucd)
!
      use m_field_file_format
      use set_ucd_data_to_type
!
      use merged_udt_vtk_file_IO
      use parallel_ucd_IO_select
!
      type(IO_step_param), intent(in) :: ucd_step
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(phys_data), intent(in) :: nod_fld
      type(field_IO_params), intent(in) :: ucd_param
!
      type(ucd_data), intent(inout) :: ucd
!
!
      if(ucd_param%iflag_format .lt. 0) return
      if(ucd_step%increment .eq. 0) return
!
      call link_local_org_mesh_4_ucd                                    &
     &   (mesh%node, mesh%ele, MHD_mesh, ucd)
      call link_field_data_to_ucd(nod_fld, ucd)
!
      if (ucd_param%iflag_format/icent .eq. iflag_single/icent) then
        call init_merged_ucd_element(ucd_param%iflag_format,            &
     &      mesh%node, mesh%ele, mesh%nod_comm, ucd, SR_sig1, SR_i1)
      end if
!
      call sel_write_parallel_ucd_mesh(ucd_param, ucd)
!
      if(   mod(ucd_param%iflag_format,icent)/iten .eq. iflag_udt/iten  &
     & .or. mod(ucd_param%iflag_format,icent)/iten .eq. iflag_vtd/iten) &
     &    then
        call deallocate_ucd_ele(ucd)
      end if
!
      if(mod(ucd_param%iflag_format,icent)/iten .eq. iflag_vtd/iten)    &
     &    call deallocate_ucd_node(ucd)
!
      end subroutine output_grd_file_w_org_connect
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine link_global_org_mesh_4_ucd(mesh, MHD_mesh, ucd)
!
      use set_ucd_data_to_type
      use set_and_cal_udt_data
      use count_overlap
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call link_node_data_2_ucd(mesh%node, ucd)
!
      ucd%nnod_4_ele = mesh%ele%nnod_4_ele
      ucd%nele = count_interier_ele_in_org(mesh%node, mesh%ele,        &
     &                                     MHD_mesh)
      call allocate_ucd_ele(ucd)
!
      call set_udt_global_connect(mesh%node%internal_node,             &
     &    mesh%ele%numele, mesh%ele%nnod_4_ele,                        &
     &    MHD_mesh%iele_global_org, MHD_mesh%ie_org, ucd)
!
!
      end subroutine link_global_org_mesh_4_ucd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine link_local_org_mesh_4_ucd(node, ele, MHD_mesh, ucd)
!
      use set_and_cal_udt_data
      use count_overlap
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(mesh_data_MHD), intent(in) :: MHD_mesh
!
      type(ucd_data), intent(inout) :: ucd
!
!
      ucd%nnod = node%numnod
      call allocate_ucd_node(ucd)
      call set_udt_local_nodes(node%numnod, node%xx, ucd)
!
      ucd%nnod_4_ele = ele%nnod_4_ele
      ucd%nele = count_interier_ele_in_org(node, ele, MHD_mesh)
      call allocate_ucd_ele(ucd)
!
      call set_udt_local_connect(node%internal_node,                    &
     &    ele%numele, ele%nnod_4_ele, MHD_mesh%ie_org, ucd)
!
      end subroutine link_local_org_mesh_4_ucd
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &              count_interier_ele_in_org(node, ele, MHD_mesh)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(mesh_data_MHD), intent(in) :: MHD_mesh
!
      integer (kind = kint) :: icou, iele
!
!
      icou = 0
!$omp parallel do private(iele) reduction(+:icou)
      do iele = 1, ele%numele
        if(check_internal_ele_in_org(iele, node, MHD_mesh))             &
     &     icou = icou + 1
      end do
!$omp end parallel do
      count_interier_ele_in_org = icou
!
      end function count_interier_ele_in_org
!
! ----------------------------------------------------------------------
!
      end module FEM_MHD_ucd_data
