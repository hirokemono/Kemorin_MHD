!>@file   copy_repart_and_itp_table.f90
!!@brief  module copy_repart_and_itp_table
!!
!!@author H. Matsui
!!@date  Programmed by H. Matsui in May, 2010
!
!> @brief Copy between repartitioning table and interpolation table
!!
!!@verbatim
!!      subroutine copy_repart_tbl_to_itp_table                         &
!!     &         (org_mesh, neib_ele, part_tbl, itp_info)
!!        type(mesh_geometry), intent(in) :: org_mesh
!!        type(element_around_node), intent(in) :: neib_ele
!!        type(calypso_comm_table), intent(in) :: part_tbl
!!        type(interpolate_table), intent(inout) :: itp_info
!!      subroutine copy_itp_table_to_repart_tbl                         &
!!     &         (irank_read, org_mesh, dest_mesh, itp_info, part_tbl)
!!        type(mesh_geometry), intent(in) :: org_mesh, dest_mesh
!!        type(interpolate_table), intent(in) :: itp_info
!!        type(calypso_comm_table), intent(inout) :: part_tbl
!!
!!      subroutine copy_repart_import_to_itp_dest(part_tbl, itp_dest)
!!        type(calypso_comm_table), intent(in) :: part_tbl
!!        type(interpolate_table_dest), intent(inout) :: itp_dest
!!      subroutine copy_itp_dest_to_repart_import                       &
!!     &         (itp_dest, nnod, part_tbl)
!!        integer(kind = kint), intent(in) :: nnod
!!        type(interpolate_table_dest), intent(in) :: itp_dest
!!        type(calypso_comm_table), intent(inout) :: part_tbl
!!@endverbatim
!
      module copy_repart_and_itp_table
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_mesh_data
      use t_geometry_data
      use t_calypso_comm_table
      use t_next_node_ele_4_node
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
!
      implicit none
!
      private :: copy_repart_export_to_itp_org
      private :: copy_itp_org_to_repart_export
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_repart_tbl_to_itp_table                           &
     &         (org_mesh, neib_ele, part_tbl, itp_info)
!
      type(mesh_geometry), intent(in) :: org_mesh
      type(element_around_node), intent(in) :: neib_ele
      type(calypso_comm_table), intent(in) :: part_tbl
      type(interpolate_table), intent(inout) :: itp_info
!
!
      itp_info%iflag_itp_recv = iflag_import_item
      call copy_repart_export_to_itp_org                                &
     &   (org_mesh%node, neib_ele, part_tbl, itp_info%tbl_org)
      call copy_repart_import_to_itp_dest(part_tbl, itp_info%tbl_dest)
!
      end subroutine copy_repart_tbl_to_itp_table
!
!-----------------------------------------------------------------------
!
      subroutine copy_itp_table_to_repart_tbl                           &
     &         (irank_read, org_mesh, dest_mesh, itp_info, part_tbl)
!
      integer(kind= kint), intent(in) :: irank_read
      type(mesh_geometry), intent(in) :: org_mesh, dest_mesh
      type(interpolate_table), intent(in) :: itp_info
      type(calypso_comm_table), intent(inout) :: part_tbl
!
!
      call copy_itp_org_to_repart_export                                &
     &   (irank_read, itp_info%tbl_org, org_mesh%ele, part_tbl)
      call copy_itp_dest_to_repart_import                               &
     &   (itp_info%tbl_dest, dest_mesh%node%numnod, part_tbl)
!
      end subroutine copy_itp_table_to_repart_tbl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_repart_export_to_itp_org                          &
     &         (node, neib_ele, part_tbl, itp_org)
!
      use copy_local_position_2_ele
!
      type(node_data), intent(in) :: node
      type(calypso_comm_table), intent(in) :: part_tbl
      type(element_around_node), intent(in) :: neib_ele
!
      type(interpolate_table_org), intent(inout) :: itp_org
!
      integer(kind = kint) :: inum, inod, k1, ist
      real(kind = kreal) :: xi_ele(3)
!
!
      itp_org%iflag_self_itp_send = part_tbl%iflag_self_copy
      itp_org%num_dest_domain = part_tbl%nrank_export
      call alloc_itp_num_org(np_smp, itp_org)
!
      if(part_tbl%nrank_export .gt. 0) then
!$omp parallel workshare
        itp_org%id_dest_domain(1:part_tbl%nrank_export)                 &
     &          = part_tbl%irank_export(1:part_tbl%nrank_export)
        itp_org%istack_nod_tbl_org(1:part_tbl%nrank_export)             &
     &          = part_tbl%istack_export(1:part_tbl%nrank_export)
!$omp end parallel workshare
      end if
!
      itp_org%istack_itp_type_org(0) =   0
      itp_org%istack_itp_type_org(1:4) = part_tbl%ntot_export
      call set_stack_tbl_wtype_org_smp(itp_org)
!
      itp_org%ntot_table_org = part_tbl%ntot_export
      call alloc_itp_table_org(itp_org)
!
!$omp parallel do private(inum,inod,ist,k1,xi_ele)
      do inum = 1, part_tbl%ntot_export
        inod = part_tbl%item_export(inum)
        ist =  neib_ele%istack_4_node(inod-1) + 1
!
        k1 =   neib_ele%iconn_4_node(ist)
        call copy_node_local_posi_2_element(k1, xi_ele)
!
        itp_org%inod_itp_send(inum) =      inod
        itp_org%inod_gl_dest_4_org(inum) = node%inod_global(inod)
        itp_org%iele_org_4_org(inum) =     neib_ele%iele_4_node(ist)
        itp_org%itype_inter_org(inum) =    k1
        itp_org%coef_inter_org(inum,1) =   xi_ele(1)
        itp_org%coef_inter_org(inum,2) =   xi_ele(2)
        itp_org%coef_inter_org(inum,3) =   xi_ele(3)
      end do
!$omp end parallel do
!
      end subroutine copy_repart_export_to_itp_org
!
!-----------------------------------------------------------------------
!
      subroutine copy_repart_import_to_itp_dest(part_tbl, itp_dest)
!
      type(calypso_comm_table), intent(in) :: part_tbl
      type(interpolate_table_dest), intent(inout) :: itp_dest
!
!
      itp_dest%iflag_self_itp_recv = part_tbl%iflag_self_copy
      itp_dest%num_org_domain =      part_tbl%nrank_import
      call alloc_itp_num_dest(itp_dest)
      itp_dest%istack_nod_tbl_dest(0) = part_tbl%istack_import(0)
!
      if(part_tbl%nrank_import .gt. 0) then
!$omp parallel workshare
        itp_dest%id_org_domain(1:part_tbl%nrank_import)                 &
     &          = part_tbl%irank_import(1:part_tbl%nrank_import)
        itp_dest%istack_nod_tbl_dest(1:part_tbl%nrank_import)           &
     &          = part_tbl%istack_import(1:part_tbl%nrank_import)
!$omp end parallel workshare
      end if
!
      itp_dest%ntot_table_dest = part_tbl%ntot_import
      call alloc_itp_table_dest(itp_dest)
!
      if(part_tbl%ntot_import .gt. 0) then
!$omp parallel workshare
        itp_dest%inod_dest_4_dest(1:part_tbl%ntot_import)               &
     &      = part_tbl%item_import(1:part_tbl%ntot_import)
        itp_dest%irev_dest_4_dest(1:part_tbl%ntot_import)               &
     &      = part_tbl%irev_import(1:part_tbl%ntot_import)
!$omp end parallel workshare
      end if
!
      end subroutine copy_repart_import_to_itp_dest
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_itp_org_to_repart_export                          &
     &         (irank_read, itp_org, org_ele, part_tbl)
!
      use copy_local_position_2_ele
!
      integer(kind= kint), intent(in) :: irank_read
      type(element_data), intent(in) :: org_ele
      type(interpolate_table_org), intent(in) :: itp_org
      type(calypso_comm_table), intent(inout) :: part_tbl
!
      integer(kind = kint) :: inum, iele, k1
!
!
      part_tbl%iflag_self_copy = itp_org%iflag_self_itp_send
      part_tbl%nrank_export =    itp_org%num_dest_domain
      call alloc_calypso_export_num(part_tbl)
!
      part_tbl%istack_export(0) = itp_org%istack_nod_tbl_org(0)
      if(part_tbl%nrank_export .gt. 0) then
!$omp parallel workshare
        part_tbl%irank_export(1:part_tbl%nrank_export)                  &
     &          = itp_org%id_dest_domain(1:part_tbl%nrank_export)
        part_tbl%istack_export(1:part_tbl%nrank_export)                 &
     &          = itp_org%istack_nod_tbl_org(1:part_tbl%nrank_export)
!$omp end parallel workshare
!
        if(part_tbl%irank_export(part_tbl%nrank_export)                 &
     &     .eq. irank_read) then
          part_tbl%iflag_self_copy = 1
        else  
          part_tbl%iflag_self_copy = 0
        end if
      end if
!
      if(itp_org%istack_itp_type_org(4)                                 &
     &   .ne. itp_org%istack_itp_type_org(1)) write(*,*) 'Wrong table!'
      if(itp_org%istack_itp_type_org(3)                                 &
     &   .ne. itp_org%istack_itp_type_org(1)) write(*,*) 'Wrong table!'
      if(itp_org%istack_itp_type_org(2)                                 &
     &   .ne. itp_org%istack_itp_type_org(1)) write(*,*) 'Wrong table!'
!
      part_tbl%ntot_export = itp_org%ntot_table_org
      call alloc_calypso_export_item(part_tbl)
!
!$omp parallel do private(inum,iele,k1)
      do inum = 1, part_tbl%ntot_export
        iele = itp_org%iele_org_4_org(inum)
        k1 =   itp_org%itype_inter_org(inum)
        part_tbl%item_export(inum) = org_ele%ie(iele,k1)
      end do
!$omp end parallel do
!
      end subroutine copy_itp_org_to_repart_export
!
!-----------------------------------------------------------------------
!
      subroutine copy_itp_dest_to_repart_import                         &
     &         (itp_dest, nnod, part_tbl)
!
      integer(kind = kint), intent(in) :: nnod
      type(interpolate_table_dest), intent(in) :: itp_dest
      type(calypso_comm_table), intent(inout) :: part_tbl
!
      integer(kind = kint) :: inum, inod
!
!
      part_tbl%nrank_import = itp_dest%num_org_domain
      call alloc_calypso_import_num(part_tbl)
      part_tbl%istack_import(0) = itp_dest%istack_nod_tbl_dest(0)
!
      if(part_tbl%nrank_import .gt. 0) then
!$omp parallel workshare
        part_tbl%irank_import(1:part_tbl%nrank_import)                  &
     &          = itp_dest%id_org_domain(1:part_tbl%nrank_import)
        part_tbl%istack_import(1:part_tbl%nrank_import)                 &
     &          = itp_dest%istack_nod_tbl_dest(1:part_tbl%nrank_import)
!$omp end parallel workshare
      end if
!
      part_tbl%ntot_import = itp_dest%ntot_table_dest
      call alloc_calypso_import_item(nnod, part_tbl)
!
!$omp parallel do private(inum,inod)
      do inum = 1, part_tbl%ntot_import
        inod = itp_dest%inod_dest_4_dest(inum)
        part_tbl%item_import(inum) =     inod
        part_tbl%irev_import(inod) =     inum
      end do
!$omp end parallel do
!$omp parallel do private(inum)
      do inum = part_tbl%ntot_import+1, nnod
        part_tbl%irev_import(inum) = part_tbl%ntot_import + 1
      end do
!$omp end parallel do
!
      end subroutine copy_itp_dest_to_repart_import
!
!-----------------------------------------------------------------------
!
      end module copy_repart_and_itp_table

