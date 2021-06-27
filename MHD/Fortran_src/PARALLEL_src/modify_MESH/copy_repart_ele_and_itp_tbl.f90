!>@file   copy_repart_ele_and_itp_tbl.f90
!!@brief  module copy_repart_ele_and_itp_tbl
!!
!!@author H. Matsui
!!@date  Programmed by H. Matsui in May, 2010
!
!> @brief Copy between repartitioning table and interpolation table
!!
!!@verbatim
!!      subroutine copy_repart_ele_tbl_to_itp_tbl                       &
!!     &         (org_mesh, part_ele_tbl, itp_info)
!!        type(mesh_geometry), intent(in) :: org_mesh
!!        type(calypso_comm_table), intent(in) :: part_ele_tbl
!!        type(interpolate_table), intent(inout) :: itp_info
!!      subroutine copy_itp_tbl_to_repart_ele_tbl                       &
!!     &         (irank_read, dest_mesh, itp_info, part_ele_tbl)
!!        integer(kind= kint), intent(in) :: irank_read
!!        type(mesh_geometry), intent(in) :: dest_mesh
!!        type(interpolate_table), intent(in) :: itp_info
!!        type(calypso_comm_table), intent(inout) :: part_ele_tbl
!!@endverbatim
!
      module copy_repart_ele_and_itp_tbl
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_mesh_data
      use t_geometry_data
      use t_calypso_comm_table
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
!
      implicit none
!
      private :: copy_repart_ele_exp_to_itp_org
      private :: copy_itp_org_to_repart_ele_exp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_repart_ele_tbl_to_itp_tbl                         &
     &         (org_mesh, part_ele_tbl, itp_info)
!
      use copy_repart_and_itp_table
!
      type(mesh_geometry), intent(in) :: org_mesh
      type(calypso_comm_table), intent(in) :: part_ele_tbl
      type(interpolate_table), intent(inout) :: itp_info
!
!
      itp_info%iflag_itp_recv = iflag_import_item
      call copy_repart_ele_exp_to_itp_org                               &
     &   (org_mesh%ele, part_ele_tbl, itp_info%tbl_org)
      call copy_repart_import_to_itp_dest                               &
     &   (part_ele_tbl, itp_info%tbl_dest)
!
      end subroutine copy_repart_ele_tbl_to_itp_tbl
!
!-----------------------------------------------------------------------
!
      subroutine copy_itp_tbl_to_repart_ele_tbl                         &
     &         (irank_read, dest_mesh, itp_info, part_ele_tbl)
!
      use copy_repart_and_itp_table
!
      integer(kind= kint), intent(in) :: irank_read
      type(mesh_geometry), intent(in) :: dest_mesh
      type(interpolate_table), intent(in) :: itp_info
      type(calypso_comm_table), intent(inout) :: part_ele_tbl
!
!
      call copy_itp_org_to_repart_ele_exp                               &
     &   (irank_read, itp_info%tbl_org, part_ele_tbl)
      call copy_itp_dest_to_repart_import                               &
     &   (itp_info%tbl_dest, dest_mesh%ele%numele, part_ele_tbl)
!
      end subroutine copy_itp_tbl_to_repart_ele_tbl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_repart_ele_exp_to_itp_org                         &
     &         (ele, part_ele_tbl, itp_org)
!
      use copy_local_position_2_ele
!
      type(element_data), intent(in) :: ele
      type(calypso_comm_table), intent(in) :: part_ele_tbl
!
      type(interpolate_table_org), intent(inout) :: itp_org
!
      integer(kind = kint) :: inum, iele
!
!
      itp_org%iflag_self_itp_send = part_ele_tbl%iflag_self_copy
      itp_org%num_dest_domain = part_ele_tbl%nrank_export
      call alloc_itp_num_org(np_smp, itp_org)
!
      if(part_ele_tbl%nrank_export .gt. 0) then
!$omp parallel workshare
        itp_org%id_dest_domain(1:part_ele_tbl%nrank_export)             &
     &        = part_ele_tbl%irank_export(1:part_ele_tbl%nrank_export)
        itp_org%istack_nod_tbl_org(1:part_ele_tbl%nrank_export)         &
     &        = part_ele_tbl%istack_export(1:part_ele_tbl%nrank_export)
!$omp end parallel workshare
      end if
!
      itp_org%istack_itp_type_org(0:3) =   0
      itp_org%istack_itp_type_org(4) = part_ele_tbl%ntot_export
      call set_stack_tbl_wtype_org_smp(itp_org)
!
      itp_org%ntot_table_org = part_ele_tbl%ntot_export
      call alloc_itp_table_org(itp_org)
!
!$omp parallel do private(inum,iele)
      do inum = 1, part_ele_tbl%ntot_export
        iele = part_ele_tbl%item_export(inum)
!
        itp_org%inod_itp_send(inum) =      iele
        itp_org%inod_gl_dest_4_org(inum) = ele%iele_global(iele)
        itp_org%iele_org_4_org(inum) =     iele
        itp_org%itype_inter_org(inum) =    izero
        itp_org%coef_inter_org(inum,1) =   zero
        itp_org%coef_inter_org(inum,2) =   zero
        itp_org%coef_inter_org(inum,3) =   zero
      end do
!$omp end parallel do
!
      end subroutine copy_repart_ele_exp_to_itp_org
!
!-----------------------------------------------------------------------
!
      subroutine copy_itp_org_to_repart_ele_exp                         &
     &         (irank_read, itp_org, part_ele_tbl)
!
      use copy_local_position_2_ele
!
      integer(kind= kint), intent(in) :: irank_read
      type(interpolate_table_org), intent(in) :: itp_org
      type(calypso_comm_table), intent(inout) :: part_ele_tbl
!
      integer(kind = kint) :: inum
!
!
      part_ele_tbl%iflag_self_copy = itp_org%iflag_self_itp_send
      part_ele_tbl%nrank_export =    itp_org%num_dest_domain
      call alloc_calypso_export_num(part_ele_tbl)
!
      part_ele_tbl%istack_export(0) = itp_org%istack_nod_tbl_org(0)
      if(part_ele_tbl%nrank_export .gt. 0) then
!$omp parallel workshare
        part_ele_tbl%irank_export(1:part_ele_tbl%nrank_export)          &
     &        = itp_org%id_dest_domain(1:part_ele_tbl%nrank_export)
        part_ele_tbl%istack_export(1:part_ele_tbl%nrank_export)         &
     &        = itp_org%istack_nod_tbl_org(1:part_ele_tbl%nrank_export)
!$omp end parallel workshare
!
        if(part_ele_tbl%irank_export(part_ele_tbl%nrank_export)         &
     &     .eq. irank_read) then
          part_ele_tbl%iflag_self_copy = 1
        else  
          part_ele_tbl%iflag_self_copy = 0
        end if
      end if
!
      if(itp_org%istack_itp_type_org(3)                                 &
     &   .ne. itp_org%istack_itp_type_org(0)) write(*,*) 'Wrong table!'
      if(itp_org%istack_itp_type_org(2)                                 &
     &   .ne. itp_org%istack_itp_type_org(0)) write(*,*) 'Wrong table!'
      if(itp_org%istack_itp_type_org(1)                                 &
     &   .ne. itp_org%istack_itp_type_org(0)) write(*,*) 'Wrong table!'
!
      part_ele_tbl%ntot_export = itp_org%ntot_table_org
      call alloc_calypso_export_item(part_ele_tbl)
!
!$omp parallel do private(inum)
      do inum = 1, part_ele_tbl%ntot_export
        part_ele_tbl%item_export(inum) = itp_org%iele_org_4_org(inum)
      end do
!$omp end parallel do
!
      end subroutine copy_itp_org_to_repart_ele_exp
!
!-----------------------------------------------------------------------
!
      end module copy_repart_ele_and_itp_tbl

