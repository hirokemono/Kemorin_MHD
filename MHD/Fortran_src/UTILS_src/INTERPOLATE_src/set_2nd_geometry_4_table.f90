!
!     module set_2nd_geometry_4_table
!
!     Written by H. Matsui on Sep., 2006
!
!!      subroutine alloc_org_mesh_type_itp_para(nprocs_2nd, itp_org)
!!      subroutine dealloc_org_mesh_type_itp_para(nprocs_2nd, itp_org)
!!        type(parallel_mesh_for_itp), intent(inout) :: para_mesh_org
!!      subroutine set_2nd_geometry_type_itp_tbl                        &
!!     &         (mesh_file, nprocs_2nd, itp_org)
!!      subroutine link_2nd_geometry_4_itp_tbl(id_rank,                 &
!!     &          nprocs_2nd, itp_org, newmesh, newgroup)
!!      subroutine unlink_2nd_geometry_4_table(newmesh, newgroup)
!!      subroutine s_set_serach_data_4_dest(itp_org, dest_node,         &
!!     &          itp_dest, itp_coef, cst_itp_wk, itp_blks)
!!        type(parallel_mesh_for_itp), intent(inout) :: para_mesh_org
!!        type(node_data), intent(in) :: dest_node
!!        type(interpolate_table_dest), intent(in) :: itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: itp_coef
!!        type(work_const_itp_table), intent(inout) :: cst_itp_wk
!!        type(para_block_4_interpolate), intent(inout) :: itp_blks
!
      module set_2nd_geometry_4_table
!
      use m_precision
!
      use m_machine_parameter
      use t_mesh_data
      use t_mesh_data_with_pointer
!
      implicit none
!
      type parallel_mesh_for_itp
        integer :: nprocs_org
        type(mesh_data), allocatable :: para_mesh_org(:)
      end type parallel_mesh_for_itp
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_org_mesh_type_itp_para(nprocs_2nd, itp_org)
!
      integer, intent(in) :: nprocs_2nd
      type(parallel_mesh_for_itp), intent(inout) :: itp_org
!
      itp_org%nprocs_org = nprocs_2nd
      allocate(itp_org%para_mesh_org(itp_org%nprocs_org))
!
      end subroutine alloc_org_mesh_type_itp_para
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_org_mesh_type_itp_para(itp_org)
!
      type(parallel_mesh_for_itp), intent(inout) :: itp_org
!
      deallocate( itp_org%para_mesh_org )
!
      end subroutine dealloc_org_mesh_type_itp_para
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_2nd_geometry_type_itp_tbl                          &
     &         (mesh_file, nprocs_2nd, itp_org)
!
      use t_file_IO_parameter
      use set_parallel_mesh_in_1pe
      use transfer_to_long_integers
!
      type(field_IO_params), intent(in) ::  mesh_file
      integer, intent(in) :: nprocs_2nd
      type(parallel_mesh_for_itp), intent(inout) :: itp_org
!
!
      call alloc_org_mesh_type_itp_para(nprocs_2nd, itp_org)
!
      write(*,*) 'mesh_file_head: ', trim(mesh_file%file_prefix),       &
     &            itp_org%nprocs_org
      call s_set_parallel_mesh_in_1pe                                   &
     &   (mesh_file, itp_org%nprocs_org, itp_org%para_mesh_org)
!
      end subroutine set_2nd_geometry_type_itp_tbl
!
! ----------------------------------------------------------------------
!
      subroutine link_2nd_geometry_4_itp_tbl(id_rank, itp_org,          &
     &                                       newmesh, newgroup)
!
      use m_geometry_constants
      use m_connect_hexa_2_tetra
!
      integer, intent(in) :: id_rank
      type(parallel_mesh_for_itp), intent(in) :: itp_org
      type(mesh_geometry_p), intent(inout) :: newmesh
      type(mesh_groups_p), intent(inout) :: newgroup
      integer(kind = kint) :: jp
!
!
      jp = id_rank + 1
      call link_pointer_mesh(itp_org%para_mesh_org(jp)%mesh,            &
     &                       itp_org%para_mesh_org(jp)%group,           &
     &                       newmesh, newgroup)
!
      if (newmesh%ele%nnod_4_ele .eq. num_t_linear) then
        call set_1_hexa_2_5_tetra
      else if (newmesh%ele%nnod_4_ele .eq. num_t_quad) then
        call set_1_hexa_2_21_tetra
      else if (newmesh%ele%nnod_4_ele .eq. num_t_lag) then
        call set_1_hexa_2_40_tetra
      end if
!
      end subroutine link_2nd_geometry_4_itp_tbl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine s_set_serach_data_4_dest(itp_org, dest_node,           &
     &          itp_dest, itp_coef, cst_itp_wk, itp_blks)
!
      use t_geometry_data
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
      use t_work_const_itp_table
      use t_search_block_4_itp
!
      use order_dest_table_by_type
      use transfer_to_long_integers
!
      type(parallel_mesh_for_itp), intent(in) :: itp_org
      type(node_data), intent(in) :: dest_node
      type(interpolate_table_dest), intent(inout) :: itp_dest
      type(interpolate_coefs_dest), intent(inout) :: itp_coef
      type(work_const_itp_table), intent(inout) :: cst_itp_wk
      type(para_block_4_interpolate), intent(inout) :: itp_blks
!
      integer :: np2
!
!
      np2 = int(itp_org%nprocs_org,KIND(np2))
      call alloc_interpolate_blocks(np2, itp_blks)
      call set_all_block_points_4_itp(itp_blks%num_xyz_block,           &
     &     dest_node%numnod, dest_node%xx, itp_org%para_mesh_org,       &
     &    itp_blks%np_org, itp_blks%org_blocks, itp_blks%dest_block)
!      call check_block_points_4_itp(50+id_rank, itp_org%nprocs_org,    &
!     &                              itp_blks)
!
!  -------------------------------
!
      if (iflag_debug.eq.1)                                             &
     &     write(*,*)  'allocate_interpolate_table'
!
      itp_dest%ntot_table_dest = dest_node%internal_node
      call set_num_org_domain(cast_kint(itp_org%nprocs_org), itp_dest)
      call alloc_itp_num_dest(itp_dest)
      call alloc_itp_table_dest(itp_dest)
      call alloc_itp_coef_dest(itp_dest, itp_coef)
      call alloc_work_const_itp_tbl(itp_dest, cst_itp_wk%orderd)
      call alloc_iflag_org_domain(dest_node%numnod, cst_itp_wk)
!
      end subroutine s_set_serach_data_4_dest
!
! ----------------------------------------------------------------------
!
      end module set_2nd_geometry_4_table
