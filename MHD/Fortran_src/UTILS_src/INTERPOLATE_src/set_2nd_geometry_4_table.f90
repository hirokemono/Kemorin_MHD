!
!     module set_2nd_geometry_4_table
!
!     Written by H. Matsui on Sep., 2006
!
!!      subroutine set_2nd_geometry_type_itp_tbl(mesh_file, nprocs_2nd)
!!        type(field_IO_params), intent(in) ::  mesh_file
!!      subroutine link_2nd_geometry_4_itp_tbl(id_rank,                 &
!!     &          newmesh, newgroup)
!!      subroutine unlink_2nd_geometry_4_table(newmesh, newgroup)
!!      subroutine s_set_serach_data_4_dest                             &
!!     &         (dest_node, itp_dest, itp_coef, cst_itp_wk, itp_blks)
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
      use m_2nd_pallalel_vector
      use m_connect_hexa_2_tetra
!
      implicit none
!
      type(mesh_data), allocatable, private :: origin_mesh(:)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_org_mesh_type_itp_para(nprocs_2nd)
!
      integer, intent(in) :: nprocs_2nd
!
!
      allocate( origin_mesh(nprocs_2nd) )
!
      end subroutine alloc_org_mesh_type_itp_para
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_org_mesh_type_itp_para
!
      deallocate( origin_mesh )
!
      end subroutine dealloc_org_mesh_type_itp_para
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_2nd_geometry_type_itp_tbl(mesh_file, nprocs_2nd)
!
      use t_file_IO_parameter
      use m_ctl_params_4_gen_table
      use set_parallel_mesh_in_1pe
      use transfer_to_long_integers
!
      type(field_IO_params), intent(in) ::  mesh_file
      integer, intent(in) :: nprocs_2nd
!
!
      call alloc_org_mesh_type_itp_para(nprocs_2nd)
!
      write(*,*) 'mesh_file_head: ', trim(mesh_file%file_prefix),       &
     &            nprocs_2nd
      call s_set_parallel_mesh_in_1pe                                   &
     &   (mesh_file, nprocs_2nd, origin_mesh)
!
      end subroutine set_2nd_geometry_type_itp_tbl
!
! ----------------------------------------------------------------------
!
      subroutine link_2nd_geometry_4_itp_tbl(id_rank,                   &
     &          newmesh, newgroup)
!
      use m_geometry_constants
!
      integer, intent(in) :: id_rank
      type(mesh_geometry_p), intent(inout) :: newmesh
      type(mesh_groups_p), intent(inout) :: newgroup
      integer(kind = kint) :: jp
!
!
      jp = id_rank + 1
      call link_pointer_mesh                                           &
     &   (origin_mesh(jp)%mesh, origin_mesh(jp)%group, newmesh, newgroup)
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
      subroutine s_set_serach_data_4_dest                               &
     &         (dest_node, itp_dest, itp_coef, cst_itp_wk, itp_blks)
!
      use t_geometry_data
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
      use t_work_const_itp_table
      use t_search_block_4_itp
!
      use m_2nd_pallalel_vector
      use m_ctl_params_4_gen_table
!
      use order_dest_table_by_type
      use transfer_to_long_integers
!
      type(node_data), intent(in) :: dest_node
      type(interpolate_table_dest), intent(inout) :: itp_dest
      type(interpolate_coefs_dest), intent(inout) :: itp_coef
      type(work_const_itp_table), intent(inout) :: cst_itp_wk
      type(para_block_4_interpolate), intent(inout) :: itp_blks
!
      integer :: np2
!
!
      np2 = int(nprocs_2nd,KIND(np2))
      call alloc_interpolate_blocks(np2, itp_blks)
      call set_all_block_points_4_itp(itp_blks%num_xyz_block,           &
    &     dest_node%numnod, dest_node%xx, origin_mesh,                  &
     &    itp_blks%np_org, itp_blks%org_blocks, itp_blks%dest_block)
!      call check_block_points_4_itp(50+id_rank, nprocs_2nd, itp_blks)
!
!  -------------------------------
!
      if (iflag_debug.eq.1)                                             &
     &     write(*,*)  'allocate_interpolate_table'
!
      itp_dest%ntot_table_dest = dest_node%internal_node
      call set_num_org_domain(cast_kint(nprocs_2nd), itp_dest)
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
