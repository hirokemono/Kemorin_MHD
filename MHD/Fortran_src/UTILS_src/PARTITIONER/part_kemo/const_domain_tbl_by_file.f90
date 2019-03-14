! const_domain_tbl_by_file.f90
!      module const_domain_tbl_by_file
!
!      modified by H. Matsui on Apr., 2008
!
!!      subroutine s_const_domain_tbl_by_file(mesh_head, nod_d_grp)
!!      subroutine count_nnod_whole_domain(mesh_file, nod_d_grp)
!!        type(domain_group_4_partition), intent(inout) :: nod_d_grp
!!      subroutine set_domain_grp_whole_domain(mesh_file, nod_d_grp)
!!        type(field_IO_params), intent(in) :: mesh_file
!!        type(domain_group_4_partition), intent(inout) :: nod_d_grp
!!      subroutine set_domain_grp_each_domain                           &
!!     &         (mesh_file, my_rank2, nod_d_grp)
!!        type(field_IO_params), intent(in) :: mesh_file
!!        type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
      module const_domain_tbl_by_file
!
      use m_precision
!
      use m_2nd_pallalel_vector
      use t_mesh_data
      use t_file_IO_parameter
      use t_domain_group_4_partition
      use set_parallel_file_name
      use mesh_IO_select
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_const_domain_tbl_by_file(mesh_file, nod_d_grp)
!
      type(field_IO_params), intent(in) :: mesh_file
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
!
      call count_nnod_whole_domain(mesh_file, nod_d_grp)
!
      call alloc_domain_group(nod_d_grp)
      call alloc_local_id_tbl(nod_d_grp)
      call alloc_org_gl_id(nod_d_grp)
!
      call set_domain_grp_whole_domain(mesh_file, nod_d_grp)
!
      end subroutine s_const_domain_tbl_by_file
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_nnod_whole_domain(mesh_file, nod_d_grp)
!
      type(field_IO_params), intent(in) :: mesh_file
!
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
      type(mesh_geometry) :: mesh_IO_p
      integer :: ip, my_rank2
      integer(kind = kint) :: ierr
!
!
      nod_d_grp%num_s_domin = 0
      do ip = 1, nprocs_2nd
        my_rank2 = ip - 1
        call sel_read_node_size(mesh_file, my_rank2, mesh_IO_p, ierr)
          if(ierr .gt. 0) then
          stop 'MESH data is wrong in count_nnod_whole_domain'
        end if
!
        nod_d_grp%num_s_domin                                           &
     &     = nod_d_grp%num_s_domin + mesh_IO_p%node%internal_node
!
        call dealloc_neib_id(mesh_IO_p%nod_comm)
      end do
!
      end subroutine count_nnod_whole_domain
!
!------------------------------------------------------------------
!
      subroutine set_domain_grp_whole_domain(mesh_file, nod_d_grp)
!
      type(field_IO_params), intent(in) :: mesh_file
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
      integer :: my_rank2
!
!
      do my_rank2 = 0, nprocs_2nd-1
        call set_domain_grp_each_domain(mesh_file, my_rank2, nod_d_grp)
      end do
!
      end subroutine set_domain_grp_whole_domain
!
! ----------------------------------------------------------------------
!
      subroutine set_domain_grp_each_domain                             &
     &         (mesh_file, my_rank2, nod_d_grp)
!
      integer, intent(in)  :: my_rank2
      type(field_IO_params), intent(in) :: mesh_file
!
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
      type(mesh_geometry) :: mesh_IO_p
      integer :: ip2
      integer(kind = kint) :: inod, ierr
      integer(kind = kint_gl) :: inod_g
!
!
      ip2 = my_rank2 + 1
      call sel_read_geometry_size(mesh_file, my_rank2, mesh_IO_p, ierr)
      if(ierr .gt. 0) then
        stop 'MESH data is wrong in set_domain_grp_each_domain'
      end if
!
      do inod = 1, mesh_IO_p%node%internal_node
        inod_g = mesh_IO_p%node%inod_global(inod)
        nod_d_grp%IGROUP(inod_g) = ip2
        nod_d_grp%id_global_org(inod_g) = inod_g
      end do
!
      call dealloc_node_geometry_IO(mesh_IO_p)
!
      end subroutine set_domain_grp_each_domain
!
! ----------------------------------------------------------------------
!
      end module const_domain_tbl_by_file
