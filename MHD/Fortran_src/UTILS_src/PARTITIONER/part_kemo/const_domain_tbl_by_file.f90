! const_domain_tbl_by_file.f90
!      module const_domain_tbl_by_file
!
!      modified by H. Matsui on Apr., 2008
!
!      subroutine s_const_domain_tbl_by_file(mesh_head)
!      subroutine count_nnod_whole_domain(mesh_head)
!      subroutine set_domain_grp_whole_domain(mesh_head)
!      subroutine set_domain_grp_each_domain(mesh_head, my_rank2)
!
      module const_domain_tbl_by_file
!
      use m_precision
!
      use m_domain_group_4_partition
      use m_2nd_pallalel_vector
      use m_read_mesh_data
      use m_comm_data_IO
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
      subroutine s_const_domain_tbl_by_file(mesh_head)
!
      character(len=kchara), intent(in) :: mesh_head
!
!
      call count_nnod_whole_domain(mesh_head)
!
      call allocate_domain_nese_group
      call allocate_org_gl_nese_id
      call allocate_local_nese_id_tbl
!
      call set_domain_grp_whole_domain(mesh_head)
!
      end subroutine s_const_domain_tbl_by_file
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_nnod_whole_domain(mesh_head)
!
      character(len=kchara), intent(in) :: mesh_head
      integer(kind = kint) :: ip, my_rank2
!
      nele_s_domin = 0
      nsurf_s_domin = 0
      nedge_s_domin = 0
!
      nnod_s_domin = 0
      do ip = 1, nprocs_2nd
        my_rank2 = ip - 1
        mesh_file_head = mesh_head
        call sel_read_node_size(my_rank2)
!
        nnod_s_domin = nnod_s_domin + nod_IO%internal_node
!
        call deallocate_neib_domain_IO
      end do
!
      end subroutine count_nnod_whole_domain
!
!------------------------------------------------------------------
!
      subroutine set_domain_grp_whole_domain(mesh_head)
!
      character(len=kchara), intent(in) :: mesh_head
!
      integer(kind = kint) :: my_rank2
!
!
      do my_rank2 = 0, nprocs_2nd-1
        call set_domain_grp_each_domain(mesh_head, my_rank2)
      end do
!
      end subroutine set_domain_grp_whole_domain
!
! ----------------------------------------------------------------------
!
      subroutine set_domain_grp_each_domain(mesh_head, my_rank2)
!
      character(len=kchara), intent(in) :: mesh_head
      integer(kind = kint), intent(in)  :: my_rank2

      integer(kind = kint) :: ip2, inod
      integer(kind = kint_gl) :: inod_g
!
!
      ip2 = my_rank2 + 1
      mesh_file_head = mesh_head
      call sel_read_geometry_size(my_rank2)
!
      do inod = 1, nod_IO%internal_node
        inod_g = nod_IO%inod_global(inod)
        IGROUP_nod(inod_g) = ip2
        id_glnode_org(inod_g) = inod_g
      end do
!
      call deallocate_node_data_dummy
      call deallocate_neib_domain_IO
!
      end subroutine set_domain_grp_each_domain
!
! ----------------------------------------------------------------------
!
      end module const_domain_tbl_by_file
