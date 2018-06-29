!set_inod_newdomain_filter.f90
!      module set_inod_newdomain_filter
!
!     Written by H. Matsui on May., 2008
!
!!      subroutine set_inod_4_newdomain_filter                          &
!!     &         (mesh_file, org_node, org_ele, new_node, ierr)
!!        type(field_IO_params), intent(in) :: mesh_file
!!        type(node_data),    intent(inout) :: org_node
!!        type(element_data), intent(inout) :: org_ele
!!        type(node_data), intent(inout) :: new_node
!
      module set_inod_newdomain_filter
!
      use m_precision
!
      use m_2nd_pallalel_vector
      use m_internal_4_partitioner
      use set_parallel_file_name
      use mesh_IO_select
      use const_newdomain_filter
      use set_filters_4_new_domains
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_inod_4_newdomain_filter                            &
     &         (mesh_file, org_node, org_ele, new_node, ierr)
!
      use t_mesh_data
      use t_file_IO_parameter
!
      use m_internal_4_partitioner
      use m_filter_file_names
      use m_field_file_format
      use copy_mesh_structures
!
      type(field_IO_params), intent(in) :: mesh_file
      type(node_data),    intent(inout) :: org_node
      type(element_data), intent(inout) :: org_ele
      type(node_data), intent(inout) :: new_node
      integer(kind = kint), intent(inout) :: ierr
!
      type(mesh_geometry) :: mesh_IO_f
      integer(kind = kint) :: ip2, my_rank2
!
      do ip2 = 1, nprocs_2nd
        my_rank2 = ip2 - 1
!
        call sel_read_geometry_size                                     &
     &     (tgt_mesh_file, my_rank2, mesh_IO_f, ierr)
        if(ierr .gt. 0) return
!
        call copy_node_geometry_types(mesh_IO_f%node, new_node)
!
        call dealloc_node_geometry_IO(mesh_IO_f)
!
        call marking_used_node_4_filtering                              &
     &     (ip2, ifmt_3d_filter, mesh_file, org_node, org_ele%numele)
!
        call set_num_globalnod_4_newdomain(ip2, new_node)
!
        call dealloc_node_geometry_base(new_node)
      end do
!
      end subroutine set_inod_4_newdomain_filter
!
!   --------------------------------------------------------------------
!
      end module set_inod_newdomain_filter
