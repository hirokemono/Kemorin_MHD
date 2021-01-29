!set_inod_newdomain_filter.f90
!      module set_inod_newdomain_filter
!
!     Written by H. Matsui on May., 2008
!
!!      subroutine set_inod_4_newdomain_filter                          &
!!     &         (nprocs_2nd, newfil_p, nod_d_grp, org_node, org_ele,   &
!!     &          new_node, itl_nod_part, fil_coef,                     &
!!     &          whole_fil_sort, fluid_fil_sort, ierr)
!!        type(field_IO_params), intent(in) :: mesh_file
!!        type(domain_group_4_partition), intent(in)  :: nod_d_grp
!!        type(node_data),    intent(inout) :: org_node
!!        type(element_data), intent(inout) :: org_ele
!!        type(node_data), intent(inout) :: new_node
!!        type(internal_4_partitioner), intent(inout) :: itl_nod_part
!!        type(each_filter_coef), intent(inout) :: fil_coef
!!        type(filter_func_4_sorting), intent(inout) :: whole_fil_sort
!!        type(filter_func_4_sorting), intent(inout) :: fluid_fil_sort
!
      module set_inod_newdomain_filter
!
      use m_precision
!
      use set_parallel_file_name
      use mesh_IO_select
      use const_newdomain_filter
      use set_filters_4_new_domains
      use t_ctl_param_newdom_filter
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
     &         (nprocs_2nd, newfil_p, nod_d_grp, org_node, org_ele,     &
     &          new_node, itl_nod_part, fil_coef,                       &
     &          whole_fil_sort, fluid_fil_sort, ierr)
!
      use t_mesh_data
      use t_file_IO_parameter
      use t_internal_4_partitioner
      use t_filter_coefs
      use t_filter_func_4_sorting
!
      use m_filter_file_names
      use m_field_file_format
      use copy_mesh_structures
!
      integer, intent(in) :: nprocs_2nd
      type(ctl_param_newdom_filter), intent(in) :: newfil_p
      type(domain_group_4_partition), intent(in)  :: nod_d_grp
      type(node_data),    intent(inout) :: org_node
      type(element_data), intent(inout) :: org_ele
      type(node_data), intent(inout) :: new_node
      type(internal_4_partitioner), intent(inout) :: itl_nod_part
      type(each_filter_coef), intent(inout) :: fil_coef
      type(filter_func_4_sorting), intent(inout) :: whole_fil_sort
      type(filter_func_4_sorting), intent(inout) :: fluid_fil_sort
      integer(kind = kint), intent(inout) :: ierr
!
      type(mesh_geometry) :: mesh_IO_f
      integer(kind = kint) :: ip2
      integer :: my_rank2
!
      do my_rank2 = 0, nprocs_2nd-1
        ip2 = int(my_rank2 + 1,KIND(ip2))
!
        call sel_read_geometry_size                                     &
     &     (newfil_p%tgt_mesh_file, my_rank2, mesh_IO_f, ierr)
        if(ierr .gt. 0) return
!
        call copy_node_geometry_types(mesh_IO_f%node, new_node)
!
        call dealloc_node_geometry_IO(mesh_IO_f)
!
        call marking_used_node_4_filtering                              &
     &     (ip2, ifmt_3d_filter, newfil_p%org_filter_coef_head,         &
     &      newfil_p%org_mesh_file, nod_d_grp, org_node,                &
     &      org_ele%numele, fil_coef, whole_fil_sort, fluid_fil_sort)
!
        call set_num_globalnod_4_newdomain                              &
     &     (ip2, nod_d_grp, itl_nod_part, new_node)
!
        call dealloc_node_geometry_base(new_node)
      end do
!
      end subroutine set_inod_4_newdomain_filter
!
!   --------------------------------------------------------------------
!
      end module set_inod_newdomain_filter
