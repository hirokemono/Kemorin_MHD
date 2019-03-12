!
!      module const_newdomain_filter
!
!      modified by H. Matsui on Apr., 2008
!
!!      subroutine marking_used_node_4_filtering                        &
!!     &         (ip2, ifile_type, mesh_file, nod_d_grp, node, numele)
!!      subroutine trans_filter_4_new_domains                           &
!!     &         (ip2, ifile_type, mesh_file, nod_d_grp, node, numele)
!!        type(field_IO_params), intent(in) ::  mesh_file
!!        type(node_data), intent(inout) :: node
!
      module const_newdomain_filter
!
      use m_precision
!
      use calypso_mpi
      use t_geometry_data
      use t_file_IO_parameter
      use t_domain_group_4_partition
      use m_filter_func_4_sorting
      use m_filter_coefs
      use set_parallel_file_name
      use mesh_IO_select
      use read_org_filter_coefs
      use copy_mesh_structures
      use set_filters_4_new_domains
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine marking_used_node_4_filtering                          &
     &         (ip2, ifile_type, mesh_file, nod_d_grp, node, numele)
!
      type(field_IO_params), intent(in) ::  mesh_file
      type(domain_group_4_partition), intent(in)  :: nod_d_grp
      integer(kind = kint), intent(in) :: ip2, ifile_type
      integer(kind = kint), intent(inout) :: numele
      type(node_data), intent(inout) :: node
!
      type(mesh_geometry) :: mesh_IO_f
      integer(kind = kint) :: ierr
      integer :: ip, id_rank
!
!
      call clear_imark_whole_nod
!
      do ip = 1, nprocs
        id_rank = ip - 1
!
        call sel_read_geometry_size                                     &
     &     (mesh_file, id_rank, mesh_IO_f, ierr)
        if(ierr .gt. 0) then
          call calypso_mpi_abort(ierr, 'Mesh data is wrong!!')
        end if
!
!
        call copy_node_geometry_types(mesh_IO_f%node, node)
        numele = mesh_IO_f%ele%numele
!
        call dealloc_node_geometry_IO(mesh_IO_f)
!
!     read filtering information
!
        call read_original_filter_coefs(ifile_type, id_rank,            &
     &      node%numnod, numele)
!
        call nod_marking_by_filtering_data                              &
     &     (node%numnod, node%internal_node, node%inod_global, node%xx, &
     &      ip2, nod_d_grp)
!
        call deallocate_whole_filter_coefs
        call deallocate_fluid_filter_coefs
!
        call dealloc_node_geometry_base(node)
      end do
!
      end subroutine marking_used_node_4_filtering
!
!------------------------------------------------------------------
!
      subroutine trans_filter_4_new_domains                             &
     &         (ip2, ifile_type, mesh_file, nod_d_grp, node, numele)
!
      type(field_IO_params), intent(in) ::  mesh_file
      type(domain_group_4_partition), intent(in)  :: nod_d_grp
      integer(kind = kint), intent(in) :: ip2, ifile_type
      integer(kind = kint), intent(inout) :: numele
      type(node_data), intent(inout) :: node
!
      type(mesh_geometry) :: mesh_IO_f
      integer(kind = kint) :: ierr, icou_st
      integer(kind = kint) :: ip, id_rank
!
!
      icou_st = 0
      do ip = 1, nprocs
        id_rank = ip - 1
!
        call sel_read_geometry_size                                     &
     &     (mesh_file, id_rank, mesh_IO_f, ierr)
        if(ierr .gt. 0) then
          call calypso_mpi_abort(ierr, 'Mesh data is wrong!!')
        end if
!
!
        call copy_node_geometry_types(mesh_IO_f%node, node)
        numele = mesh_IO_f%ele%numele
!
        call dealloc_node_geometry_IO(mesh_IO_f)
!
!     read filtering information
!
        call read_original_filter_coefs(ifile_type, id_rank,            &
     &      node%numnod, numele)
!
        call set_filter_for_new_each_domain                             &
     &     (node%numnod, node%internal_node, node%inod_global,          &
     &      ip2, nod_d_grp, icou_st)
!
        call deallocate_whole_filter_coefs
        call deallocate_fluid_filter_coefs
!
        call dealloc_node_geometry_base(node)
      end do
!
      end subroutine trans_filter_4_new_domains
!
!------------------------------------------------------------------
!
      end module const_newdomain_filter
