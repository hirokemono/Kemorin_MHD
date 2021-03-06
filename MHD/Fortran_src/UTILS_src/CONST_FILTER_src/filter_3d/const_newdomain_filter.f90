!
!      module const_newdomain_filter
!
!      modified by H. Matsui on Apr., 2008
!
!!      subroutine marking_used_node_4_filtering                        &
!!     &         (ip2, ifile_type, org_filter_coef_head, mesh_file,     &
!!     &          nod_d_grp, node, numele, filter_node, fil_coef,       &
!!     &          whole_fil_sort, fluid_fil_sort)
!!      subroutine trans_filter_4_new_domains                           &
!!     &         (ip2, ifile_type, org_filter_coef_head, mesh_file,     &
!!     &          nod_d_grp, node, numele, filter_node,                 &
!!     &          fil_coef, fils_sort)
!!        type(field_IO_params), intent(in) ::  mesh_file
!!        type(node_data), intent(inout) :: node
!!        type(filters_4_sorting), intent(inout) :: fils_sort
!
      module const_newdomain_filter
!
      use m_precision
!
      use calypso_mpi
      use t_geometry_data
      use t_file_IO_parameter
      use t_domain_group_4_partition
      use t_filter_coefs
      use t_filter_func_4_sorting
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
     &         (ip2, ifile_type, org_filter_coef_head, mesh_file,       &
     &          nod_d_grp, node, numele, filter_node, fil_coef,         &
     &          whole_fil_sort, fluid_fil_sort)
!
      type(field_IO_params), intent(in) ::  mesh_file
      type(domain_group_4_partition), intent(in)  :: nod_d_grp
      integer(kind = kint), intent(in) :: ip2
      integer(kind = kint), intent(in) :: ifile_type
      character(len=kchara), intent(in) :: org_filter_coef_head
!
      integer(kind = kint), intent(inout) :: numele
      type(node_data), intent(inout) :: node
      type(node_data), intent(inout) :: filter_node
      type(each_filter_coef), intent(inout) :: fil_coef
      type(filter_func_4_sorting), intent(inout) :: whole_fil_sort
      type(filter_func_4_sorting), intent(inout) :: fluid_fil_sort
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
        call sel_read_geometry_size(mesh_file, id_rank,                 &
     &                              mesh_IO_f, ierr)
        if(ierr .gt. 0) then
          call calypso_mpi_abort(ierr, 'Mesh data is wrong!!')
        end if
!
!
        call copy_node_geometry(mesh_IO_f%node, node)
        numele = mesh_IO_f%ele%numele
!
        call dealloc_node_geometry_IO(mesh_IO_f)
!
!     read filtering information
!
        call read_original_filter_coefs(org_filter_coef_head,           &
     &      ifile_type, id_rank, node%numnod, numele,                   &
     &      filter_node, fil_coef, whole_fil_sort, fluid_fil_sort)
!
        call nod_marking_by_filtering_data                              &
     &     (node%numnod, node%internal_node, node%inod_global, node%xx, &
     &      ip2, nod_d_grp, whole_fil_sort, fluid_fil_sort)
!
        call dealloc_filter_func_4_sort(whole_fil_sort)
        call dealloc_filter_num_4_sort(whole_fil_sort)
        call dealloc_filter_func_4_sort(fluid_fil_sort)
        call dealloc_filter_num_4_sort(fluid_fil_sort)
!
        call dealloc_node_geometry_base(node)
      end do
!
      end subroutine marking_used_node_4_filtering
!
!------------------------------------------------------------------
!
      subroutine trans_filter_4_new_domains                             &
     &         (ip2, ifile_type, org_filter_coef_head, mesh_file,       &
     &          nod_d_grp, node, numele, filter_node,                   &
     &          fil_coef, fils_sort)
!
      type(field_IO_params), intent(in) :: mesh_file
      type(domain_group_4_partition), intent(in)  :: nod_d_grp
      integer(kind = kint), intent(in) :: ip2
      integer(kind = kint), intent(in) :: ifile_type
      character(len=kchara), intent(in) :: org_filter_coef_head
!
      integer(kind = kint), intent(inout) :: numele
      type(node_data), intent(inout) :: node
      type(node_data), intent(inout) :: filter_node
      type(each_filter_coef), intent(inout) :: fil_coef
      type(filters_4_sorting), intent(inout) :: fils_sort
!
      type(mesh_geometry) :: mesh_IO_f
      integer(kind = kint) :: ierr, icou_st
      integer :: ip, id_rank
!
!
      icou_st = 0
      do ip = 1, nprocs
        id_rank = ip - 1
!
        call sel_read_geometry_size(mesh_file, id_rank,                 &
     &                              mesh_IO_f, ierr)
        if(ierr .gt. 0) then
          call calypso_mpi_abort(ierr, 'Mesh data is wrong!!')
        end if
!
!
        call copy_node_geometry(mesh_IO_f%node, node)
        numele = mesh_IO_f%ele%numele
!
        call dealloc_node_geometry_IO(mesh_IO_f)
!
!     read filtering information
!
        call read_original_filter_coefs(org_filter_coef_head,           &
     &      ifile_type, id_rank, node%numnod, numele, filter_node,      &
     &      fil_coef, fils_sort%whole_fil_sort,                         &
     &      fils_sort%fluid_fil_sort)
!
        call set_filter_for_new_each_domain                             &
     &     (node%numnod, node%internal_node, node%inod_global, ip2,     &
     &      fils_sort%intnod_w_fliter2, nod_d_grp,                      &
     &      fils_sort%whole_fil_sort, fils_sort%fluid_fil_sort,         &
     &      fils_sort%whole_fil_sort2, fils_sort%fluid_fil_sort2,       &
     &      fils_sort%inod_filter_new_2, icou_st)
!
        call dealloc_filter_func_4_sort(fils_sort%whole_fil_sort)
        call dealloc_filter_num_4_sort(fils_sort%whole_fil_sort)
        call dealloc_filter_func_4_sort(fils_sort%fluid_fil_sort)
        call dealloc_filter_num_4_sort(fils_sort%fluid_fil_sort)
!
        call dealloc_node_geometry_base(node)
      end do
!
      end subroutine trans_filter_4_new_domains
!
!------------------------------------------------------------------
!
      end module const_newdomain_filter
