!read_org_filter_coefs.f90
!      module read_org_filter_coefs
!
!      Written by H. Matsui on May, 2008
!
!!      subroutine read_original_filter_coefs(org_filter_coef_head,     &
!!     &          ifile_type, id_rank, numnod, numele,                  &
!!     &          filter_node, fil_coef, whole_fil_sort, fluid_fil_sort)
!!        type(node_data), intent(inout) :: filter_node
!!        type(each_filter_coef), intent(inout) :: fil_coef
!!        type(filter_func_4_sorting), intent(inout) :: whole_fil_sort
!!        type(filter_func_4_sorting), intent(inout) :: fluid_fil_sort
!
      module read_org_filter_coefs
!
      use m_precision
!
      use t_geometry_data
      use t_binary_IO_buffer
      use set_parallel_file_name
      use filter_IO_for_newdomain
!
      implicit none
!
      integer(kind = kint), parameter :: id_org_filter_coef = 23
      integer(kind = kint), parameter :: id_read_filter =  21
      type(binary_IO_buffer) :: bbuf_flt
!
      private :: id_org_filter_coef, id_read_filter, bbuf_flt
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_original_filter_coefs(org_filter_coef_head,       &
     &          ifile_type, id_rank, numnod, numele,                    &
     &          filter_node, fil_coef, whole_fil_sort, fluid_fil_sort)
!
      use m_filter_file_names
      use t_filter_coefs
      use t_comm_table
      use t_geometry_data
      use t_filter_func_4_sorting
!
      use filter_coefs_file_IO
      use filter_coefs_file_IO_b
      use mesh_data_IO
      use mesh_data_IO_b
      use binary_IO
!
      character(len=kchara), intent(in) :: org_filter_coef_head
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: ifile_type
      integer(kind = kint), intent(in) :: numnod, numele
!
      type(node_data), intent(inout) :: filter_node
      type(each_filter_coef), intent(inout) :: fil_coef
      type(filter_func_4_sorting), intent(inout) :: whole_fil_sort
      type(filter_func_4_sorting), intent(inout) :: fluid_fil_sort
!
      integer(kind = kint):: ierr
      character(len=kchara) :: file_name
!
      type(communication_table) :: comm_IO
      type(node_data) :: nod_IO
!
!
      call alloc_each_filter_coef(numnod, fil_coef)
      call alloc_each_ele_filter_coef(numele, fil_coef)
!
      file_name = add_process_id(id_rank, org_filter_coef_head)
!
      if (ifile_type .eq. 0) then
        write(*,*) 'ascii coefficients file name: ', trim(file_name)
        open(id_org_filter_coef, file=file_name, form='formatted')
        call read_filter_geometry                                       &
     &     (id_org_filter_coef, id_rank, comm_IO, nod_IO, ierr)
!
        filter_node%internal_node = nod_IO%internal_node
        call read_filter_coef_4_newdomain                               &
     &     (id_org_filter_coef, filter_node, fil_coef,                  &
     &      whole_fil_sort, fluid_fil_sort, ierr)
        close(id_org_filter_coef)
        if(ierr .gt. 0) stop "Error rading"
      else if(ifile_type .eq. 1) then
        write(*,*) 'binary coefficients file name: ', trim(file_name)
        bbuf_flt%id_binary = id_read_filter
        call open_read_binary_file(file_name, id_rank, bbuf_flt)
        if(bbuf_flt%ierr_bin .ne. 0) goto 98
        call read_filter_geometry_b                                     &
     &     (id_rank, bbuf_flt, comm_IO, nod_IO)
        if(bbuf_flt%ierr_bin .gt. 0) go to 98
!
        filter_node%internal_node = nod_IO%internal_node
        call read_filter_coef_4_newdomain_b(bbuf_flt, filter_node,      &
     &      fil_coef, whole_fil_sort, fluid_fil_sort)
!
  98    continue
        call close_binary_file(bbuf_flt)
        if(bbuf_flt%ierr_bin .gt. 0) stop "Error rading"
      end if
!
      call dealloc_each_filter_coef(fil_coef)
!
      call dealloc_node_geometry_base(nod_IO)
      call dealloc_comm_table(comm_IO)
!
      end subroutine read_original_filter_coefs
!
!  ---------------------------------------------------------------------
!
      end module read_org_filter_coefs
