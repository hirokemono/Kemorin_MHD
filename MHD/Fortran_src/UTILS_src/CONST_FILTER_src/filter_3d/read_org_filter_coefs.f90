!read_org_filter_coefs.f90
!      module read_org_filter_coefs
!
!      Written by H. Matsui on May, 2008
!
!!      subroutine read_original_filter_coefs                           &
!!     &         (ifile_type, id_rank, numnod, numele)
!
      module read_org_filter_coefs
!
      use m_precision
!
      use t_geometry_data
      use m_ctl_param_newdom_filter
      use set_parallel_file_name
      use filter_IO_for_newdomain
!
      implicit none
!
      type(binary_IO_flags), private :: bin_flflags
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_original_filter_coefs                             &
     &         (ifile_type, id_rank, numnod, numele)
!
      use m_filter_file_names
      use m_filter_coefs
      use t_comm_table
      use t_geometry_data
!
      use filter_coefs_file_IO
      use filter_coefs_file_IO_b
      use mesh_data_IO
      use mesh_data_IO_b
      use binary_IO
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: ifile_type
      integer(kind = kint), intent(in) :: numnod, numele
!
      integer(kind = kint):: ierr
      character(len=kchara) :: file_name
!
      type(communication_table) :: comm_IO
      type(node_data) :: nod_IO
!
!
      call allocate_nod_ele_near_1nod(numnod, numele)
!
      file_name = add_process_id(id_rank, org_filter_coef_head)
!
      if (ifile_type .eq. 0) then
        write(*,*) 'ascii coefficients file name: ', trim(file_name)
        open(id_org_filter_coef, file=file_name, form='formatted')
        call read_filter_geometry                                       &
     &     (id_org_filter_coef, id_rank, comm_IO, nod_IO, ierr)
!
        inter_nod_3dfilter = nod_IO%internal_node
        call read_filter_coef_4_newdomain(id_org_filter_coef)
        close(id_org_filter_coef)
      else if(ifile_type .eq. 1) then
        write(*,*) 'binary coefficients file name: ', trim(file_name)
        call open_read_binary_file(file_name, id_rank, bin_flflags)
        call read_filter_geometry_b                                     &
     &     (id_rank, bin_flflags, comm_IO, nod_IO)
        if(bin_flflags%ierr_IO .gt. 0) go to 98
!
        inter_nod_3dfilter = nod_IO%internal_node
        call read_filter_coef_4_newdomain_b(bin_flflags)
!
  98    continue
        call close_binary_file
        if(bin_flflags%ierr_IO .gt. 0) stop "Error rading"
      end if
!
      call deallocate_nod_ele_near_1nod
!
      call dealloc_node_geometry_base(nod_IO)
      call dealloc_comm_table(comm_IO)
!
      end subroutine read_original_filter_coefs
!
!  ---------------------------------------------------------------------
!
      end module read_org_filter_coefs
