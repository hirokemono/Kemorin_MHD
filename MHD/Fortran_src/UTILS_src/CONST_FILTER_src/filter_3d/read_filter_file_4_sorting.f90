!read_filter_file_4_sorting.f90
!      module read_filter_file_4_sorting
!
!
!     Written by H. Matsui on Mar., 2008
!
!!      subroutine s_read_filter_file_4_sorting                         &
!!     &          (ifile_type, id_rank, filter, fil_coef)
!!       type(filtering_data_type), intent(inout) :: filtering
!!       type(each_filter_coef), intent(inout) :: fil_coef
!
      module read_filter_file_4_sorting
!
      use m_precision
!
      use t_filtering_data
      use t_comm_table
      use t_geometry_data
      use binary_IO
!
      implicit none
!
      type(binary_IO_flags), private :: bin_flflags
!
      private :: set_num_filter_group_4_sort
      private :: count_num_neib_4_filter_sort
      private :: set_num_of_neib_4_filter_sort
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_read_filter_file_4_sorting                           &
     &          (ifile_type, id_rank, filtering, fil_coef)
!
      use t_filter_coefs
      use m_filter_coefs
      use m_filter_file_names
      use m_filter_func_4_sorting
      use filter_IO_for_sorting
      use set_parallel_file_name
      use filter_coefs_file_IO
      use filter_coefs_file_IO_b
      use set_filter_geometry_4_IO
      use mesh_data_IO
      use mesh_data_IO_b
      use binary_IO
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: ifile_type
      type(filtering_data_type), intent(inout) :: filtering
      type(each_filter_coef), intent(inout) :: fil_coef
!
      integer(kind = kint) :: ierr
      character(len=kchara) :: file_name
      type(communication_table) :: comm_IO
      type(node_data) :: nod_IO
!
!
      file_name = add_process_id(id_rank, filter_coef_head)
      if ( ifile_type .eq. 0) then
        open(filter_coef_code, file=file_name, form='formatted')
        call read_filter_geometry                                       &
     &     (filter_coef_code, id_rank, comm_IO, nod_IO, ierr)
!
        call copy_comm_tbl_type(comm_IO, filtering%comm)
        call copy_filtering_geometry_from_IO(nod_IO)
!
        call dealloc_node_geometry_base(nod_IO)
        call dealloc_comm_table(comm_IO)
!
        write(*,*) 'read_filter_neib_4_sort', inter_nod_3dfilter
        call read_filter_neib_4_sort(filter_coef_code)
        close(filter_coef_code)
      else if( ifile_type .eq. 1) then
        call open_read_binary_file(file_name, id_rank, bin_flflags)
        call read_filter_geometry_b                                     &
     &     (id_rank, bin_flflags, comm_IO, nod_IO)
        if(bin_flflags%ierr_IO .gt. 0) go to 99
!
        call copy_comm_tbl_type(comm_IO, filtering%comm)
        call copy_filtering_geometry_from_IO(nod_IO)
!
        call dealloc_node_geometry_base(nod_IO)
        call dealloc_comm_table(comm_IO)
!
        call read_filter_neib_4_sort_b(bin_flflags)

  99    continue
        call close_binary_file
        if(bin_flflags%ierr_IO .gt. 0) stop "Error rading"
      end if
!
!
      ntot_nod_near_w_filter = 0
      ntot_nod_near_f_filter = 0
      call allocate_whole_filter_coefs
      call allocate_fluid_filter_coefs
!
      write(*,*) 'set_num_filter_group_4_sort'
      call set_num_filter_group_4_sort(filtering%filter)
      write(*,*) 'allocate_num_filtering_comb'
      call alloc_num_filtering_comb(ione, filtering%filter)
!
      write(*,*) 'count_num_neib_4_filter_sort'
      call count_num_neib_4_filter_sort(filtering%filter)
!
      write(*,*) 'allocate_num_near_all_w'
      call allocate_num_near_all_w(filtering%filter)
!
      write(*,*) 'set_num_of_neib_4_filter_sort'
      call set_num_of_neib_4_filter_sort(filtering%filter)
      write(*,*) 'allocate_nod_ele_near_1nod',                          &
     &          nmax_nod_near_all_w, nmax_ele_near_all_w
      call allocate_nod_ele_near_1nod(nmax_nod_near_all_w,              &
     &                                nmax_ele_near_all_w, fil_coef)
!
      call allocate_filter_coefs
      call allocate_nod_ele_near_all_w
!
!
      file_name = add_process_id(id_rank, filter_coef_head)
      if ( ifile_type .eq. 0) then
        open(filter_coef_code, file=file_name, form='formatted')
!
        call read_filter_geometry                                       &
     &     (filter_coef_code, id_rank, comm_IO, nod_IO, ierr)
        write(*,*) 'read_filter_coef_4_sort'
        call read_filter_coef_4_sort(filter_coef_code, fil_coef)
        close(filter_coef_code)
      else if( ifile_type .eq. 1) then
        call open_read_binary_file(file_name, id_rank, bin_flflags)
        call read_filter_geometry_b                                     &
     &     (id_rank, bin_flflags, comm_IO, nod_IO)
        if(bin_flflags%ierr_IO .gt. 0) go to 98
!
        call read_filter_neib_4_sort_b(bin_flflags)
        if(bin_flflags%ierr_IO .gt. 0) goto 98
!
        call read_filter_coef_4_sort_b                                  &
     &     (bin_flflags, filtering%filter, fil_coef)
!
  98    continue
        call close_binary_file
        if(bin_flflags%ierr_IO .gt. 0) stop "Error rading"
      end if
!
      call dealloc_node_geometry_base(nod_IO)
      call dealloc_comm_table(comm_IO)
      call deallocate_nod_ele_near_1nod(fil_coef)
!
      end subroutine s_read_filter_file_4_sorting
!
!  ---------------------------------------------------------------------
!
      subroutine set_num_filter_group_4_sort(filter)
!
      use m_nod_filter_comm_table
      use m_filter_coefs
      use m_filter_func_4_sorting
!
      type(filter_coefficients_type), intent(inout) :: filter
      integer(kind = kint) :: inod
!
      filter%ngrp_node = 1
      do inod = 1, inter_nod_3dfilter
        if ( nnod_near_nod_f_filter(inod) .ge. 0) then
          filter%ngrp_node = 3
          exit
        end if
      end do
!
      end subroutine set_num_filter_group_4_sort
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_neib_4_filter_sort(filter)
!
      use m_constants
      use m_nod_filter_comm_table
      use m_filter_coefs
      use m_filter_func_4_sorting
      use cal_minmax_and_stacks
!
      type(filter_coefficients_type), intent(inout) :: filter
      integer(kind = kint) :: inod
!
      if ( filter%ngrp_node .eq. 1) then
        filter%group_name(1) = "All"
        filter%num_node(1) =  inter_nod_3dfilter
      else if ( filter%ngrp_node .eq. 3) then
        filter%group_name(1) = "Both"
        filter%group_name(2) = "Whole"
        filter%group_name(3) = "Fluid"
        filter%num_node(1:3) = 0
        do inod = 1, inter_nod_3dfilter
          if     (nnod_near_nod_f_filter(inod) .lt. 0) then
            filter%num_node(1) = filter%num_node(1) + 1
          else if(nnod_near_nod_f_filter(inod) .eq. 0) then
            filter%num_node(2) = filter%num_node(2) + 1
          else if(nnod_near_nod_f_filter(inod) .gt. 0) then
            filter%num_node(2) = filter%num_node(2) + 1
            filter%num_node(3) = filter%num_node(3) + 1
          end if
        end do
      end if
!
      call s_cal_total_and_stacks(filter%ngrp_node,                     &
     &    filter%num_node, izero, filter%istack_node,                   &
     &    filter%ntot_nod)
      write(*,*) 'inter_nod_3dfilter', inter_nod_3dfilter
      write(*,*) 'istack_nod_3d_filter', filter%istack_node
!
      end subroutine count_num_neib_4_filter_sort
!
!  ---------------------------------------------------------------------
!
      subroutine set_num_of_neib_4_filter_sort(filter)
!
      use m_constants
      use m_nod_filter_comm_table
      use m_filter_coefs
      use m_filter_func_4_sorting
      use cal_minmax_and_stacks
!
      type(filter_coefficients_type), intent(in) :: filter
      integer(kind = kint) :: inod, icou, jcou, kcou
!
!
      if ( filter%ngrp_node .eq. 1) then
        do inod = 1, inter_nod_3dfilter
          inod_all_w(inod) = inod
          nnod_near_nod_all_w(inod) = nnod_near_nod_w_filter(inod)
          itbl_near_nod_whole(inod) = inod
        end do
      else if ( filter%ngrp_node .eq. 3) then
        icou = filter%istack_node(0)
        jcou = filter%istack_node(1)
        kcou = filter%istack_node(2)
        do inod = 1, inter_nod_3dfilter
          if     (nnod_near_nod_f_filter(inod) .lt. 0) then
            icou = icou + 1
            inod_all_w(icou) = inod
            nnod_near_nod_all_w(icou) = nnod_near_nod_w_filter(inod)
            itbl_near_nod_whole(inod) = icou
            itbl_near_nod_fluid(inod) = filter%ntot_nod + 1
            nnod_near_nod_f_filter(inod) = 0
          else if(nnod_near_nod_f_filter(inod) .eq. 0) then
            jcou = jcou + 1
            inod_all_w(jcou) = inod
            nnod_near_nod_all_w(jcou) = nnod_near_nod_w_filter(inod)
            itbl_near_nod_whole(inod) = jcou
            itbl_near_nod_fluid(inod) = filter%ntot_nod + 1
            nnod_near_nod_f_filter(inod) = 0
          else if(nnod_near_nod_f_filter(inod) .gt. 0) then
            jcou = jcou + 1
            kcou = kcou + 1
            inod_all_w(jcou) = inod
            inod_all_w(kcou) = inod
            nnod_near_nod_all_w(jcou) = nnod_near_nod_w_filter(inod)
            nnod_near_nod_all_w(kcou) = nnod_near_nod_f_filter(inod)
            itbl_near_nod_whole(inod) = jcou
            itbl_near_nod_fluid(inod) = kcou
          end if
        end do
      end if
!
      call s_cal_minmax_and_stacks(filter%ntot_nod,                     &
     &    nnod_near_nod_all_w, izero, inod_stack_nod_all_w,             &
     &    ntot_nod_near_all_w, nmax_nod_near_all_w,                     &
     &    nmin_nod_near_all_w)
      call s_cal_minmax_and_stacks(filter%ntot_nod,                     &
     &    nele_near_nod_all_w, izero, iele_stack_nod_all_w,             &
     &    ntot_ele_near_all_w, nmax_ele_near_all_w,                     &
     &    nmin_ele_near_all_w)
!
      end subroutine set_num_of_neib_4_filter_sort
!
!  ---------------------------------------------------------------------
!
      end module read_filter_file_4_sorting
