!read_filter_file_4_sorting.f90
!      module read_filter_file_4_sorting
!
!
!     Written by H. Matsui on Mar., 2008
!
!!      subroutine s_read_filter_file_4_sorting                         &
!!     &          (ifile_type, id_rank, filtering, fil_gen,             &
!!     &           whole_fil_sort, fluid_fil_sort)
!!        type(filtering_data_type), intent(inout) :: filtering
!!        type(const_filter_coefs), intent(inout) :: fil_gen
!
      module read_filter_file_4_sorting
!
      use m_precision
!
      use t_filtering_data
      use t_comm_table
      use t_geometry_data
      use t_filter_coefs
      use t_filter_coefficients
      use t_filter_func_4_sorting
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
     &          (ifile_type, id_rank, filtering, fil_gen,               &
     &           whole_fil_sort, fluid_fil_sort)
!
      use m_filter_file_names
      use filter_IO_for_sorting
      use set_parallel_file_name
      use filter_coefs_file_IO
      use filter_coefs_file_IO_b
      use set_filter_geometry_4_IO
      use cal_minmax_and_stacks
      use mesh_data_IO
      use mesh_data_IO_b
      use binary_IO
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: ifile_type
      type(filtering_data_type), intent(inout) :: filtering
      type(const_filter_coefs), intent(inout) :: fil_gen
      type(filter_func_4_sorting), intent(inout) :: whole_fil_sort
      type(filter_func_4_sorting), intent(inout) :: fluid_fil_sort
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
!
        call read_filter_geometry                                       &
     &     (filter_coef_code, id_rank, comm_IO, nod_IO, ierr)
!
        call copy_comm_tbl_type(comm_IO, filtering%comm)
        call copy_filtering_geometry_from_IO(nod_IO)
!
        call dealloc_node_geometry_base(nod_IO)
        call dealloc_comm_table(comm_IO)
!
        fil_gen%nmax_nod_near_all_w = 0
        call read_filter_neib_4_sort                                    &
     &     (filter_coef_code, fil_gen%whole_area, whole_fil_sort,       &
     &      fil_gen%nmax_nod_near_all_w)
        call read_filter_neib_4_sort                                    &
     &     (filter_coef_code, fil_gen%fluid_area, fluid_fil_sort,       &
     &      fil_gen%nmax_nod_near_all_w)
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
        fil_gen%nmax_nod_near_all_w = 0
        call read_filter_neib_4_sort_b                                  &
     &     (bin_flflags, fil_gen%whole_area, whole_fil_sort,            &
     &     fil_gen%nmax_nod_near_all_w)
        if(bin_flflags%ierr_IO .gt. 0) go to 99
        call read_filter_neib_4_sort_b                                  &
     &     (bin_flflags, fil_gen%fluid_area, fluid_fil_sort,            &
     &     fil_gen%nmax_nod_near_all_w)
        if(bin_flflags%ierr_IO .gt. 0) go to 99
!
  99    continue
        call close_binary_file
        if(bin_flflags%ierr_IO .gt. 0) stop "Error rading"
      end if
!
!
      whole_fil_sort%ntot_nod_near_filter = 0
      fluid_fil_sort%ntot_nod_near_filter = 0
      call alloc_filter_func_4_sort(whole_fil_sort)
      call alloc_filter_func_4_sort(fluid_fil_sort)
!
      write(*,*) 'set_num_filter_group_4_sort'
      call set_num_filter_group_4_sort                                  &
     &   (fluid_fil_sort, filtering%filter)
      write(*,*) 'allocate_num_filtering_comb'
      call alloc_num_filtering_comb(ione, filtering%filter)
!
      write(*,*) 'count_num_neib_4_filter_sort'
      call count_num_neib_4_filter_sort                                 &
     &   (fluid_fil_sort, filtering%filter)
!
      write(*,*) 'alloc_inod_filter_comb'
      fil_gen%fil_sorted%ntot_nod = filtering%filter%ntot_nod
      call alloc_inod_filter_comb(fil_gen%fil_sorted)
!
      write(*,*) 'set_num_of_neib_4_filter_sort'
      call set_num_of_neib_4_filter_sort(filtering%filter,              &
     &    whole_fil_sort, fil_gen%fil_sorted, fil_gen%whole_area,       &
     &    fil_gen%fluid_area, fluid_fil_sort,                           &
     &    fil_gen%nmax_nod_near_all_w, fil_gen%nmin_nod_near_all_w)
!
!
      write(*,*) 'alloc_each_filter_coef', fil_gen%nmax_nod_near_all_w
      call alloc_each_filter_coef                                       &
     &   (fil_gen%nmax_nod_near_all_w, fil_gen%fil_coef)
!
      call alloc_3d_filter_comb(fil_gen%fil_sorted)
      call alloc_3d_filter_func(fil_gen%fil_sorted)
!
      file_name = add_process_id(id_rank, filter_coef_head)
      if ( ifile_type .eq. 0) then
        open(filter_coef_code, file=file_name, form='formatted')
!
        call read_filter_geometry                                       &
     &     (filter_coef_code, id_rank, comm_IO, nod_IO, ierr)
        write(*,*) 'read_filter_coef_4_sort'
        call read_filter_coef_4_sort(filter_coef_code,                  &
     &      fil_gen%whole_area, fil_gen%fluid_area,                     &
     &      fil_gen%fil_coef, fil_gen%fil_sorted)
        close(filter_coef_code)
      else if( ifile_type .eq. 1) then
        call open_read_binary_file(file_name, id_rank, bin_flflags)
        call read_filter_geometry_b                                     &
     &     (id_rank, bin_flflags, comm_IO, nod_IO)
        if(bin_flflags%ierr_IO .gt. 0) go to 98
!
        fil_gen%nmax_nod_near_all_w = 0
        call read_filter_neib_4_sort_b                                  &
     &     (bin_flflags, fil_gen%whole_area, whole_fil_sort,            &
     &     fil_gen%nmax_nod_near_all_w)
        if(bin_flflags%ierr_IO .gt. 0) go to 98
        call read_filter_neib_4_sort_b                                  &
     &     (bin_flflags, fil_gen%fluid_area, fluid_fil_sort,            &
     &     fil_gen%nmax_nod_near_all_w)
        if(bin_flflags%ierr_IO .gt. 0) go to 98
!
        call read_filter_coef_4_sort_b(bin_flflags, filtering%filter,   &
     &      fil_gen%whole_area, fil_gen%fluid_area,                     &
     &      fil_gen%fil_coef, fil_gen%fil_sorted)
!
  98    continue
        call close_binary_file
        if(bin_flflags%ierr_IO .gt. 0) stop "Error rading"
      end if
!
      call dealloc_node_geometry_base(nod_IO)
      call dealloc_comm_table(comm_IO)
      call dealloc_each_filter_coef(fil_gen%fil_coef)
!
      end subroutine s_read_filter_file_4_sorting
!
!  ---------------------------------------------------------------------
!
      subroutine set_num_filter_group_4_sort(fluid_fil_sort, filter)
!
      use m_nod_filter_comm_table
!
      type(filter_func_4_sorting), intent(in) :: fluid_fil_sort
      type(filter_coefficients_type), intent(inout) :: filter
!
      integer(kind = kint) :: inod
!
      filter%ngrp_node = 1
      do inod = 1, inter_nod_3dfilter
        if ( fluid_fil_sort%nnod_near_nod_filter(inod) .ge. 0) then
          filter%ngrp_node = 3
          exit
        end if
      end do
!
      end subroutine set_num_filter_group_4_sort
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_neib_4_filter_sort(fluid_fil_sort, filter)
!
      use m_constants
      use m_nod_filter_comm_table
      use cal_minmax_and_stacks
!
      type(filter_func_4_sorting), intent(in) :: fluid_fil_sort
      type(filter_coefficients_type), intent(inout) :: filter
!
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
          if     (fluid_fil_sort%nnod_near_nod_filter(inod) .lt. 0) then
            filter%num_node(1) = filter%num_node(1) + 1
          else if(fluid_fil_sort%nnod_near_nod_filter(inod) .eq. 0) then
            filter%num_node(2) = filter%num_node(2) + 1
          else if(fluid_fil_sort%nnod_near_nod_filter(inod) .gt. 0) then
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
      subroutine set_num_of_neib_4_filter_sort                          &
     &         (filter, whole_fil_sort, fil_sorted,                     &
     &          whole_area, fluid_area, fluid_fil_sort,                 &
     &          nmax_nod_near_all_w, nmin_nod_near_all_w)
!
      use m_constants
      use m_nod_filter_comm_table
      use cal_minmax_and_stacks
!
      type(filter_coefficients_type), intent(in) :: filter
      type(filter_func_4_sorting), intent(in) :: whole_fil_sort
!
      type(filter_coefficients_type), intent(inout) :: fil_sorted
      type(filter_area_flag), intent(inout) :: whole_area, fluid_area
      type(filter_func_4_sorting), intent(inout) :: fluid_fil_sort
      integer(kind = kint), intent(inout) :: nmax_nod_near_all_w
      integer(kind = kint), intent(inout) :: nmin_nod_near_all_w
!
      integer(kind = kint) :: inod, icou, jcou, kcou
!
!
      if ( filter%ngrp_node .eq. 1) then
        do inod = 1, inter_nod_3dfilter
          fil_sorted%inod_filter(inod) = inod
          fil_sorted%nnod_near(inod)                                    &
     &          = whole_fil_sort%nnod_near_nod_filter(inod)
          whole_area%itbl_near_nod(inod) = inod
        end do
      else if ( filter%ngrp_node .eq. 3) then
        icou = filter%istack_node(0)
        jcou = filter%istack_node(1)
        kcou = filter%istack_node(2)
        do inod = 1, inter_nod_3dfilter
          if     (fluid_fil_sort%nnod_near_nod_filter(inod) .lt. 0) then
            icou = icou + 1
            fil_sorted%inod_filter(icou) = inod
            fil_sorted%nnod_near(icou)                                  &
     &            = whole_fil_sort%nnod_near_nod_filter(inod)
            whole_area%itbl_near_nod(inod) = icou
            fluid_area%itbl_near_nod(inod) = filter%ntot_nod + 1
            fluid_fil_sort%nnod_near_nod_filter(inod) = 0
          else if(fluid_fil_sort%nnod_near_nod_filter(inod) .eq. 0) then
            jcou = jcou + 1
            fil_sorted%inod_filter(jcou) = inod
            fil_sorted%nnod_near(jcou)                                  &
     &            = whole_fil_sort%nnod_near_nod_filter(inod)
            whole_area%itbl_near_nod(inod) = jcou
            fluid_area%itbl_near_nod(inod) = filter%ntot_nod + 1
            fluid_fil_sort%nnod_near_nod_filter(inod) = 0
          else if(fluid_fil_sort%nnod_near_nod_filter(inod) .gt. 0) then
            jcou = jcou + 1
            kcou = kcou + 1
            fil_sorted%inod_filter(jcou) = inod
            fil_sorted%inod_filter(kcou) = inod
            fil_sorted%nnod_near(jcou)                                  &
     &            = whole_fil_sort%nnod_near_nod_filter(inod)
            fil_sorted%nnod_near(kcou)                                  &
     &            = fluid_fil_sort%nnod_near_nod_filter(inod)
            whole_area%itbl_near_nod(inod) = jcou
            fluid_area%itbl_near_nod(inod) = kcou
          end if
        end do
      end if
!
      call s_cal_minmax_and_stacks(filter%ntot_nod,                     &
     &    fil_sorted%nnod_near, izero, fil_sorted%istack_near_nod,      &
     &    fil_sorted%ntot_near_nod, nmax_nod_near_all_w,                &
     &    nmin_nod_near_all_w)
!
      end subroutine set_num_of_neib_4_filter_sort
!
!  ---------------------------------------------------------------------
!
      end module read_filter_file_4_sorting
