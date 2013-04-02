!read_filter_file_4_sorting.f90
!      module read_filter_file_4_sorting
!
      module read_filter_file_4_sorting
!
!     Written by H. Matsui on Mar., 2008
!
      use m_precision
!
      implicit none
!
      private :: set_num_filter_group_4_sort
      private :: count_num_neib_4_filter_sort
      private :: set_num_of_neib_4_filter_sort
!
!      subroutine s_read_filter_file_4_sorting
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_read_filter_file_4_sorting(ifile_type, my_rank)
!
      use m_filter_file_names
      use m_filter_coef_combained
      use m_filter_coefs
      use m_filter_func_4_sorting
      use filter_IO_for_sorting
      use set_parallel_file_name
      use filter_geometry_IO
      use set_filter_geometry_4_IO
      use set_filter_comm_tbl_4_IO
!
      integer(kind = kint), intent(in) :: ifile_type, my_rank
      character(len=kchara) :: file_name
!
!
      call add_int_suffix(my_rank, filter_coef_head, file_name)
      if ( ifile_type .eq. 0) then
        open(filter_coef_code, file=file_name, form='formatted')
        call read_filter_geometry(filter_coef_code)
      else if( ifile_type .eq. 1) then
        open(filter_coef_code, file=file_name, form='unformatted')
        call read_filter_geometry_b(filter_coef_code)
      end if
!
      call copy_filter_comm_tbl_from_IO
      call copy_filtering_geometry_from_IO
!
      if ( ifile_type .eq. 0) then
        write(*,*) 'read_filter_neib_4_sort', inter_nod_3dfilter
        call read_filter_neib_4_sort(filter_coef_code)
      else if( ifile_type .eq. 1) then
        call read_filter_neib_4_sort_b(filter_coef_code)
      end if
      close(filter_coef_code)
!
!
      ntot_nod_near_w_filter = 0
      ntot_nod_near_f_filter = 0
      call allocate_whole_filter_coefs
      call allocate_fluid_filter_coefs
!
      write(*,*) 'set_num_filter_group_4_sort'
      call set_num_filter_group_4_sort
      write(*,*) 'allocate_num_filtering_comb'
      call allocate_num_filtering_comb
!
      write(*,*) 'count_num_neib_4_filter_sort'
      call count_num_neib_4_filter_sort
!
      write(*,*) 'allocate_inod_all_w'
      call allocate_inod_all_w
      write(*,*) 'allocate_num_near_all_w'
      call allocate_num_near_all_w
!
      write(*,*) 'set_num_of_neib_4_filter_sort'
      call set_num_of_neib_4_filter_sort
      write(*,*) 'allocate_nod_ele_near_1nod', nmax_nod_near_all_w, nmax_ele_near_all_w
      call allocate_nod_ele_near_1nod(nmax_nod_near_all_w,              &
     &                                nmax_ele_near_all_w)
!
      call allocate_filter_coefs
      call allocate_nod_ele_near_all_w
!
!
      call add_int_suffix(my_rank, filter_coef_head, file_name)
      if ( ifile_type .eq. 0) then
        open(filter_coef_code, file=file_name, form='formatted')
!
        call read_filter_geometry(filter_coef_code)
        write(*,*) 'read_filter_coef_4_sort'
        call read_filter_coef_4_sort(filter_coef_code)
!
      else if( ifile_type .eq. 1) then
        open(filter_coef_code, file=file_name, form='unformatted')
!
        call read_filter_geometry_b(filter_coef_code)
!
        call read_filter_neib_4_sort_b(filter_coef_code)
        call read_filter_coef_4_sort_b(filter_coef_code)
      end if
      close(filter_coef_code)
!
      call deallocate_node_data_dummy
      call deallocate_comm_item_IO
      call deallocate_nod_ele_near_1nod
!
      end subroutine s_read_filter_file_4_sorting
!
!  ---------------------------------------------------------------------
!
!
      subroutine set_num_filter_group_4_sort
!
      use m_nod_filter_comm_table
      use m_filter_coefs
      use m_filter_func_4_sorting
      use m_filter_coef_combained
!
      integer(kind = kint) :: inod
!
      ngrp_nod_3d_filter = 1
      do inod = 1, inter_nod_3dfilter
        if ( nnod_near_nod_f_filter(inod) .ge. 0) then
          ngrp_nod_3d_filter = 3
          exit
        end if
      end do
!
      end subroutine set_num_filter_group_4_sort
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_neib_4_filter_sort
!
      use m_constants
      use m_nod_filter_comm_table
      use m_filter_coefs
      use m_filter_coef_combained
      use m_filter_func_4_sorting
      use cal_minmax_and_stacks
!
      integer(kind = kint) :: inod
!
      if ( ngrp_nod_3d_filter .eq. 1) then
        grp_name_3d_filter(1) = "All"
        num_nod_3d_filter(1) =  inter_nod_3dfilter
      else if ( ngrp_nod_3d_filter .eq. 3) then
        grp_name_3d_filter(1) = "Both"
        grp_name_3d_filter(2) = "Whole"
        grp_name_3d_filter(3) = "Fluid"
        num_nod_3d_filter(1:3) = 0
        do inod = 1, inter_nod_3dfilter
          if     (nnod_near_nod_f_filter(inod) .lt. 0) then
            num_nod_3d_filter(1) = num_nod_3d_filter(1) + 1
          else if(nnod_near_nod_f_filter(inod) .eq. 0) then
            num_nod_3d_filter(2) = num_nod_3d_filter(2) + 1
          else if(nnod_near_nod_f_filter(inod) .gt. 0) then
            num_nod_3d_filter(2) = num_nod_3d_filter(2) + 1
            num_nod_3d_filter(3) = num_nod_3d_filter(3) + 1
          end if
        end do
      end if
!
      call s_cal_total_and_stacks(ngrp_nod_3d_filter,                   &
     &    num_nod_3d_filter, izero, istack_nod_3d_filter,               &
     &    ntot_nod_3d_filter)
      write(*,*) 'inter_nod_3dfilter', inter_nod_3dfilter
      write(*,*) 'istack_nod_3d_filter', istack_nod_3d_filter
!
      end subroutine count_num_neib_4_filter_sort
!
!  ---------------------------------------------------------------------
!
      subroutine set_num_of_neib_4_filter_sort
!
      use m_constants
      use m_nod_filter_comm_table
      use m_filter_coefs
      use m_filter_coef_combained
      use m_filter_func_4_sorting
      use cal_minmax_and_stacks
!
      integer(kind = kint) :: inod, icou, jcou, kcou
!
!
      if ( ngrp_nod_3d_filter .eq. 1) then
        do inod = 1, inter_nod_3dfilter
          inod_all_w(inod) = inod
          nnod_near_nod_all_w(inod) = nnod_near_nod_w_filter(inod)
          itbl_near_nod_whole(inod) = inod
        end do
      else if ( ngrp_nod_3d_filter .eq. 3) then
        icou = istack_nod_3d_filter(0)
        jcou = istack_nod_3d_filter(1)
        kcou = istack_nod_3d_filter(2)
        do inod = 1, inter_nod_3dfilter
          if     (nnod_near_nod_f_filter(inod) .lt. 0) then
            icou = icou + 1
            inod_all_w(icou) = inod
            nnod_near_nod_all_w(icou) = nnod_near_nod_w_filter(inod)
            itbl_near_nod_whole(inod) = icou
            itbl_near_nod_fluid(inod) = ntot_nod_3d_filter + 1
            nnod_near_nod_f_filter(inod) = 0
          else if(nnod_near_nod_f_filter(inod) .eq. 0) then
            jcou = jcou + 1
            inod_all_w(jcou) = inod
            nnod_near_nod_all_w(jcou) = nnod_near_nod_w_filter(inod)
            itbl_near_nod_whole(inod) = jcou
            itbl_near_nod_fluid(inod) = ntot_nod_3d_filter + 1
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
      call s_cal_minmax_and_stacks(ntot_nod_3d_filter,                  &
     &    nnod_near_nod_all_w, izero, inod_stack_nod_all_w,             &
     &    ntot_nod_near_all_w, nmax_nod_near_all_w,                     &
     &    nmin_nod_near_all_w)
      call s_cal_minmax_and_stacks(ntot_nod_3d_filter,                  &
     &    nele_near_nod_all_w, izero, iele_stack_nod_all_w,             &
     &    ntot_ele_near_all_w, nmax_ele_near_all_w,                     &
     &    nmin_ele_near_all_w)
!
      end subroutine set_num_of_neib_4_filter_sort
!
!  ---------------------------------------------------------------------
!
      end module read_filter_file_4_sorting
