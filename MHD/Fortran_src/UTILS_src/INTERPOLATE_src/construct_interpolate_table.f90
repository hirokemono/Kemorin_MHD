!
!     module construct_interpolate_table
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine s_construct_interpolate_table(ierr_missing)
!
      module construct_interpolate_table
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_construct_interpolate_table(ierr_missing)
!
      use calypso_mpi
      use m_parallel_var_dof
      use m_machine_parameter
      use m_ctl_params_4_gen_table
      use m_geometry_parameter
      use m_2nd_geometry_param
      use m_2nd_pallalel_vector
      use m_read_mesh_data
      use m_work_const_itp_table
!
      use set_2nd_geometry_4_table
      use set_area_4_searching
      use search_node_in_element
      use subroutines_4_search_table
!
      integer(kind = kint), intent(inout) :: ierr_missing
      integer(kind = kint) :: ierr_local
      integer(kind = kint) :: ilevel, jp
      integer(kind = kint) :: my_rank_2nd
      real(kind = kreal) :: error_level_final
!
!
      ierr_local = 1
      do ilevel = 0, num_search_times + 2
!
        if(iflag_debug.eq.1) write(*,*) 'ilevel', ilevel
        if(ierr_local .eq. 0) go to 1
!
        do jp = 1, nprocs_2nd
!
          my_rank_2nd = mod(my_rank+jp-1,nprocs_2nd)
!
          call link_2nd_geometry_4_itp_tbl(my_rank_2nd)
!
          if (iflag_debug.eq.1 .and. jp.eq.1)                           &
     &           write(*,*) 's_set_area_4_searching'
          call s_set_area_4_searching
!
!
          call allocate_work_4_interpolate
!
          if (ilevel .eq. 0) then
            if (i_debug.ge.iflag_routine_msg .and. jp.eq.1)             &
     &        write(*,*) 'search_node_in_element_1st', ilevel, my_rank
            call search_node_in_element_1st(my_rank_2nd)
          else if (ilevel .eq. (num_search_times+1)) then
            if (i_debug.ge.iflag_routine_msg .and. jp.eq.1)             &
     &        write(*,*) 'search_node_in_all_element', ilevel, my_rank
            error_level_final = search_error_level(num_search_times)*2
            call search_node_in_all_element(my_rank_2nd,                &
     &          error_level_final)
          else if (ilevel .eq. (num_search_times+2)) then
            if (i_debug.ge.iflag_routine_msg .and. jp.eq.1)             &
     &        write(*,*) 'giveup_to_search_element', ilevel, my_rank
            error_level_final = search_error_level(num_search_times)*2
            call giveup_to_search_element(my_rank_2nd,                  &
     &          error_level_final)
          else
            if (i_debug.ge.iflag_routine_msg .and. jp.eq.1)             &
     &        write(*,*) 'search_node_in_element_2nd', ilevel, my_rank
            call search_node_in_element_2nd(my_rank_2nd,                &
     &         i_search_sleeve(ilevel), search_error_level(ilevel) )
          end if
!
!          if (ilevel.eq.3) call check_interpolation(14, my_rank_2nd)
!
          call deallocate_work_4_interpolate
          call deallocate_search_area
          call unlink_2nd_geometry_4_table
        end do
!
        call check_missing_nodes(ierr_local, my_rank)
!
  1     continue
!
        call MPI_allREDUCE (ierr_local, ierr_missing, 1,                &
     &      CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM, ierr_MPI)
        if (ierr_missing .eq. 0) exit
!
      end do
!
      if (ierr_missing .gt. 0) then
        write(*,*) 'There is missing nodes in domain ', my_rank
      end if
!
!      call check_table_in_org_1(13)
!
      end subroutine s_construct_interpolate_table
!
! ----------------------------------------------------------------------
!
      end module construct_interpolate_table
