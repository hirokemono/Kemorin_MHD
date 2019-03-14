!
!     module construct_interpolate_table
!
!     Written by H. Matsui on Aug., 2006
!
!!      subroutine s_construct_interpolate_table                        &
!!     &         (node, neib_nod, itp_coef_dest, ierr_missing)
!!        type(node_data), intent(in) :: node
!!        type(next_nod_id_4_nod), intent(in)  :: neib_nod
!!        type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
!
      module construct_interpolate_table
!
      use m_precision
      use t_geometry_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_construct_interpolate_table                          &
     &         (node, neib_nod, itp_coef_dest, ierr_missing)
!
      use calypso_mpi
      use m_machine_parameter
      use m_ctl_params_4_gen_table
      use m_2nd_pallalel_vector
      use m_work_const_itp_table
      use m_search_bolck_4_itp
!
      use t_mesh_data
      use t_next_node_ele_4_node
      use t_interpolate_coefs_dest
!
      use set_2nd_geometry_4_table
      use search_node_in_element
      use subroutines_4_search_table
!
      type(node_data), intent(in) :: node
      type(next_nod_id_4_nod), intent(in)  :: neib_nod
!
      integer(kind = kint), intent(inout) :: ierr_missing
      type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
!
      type(mesh_geometry_p) :: org_mesh
      type(mesh_groups_p) ::   org_grp
!
      integer(kind = kint) :: ierr_local
      integer(kind = kint) :: ilevel
      integer :: my_rank_2nd, jp
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
          call link_2nd_geometry_4_itp_tbl                              &
     &       (my_rank_2nd, org_mesh, org_grp)
!
          call allocate_work_4_interpolate(org_mesh%ele%nnod_4_ele)
!
          if (ilevel .eq. 0) then
            if (i_debug.ge.iflag_routine_msg .and. jp.eq.1)             &
     &        write(*,*) 'search_node_in_element_1st', ilevel, my_rank
            call search_node_in_element_1st(my_rank_2nd,                &
     &          org_mesh%node, org_mesh%ele, org_blocks(my_rank_2nd+1), &
     &          node, itp_coef_dest)
          else if (ilevel .eq. (num_search_times+1)) then
            if (i_debug.ge.iflag_routine_msg .and. jp.eq.1)             &
     &        write(*,*) 'search_node_in_all_element', ilevel, my_rank
            error_level_final = search_error_level(num_search_times)*2
            call search_node_in_all_element(my_rank_2nd,                &
     &          error_level_final, org_mesh%node, org_mesh%ele, node,   &
     &          itp_coef_dest)
          else if (ilevel .eq. (num_search_times+2)) then
            if (i_debug.ge.iflag_routine_msg .and. jp.eq.1)             &
     &        write(*,*) 'giveup_to_search_element', ilevel, my_rank
            error_level_final = search_error_level(num_search_times)*2
            call giveup_to_search_element(my_rank_2nd,                  &
     &          error_level_final, neib_nod%istack_next,                &
     &          org_mesh%node, org_mesh%ele, node, itp_coef_dest)
          else
            if (i_debug.ge.iflag_routine_msg .and. jp.eq.1)             &
     &        write(*,*) 'search_node_in_element_2nd', ilevel, my_rank
            call search_node_in_element_2nd                             &
     &         ((ilevel-1), my_rank_2nd, org_mesh%node, org_mesh%ele,   &
     &          org_blocks(my_rank_2nd+1), node, itp_coef_dest)
          end if
!
!          if (ilevel.eq.3) call check_interpolation                    &
!     &     (node, org_mesh%node, org_mesh%ele, itp_coef_dest,          &
!     &      14, my_rank_2nd)
!
          call deallocate_work_4_interpolate
!
          call deallocate_hex_2_tetra
          call unlink_pointer_mesh(org_mesh, org_grp)
        end do
!
        call check_missing_nodes(ierr_local, my_rank, node)
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
      end subroutine s_construct_interpolate_table
!
! ----------------------------------------------------------------------
!
      end module construct_interpolate_table
