!
!     module construct_interpolate_table
!
!     Written by H. Matsui on Aug., 2006
!
!!      subroutine s_construct_interpolate_table(itp_org, gen_itp_p,    &
!!     &          nnod_dest, internod_dest, xx_dest, neib_nod,          &
!!     &          org_blocks, itp_coef_dest, iflag_org_domain,          &
!!     &          ierr_missing)
!!        type(ctl_params_4_gen_table), intent(in) :: gen_itp_p
!!        type(next_nod_id_4_nod), intent(in)  :: neib_nod
!!        type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
!!        type(block_4_interpolate), intent(in) :: org_blocks(nprocs_2nd)
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
      subroutine s_construct_interpolate_table(itp_org, gen_itp_p,      &
     &          nnod_dest, internod_dest, xx_dest, neib_nod,            &
     &          org_blocks, itp_coef_dest, iflag_org_domain,            &
     &          ierr_missing)
!
      use calypso_mpi
      use calypso_mpi_int
      use m_machine_parameter
      use m_connect_hexa_2_tetra
!
      use t_ctl_params_4_gen_table
      use t_mesh_data
      use t_next_node_ele_4_node
      use t_interpolate_coefs_dest
      use t_search_block_4_itp
!
      use set_2nd_geometry_4_table
      use search_node_in_element
      use subroutines_4_search_table
!
      type(parallel_mesh_for_itp), intent(in) :: itp_org
      type(ctl_params_4_gen_table), intent(in) :: gen_itp_p
      type(next_nod_id_4_nod), intent(in)  :: neib_nod
      type(block_4_interpolate), intent(in)                             &
     &                           :: org_blocks(itp_org%nprocs_org)
!
      integer(kind = kint), intent(in) :: nnod_dest, internod_dest
      real(kind = kreal), intent(in) :: xx_dest(nnod_dest,3)
! 
      integer(kind = kint), intent(inout) :: ierr_missing
      type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
      integer(kind = kint), intent(inout)                               &
     &                    :: iflag_org_domain(nnod_dest)
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
      do ilevel = 0, gen_itp_p%num_search_times + 2
!
        if(iflag_debug.eq.1) write(*,*) 'ilevel', ilevel
        if(ierr_local .eq. 0) go to 1
!
        do jp = 1, itp_org%nprocs_org
!
          my_rank_2nd = mod(my_rank+jp-1,itp_org%nprocs_org)
!
          call link_2nd_geometry_4_itp_tbl(my_rank_2nd, itp_org,        &
      &                                    org_mesh, org_grp)
!
          call allocate_work_4_interpolate(org_mesh%ele%nnod_4_ele)
!
          if (ilevel .eq. 0) then
            if (i_debug.ge.iflag_routine_msg .and. jp.eq.1)             &
     &        write(*,*) 'search_node_in_element_1st', ilevel, my_rank
            call search_node_in_element_1st(my_rank_2nd, gen_itp_p,     &
     &          org_mesh%node, org_mesh%ele, org_blocks(my_rank_2nd+1), &
     &          nnod_dest, internod_dest, xx_dest,                      &
     &          itp_coef_dest, iflag_org_domain)
          else if (ilevel .eq. (gen_itp_p%num_search_times+1)) then
            if (i_debug.ge.iflag_routine_msg .and. jp.eq.1)             &
     &        write(*,*) 'search_node_in_all_element', ilevel, my_rank
            error_level_final = 2                                       &
     &       * gen_itp_p%search_error_level(gen_itp_p%num_search_times)
            call search_node_in_all_element                             &
     &          (my_rank_2nd, error_level_final, gen_itp_p,             &
     &          org_mesh%node, org_mesh%ele,                            &
     &          nnod_dest, internod_dest, xx_dest,                      &
     &          itp_coef_dest, iflag_org_domain)
          else if (ilevel .eq. (gen_itp_p%num_search_times+2)) then
            if (i_debug.ge.iflag_routine_msg .and. jp.eq.1)             &
     &        write(*,*) 'giveup_to_search_element', ilevel, my_rank
            error_level_final = 2                                       &
     &       * gen_itp_p%search_error_level(gen_itp_p%num_search_times)
            call giveup_to_search_element(my_rank_2nd,                  &
     &          error_level_final, neib_nod%istack_next, gen_itp_p,     &
     &          org_mesh%node, org_mesh%ele,                            &
     &          nnod_dest, internod_dest, xx_dest,                      &
     &          itp_coef_dest, iflag_org_domain)
          else
            if (i_debug.ge.iflag_routine_msg .and. jp.eq.1)             &
     &        write(*,*) 'search_node_in_element_2nd', ilevel, my_rank
            call search_node_in_element_2nd((ilevel-1), my_rank_2nd,    &
     &          gen_itp_p, org_mesh%node, org_mesh%ele,                 &
     &          org_blocks(my_rank_2nd+1),                              &
     &          nnod_dest, internod_dest, xx_dest,                      &
     &          itp_coef_dest, iflag_org_domain)
          end if
!
!          if (ilevel.eq.3) call check_interpolation                    &
!     &     (nnod_dest, internod_dest, xx_dest,                         &
!     &      org_mesh%node, org_mesh%ele, itp_coef_dest,                &
!     &      14, my_rank_2nd, iflag_org_domain)
!
          call deallocate_work_4_interpolate
!
          call deallocate_hex_2_tetra
          call unlink_pointer_mesh(org_mesh, org_grp)
        end do
!
        call check_missing_nodes                                        &
     &     (my_rank, nnod_dest, internod_dest, xx_dest,                 &
     &      iflag_org_domain, ierr_local)
!
  1     continue
!
        call calypso_mpi_allreduce_one_int(ierr_local,                  &
     &                                     ierr_missing, MPI_SUM)
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
      subroutine const_interpolate_table_each                           &
     &         (id_rank_search, org_mesh, org_grp, gen_itp_p,           &
     &          nnod_dest, internod_dest, xx_dest, neib_nod,            &
     &          org_blocks, itp_coef_dest, iflag_org_domain,            &
     &          ierr_missing)
!
      use calypso_mpi
      use calypso_mpi_int
      use m_machine_parameter
      use m_connect_hexa_2_tetra
!
      use t_ctl_params_4_gen_table
      use t_mesh_data
      use t_next_node_ele_4_node
      use t_interpolate_coefs_dest
      use t_search_block_4_itp
!
      use set_2nd_geometry_4_table
      use search_node_in_element
      use subroutines_4_search_table
      use transfer_to_long_integers
!
      integer, intent(in) :: id_rank_search
      type(mesh_geometry_p), intent(in) :: org_mesh
      type(mesh_groups_p), intent(in) ::   org_grp
!
      type(ctl_params_4_gen_table), intent(in) :: gen_itp_p
      type(next_nod_id_4_nod), intent(in)  :: neib_nod
      type(block_4_interpolate), intent(in) :: org_blocks
!
      integer(kind = kint), intent(in) :: nnod_dest, internod_dest
      real(kind = kreal), intent(in) :: xx_dest(nnod_dest,3)
! 
      integer(kind = kint), intent(inout) :: ierr_missing
      type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
      integer(kind = kint), intent(inout)                               &
     &                    :: iflag_org_domain(nnod_dest)
!
      integer(kind = kint) :: ilevel
      real(kind = kreal) :: error_level_final
      integer (kind = kint) :: inod, icou
      integer (kind = kint), allocatable :: iflag_org_gl(:)
!
!
      allocate(iflag_org_gl(1:nnod_dest))
!$omp parallel workshare
      iflag_org_gl(1:nnod_dest) = 0
!$omp end parallel workshare

      
      call allocate_work_4_interpolate(org_mesh%ele%nnod_4_ele)
!
            if (i_debug.ge.iflag_routine_msg)                           &
     &        write(*,*) 'search_node_in_element_1st', ilevel, my_rank
            call search_node_in_element_1st(my_rank, gen_itp_p,         &
     &          org_mesh%node, org_mesh%ele, org_blocks,                &
     &          nnod_dest, internod_dest, xx_dest,                      &
     &          itp_coef_dest, iflag_org_domain)
!
      call calypso_mpi_allreduce_int(iflag_org_domain, iflag_org_gl,    &
     &                               cast_long(internod_dest), MPI_MAX)
      icou = 0
      do inod = 1, internod_dest
        if(iflag_org_gl(inod) .le. 0) then
          icou = icou + 1
        else
          iflag_org_domain(inod) = iflag_org_gl(inod)
        end if
      end do
      if(my_rank .eq. 0) write(*,*) icou, 'points are missing'
!
!
      do ilevel = 1, gen_itp_p%num_search_times + 2
            if (i_debug.ge.iflag_routine_msg)                           &
     &        write(*,*) 'search_node_in_element_2nd', ilevel, my_rank
            call search_node_in_element_2nd((ilevel-1), my_rank,        &
     &          gen_itp_p, org_mesh%node, org_mesh%ele, org_blocks,     &
     &          nnod_dest, internod_dest, xx_dest,                      &
     &          itp_coef_dest, iflag_org_domain)
        call calypso_mpi_allreduce_int(iflag_org_domain, iflag_org_gl,  &
     &                               cast_long(internod_dest), MPI_MAX)
        icou = 0
        do inod = 1, internod_dest
          if(iflag_org_gl(inod) .le. 0) then
            icou = icou + 1
          else
            iflag_org_domain(inod) = iflag_org_gl(inod)
          end if
        end do
        if(my_rank .eq. 0) write(*,*) icou, 'points are still missing'
      end do
!

            if (i_debug.ge.iflag_routine_msg)                           &
     &        write(*,*) 'search_node_in_all_element', ilevel, my_rank
            error_level_final = 2                                       &
     &       * gen_itp_p%search_error_level(gen_itp_p%num_search_times)
            call search_node_in_all_element                             &
     &          (my_rank, error_level_final, gen_itp_p,                 &
     &          org_mesh%node, org_mesh%ele,                            &
     &          nnod_dest, internod_dest, xx_dest,                      &
     &          itp_coef_dest, iflag_org_domain)
      call calypso_mpi_allreduce_int(iflag_org_domain, iflag_org_gl,    &
     &                               cast_long(internod_dest), MPI_MAX)
      icou = 0
      do inod = 1, internod_dest
        if(iflag_org_gl(inod) .le. 0) then
          icou = icou + 1
        else
          iflag_org_domain(inod) = iflag_org_gl(inod)
        end if
      end do
      if(my_rank .eq. 0) write(*,*) icou, 'points are missing yet'
!
            if (i_debug.ge.iflag_routine_msg)                           &
     &        write(*,*) 'giveup_to_search_element', ilevel, my_rank
            error_level_final = 2                                       &
     &       * gen_itp_p%search_error_level(gen_itp_p%num_search_times)
            call giveup_to_search_element(my_rank,                      &
     &          error_level_final, neib_nod%istack_next, gen_itp_p,     &
     &          org_mesh%node, org_mesh%ele,                            &
     &          nnod_dest, internod_dest, xx_dest,                      &
     &          itp_coef_dest, iflag_org_domain)
      call calypso_mpi_allreduce_int(iflag_org_domain, iflag_org_gl,    &
     &                               cast_long(internod_dest), MPI_MAX)
!
      icou = 0
      do inod = 1, internod_dest
        if(iflag_org_gl(inod) .le. 0) then
          icou = icou + 1
        else
          iflag_org_domain(inod) = iflag_org_gl(inod)
        end if
      end do
      if(my_rank .eq. 0) write(*,*) icou, 'points are given up'
!
      deallocate(iflag_org_gl)
          call deallocate_work_4_interpolate
          call deallocate_hex_2_tetra
!
      end subroutine const_interpolate_table_each
!
! ----------------------------------------------------------------------
!
      end module construct_interpolate_table
