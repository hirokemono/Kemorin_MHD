!
!     module cal_sol_deltax_by_consist
!
!     Written by H. Matsui on Nov., 2006
!     Modified by H. Matsui on Apr., 2008
!
!!      subroutine cal_sol_dx_by_consist                                &
!!     &         (nd_dx, node, nod_comm, tbl_crs, f_l, mass, dx_nod)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(CRS_matrix_connect), intent(in) :: tbl_crs
!!        type(finite_ele_mat_node), intent(in) :: f_l
!!        type(CRS_matrix), intent(inout) :: mass
!
!
      module cal_sol_deltax_by_consist
!
      use m_precision
      use m_machine_parameter
!
      use t_comm_table
      use t_geometry_data
      use t_finite_element_mat
      use t_crs_matrix
!
      implicit none
!
      integer(kind=kint), parameter :: nset = 1
      integer(kind=kint) :: itr_res, imonitor_solve
      private :: itr_res, nset, imonitor_solve
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sol_dx_by_consist                                  &
     &         (nd_dx, node, nod_comm, tbl_crs, f_l, mass, dx_nod)
!
      use calypso_mpi
      use m_ctl_params_4_gen_filter
      use m_array_for_send_recv
!
      use   solver
!
      integer (kind = kint), intent(in) :: nd_dx
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(finite_ele_mat_node), intent(in) :: f_l
!
      type(CRS_matrix), intent(inout) :: mass
      real(kind= kreal), intent(inout) :: dx_nod(node%numnod)
!
!      integer (kind = kint) :: inod
      integer(kind = kint) :: ierr
!
!
      call init_solver(ierr)
!
      INTARRAY(1) = itr_elesize
      REALARRAY(1) = eps_elesize
      REALARRAY(2) = sigma_diag_elesize
      REALARRAY(3) = sigma_elesize
!
!
      imonitor_solve = i_debug
!
!$omp parallel workshare
      x_vec(1:node%numnod) = f_l%ff(1:node%numnod,nd_dx)
      b_vec(1:node%numnod) = f_l%ff(1:node%numnod,nd_dx)
!$omp end parallel workshare
!
!       write(50+my_rank,*) 'div_b'
!       do inod=1, node%numnod
!         write(50+my_rank,*) b_vec(inod)
!       end do
!
      if (my_rank .eq. 0 ) then
          write(*,*) 'solver_in', nd_dx,                                &
     &              method_elesize, precond_elesize
      end if
!
      call solve(node%internal_node, node%numnod,                       &
     &             tbl_crs%ntot_l, tbl_crs%ntot_u, mass%D_crs,          &
     &             mass%AL_crs, tbl_crs%istack_l, tbl_crs%item_l,       &
     &             mass%AU_crs, tbl_crs%istack_u, tbl_crs%item_u,       &
     &             b_vec(1), x_vec(1), nset,                            &
     &             nod_comm%num_neib, nod_comm%id_neib,                 &
     &             nod_comm%istack_import, nod_comm%item_import,        &
     &             nod_comm%istack_export, nod_comm%item_export,        &
     &             itr_res, imonitor_solve,                             &
     &             method_elesize, precond_elesize,                     &
     &             INTARRAY, REALARRAY )
!
      if (my_rank .eq. 0 ) then
        write(*,*) ' iteration finish:', itr_res
      end if
!
!$omp parallel workshare
      dx_nod(1:node%numnod) = x_vec(1:node%numnod)
!$omp end parallel workshare
!
      end subroutine cal_sol_dx_by_consist
!
!  ---------------------------------------------------------------------
!
      end module cal_sol_deltax_by_consist
