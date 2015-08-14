!
!     module cal_sol_deltax_by_consist
!
      module cal_sol_deltax_by_consist
!
!     Written by H. Matsui on Nov., 2006
!     Modified by H. Matsui on Apr., 2008
!
      use m_precision
!
      implicit none
!
      integer(kind=kint), parameter :: nset = 1
      integer(kind=kint) :: itr_res, imonitor_solve
      private :: itr_res, nset, imonitor_solve
!
!      subroutine cal_sol_dx_by_consist(dx_nod)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sol_dx_by_consist(dx_nod, nd_dx)
!
      use calypso_mpi
      use m_array_for_send_recv
      use m_machine_parameter
      use m_nod_comm_table
      use m_ctl_params_4_gen_filter
      use m_geometry_data
      use m_finite_element_matrix
      use m_crs_consist_mass_mat
!
      use   solver
!
      integer (kind = kint), intent(in) :: nd_dx
      real(kind= kreal), intent(inout) :: dx_nod(node1%numnod)
!
      integer (kind = kint) :: inod
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
      do inod = 1, node1%numnod
        x_vec(inod) = ff(inod,nd_dx)
        b_vec(inod) = ff(inod,nd_dx)
      end do
!
!       write(50+my_rank,*) 'div_b'
!       do inod=1, node1%numnod
!         write(50+my_rank,*) b_vec(inod)
!       end do
!
      if (my_rank .eq. 0 ) then
          write(*,*) 'solver_in', nd_dx,                                &
     &              method_elesize, precond_elesize
      end if
!
      call solve(node1%internal_node, node1%numnod,                     &
     &             ntot_mass_l, ntot_mass_u,                            &
     &             aiccg_mass(im_mass_d), aiccg_mass(im_mass_l),        &
     &             istack_mass_l, item_mass_l, aiccg_mass(im_mass_u),   &
     &             istack_mass_u, item_mass_u, b_vec(1), x_vec(1),      &
     &             nset, nod_comm%num_neib, nod_comm%id_neib,           &
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
      do inod = 1, node1%numnod
        dx_nod(inod) = x_vec(inod)
      end do
!
      end subroutine cal_sol_dx_by_consist
!
!  ---------------------------------------------------------------------
!
      end module cal_sol_deltax_by_consist
